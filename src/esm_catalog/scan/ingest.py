"""The scan orchestrator: an experiment's output → a stac-geoparquet catalog.

``scan_experiment`` ties the scan layers together::

    source_experiment   -> ExperimentMetadata (+ run span, for fx items)
    output_files        -> the run's OutputFiles (path, component, md5)
    parallel_map(read)  -> FileMetadata per file (in worker processes)
    make_item           -> a STAC Item per file (in this process)
    write_shard         -> <expid>_stac_<runstamp>.parquet (new) + <expid>_stac_fx.parquet (rewritten)
    make_collection     -> collection.json (extent widened over all items)

Reading runs in worker processes (a bad file is caught and collected, never
sinks the scan); item-building and shard-writing run here. Incremental: a file
whose recorded fingerprint is unchanged is skipped; time-invariant (fx) files
are never recorded, so they are re-read every scan and the fx shard tracks the
current run span (which grows when a run is extended).
"""

from __future__ import annotations

import json
from dataclasses import dataclass
from typing import Callable, Optional

from upath import UPath

from esm_catalog.collection import make_collection, update_extent
from esm_catalog.item import FX_FREQUENCY, make_item
from esm_catalog.models import ExperimentMetadata
from esm_catalog.scan.format import UnknownFormatError, detect
from esm_catalog.scan.parallel import parallel_map
from esm_catalog.scan.reader import UnsupportedContentError, reader_for
from esm_catalog.scan.readers import (
    netcdf as _netcdf,
)  # noqa: F401  (registers readers)
from esm_catalog.scan.sourcing import output_files, source_experiment
from esm_catalog.scan.types import (
    OutputFile,
    ProgressEvent,
    RunStamp,
    ScanFailure,
    ScanPhase,
    ScanReport,
)
from esm_catalog.scan.workspace import (
    WorkspaceState,
    catalog_dir,
    load_state,
    save_state,
)
from esm_catalog.storage.geoparquet import fx_shard_name, ts_shard_name, write_shard
from esm_catalog.types import FileMetadata

FAILURES_FILENAME = "scan-failures.json"
"""Sidecar listing files that failed to read (path + error), for follow-up."""


class ScanError(Exception):
    """Raised in ``--strict`` mode when any file failed to scan."""


@dataclass(frozen=True)
class _ReadResult:
    """One worker's outcome: metadata on success, else a failure or unsupported flag.

    A file whose format the scan cannot handle at all (unrecognised format, or a
    format with no registered reader) is neither read nor a failure: it is flagged
    ``unsupported`` and simply left out of the catalog.
    """

    output_file: OutputFile
    file_metadata: Optional[FileMetadata]
    failure: Optional[ScanFailure]
    unsupported: bool = False


def _read_output_file(output_file: OutputFile) -> _ReadResult:
    """Read one file to FileMetadata; runs in a worker process, so it is picklable.

    A file the scan simply cannot handle — an unrecognised format, or a format
    with no registered reader — is flagged ``unsupported`` (a clean skip, not a
    failure). Any other reader error is caught and returned as a
    :class:`ScanFailure`: a single bad file must not abort a multi-hour scan.
    """
    try:
        file_format = detect(output_file.path)
        reader = reader_for(file_format)
    except (UnknownFormatError, LookupError):
        return _ReadResult(output_file, None, None, unsupported=True)
    try:
        file_metadata = dict(reader.read(output_file.path))
        file_metadata["component"] = output_file.component
        return _ReadResult(output_file, file_metadata, None)
    except UnsupportedContentError:
        return _ReadResult(output_file, None, None, unsupported=True)
    except (
        Exception
    ) as exc:  # noqa: BLE001 — deliberately never let one file abort the scan
        return _ReadResult(
            output_file, None, ScanFailure(str(output_file.path), repr(exc))
        )


def scan_experiment(
    exp_root: UPath,
    *,
    catalog: Optional[UPath] = None,
    distributed: bool = False,
    scheduler: Optional[str] = None,
    jobs: Optional[int] = None,
    strict: bool = False,
    on_progress: Optional[Callable[[ProgressEvent], None]] = None,
    worker_initializer: Optional[Callable[..., None]] = None,
    worker_initargs: tuple = (),
) -> ScanReport:
    """Scan *exp_root*'s output into the catalog directory and return a report.

    Parameters
    ----------
    exp_root : UPath
        The experiment root (must hold a completed ESM-Tools run's config). May
        be remote (e.g. ``sftp://…``); the scan reads it through UPath.
    catalog : UPath, optional
        Where to write the catalog (shards, collection.json, state). Defaults to
        ``<exp_root>/catalog``; point it at a local path to scan a remote
        experiment without writing back over the network.
    distributed, scheduler, jobs
        Parallelism: process pool by default; Dask when *distributed* (attaching
        *scheduler* if given), with *jobs* workers.
    strict : bool
        Raise :class:`ScanError` if any file failed to read.
    on_progress : Callable or None, optional
        Called with a :class:`ProgressEvent` as the scan advances (sourcing ->
        reading, one per file -> writing). A UI-free hook: the CLI renders a
        spinner and bar from it; a library caller may ignore it.

    Returns
    -------
    ScanReport
        Counts of scanned / catalogued / skipped / unsupported files and the failures.
    """

    def _emit(phase: ScanPhase, current: int = 0, total: int = 0, detail: str = ""):
        if on_progress is not None:
            on_progress(ProgressEvent(phase, current, total, detail))

    if catalog is None:
        catalog = catalog_dir(exp_root)
    _emit("sourcing")
    exp_metadata = source_experiment(exp_root)
    files = output_files(
        exp_root, on_file=lambda n: _emit("sourcing", detail=f"{n} files found")
    )
    state = load_state(catalog) or WorkspaceState(
        experiment_id=exp_metadata.experiment_id
    )

    todo = [file for file in files if str(file.path) not in state.scanned]
    read_count = 0
    _emit("reading", 0, len(todo))

    def _tick(output_file: OutputFile) -> None:
        nonlocal read_count
        read_count += 1
        _emit("reading", read_count, len(todo), output_file.path.name)

    results = parallel_map(
        todo,
        _read_output_file,
        distributed=distributed,
        scheduler=scheduler,
        jobs=jobs,
        label="scan",
        on_item=_tick,
        initializer=worker_initializer,
        initargs=worker_initargs,
    )
    _emit("writing")

    collection = make_collection(exp_metadata)
    ts_items = []
    fx_items = []
    failures = []
    unsupported = 0
    for result in results:
        if result.unsupported:
            unsupported += 1
            continue
        if result.failure is not None:
            failures.append(result.failure)
            continue
        item = make_item(result.output_file.path, result.file_metadata, exp_metadata)
        update_extent(collection, item)
        if item.properties.get("frequency") == FX_FREQUENCY:
            fx_items.append(item)
        else:
            ts_items.append(item)
            state.scanned[str(result.output_file.path)] = _fingerprint(
                result.output_file
            )

    _write_catalog(
        catalog,
        collection,
        ts_items,
        fx_items,
        _run_stamp(exp_metadata),
        exp_metadata.experiment_id,
    )
    _write_failures(catalog, failures)
    save_state(catalog, state)

    if strict and failures:
        raise ScanError(
            f"{len(failures)} file(s) failed to scan; see {FAILURES_FILENAME}."
        )
    return ScanReport(
        scanned=len(todo),
        items=len(ts_items) + len(fx_items),
        skipped=len(files) - len(todo),
        unsupported=unsupported,
        failures=tuple(failures),
    )


def _write_catalog(
    catalog, collection, ts_items, fx_items, run_stamp, experiment_id
) -> None:
    """Write collection.json, the new ts shard, and the (rewritten) fx shard."""
    items_dir = catalog / "items"
    items_dir.mkdir(parents=True, exist_ok=True)
    if ts_items:
        write_shard(ts_items, items_dir / ts_shard_name(experiment_id, run_stamp))
    # The fx shard is rewritten every scan so its items carry the current run span.
    write_shard(fx_items, items_dir / fx_shard_name(experiment_id))
    collection_path = catalog / "collection.json"
    collection_path.write_text(json.dumps(collection.to_dict(), indent=2))


def _write_failures(catalog, failures) -> None:
    """Write (or clear) the failures sidecar."""
    path = catalog / FAILURES_FILENAME
    if not failures:
        if path.exists():
            path.unlink()
        return
    path.write_text(
        json.dumps([{"path": f.path, "error": f.error} for f in failures], indent=2)
    )


def _run_stamp(exp_metadata: ExperimentMetadata) -> RunStamp:
    """The ``YYYYMMDD-YYYYMMDD`` stamp naming this run span's ts shard."""
    start, end = exp_metadata.run_start, exp_metadata.run_end
    if start is None or end is None:
        return "unknown"
    return f"{start:%Y%m%d}-{end:%Y%m%d}"


def _fingerprint(output_file: OutputFile) -> str:
    """The incremental key for a time-varying file: its md5, else size+mtime."""
    if output_file.md5:
        return output_file.md5
    stat = output_file.path.stat()
    return f"{stat.st_size}-{getattr(stat, 'st_mtime_ns', stat.st_mtime)}"
