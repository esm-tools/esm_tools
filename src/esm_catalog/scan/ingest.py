"""The scan orchestrator: an experiment's output → a stac-geoparquet catalog.

``scan_experiment`` ties the scan layers together::

    source_experiment   -> ExperimentMetadata (+ run span, for fx items)
    source_files         -> the run's OutputFiles (path, component, stream, role, md5)
    _triage             -> which files need a real read vs a path-facet shortcut
    parallel_map(read)  -> FileMetadata per file (in worker processes)
    make_item           -> a STAC Item per file (in this process)
    write_shard         -> <expid>_stac_<runstamp>.parquet (new) + <expid>_stac_fx.parquet (rewritten)
    make_collection     -> collection.json (extent widened over all items)

Reading runs in worker processes (a bad file is caught and collected, never
sinks the scan); item-building and shard-writing run here. Incremental: a file
whose recorded fingerprint is unchanged is skipped; time-invariant (fx) files
are never recorded, so they are re-read every scan and the fx shard tracks the
current run span (which grows when a run is extended). Schema (variables/
dims/geometry/format) is a stream-level fact, not a per-file one: only a
stream's first file (plus periodic revalidation checkpoints, see
``revalidate_every``) is actually read -- every other file of an
already-frozen stream tries a :mod:`~esm_catalog.scan.path_facets` extractor
for its datetime and reuses the frozen schema, skipping the read entirely.
"""

from __future__ import annotations

import json
import os
from collections import defaultdict
from dataclasses import dataclass
from datetime import datetime
from typing import Callable, Optional

import pandas as pd
from loguru import logger
from pandas.tseries import offsets as pd_offsets
from upath import UPath

from esm_catalog.collection import make_collection, update_extent
from esm_catalog.item import FX_FREQUENCY, make_item
from esm_catalog.models import ExperimentMetadata
from esm_catalog.scan.format import UnknownFormatError, detect
from esm_catalog.scan.parallel import parallel_map
from esm_catalog.scan.path_facets import get_path_facet_plugin_manager
from esm_catalog.scan.reader import UnsupportedContentError, reader_for
from esm_catalog.scan.sourcing import _load_run_cfgs, source_experiment, source_files
from esm_catalog.scan.types import (
    OutputFile,
    ProgressEvent,
    RunStamp,
    ScanFailure,
    ScanPhase,
    ScanReport,
)
from esm_catalog.scan.workspace import (
    QUERYABLES_FILENAME,
    WorkspaceState,
    catalog_dir,
    load_state,
    save_state,
)
from esm_catalog.storage.geoparquet import fx_shard_name, ts_shard_name, write_shard
from esm_catalog.types import FileMetadata

FAILURES_FILENAME = "scan-failures.json"
"""Sidecar listing files that failed to read (path + error), for follow-up."""

_READ_TIMEOUT_SECONDS = int(os.environ.get("ESM_CATALOG_READ_TIMEOUT", "300"))
"""Per-file read budget (seconds) -- e.g. cfgrib reindexing a huge, unusually
message-dense GRIB file (confirmed live: an untidied ECHAM tracer-forcing file
sitting in a run's ephemeral work dir with no '.codes' sidecar, forcing the
slow generic path) can otherwise stall an entire multi-hour scan on one file,
which breaks the "one bad file must not sink the scan" contract. Override via
``ESM_CATALOG_READ_TIMEOUT`` for unusually large legitimate files."""

_REVALIDATE_EVERY = int(os.environ.get("ESM_CATALOG_REVALIDATE_EVERY", "20"))
"""How often a schema-frozen stream's path-facet shortcut is skipped in favour
of a real read, to catch schema drift (see :func:`_triage`). ``0`` disables
periodic revalidation entirely (trust the frozen schema forever). Override via
``ESM_CATALOG_REVALIDATE_EVERY``."""

_PERIOD_OFFSET = {
    "yr": pd_offsets.YearBegin(1),
    "mon": pd_offsets.MonthBegin(1),
    "day": pd_offsets.Day(1),
    "6hr": pd.Timedelta(hours=6),
    "3hr": pd.Timedelta(hours=3),
    "1hr": pd.Timedelta(hours=1),
}
"""A CMIP frequency code -> the span of one reporting period, for computing a
path-facet-synthesized file's ``datetime_end`` from its ``datetime_start``
(see :func:`_synthesize_from_template`). Missing here (``subhr``, an ``NNhr``
code, or no frequency at all) -- the safe fallback is a zero-width range
(``datetime_end = datetime_start``), same as :mod:`netcdf.frequency`'s own
"we do not guess" stance for anything it cannot name precisely."""


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
    The *timeout* on a pathologically slow read (e.g. cfgrib stuck reindexing
    a file over contended NFS) is enforced by the caller
    (:func:`~esm_catalog.scan.parallel.parallel_map`'s ``timeout``), not here
    -- killing this function's own process mid-C-call was tried first and
    rejected (see :data:`_READ_TIMEOUT_SECONDS`).
    """
    try:
        file_format = detect(output_file.path)
        reader = reader_for(file_format)
    except (UnknownFormatError, LookupError):
        return _ReadResult(output_file, None, None, unsupported=True)
    try:
        file_metadata = FileMetadata.model_validate(reader.read(output_file.path))
        file_metadata.component = output_file.component
        file_metadata.role = output_file.role
        file_metadata.category = output_file.category
        # A walked outdata file (no outdata_targets entry) carries no declared
        # stream -- fall back to its own primary variable, the closest thing
        # to a stream identity a raw filesystem walk can offer.
        file_metadata.stream = output_file.stream or file_metadata.variable
        return _ReadResult(output_file, file_metadata, None)
    except UnsupportedContentError:
        return _ReadResult(output_file, None, None, unsupported=True)
    except (
        Exception
    ) as exc:  # noqa: BLE001 — deliberately never let one file abort the scan
        return _ReadResult(
            output_file, None, ScanFailure(str(output_file.path), repr(exc))
        )


def _on_read_timeout(output_file: OutputFile) -> _ReadResult:
    """The result recorded when *output_file* exceeds :data:`_READ_TIMEOUT_SECONDS`.

    Passed to :func:`~esm_catalog.scan.parallel.parallel_map` as ``on_timeout``:
    it kills and replaces the whole worker pool rather than the file's own
    process. Interrupting the read via a Python-level signal inside the
    worker (an earlier version of this used ``signal.alarm``) does not work:
    eccodes is not signal-safe, and forcing an exception mid-C-call left its
    internal GRIB handle table corrupted, cascading into a wall of unrelated
    "Exception ignored in Message.__del__" errors -- and the worker, no
    longer trustworthy, could still hang afterwards (confirmed live: stuck on
    the same file 19 minutes past its 300s alarm). Killing the whole pool
    from the driver, outside any process that might be mid-C-call, is the
    only thing that is actually safe.
    """
    return _ReadResult(
        output_file,
        None,
        ScanFailure(
            str(output_file.path), f"read timed out after {_READ_TIMEOUT_SECONDS}s"
        ),
    )


def _try_path_facets(output_file: OutputFile) -> Optional[datetime]:
    """*output_file*'s start datetime from its path alone, or ``None``.

    Thin wrapper around the path-facet plugin manager -- see
    :mod:`esm_catalog.scan.path_facets`. Pure and cheap (no I/O): safe to call
    for every candidate during triage, not just ones that end up shortcut.
    """
    return get_path_facet_plugin_manager().hook.extract_start_datetime(
        path=output_file.path,
        component=output_file.component,
        stream=output_file.stream,
    )


def _period_end(start: datetime, frequency: Optional[str]) -> datetime:
    """The last instant of the reporting period starting at *start*.

    Falls back to *start* itself (a zero-width range) for a frequency
    :data:`_PERIOD_OFFSET` does not cover, or no frequency at all -- same "do
    not guess past what is actually known" stance as the rest of the scan
    layer.
    """
    offset = _PERIOD_OFFSET.get(frequency or "")
    if offset is None:
        return start
    return (pd.Timestamp(start) + offset - pd.Timedelta(seconds=1)).to_pydatetime()


def _synthesize_from_template(
    output_file: OutputFile, template: FileMetadata, start: datetime
) -> FileMetadata:
    """*output_file*'s metadata, reusing a frozen stream's schema.

    Confirmed live: once a stream's variables/dims/geometry/format are known
    from its first file, every later file of that stream shares them --
    reading each one again just to rediscover the same facts is the actual
    cost this whole mechanism (see :func:`_triage`) exists to cut. Only the
    per-file datetime, taken from the path (:func:`_try_path_facets`), is
    genuinely new; the period's end is derived from the frozen frequency.
    """
    synthesized = template.model_copy(deep=True)
    synthesized.component = output_file.component
    synthesized.role = output_file.role
    synthesized.category = output_file.category
    synthesized.stream = output_file.stream or synthesized.variable
    synthesized.datetime_start = start
    synthesized.datetime_end = _period_end(start, template.frequency)
    synthesized.datetime_str = start.strftime("%Y%m")
    return synthesized


@dataclass(frozen=True)
class _Triage:
    """The reading phase's work plan: which files need a real read, and which
    can be synthesized from an already-frozen stream schema + a path-derived
    datetime. See :func:`_triage`."""

    must_read: list[OutputFile]
    """In original order: first-of-stream files, revalidation checkpoints,
    and any file path-facet extraction could not claim."""
    facet_candidates: list[tuple[OutputFile, datetime]]
    """Files a path-facet extractor claimed, paired with the extracted start
    datetime -- finalized only after their stream's first file has actually
    been read (see ``scan_experiment``)."""
    stream_first_seen: dict[tuple[Optional[str], Optional[str]], OutputFile]
    """``(component, stream)`` -> the file whose real read result becomes
    that stream's frozen schema."""
    revalidation_paths: set[str]
    """Paths of ``must_read`` entries that are periodic trust-but-verify
    checks (not a stream's first file, not a facet-extraction fallback) --
    their real read result is compared against the frozen schema."""


def _triage(todo: list[OutputFile], revalidate_every: int) -> _Triage:
    """Split *todo* into what genuinely needs reading and what a path-facet
    extractor can shortcut, without touching the filesystem.

    A stream's first file always needs a real read (nothing to freeze from
    yet). After that, every ``revalidate_every``-th file of the same stream
    is still read for real -- a trust-but-verify check against schema drift
    (a resolution change, a variable added mid-run) -- everything else tries
    the path-facet shortcut, falling back to a real read when no extractor
    claims it.
    """
    stream_first_seen: dict[tuple[Optional[str], Optional[str]], OutputFile] = {}
    occurrences: dict[tuple[Optional[str], Optional[str]], int] = defaultdict(int)
    must_read: list[OutputFile] = []
    facet_candidates: list[tuple[OutputFile, datetime]] = []
    revalidation_paths: set[str] = set()
    for output_file in todo:
        key = (output_file.component, output_file.stream)
        occurrences[key] += 1
        if key not in stream_first_seen:
            stream_first_seen[key] = output_file
            must_read.append(output_file)
            continue
        if revalidate_every and occurrences[key] % revalidate_every == 0:
            must_read.append(output_file)
            revalidation_paths.add(str(output_file.path))
            continue
        start = _try_path_facets(output_file)
        if start is None:
            must_read.append(output_file)
            continue
        facet_candidates.append((output_file, start))
    return _Triage(must_read, facet_candidates, stream_first_seen, revalidation_paths)


def _variable_names(metadata: FileMetadata) -> set:
    return {v.name for v in metadata.variables}


def _warn_on_schema_drift(result: _ReadResult, frozen_schema: dict) -> None:
    """Compare a revalidation checkpoint's real read against its stream's
    frozen schema; log, never raise -- the fresh read is used for this item
    regardless, this is purely an operator signal that later files of the
    stream may now be getting a stale schema via the path-facet shortcut."""
    if result.file_metadata is None:
        return
    key = (result.output_file.component, result.output_file.stream)
    frozen = frozen_schema.get(key)
    if frozen is None:
        return
    if _variable_names(result.file_metadata) != _variable_names(frozen):
        logger.warning(
            "{}: variables differ from {}'s frozen schema ({} vs {}) -- "
            "later files of this stream may be catalogued with a stale "
            "schema via the path-facet shortcut until the next revalidation",
            result.output_file.path,
            key,
            sorted(_variable_names(frozen)),
            sorted(_variable_names(result.file_metadata)),
        )


def scan_experiment(
    exp_root: UPath,
    *,
    catalog: Optional[UPath] = None,
    distributed: bool = False,
    scheduler: Optional[str] = None,
    jobs: Optional[int] = None,
    strict: bool = False,
    revalidate_every: int = _REVALIDATE_EVERY,
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
    revalidate_every : int, optional
        Once a ``(component, stream)``'s schema is frozen (from its first
        file), every this-many-th later file of that stream is still read for
        real rather than shortcut via a path-facet datetime -- catches schema
        drift a shortcut would otherwise mask forever. ``0`` disables it,
        trusting the frozen schema for the rest of the scan. Defaults to
        :data:`_REVALIDATE_EVERY` (``ESM_CATALOG_REVALIDATE_EVERY``).
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
    run_cfgs = _load_run_cfgs(
        exp_root, distributed=distributed, scheduler=scheduler, jobs=jobs
    )
    exp_metadata = source_experiment(exp_root, run_cfgs=run_cfgs)
    files = source_files(
        exp_root,
        on_file=lambda n: _emit("sourcing", detail=f"{n} files found"),
        run_cfgs=run_cfgs,
    )
    state = load_state(catalog) or WorkspaceState(
        experiment_id=exp_metadata.experiment_id
    )

    todo = [file for file in files if str(file.path) not in state.scanned]
    # Schema (variables/dims/geometry/format) is a stream-level fact once
    # Item = (component, stream) -- confirmed live: re-deriving it file by
    # file, for tens of thousands of files of the same handful of streams,
    # is the actual bulk of scan cost. Only a stream's first file (plus
    # periodic revalidation checkpoints) needs a real read; everything else
    # tries a path-facet datetime and reuses the frozen schema.
    triage = _triage(todo, revalidate_every)
    must_read = triage.must_read
    read_count = 0
    _emit("reading", 0, len(todo))

    def _tick(output_file: OutputFile) -> None:
        nonlocal read_count
        read_count += 1
        # The process-pool backend collects strictly in submission order
        # (chunksize=1, so each file is its own future): by the time this
        # fires, `output_file` already finished -- if progress then stalls,
        # showing its name is misleading (confirmed live: a scan frozen at
        # "reading <name>" actually meant that file had already read fine,
        # and the next uncollected item was the one a worker was still stuck
        # on). Look ahead to the item that's actually in flight instead. The
        # Dask backend instead calls on_item via as_completed (real
        # completion order, not input order), so its output_file is already
        # the right one to show.
        if not distributed and read_count < len(must_read):
            detail = must_read[read_count].path.name
        else:
            detail = output_file.path.name
        _emit("reading", read_count, len(todo), detail)

    must_results = parallel_map(
        must_read,
        _read_output_file,
        distributed=distributed,
        scheduler=scheduler,
        jobs=jobs,
        label="scan",
        on_item=_tick,
        initializer=worker_initializer,
        initargs=worker_initargs,
        # Dask has its own scheduling/retry story; this timeout is
        # process-pool-specific (kills and replaces that pool on overrun).
        timeout=None if distributed else _READ_TIMEOUT_SECONDS,
        on_timeout=_on_read_timeout,
    )
    result_by_path = {str(r.output_file.path): r for r in must_results}

    frozen_schema: dict = {}
    for key, first_file in triage.stream_first_seen.items():
        first_result = result_by_path[str(first_file.path)]
        if first_result.file_metadata is not None:
            frozen_schema[key] = first_result.file_metadata
    for result in must_results:
        if str(result.output_file.path) in triage.revalidation_paths:
            _warn_on_schema_drift(result, frozen_schema)

    synthesized_results = []
    for output_file, start in triage.facet_candidates:
        key = (output_file.component, output_file.stream)
        template = frozen_schema.get(key)
        if template is None:
            # The stream's first file failed or was unsupported -- nothing
            # to synthesize from. Rare; falls back to a real (serial) read.
            result = _read_output_file(output_file)
        else:
            result = _ReadResult(
                output_file,
                _synthesize_from_template(output_file, template, start),
                None,
            )
        synthesized_results.append(result)
        read_count += 1
        _emit("reading", read_count, len(todo), output_file.path.name)

    results = must_results + synthesized_results
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
        # namelist.py/paleo.py memoize their own per-experiment properties and
        # validate-once internally (see their apply_to_item hookimpls) -- this
        # loop no longer precomputes or tracks a validated_once flag itself.
        item = make_item(
            result.output_file.path,
            result.file_metadata,
            exp_metadata,
        )
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
    _write_queryables(catalog, exp_metadata)
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


def _write_queryables(catalog, exp_metadata: ExperimentMetadata) -> None:
    """Write (or clear) the queryables sidecar for this experiment's namelists.

    Its ``properties`` are the item-level ``nml__`` keys and their JSON types --
    the exact file ``pypgstac load-queryables`` consumes. ``push`` diffs it
    against the server's registered queryables to tell an operator which (if
    any) are new.
    """
    from esm_catalog.namelist import namelist_queryables

    path = catalog / QUERYABLES_FILENAME
    properties = namelist_queryables(exp_metadata.namelists_by_component)
    if not properties:
        if path.exists():
            path.unlink()
        return
    path.write_text(json.dumps({"properties": properties}, indent=2))


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
