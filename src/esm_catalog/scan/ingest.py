"""The scan orchestrator: an experiment's output → a stac-geoparquet catalog.

``scan_experiment`` ties the scan layers together::

    source_experiment   -> ExperimentMetadata (+ run span, for fx items)
    source_files         -> the run's OutputFiles (path, component, stream, role, md5)
    _triage             -> which files need a real read vs a path-facet shortcut
    parallel_map(read)  -> FileMetadata per file (in worker processes)
    make_item           -> a single-asset STAC Item per file (in this process;
                           reconciled into one growing Item per (component,
                           stream) at push time, not here -- see item.py)
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
    stream_key,
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
        # stream. Try path-facet extraction first, same resolution _triage
        # uses -- so a later file of this stream, shortcut via the facet
        # extractor, agrees with this (the frozen, actually-read) file on the
        # stream identity, rather than each independently guessing. Only
        # when no extractor claims it does this fall back to the reader's
        # own primary variable, the closest thing to a stream identity a raw
        # filesystem walk can otherwise offer.
        if output_file.stream:
            file_metadata.stream = output_file.stream
        else:
            facets = _try_path_facets(output_file)
            file_metadata.stream = facets[0] if facets else file_metadata.variable
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


def _try_path_facets(output_file: OutputFile) -> Optional[tuple[str, datetime]]:
    """``(stream, start)`` recovered from *output_file*'s path alone, or ``None``.

    Thin wrapper around the path-facet plugin manager -- see
    :mod:`esm_catalog.scan.path_facets`. Pure and cheap (no I/O): safe to call
    for every candidate during triage, not just ones that end up shortcut, and
    from a worker process (see :func:`_read_output_file`).
    """
    return get_path_facet_plugin_manager().hook.extract_path_facets(
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
    output_file: OutputFile, template: FileMetadata, stream: str, start: datetime
) -> FileMetadata:
    """*output_file*'s metadata, reusing a frozen stream's schema.

    Confirmed live: once a stream's variables/dims/geometry/format are known
    from its first file, every later file of that stream shares them --
    reading each one again just to rediscover the same facts is the actual
    cost this whole mechanism (see :func:`_triage`) exists to cut. Only the
    per-file datetime, taken from the path (:func:`_try_path_facets`), is
    genuinely new; the period's end is derived from the frozen frequency.
    *stream* is the path-resolved one (not ``output_file.stream``, which is
    ``None`` for a walked/undeclared file), matching what the frozen file's
    own real read was made to agree with -- see :func:`_read_output_file`.
    """
    synthesized = template.model_copy(deep=True)
    synthesized.component = output_file.component
    synthesized.role = output_file.role
    synthesized.category = output_file.category
    synthesized.stream = stream
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
    facet_candidates: list[tuple[OutputFile, str, datetime]]
    """Files a path-facet extractor claimed, paired with the resolved stream
    and extracted start datetime -- finalized only after their stream's
    first file has actually been read (see ``scan_experiment``)."""
    stream_first_seen: dict[tuple[Optional[str], Optional[str]], OutputFile]
    """``(component, stream)`` -> the file whose real read result becomes
    that stream's frozen schema. *stream* here is the resolved one from
    path-facet extraction when available (see :func:`_triage`), not
    necessarily ``output_file.stream`` -- a walked/undeclared file has no
    stream of its own until either a read or a path-facet match supplies
    one."""
    revalidation_paths: set[str]
    """Paths of ``must_read`` entries that are periodic trust-but-verify
    checks (not a stream's first file, not a facet-extraction fallback) --
    their real read result is compared against the frozen schema."""


def _triage(
    todo: list[OutputFile],
    revalidate_every: int,
    persisted_schema: dict[tuple[Optional[str], Optional[str]], FileMetadata],
    persisted_occurrences: dict[str, int],
) -> _Triage:
    """Split *todo* into what genuinely needs reading and what a path-facet
    extractor can shortcut, without touching the filesystem.

    Path-facet extraction is tried for every file up front, not just repeats
    -- confirmed live: a real experiment's declared ``outdata_targets`` can
    be entirely stale, meaning every file is discovered through the
    undeclared filesystem walk and carries no stream identity of its own
    until a facet extractor (or a real read) supplies one.

    A stream's first occurrence needs a real read only if nothing is known
    about it yet -- *including* from a previous scan (``persisted_schema``,
    ``<exp_root>/catalog/esm-catalog.json``'s ``schema_by_stream``): an
    incrementally-growing experiment that has already been scanned once
    should not need to re-establish the same handful of streams' schemas on
    every later scan. Once a stream is known (this run or a prior one),
    every ``revalidate_every``-th occurrence -- counted in
    ``persisted_occurrences``, so the cadence survives across scans too --
    is still read for real, a trust-but-verify check against schema drift
    (a resolution change, a variable added mid-run). Everything else tries
    the path-facet shortcut, falling back to a real read when no extractor
    claims it (or claims it without full enough confidence to name a
    stream).
    """
    stream_first_seen: dict[tuple[Optional[str], Optional[str]], OutputFile] = {}
    seen_this_run: set[tuple[Optional[str], Optional[str]]] = set()
    must_read: list[OutputFile] = []
    facet_candidates: list[tuple[OutputFile, str, datetime]] = []
    revalidation_paths: set[str] = set()
    for output_file in todo:
        facets = _try_path_facets(output_file)
        stream, start = facets if facets is not None else (output_file.stream, None)
        key = (output_file.component, stream)
        truly_unknown = key not in seen_this_run and key not in persisted_schema
        seen_this_run.add(key)
        skey = stream_key(*key)
        # Counted even for the very first occurrence (matching the prior
        # in-run-only counter's semantics): a checkpoint fires at an
        # absolute occurrence count, not "count since it became known".
        count = persisted_occurrences.get(skey, 0) + 1
        persisted_occurrences[skey] = count
        if truly_unknown:
            stream_first_seen[key] = output_file
            must_read.append(output_file)
            continue
        if revalidate_every and count % revalidate_every == 0:
            must_read.append(output_file)
            revalidation_paths.add(str(output_file.path))
            continue
        if start is None:
            must_read.append(output_file)
            continue
        facet_candidates.append((output_file, stream, start))
    return _Triage(must_read, facet_candidates, stream_first_seen, revalidation_paths)


def ingest_single_file(
    output_file: OutputFile, state: WorkspaceState, *, force_read: bool
) -> _ReadResult:
    """Ingest one file outside a full scan -- the shared engine behind the
    ``esm-catalog add`` and ``validate`` CLI commands (a full ``scan`` is the
    only bulk path; these are the single-file primitives it is itself built
    from, per the file's own item id -- so there is exactly one
    implementation of "ingest a file", not a separate one for the CLI
    commands vs the batch scan loop).

    *output_file* must already carry both ``component`` and ``stream`` --
    unlike a full scan's undeclared/walked files, a single-file operation's
    caller always knows both from the item id it was given (``add``/
    ``validate <item> <file>`` -- see :func:`~esm_catalog.item._build_id`),
    so there is no path-facet-based stream *recovery* to do here, only
    (optionally) a datetime shortcut.

    *force_read* always performs a real read (``validate``'s contract: an
    authoritative, current check, regardless of any cached schema).
    Otherwise (``add``), an already-cached schema for this
    ``(component, stream)`` is reused via the path-facet datetime shortcut,
    falling back to a real read only when nothing is cached yet (this
    stream's genuine first asset) or the path-facet extractor cannot
    confidently confirm *this* file belongs to the expected stream.

    A successful real read refreshes ``state.schema_by_stream`` in place
    (mutated, not returned -- the caller is responsible for
    :func:`~esm_catalog.scan.workspace.save_state` once it is done writing
    the resulting shard). ``state.occurrences_since_check`` is deliberately
    never touched here -- see the module docstring on
    :class:`~esm_catalog.scan.workspace.WorkspaceState`: a one-off,
    human-driven single-file operation has no "occurrence count" to advance
    or trip a revalidation checkpoint from.
    """
    key = (output_file.component, output_file.stream)
    skey = stream_key(*key)

    if not force_read:
        template = state.schema_by_stream.get(skey)
        if template is not None:
            facets = _try_path_facets(output_file)
            if facets is not None and facets[0] == output_file.stream:
                _, start = facets
                return _ReadResult(
                    output_file,
                    _synthesize_from_template(
                        output_file, template, output_file.stream, start
                    ),
                    None,
                )

    result = _read_output_file(output_file)
    if result.file_metadata is not None:
        resolved_key = stream_key(
            result.output_file.component, result.file_metadata.stream
        )
        state.schema_by_stream[resolved_key] = result.file_metadata
    return result


def _variable_names(metadata: FileMetadata) -> set:
    return {v.name for v in metadata.variables}


def _warn_on_schema_drift(
    result: _ReadResult, frozen_schema: dict, shortcut_keys: set
) -> None:
    """Compare a revalidation checkpoint's real read against its stream's
    frozen schema; log, never raise -- the fresh read is used for this item
    regardless, this is purely an operator signal that later files of the
    stream may now be getting a stale schema via the path-facet shortcut.

    *shortcut_keys* -- ``(component, stream)`` pairs with at least one real
    path-facet-shortcut candidate this scan -- gates the check: a stream no
    file ever actually shortcuts (confirmed live: every restart stream,
    since sourcing gives every restart file the same fixed stream="restart"
    regardless of category, which no path-facet template can ever resolve
    to) has nothing to go stale, so comparing its real reads against each
    other is just noise, not a genuine drift signal.
    """
    if result.file_metadata is None:
        return
    key = (result.output_file.component, result.file_metadata.stream)
    if key not in shortcut_keys:
        return
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
        file, this scan or a previous one -- see ``schema_by_stream`` on
        :class:`~esm_catalog.scan.workspace.WorkspaceState`), every
        this-many-th later occurrence of that stream is still read for real
        rather than shortcut via a path-facet datetime -- catches schema
        drift a shortcut would otherwise mask forever. The occurrence count
        is itself persisted (``occurrences_since_check``), so the cadence
        holds across scans, not just within one. ``0`` disables it, trusting
        the frozen schema indefinitely. Defaults to :data:`_REVALIDATE_EVERY`
        (``ESM_CATALOG_REVALIDATE_EVERY``).
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
    persisted_schema: dict[tuple[Optional[str], Optional[str]], FileMetadata] = {
        tuple(skey.split("|", 1)): metadata
        for skey, metadata in state.schema_by_stream.items()
    }

    todo = [file for file in files if str(file.path) not in state.scanned]
    # Schema (variables/dims/geometry/format) is a stream-level fact once
    # Item = (component, stream) -- confirmed live: re-deriving it file by
    # file, for tens of thousands of files of the same handful of streams,
    # is the actual bulk of scan cost. Only a stream's first-ever occurrence
    # (this scan or a previous one -- persisted_schema) plus periodic
    # revalidation checkpoints need a real read; everything else tries a
    # path-facet datetime and reuses the frozen schema.
    triage = _triage(
        todo, revalidate_every, persisted_schema, state.occurrences_since_check
    )
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
    shortcut_keys = {
        (of.component, stream) for of, stream, _ in triage.facet_candidates
    }
    # Drift-check a revalidation checkpoint's fresh read against what was
    # trusted *before* this run touched it (persisted_schema) -- compare
    # against the about-to-be-refreshed frozen_schema built below instead,
    # and a checkpoint's own new read would trivially match itself, silently
    # defeating the whole check.
    for result in must_results:
        if str(result.output_file.path) in triage.revalidation_paths:
            _warn_on_schema_drift(result, persisted_schema, shortcut_keys)

    # Refresh (not just first-establish): any must_read result that
    # succeeded -- a stream's genuine first-ever read, a revalidation
    # checkpoint, or a one-off fallback when path-facet couldn't name a
    # datetime -- is a real, current read of that stream and updates the
    # trusted schema (and resets its persisted revalidation countdown)
    # accordingly, not just the narrower first-occurrence case.
    frozen_schema: dict = dict(persisted_schema)
    for result in must_results:
        if result.file_metadata is None:
            continue
        key = (result.output_file.component, result.file_metadata.stream)
        frozen_schema[key] = result.file_metadata
        state.occurrences_since_check[stream_key(*key)] = 0

    synthesized_results = []
    for output_file, stream, start in triage.facet_candidates:
        key = (output_file.component, stream)
        template = frozen_schema.get(key)
        if template is None:
            # The stream's first file failed or was unsupported -- nothing
            # to synthesize from. Rare; falls back to a real (serial) read.
            result = _read_output_file(output_file)
        else:
            result = _ReadResult(
                output_file,
                _synthesize_from_template(output_file, template, stream, start),
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
    # Persist this run's (re-)established schemas so the *next* scan -- or a
    # single-file `add`/`validate` -- can shortcut a stream from its very
    # first occurrence, not just within this one run.
    for key, metadata in frozen_schema.items():
        state.schema_by_stream[stream_key(*key)] = metadata
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
