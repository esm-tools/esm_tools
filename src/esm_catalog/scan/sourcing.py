"""Source experiment metadata and output-file paths from an ESM-Tools run.

The catalog is built from what an ESM-Tools run *declared*, not from what a
filesystem walk happens to find. Every completed run writes a ``finished_config``
(one per run segment) under ``<exp_root>/config/`` recording the experiment's
identity, its scientific metadata, and the absolute target path of every output
file. This module reads those configs and turns them into the scan layer's fixed
contracts -- :class:`~esm_catalog.models.ExperimentMetadata` and
:class:`~esm_catalog.scan.types.OutputFile` -- so the config is the single,
authoritative discovery path.

Assumptions about the (loosely specified) finished_config schema, all handled
tolerantly with ``.get`` and defaults:

* ``general.expid`` holds the experiment id; the filename prefix is the fallback.
* A run segment's window comes from ``general`` run dates (``start_date`` /
  ``end_date``, or ``run_datestamp``), falling back to the ``YYYYMMDD-YYYYMMDD``
  suffix ESM-Tools appends to the filename. ``run_start`` / ``run_end`` on the
  returned metadata are the min / max across all segments (their union).
* Experiment metadata lives in ``general.metadata``: ``Description``, ``Authors``
  (-> :class:`Contact` list) and ``License``. A component's own ``metadata``
  describes the model, not the experiment, and is not read.
* The concrete output files (and their MD5s) come from the tidy-phase
  file-operations logs, the authoritative record of what the run produced. Each
  component's ``outdata_targets`` mapping holds glob *patterns*, used only as a
  fallback when tidy logs are absent.

``namelists_by_component`` is left empty here; populating it is out of scope for
this module.
"""

from __future__ import annotations

from dataclasses import dataclass
from datetime import date, datetime
from itertools import chain
from pathlib import Path
from typing import Any, Callable, Iterable, Optional

from pydantic import BaseModel, ConfigDict
from ruamel.yaml import YAML
from upath import UPath

from esm_catalog.models import Contact, ExperimentMetadata
from esm_catalog.paleo import PaleoConfig
from esm_catalog.scan.types import Md5, OutputFile, RunStamp
from esm_catalog.types import ComponentName, ExperimentId

FinishedConfigDoc = dict[str, Any]
"""A raw parsed finished_config: the ``general`` block plus per-component blocks.

The keys are dynamic (arbitrary component names), so this stays an untyped
mapping; typed slices (:class:`GeneralBlock`, :class:`MetadataBlock`) are parsed
out of it at the point of use.
"""

RunWindow = tuple[datetime, datetime]
"""A single run segment's (start, end) span."""


class GeneralBlock(BaseModel):
    """The ``general`` section of a finished_config, validated at the parse boundary.

    Fields are the keys the scan reads; every other key is preserved
    (``extra="allow"``). Values are kept as parsed (``Any``) so the date/paleo
    coercion helpers see exactly what the YAML supplied.
    """

    model_config = ConfigDict(extra="allow")

    expid: Any = None
    start_date: Any = None
    initial_date: Any = None
    current_date: Any = None
    end_date: Any = None
    final_date: Any = None
    run_datestamp: Any = None
    paleo: Any = None


class MetadataBlock(BaseModel):
    """A ``general.metadata`` block (config-supplied experiment metadata).

    Carries the keys the scan reads for identity and contacts; any other key is
    preserved (``extra="allow"``).
    """

    model_config = ConfigDict(extra="allow")

    Description: Any = None
    Authors: Any = None
    License: Any = None
    Institute: Any = None


@dataclass(frozen=True)
class TidyOutdataEntry:
    """One ``outdata`` entry from a tidy log: a produced file and its checksum."""

    component: ComponentName
    destination: str
    md5: Optional[Md5]


@dataclass(frozen=True)
class ComponentPath:
    """A component paired with one of its output paths (config target or walked)."""

    component: ComponentName
    path: UPath

_CONFIG_SUBDIR = "config"
_LOG_SUBDIR = "log"
_OUTDATA_SUBDIR = "outdata"
_FINISHED_CONFIG_GLOB = "*_finished_config.yaml*"
_TIDY_LOG_GLOB = "*_file_operations_tidy_*.yaml"
_FINISHED_CONFIG_MARKER = "_finished_config.yaml"

_START_DATE_KEYS = ("start_date", "initial_date", "current_date")
_END_DATE_KEYS = ("end_date", "final_date")
_RESTART_MARKERS = ("restart", "rerun")
_SIDECAR_SUFFIXES = (".codes", ".idx")


class SourcingError(Exception):
    """Raised when an experiment cannot be sourced from its ESM-Tools config."""


@dataclass(frozen=True)
class _Segment:
    """One run segment: its finished_config path and parsed document."""

    path: UPath
    doc: FinishedConfigDoc


def source_experiment(exp_root: UPath) -> ExperimentMetadata:
    """Build :class:`ExperimentMetadata` from an experiment's finished_config(s).

    Parameters
    ----------
    exp_root : UPath
        The experiment root directory, containing a ``config/`` subdirectory with
        one ``*_finished_config.yaml`` per run segment.

    Returns
    -------
    ExperimentMetadata
        Identity, description, license, contacts, paleo config, and the run span
        (min start / max end across segments). ``namelists_by_component`` is left
        empty (out of scope here).

    Raises
    ------
    SourcingError
        If no finished_config is found under ``<exp_root>/config``.
    """
    segments = _find_segments(exp_root)
    docs = [segment.doc for segment in segments]

    general_metadata = list(_general_metadata_blocks(docs))
    run_start, run_end = _run_span(segments)

    return ExperimentMetadata(
        experiment_id=_experiment_id(segments),
        experiment_path=Path(str(exp_root)),
        description=_first_value(general_metadata, "Description"),
        data_license=_first_value(general_metadata, "License"),
        contacts=_contacts(general_metadata),
        paleo_config=_paleo_config(docs),
        run_start=run_start,
        run_end=run_end,
    )


def _on_exp_fs(exp_root: UPath, destination: str) -> UPath:
    """Re-anchor a recorded destination path onto exp_root's filesystem.

    Tidy logs and ``outdata_targets`` record destinations as they were seen on
    the machine that produced the run -- an absolute path, or a full URL. When
    exp_root is remote (e.g. ``sftp://``) the bare path must be reinterpreted on
    that same filesystem: its host and credentials live in exp_root's storage
    options, not in the recorded string, so ``UPath(destination)`` alone would
    resolve to the local disk. For a local exp_root this is a no-op.
    """
    return UPath(
        UPath(destination).path,
        protocol=exp_root.protocol,
        **exp_root.storage_options,
    )


def _walk_outdata(
    exp_root: UPath, on_file: Optional[Callable[[int], None]] = None
) -> Iterable[ComponentPath]:
    """Yield a :class:`ComponentPath` for every real file under ``<exp_root>/outdata``.

    A filesystem walk reports what the run actually wrote, which is the only
    reliable source when the tidy manifest is absent and the config's
    ``outdata_targets`` are missing or theoretical. Component is the immediate
    subdirectory of ``outdata/``.

    The walk streams through the filesystem's own ``walk`` (one listing per
    directory, files only -- no per-entry stat), yielding as it descends rather
    than materialising the whole tree first: essential over a remote root with
    thousands of files. *on_file* is called with the running file count so a
    caller can show progress during the (potentially slow) listing.
    """
    outdata = exp_root / _OUTDATA_SUBDIR
    if not outdata.exists():
        return
    fs = outdata.fs
    base = outdata.path.rstrip("/")
    count = 0
    for root, _dirs, files in fs.walk(base):
        rel = root[len(base) :].lstrip("/")
        if not rel:
            continue  # files sitting directly in outdata/ have no component
        component = rel.split("/", 1)[0]
        for name in sorted(files):
            count += 1
            if on_file is not None:
                on_file(count)
            yield ComponentPath(
                component=component, path=_on_exp_fs(exp_root, f"{root}/{name}")
            )


def output_files(
    exp_root: UPath, on_file: Optional[Callable[[int], None]] = None
) -> list[OutputFile]:
    """Return the experiment's concrete output files, one per produced file.

    The authoritative source is the tidy-phase logs: they list every file the run
    actually produced, with its real destination path and MD5, keyed by component.
    When no tidy manifest exists, fall back to the config's ``outdata_targets``
    that resolve to real files, then walk ``outdata/`` for anything the config did
    not declare -- a run with non-default output writes files the config never
    names, and some components (e.g. fesom) declare no targets at all, so the walk
    is what actually finds them. Restart, rerun, and non-data sidecar files
    (GRIB ``.codes``/``.idx``) are excluded. MD5 is left ``None`` when unknown.

    Parameters
    ----------
    exp_root : UPath
        The experiment root directory.
    on_file : Callable or None, optional
        Called with the running file count while walking ``outdata/`` -- a
        progress hook for the slow remote-listing case.

    Returns
    -------
    list of OutputFile
        Deduplicated by path, in first-seen order.

    Raises
    ------
    SourcingError
        If no finished_config is found under ``<exp_root>/config``.
    """
    segments = _find_segments(exp_root)  # validates the run completed; may raise

    tidy_entries = list(_tidy_outdata(exp_root))
    if tidy_entries:
        candidates: Iterable[OutputFile] = (
            OutputFile(
                path=_on_exp_fs(exp_root, entry.destination),
                component=entry.component,
                md5=entry.md5,
            )
            for entry in tidy_entries
        )
    else:
        # No tidy manifest: trust config outdata_targets that resolve to real
        # files, then walk outdata/ for whatever the config missed.
        configured = (
            ComponentPath(target.component, _on_exp_fs(exp_root, str(target.path)))
            for segment in segments
            for target in _outdata_targets(segment.doc)
        )
        candidates = (
            OutputFile(path=cp.path, component=cp.component, md5=None)
            for cp in chain(
                (cp for cp in configured if cp.path.exists()),
                _walk_outdata(exp_root, on_file),
            )
        )

    files: list[OutputFile] = []
    seen: set[str] = set()
    for candidate in candidates:
        key = str(candidate.path)
        if key in seen or _is_restart(candidate.path) or _is_sidecar(candidate.path):
            continue
        seen.add(key)
        files.append(candidate)
    return files


def _find_segments(exp_root: UPath) -> list[_Segment]:
    """Load every finished_config under ``<exp_root>/config``, sorted chronologically.

    The finished_config filenames sort chronologically because their
    ``YYYYMMDD-YYYYMMDD`` suffixes are zero-padded.

    Raises
    ------
    SourcingError
        If none are found.
    """
    config_dir = exp_root / _CONFIG_SUBDIR
    paths = sorted(
        candidate
        for candidate in config_dir.glob(_FINISHED_CONFIG_GLOB)
        if _FINISHED_CONFIG_MARKER in candidate.name
    )
    if not paths:
        raise SourcingError(
            "scan requires a completed ESM-Tools run; "
            f"no finished_config under {config_dir}"
        )
    return [_Segment(path=path, doc=_load_yaml(path)) for path in paths]


def _experiment_id(segments: list[_Segment]) -> ExperimentId:
    """The experiment id, from ``general.expid`` or the filename prefix fallback."""
    for segment in segments:
        expid = _general(segment.doc).expid
        if expid:
            return str(expid)
    name = segments[0].path.name
    return name.split(_FINISHED_CONFIG_MARKER, 1)[0].rstrip("_")


def _general_metadata_blocks(
    docs: Iterable[FinishedConfigDoc],
) -> Iterable[MetadataBlock]:
    """Yield the ``general.metadata`` block of each segment doc.

    Experiment-level metadata (description, license, contacts) lives under
    ``general``. A component's own ``metadata`` describes the model, not the
    experiment, so it is not consulted here.
    """
    for doc in docs:
        general = doc.get("general")
        if isinstance(general, dict):
            meta = general.get("metadata")
            if isinstance(meta, dict) and meta:
                yield MetadataBlock.model_validate(meta)


def _first_value(blocks: Iterable[MetadataBlock], key: str) -> Optional[str]:
    """The first non-empty ``key`` across metadata *blocks*, or None."""
    for block in blocks:
        value = getattr(block, key, None)
        if value:
            return str(value)
    return None


def _contacts(blocks: Iterable[MetadataBlock]) -> list[Contact]:
    """The de-duplicated :class:`Contact` list parsed from every ``Authors`` block."""
    contacts: list[Contact] = []
    seen: set[tuple] = set()
    for block in blocks:
        institution = block.Institute
        for author in _as_list(block.Authors):
            contact = _to_contact(author, institution)
            if contact is None:
                continue
            fingerprint = (contact.name, contact.orcid, contact.institution)
            if fingerprint not in seen:
                seen.add(fingerprint)
                contacts.append(contact)
    return contacts


def _as_list(value: object) -> list:
    """Wrap a scalar in a list, pass a list through, treat None as empty."""
    if value is None:
        return []
    return list(value) if isinstance(value, (list, tuple)) else [value]


def _to_contact(author: object, institution: Optional[str]) -> Optional[Contact]:
    """Coerce one ``Authors`` entry (a name string or a mapping) into a Contact."""
    if isinstance(author, str):
        name = author.strip()
        return Contact(name=name, institution=institution) if name else None
    if isinstance(author, dict):
        name = author.get("name") or author.get("Name")
        return Contact(
            name=str(name).strip() if name else None,
            orcid=author.get("orcid") or author.get("ORCID"),
            institution=author.get("institution")
            or author.get("Institute")
            or author.get("affiliation")
            or institution,
        )
    return None


def _paleo_config(docs: Iterable[FinishedConfigDoc]) -> Optional[PaleoConfig]:
    """The ``general.paleo`` section, from the first segment that declares one."""
    for doc in docs:
        paleo = _general(doc).paleo
        if isinstance(paleo, dict) and paleo:
            return paleo
    return None


def _run_span(
    segments: list[_Segment],
) -> tuple[Optional[datetime], Optional[datetime]]:
    """The union (min start, max end) of every segment's run window."""
    windows = [w for w in map(_segment_window, segments) if w is not None]
    if not windows:
        return None, None
    return min(start for start, _ in windows), max(end for _, end in windows)


def _segment_window(segment: _Segment) -> Optional[RunWindow]:
    """One segment's (start, end), from its config run dates or filename suffix."""
    general = _general(segment.doc)

    start = _first_date(general, _START_DATE_KEYS)
    end = _first_date(general, _END_DATE_KEYS)
    if start and end:
        return start, end

    stamp = general.run_datestamp
    from_stamp = _parse_datestamp(stamp) if isinstance(stamp, str) else None
    if from_stamp:
        return from_stamp

    return _suffix_window(segment.path.name)


def _first_date(general: GeneralBlock, keys: Iterable[str]) -> Optional[datetime]:
    """The first parseable datetime among *keys* in the ``general`` block."""
    for key in keys:
        parsed = _to_datetime(getattr(general, key, None))
        if parsed is not None:
            return parsed
    return None


def _suffix_window(filename: str) -> Optional[RunWindow]:
    """The run window from a ``..._YYYYMMDD-YYYYMMDD`` filename suffix, if present."""
    _, _, suffix = filename.partition(_FINISHED_CONFIG_MARKER)
    return _parse_datestamp(suffix.lstrip("._"))


def _parse_datestamp(stamp: Optional[RunStamp]) -> Optional[RunWindow]:
    """Parse a ``YYYYMMDD-YYYYMMDD`` :data:`RunStamp` into a (start, end) window."""
    if not stamp:
        return None
    text = stamp.strip()
    if "-" not in text:
        return None
    start_text, _, end_text = text.partition("-")
    start = _to_datetime(start_text)
    end = _to_datetime(end_text)
    if start is None or end is None:
        return None
    return start, end


def _to_datetime(value: object) -> Optional[datetime]:
    """Coerce a datetime, date, or date-like string to a datetime, else None."""
    if isinstance(value, datetime):
        return value
    if isinstance(value, date):
        return datetime(value.year, value.month, value.day)
    if not isinstance(value, str):
        return None
    text = value.strip()
    if not text:
        return None
    try:
        return datetime.fromisoformat(text)
    except ValueError:
        pass
    try:
        return datetime.strptime(text, "%Y%m%d")
    except ValueError:
        return None


def _outdata_targets(doc: FinishedConfigDoc) -> Iterable[ComponentPath]:
    """Yield a :class:`ComponentPath` for every ``outdata_targets`` entry in *doc*."""
    for name, block in doc.items():
        if not isinstance(block, dict):
            continue
        targets = block.get("outdata_targets")
        if not isinstance(targets, dict):
            continue
        for target in targets.values():
            if target:
                yield ComponentPath(component=str(name), path=UPath(str(target)))


def _tidy_outdata(exp_root: UPath) -> Iterable[TidyOutdataEntry]:
    """Yield a :class:`TidyOutdataEntry` for every tidy-log ``outdata`` entry.

    Iterates all tidy-phase logs under ``<exp_root>/log`` in chronological order.
    This is the authoritative list of files the run actually produced.
    """
    log_dir = exp_root / _LOG_SUBDIR
    for log_path in sorted(log_dir.glob(_TIDY_LOG_GLOB)):
        yield from _tidy_log_outdata(_load_yaml(log_path))


def _load_yaml(path: UPath) -> FinishedConfigDoc:
    """Parse a finished-config or tidy-log document as plain YAML.

    Loaded with a plain safe parser through the path's own opener, so it works
    on remote roots (e.g. ``sftp://``) where the host lives in the UPath's
    storage options and would be lost by a ``str(path)`` round-trip. The
    esm_parser config loader is deliberately avoided: it re-wraps the path as a
    string, and would add provenance and config-only checks these documents
    should never be subject to.
    """
    with path.open() as stream:
        doc = YAML(typ="safe").load(stream)
    return doc if isinstance(doc, dict) else {}


def _tidy_log_outdata(doc: FinishedConfigDoc) -> Iterable[TidyOutdataEntry]:
    """Yield a :class:`TidyOutdataEntry` per ``outdata`` entry in a tidy log.

    The tidy log nests as ``{component: {files: {outdata: {name: entry}}}}``;
    ``restart_out``, ``log`` and other categories are skipped, so restart files
    never appear here. An entry without a ``destination`` is skipped; a missing
    ``checksum`` yields ``None`` (the file is still catalogued, just without an md5).
    """
    for component, block in doc.items():
        if not isinstance(block, dict):
            continue
        outdata = (block.get("files") or {}).get("outdata")
        if not isinstance(outdata, dict):
            continue
        for entry in outdata.values():
            if not isinstance(entry, dict):
                continue
            destination = entry.get("destination")
            if not destination:
                continue
            checksum = entry.get("checksum")
            md5 = str(checksum).strip() if checksum else None
            yield TidyOutdataEntry(
                component=str(component),
                destination=str(destination).strip(),
                md5=md5,
            )


def _is_restart(path: UPath) -> bool:
    """Whether *path* names a restart or rerun file (excluded from the catalog)."""
    name = path.name.lower()
    return any(marker in name for marker in _RESTART_MARKERS)


def _is_sidecar(path: UPath) -> bool:
    """Whether *path* is a non-data sidecar (e.g. GRIB ``.codes``/``.idx`` index)."""
    return path.suffix.lower() in _SIDECAR_SUFFIXES


def _general(doc: FinishedConfigDoc) -> GeneralBlock:
    """The ``general`` block of *doc*, parsed into a :class:`GeneralBlock`."""
    general = doc.get("general")
    return GeneralBlock.model_validate(general if isinstance(general, dict) else {})
