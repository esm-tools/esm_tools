"""One generic engine reading declarative filename templates.

Confirmed live: a real on-disk filename convention (ECHAM's
``<expid>_YYYYMM.DD_<stream>``, FESOM's ``<stream>.fesom.<year>.nc`` /
``fesom.<year>.<stream>.restart``) is a property of how each model writes its
own output -- not something specific to one component's Python code. A single
template-compiling engine, driven by a plain per-component data table, covers
every format (GRIB, NetCDF, ...) the same way; there is nothing GRIB- or
NetCDF- or FESOM-specific about *matching a filename shape*.

The data itself (which patterns exist for which component) is pluggable, not
baked into this module or any central file: each component contributes its
own entries by implementing :func:`get_path_facet_templates` and declaring it
under the ``esm_catalog.path_facet_data`` entry-point group -- same mechanism
as the ``esm_catalog.enrichers``/``esm_catalog.fastpath.grib`` groups, just
for data instead of a hookimpl with behaviour. See
``scan/model_plugins/path_facets/<format>/<component>.py`` for the shipped
ones (echam, fesom, jsbach, oifs); a separately-installed package contributes
one the same way, e.g. in ``pyproject.toml``::

    [project.entry-points."esm_catalog.path_facet_data"]
    my_model = "my_package.path_facet_data"

This module itself carries no component-specific data or logic at all --
only the generic pattern-compiling/matching engine and the aggregation of
whatever every registered contributor returns.

No component declares a restart template (echam/fesom/jsbach/hdmodel/...):
every restart ``OutputFile`` -- declared or walked -- carries a fixed,
component-wide ``stream="restart"`` (see ``sourcing.py``'s
``restart_files()``; category, not stream, differentiates oce/ice/accw/etc
within it), so a template resolving a per-category name from the filename
could never match the stream it is compared against. Nor does oasis3mct
declare one: its restart files are stamped with a run-segment date *range*
sharing a filename convention this template model cannot express, and some
carry no date at all -- both fall back to a real read, the same safe default
as any other unmatched file.
"""

from __future__ import annotations

import re
from dataclasses import dataclass
from datetime import datetime
from typing import Optional

import pluggy
from upath import UPath

from esm_catalog.scan.path_facets import hookimpl

_template_data_hookspec = pluggy.HookspecMarker("esm_catalog.path_facet_data")
template_data_hookimpl = pluggy.HookimplMarker("esm_catalog.path_facet_data")
"""Re-exported for a component module's own get_path_facet_templates()."""


class _TemplateDataSpec:
    """One hook call per registered component module, every result merged."""

    @_template_data_hookspec
    def get_path_facet_templates() -> Optional[dict]:
        """Return ``{component: [entries...]}`` this module contributes, or
        ``None``. Each entry is ``{"pattern": ..., "date_format": ...,
        "end_date_format": ...}`` (the last optional) -- see :func:`_compile`
        for what the pattern placeholders mean.
        """


def _build_template_data_plugin_manager() -> pluggy.PluginManager:
    pm = pluggy.PluginManager("esm_catalog.path_facet_data")
    pm.add_hookspecs(_TemplateDataSpec)
    # echam/jsbach/fesom/oifs are discovered via this entry-point group,
    # declared in this package's own setup.py for now -- the same mechanism
    # a separately-installed package would use (see esm-tools/esm_tools#1614).
    pm.load_setuptools_entrypoints("esm_catalog.path_facet_data")
    return pm

_PLACEHOLDER_RE = re.compile(r"\{\{\s*(\w+)\s*\}\}")

_STRFTIME_FIELD_RE = {
    "%Y": r"\d{4}",
    "%y": r"\d{2}",
    "%m": r"\d{2}",
    "%d": r"\d{2}",
    "%H": r"\d{2}",
    "%M": r"\d{2}",
    "%S": r"\d{2}",
    "%j": r"\d{1,3}",
}
"""strftime code -> the regex matching exactly what it prints. Extend as a
real template needs a field not listed here -- deliberately not exhaustive."""


_IGNORE_RE = r"\d+[hdmy]"
"""A frequency-code-shaped field (e.g. ``1d``, ``1m``, ``3h``, ``1y``) matched
but not bound to anything -- restrictive on purpose: a real variable name can
share the shape "digits then letters" (OIFS' ``10u``/``10v``), but none of the
observed ones end in ``h``/``d``/``m``/``y``, so this cannot be confused with
``{{ stream }}`` in practice. Use ``{{ ignore }}`` for a structural field
whose value is never needed (OIFS repeats its frequency code twice per
filename; neither occurrence is itself useful once parsed)."""


@dataclass(frozen=True)
class _Template:
    component: str
    date_format: str
    end_date_format: Optional[str]
    regex: "re.Pattern"


def _date_field_regex(date_format: str) -> str:
    """*date_format* (a plain strftime string, e.g. ``"%Y%m.%d"``) as a regex
    matching exactly what it would print -- literal characters escaped,
    each recognised ``%X`` code replaced by its fixed-width digit class."""
    pattern = re.escape(date_format)
    for code, field_regex in _STRFTIME_FIELD_RE.items():
        # re.escape leaves '%' and letters alone (not regex-special), so the
        # escaped string still contains the literal "%Y" etc. to replace.
        pattern = pattern.replace(code, field_regex)
    return pattern


def _compile(
    component: str,
    pattern: str,
    date_format: str,
    end_date_format: Optional[str] = None,
) -> _Template:
    """Compile one declarative *pattern* into a matchable :class:`_Template`.

    ``{{ expid }}`` -> greedy ``.+`` (disambiguated by backtracking against
    the far more specific date/stream fields around it -- the same
    resolution a hand-written regex anchoring on the filename's tail would
    need). ``{{ stream }}`` -> everything up to the next ``.``/``/`` --
    confirmed live: a real stream name can itself contain an underscore
    (FESOM's ``a_ice``), so only the filename/path structural separators are
    excluded, not ``_``. ``{{ date }}`` -> derived from *date_format*;
    ``{{ end_date }}`` likewise from *end_date_format* (present only when a
    file's real end genuinely cannot be derived from frequency alone -- see
    the hookspec's own docstring). ``{{ ignore }}`` -> a frequency-code-shaped
    field matched but not bound (see :data:`_IGNORE_RE`).
    """
    date_regex = _date_field_regex(date_format)
    end_date_regex = _date_field_regex(end_date_format) if end_date_format else None
    pieces: list[str] = []
    last = 0
    for match in _PLACEHOLDER_RE.finditer(pattern):
        pieces.append(re.escape(pattern[last : match.start()]))
        name = match.group(1)
        if name == "expid":
            pieces.append(r"(?P<expid>.+)")
        elif name == "stream":
            pieces.append(r"(?P<stream>[^./]+)")
        elif name == "date":
            pieces.append(f"(?P<date>{date_regex})")
        elif name == "end_date":
            if end_date_regex is None:
                raise ValueError(
                    "path-facet template: {{ end_date }} used without an end_date_format"
                )
            pieces.append(f"(?P<end_date>{end_date_regex})")
        elif name == "ignore":
            pieces.append(f"(?:{_IGNORE_RE})")
        else:
            raise ValueError(f"path-facet template: unknown placeholder {{{{ {name} }}}}")
        last = match.end()
    pieces.append(re.escape(pattern[last:]))
    regex = re.compile("^" + "".join(pieces) + "$")
    return _Template(
        component=component,
        date_format=date_format,
        end_date_format=end_date_format,
        regex=regex,
    )


_templates_cache: Optional[dict[str, list[_Template]]] = None


def _load_templates() -> dict[str, list[_Template]]:
    """Every registered component's templates, merged and compiled once per
    process -- building the data-contributor plugin manager and compiling
    every pattern is not free, and this is consulted per file."""
    global _templates_cache
    if _templates_cache is not None:
        return _templates_cache
    pm = _build_template_data_plugin_manager()
    doc: dict[str, list[dict]] = {}
    for contribution in pm.hook.get_path_facet_templates():
        if not contribution:
            continue
        for component, entries in contribution.items():
            doc.setdefault(component, []).extend(entries)
    _templates_cache = {
        component: [
            _compile(
                component,
                entry["pattern"],
                entry["date_format"],
                entry.get("end_date_format"),
            )
            for entry in entries
        ]
        for component, entries in doc.items()
    }
    return _templates_cache


@hookimpl
def extract_path_facets(
    path: UPath, component: str, stream: Optional[str]
) -> Optional[tuple[str, datetime, Optional[datetime]]]:
    for template in _load_templates().get(component, []):
        match = template.regex.match(path.name)
        if match is None:
            continue
        groups = match.groupdict()
        resolved_stream = groups.get("stream")
        if resolved_stream is None:
            continue
        if stream is not None and stream != resolved_stream:
            continue
        try:
            start = datetime.strptime(groups["date"], template.date_format)
        except ValueError:  # noqa: BLE001 -- an out-of-range date is not a match
            continue
        end: Optional[datetime] = None
        if template.end_date_format is not None and "end_date" in groups:
            try:
                end = datetime.strptime(groups["end_date"], template.end_date_format)
            except ValueError:  # noqa: BLE001 -- an out-of-range date is not a match
                continue
        return resolved_stream, start, end
    return None
