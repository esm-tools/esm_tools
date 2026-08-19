"""Template-driven archive specs (a proto-filedict).

Each model's ``archive`` configuration is a list of :class:`ArchiveSpec`, one per
file group the model emits. A spec says how to *find* its files (a jinja
``match`` glob, with ``{{ date }}`` rendered per step) and how to *name* the
tarball they go into (a jinja ``tar_template``). This replaces the fragile
datestamp-location guessing with explicit, per-model templates.

The three ``Annotated`` types (:data:`Frequency`, :data:`StrftimeFormat`,
:data:`JinjaTemplate`) validate the config at load time and are the reusable
seeds for a future shared filedict object.
"""
from __future__ import annotations

import datetime
import glob
import os
from functools import cached_property
from typing import Annotated

import pandas as pd
from jinja2 import Template
from pydantic import AfterValidator, BaseModel, Field


def _valid_pandas_freq(value: str) -> str:
    pd.tseries.frequencies.to_offset(value)  # raises on an unknown offset alias
    return value


def _valid_strftime(value: str) -> str:
    datetime.datetime(2000, 1, 1).strftime(value)  # raises on a bad format
    if "%" not in value:
        raise ValueError("date_format must contain a strftime directive, e.g. %Y%m")
    return value


def _valid_jinja(value: str) -> str:
    Template(value)  # raises on a jinja syntax error
    return value


#: A pandas offset alias, e.g. ``"1M"`` or ``"1Y"``.
Frequency = Annotated[str, AfterValidator(_valid_pandas_freq)]
#: A strftime format string, e.g. ``"%Y%m"``.
StrftimeFormat = Annotated[str, AfterValidator(_valid_strftime)]
#: A jinja template string, validated as parseable at load time.
JinjaTemplate = Annotated[str, AfterValidator(_valid_jinja)]


class ArchiveSpec(BaseModel):
    """One file group to archive — a proto-filedict."""

    match: JinjaTemplate = Field(
        description="jinja glob to find files; {{ date }} is rendered per step"
    )
    tar_template: JinjaTemplate = Field(
        description="jinja name for the output tarball (without the .tgz suffix)"
    )
    frequency: Frequency = "1M"
    date_format: StrftimeFormat = "%Y%m"
    static: bool = Field(
        default=False,
        description="match once instead of stepping dates; for undated files "
        "(coupling diagnostics, grids/weights). Use no {{ date }}/{{ decade }}.",
    )

    @cached_property
    def _match_tmpl(self) -> Template:
        return Template(self.match)

    @cached_property
    def _tar_tmpl(self) -> Template:
        return Template(self.tar_template)

    def render_match(self, **ctx) -> str:
        return self._match_tmpl.render(**ctx)

    def render_tar_name(self, **ctx) -> str:
        return self._tar_tmpl.render(**ctx)


def _paleo_safe_stamp(period: pd.Period, date_format: str) -> str:
    # strftime("%Y") does not zero-pad years below 1000; ESM files use a
    # 4-digit year, so substitute a padded year for those.
    fmt = date_format
    if "%Y" in fmt and 0 <= period.year < 1000:
        fmt = fmt.replace("%Y", f"{period.year:04d}")
    return period.strftime(fmt)


def date_context(period: pd.Period, date_format: str) -> dict:
    """Readable date variables handed to the jinja templates for one step."""
    decade_start = period.year - period.year % 10
    return {
        "date": _paleo_safe_stamp(period, date_format),
        "year": f"{period.year:04d}",
        "month": f"{period.month:02d}",
        "decade": f"{decade_start:04d}-{decade_start + 9:04d}",
    }


def collect_tarballs(base_dir, filetype, model, specs, start_date, end_date, expid):
    """Group a model's files into ``{tar_name: [files]}`` for one filetype.

    Steps each spec's ``frequency`` over ``[start_date, end_date)`` (end
    exclusive), renders ``match`` to a glob, and buckets the found files by the
    rendered ``tar_template`` — so files that render to the same name (e.g. a
    whole decade) land in one tarball.
    """
    model_dir = os.path.join(base_dir, filetype, model)
    buckets: dict[str, list[str]] = {}
    for spec in specs:
        if spec.static:
            # Undated files: render/glob once, one tarball, no date loop.
            ctx = {"expid": expid, "filetype": filetype, "model": model}
            matched = glob.glob(os.path.join(model_dir, spec.render_match(**ctx)))
            if matched:
                buckets.setdefault(spec.render_tar_name(**ctx), []).extend(matched)
            continue
        end_period = pd.Period(end_date, freq=spec.frequency)
        for period in pd.period_range(
            start=start_date, end=end_date, freq=spec.frequency
        ):
            if period >= end_period:
                continue
            ctx = date_context(period, spec.date_format)
            ctx.update(expid=expid, filetype=filetype, model=model)
            matched = glob.glob(os.path.join(model_dir, spec.render_match(**ctx)))
            if not matched:
                continue
            buckets.setdefault(spec.render_tar_name(**ctx), []).extend(matched)
    return {tar: sorted(set(files)) for tar, files in buckets.items()}


def load_archive_specs(config) -> dict:
    """Parse a raw config dict into ``{model: [ArchiveSpec]}``.

    Only models whose ``archive`` value is a list are returned; the legacy
    single-dict ``archive`` form is left to the heuristic code path.
    """
    specs: dict[str, list[ArchiveSpec]] = {}
    for model, model_cfg in (config or {}).items():
        if not isinstance(model_cfg, dict):
            continue  # top-level settings (archive_dir, hsm_target, ...), not a model
        archive = model_cfg.get("archive")
        if isinstance(archive, list):
            specs[model] = [ArchiveSpec(**entry) for entry in archive]
    return specs
