"""ECHAM enrichment for the generic GRIB reader (cfgrib fallback case).

ECHAM writes GRIB1 with ``paramId=0`` for every field, so cfgrib cannot tell
the parameters apart and collapses them into a single ``unknown`` variable.
Reading each message's ``indicatorOfParameter`` with eccodes and joining it
to the ``.codes`` table (see ``model_plugins.helpers.grib.echam``, shared
with the fast path in ``model_plugins.fastpath.grib.echam``) recovers the
real variable set.

This runs only when the basic reader left an ``unknown`` variable *and* a
``.codes`` file sits beside the data -- otherwise it is a no-op, so it never
disturbs well-formed (e.g. GRIB2) files.
"""

from __future__ import annotations

from loguru import logger
from upath import UPath

from esm_catalog.scan.enrichers import hookimpl
from esm_catalog.scan.format import FileFormat
from esm_catalog.scan.model_plugins.helpers.grib.echam import codes_path, parse_codes
from esm_catalog.types import FileMetadata, ScannedVariable

_UNKNOWN = ("unknown", "")


def _indicators_present(grib_path: UPath) -> list[int]:
    """The distinct ``indicatorOfParameter`` values across the file's messages.

    In file order, de-duplicated -- this is the real set of parameters cfgrib
    collapsed into ``unknown``.
    """
    import eccodes

    indicators: list[int] = []
    seen: set[int] = set()
    with grib_path.open("rb") as handle:
        while True:
            gid = eccodes.codes_grib_new_from_file(handle)
            if gid is None:
                break
            try:
                indicator = eccodes.codes_get(gid, "indicatorOfParameter")
                if indicator not in seen:
                    seen.add(indicator)
                    indicators.append(indicator)
            except Exception:  # noqa: BLE001 -- a message without the key is skipped
                pass
            finally:
                eccodes.codes_release(gid)
    return indicators


def enrich(path: UPath, metadata: FileMetadata, datasets: list) -> FileMetadata:
    """Replace cfgrib's ``unknown`` ECHAM variables with their real names."""
    variables = metadata.get("variables", [])
    if not any(v.get("name") in _UNKNOWN for v in variables):
        return metadata  # nothing collapsed -> not the ECHAM case

    codes_file = codes_path(path)
    if codes_file is None:
        return metadata
    table = parse_codes(codes_file)
    if not table:
        return metadata

    try:
        indicators = _indicators_present(path)
    except ImportError:
        logger.warning(
            "{}: ECHAM .codes present but eccodes missing; leaving GRIB names as-is",
            path,
        )
        return metadata
    except Exception as exc:  # noqa: BLE001 -- enrichment is best-effort
        logger.warning("{}: ECHAM indicator scan failed: {}", path, exc)
        return metadata

    # The collapsed 'unknown' carries the shared grid/time dims; reuse them.
    template = next((v for v in variables if v.get("name") in _UNKNOWN), None)
    dims = list(template.get("dimensions", [])) if template else []

    named: list[ScannedVariable] = []
    for indicator in indicators:
        info = table.get(indicator)
        if info is None:
            continue
        entry: ScannedVariable = {
            "name": info["name"],
            "units": info["units"],
            "long_name": info["long_name"],
            "dimensions": dims,
        }
        named.append(entry)
    if not named:
        return metadata

    kept = [v for v in variables if v.get("name") not in _UNKNOWN]
    metadata["variables"] = kept + named
    metadata["variable"] = named[0]["name"]
    return metadata


@hookimpl
def enrich_metadata(
    path: UPath, file_format: FileFormat, metadata: FileMetadata, datasets: list
) -> None:
    if file_format != FileFormat.grib:
        return
    enrich(path, metadata, datasets)
