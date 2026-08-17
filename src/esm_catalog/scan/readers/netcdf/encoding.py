"""Make scanned metadata UTF-8 encodable for the geoparquet shard."""

from __future__ import annotations


def _drop_surrogates(value):
    """Recursively strip lone surrogate code points from every string in *value*.

    NetCDF attributes written by Fortran models can carry non-UTF-8 bytes, which
    xarray surfaces as lone surrogates (``\\udc80``…). Those cannot be encoded to
    the UTF-8 of a stac-geoparquet shard -- pyarrow raises ``UnicodeEncodeError:
    surrogates not allowed`` when the item is serialised. Round-trip through
    UTF-8 with replacement so the metadata is always encodable.
    """
    if isinstance(value, str):
        return value.encode("utf-8", "replace").decode("utf-8")
    if isinstance(value, dict):
        return {key: _drop_surrogates(item) for key, item in value.items()}
    if isinstance(value, list):
        return [_drop_surrogates(item) for item in value]
    return value
