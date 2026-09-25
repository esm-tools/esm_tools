"""The ECHAM GRIB fast path (esm_catalog.scan.readers.grib.echam).

eccodes/cfgrib are optional extras not installed in this dev environment, so
``eccodes`` itself is faked here rather than skipped -- these tests exercise
real message-handling logic (:func:`_read_from_headers`'s loop), just against
a stand-in for the C extension.
"""

from __future__ import annotations

import sys
import types

import pytest

from esm_catalog.scan.readers.grib import echam


class _FakeEccodesError(Exception):
    """Stands in for eccodes' real "Key/value not found" error."""


def _install_fake_eccodes(monkeypatch, messages: list[dict]) -> None:
    """Register a fake ``eccodes`` module whose messages are *messages* --
    each a dict of only the keys that message actually carries, so a lookup
    for a missing key raises like the real C extension does for e.g. a
    spectral ('sh') GRIB message queried for gridpoint geometry."""
    state = {"next": 0}

    def codes_grib_new_from_file(handle):
        if state["next"] >= len(messages):
            return None
        gid = state["next"]
        state["next"] += 1
        return gid

    def codes_get(gid, key):
        try:
            return messages[gid][key]
        except KeyError:
            raise _FakeEccodesError(f"Key/value not found: {key}") from None

    def codes_release(gid):
        pass

    fake = types.SimpleNamespace(
        codes_grib_new_from_file=codes_grib_new_from_file,
        codes_get=codes_get,
        codes_release=codes_release,
    )
    monkeypatch.setitem(sys.modules, "eccodes", fake)


def _write_grib_and_codes(tmp_path):
    grib_path = tmp_path / "historical_c14_init_185001.01_echam"
    grib_path.write_bytes(b"")  # content unread -- the fake eccodes ignores it
    codes_path = tmp_path / (grib_path.name + ".codes")
    codes_path.write_text(
        "130 1 var130 0 1 temperature [K]\n"
        "131 1 var131 0 1 u_wind [m/s]\n"
        "132 1 var132 0 1 v_wind [m/s]\n"
    )
    return grib_path, codes_path


def test_fast_path_survives_a_spectral_message_before_the_first_gridpoint_one(
    tmp_path, monkeypatch
):
    """Confirmed live: an ECHAM file mixes spectral ('sh') and gridpoint
    ('regular_gg') GRIB1 messages. The fast path's geometry lookup used to
    run unconditionally on whatever message it saw first; if that was
    spectral (no lat/lon keys), the whole fast path aborted and fell back to
    cfgrib's slow generic path for the entire file -- confirmed live: several
    minutes for a 543 MB file, versus sub-second for the fast path alone."""
    messages = [
        {"edition": 1, "indicatorOfParameter": 130, "dataDate": 18500101, "dataTime": 0},
        {
            "edition": 1,
            "indicatorOfParameter": 131,
            "dataDate": 18500101,
            "dataTime": 0,
            "latitudeOfFirstGridPointInDegrees": 88.5,
            "longitudeOfFirstGridPointInDegrees": 0.0,
            "latitudeOfLastGridPointInDegrees": -88.5,
            "longitudeOfLastGridPointInDegrees": 358.0,
        },
        {"edition": 1, "indicatorOfParameter": 132, "dataDate": 18500101, "dataTime": 600},
    ]
    _install_fake_eccodes(monkeypatch, messages)
    grib_path, codes_path = _write_grib_and_codes(tmp_path)

    result = echam._read_from_headers(grib_path, codes_path)

    assert result is not None
    assert {v["name"] for v in result["variables"]} == {"var130", "var131", "var132"}
    # lon 358 folds to -2 (>180 wraps to -180..180), then west/east get swapped
    # into ascending order.
    assert result["bbox"] == pytest.approx([-2.0, -88.5, 0.0, 88.5])


def test_fast_path_returns_none_when_every_message_is_spectral(tmp_path, monkeypatch):
    """No gridpoint message anywhere -- correctly defer to cfgrib rather than
    guess at geometry a spectral-only file cannot provide."""
    messages = [
        {"edition": 1, "indicatorOfParameter": 130, "dataDate": 18500101, "dataTime": 0},
        {"edition": 1, "indicatorOfParameter": 131, "dataDate": 18500101, "dataTime": 0},
    ]
    _install_fake_eccodes(monkeypatch, messages)
    grib_path, codes_path = _write_grib_and_codes(tmp_path)

    result = echam._read_from_headers(grib_path, codes_path)

    assert result is None
