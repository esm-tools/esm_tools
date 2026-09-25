"""The scan progress display's non-tty/verbose fallback (esm_catalog.cli).

Confirmed live: with the rich bar disabled (``-v``, or stdout piped), a scan
went completely silent for the whole reading phase -- many real minutes with
zero feedback, indistinguishable from a hang. The fallback must still emit
something periodically.
"""

from __future__ import annotations

import loguru

from esm_catalog.cli import _scan_progress
from esm_catalog.scan.types import ProgressEvent


def test_disabled_progress_logs_each_phase_transition(monkeypatch):
    lines = []
    monkeypatch.setattr(loguru.logger, "info", lambda *a, **k: lines.append((a, k)))

    with _scan_progress(False) as on_progress:
        on_progress(ProgressEvent("sourcing", detail="5 files found"))
        on_progress(ProgressEvent("reading", 0, 10))
        on_progress(ProgressEvent("writing"))

    assert len(lines) == 3


def test_disabled_progress_throttles_same_phase_reading_ticks(monkeypatch):
    lines = []
    monkeypatch.setattr(loguru.logger, "info", lambda *a, **k: lines.append((a, k)))
    now = {"t": 0.0}
    monkeypatch.setattr("time.monotonic", lambda: now["t"])

    with _scan_progress(False) as on_progress:
        on_progress(ProgressEvent("reading", 1, 100))  # phase change -> logs
        now["t"] = 5.0
        on_progress(ProgressEvent("reading", 2, 100))  # within 30s -> throttled
        now["t"] = 40.0
        on_progress(ProgressEvent("reading", 3, 100))  # past 30s -> logs

    assert len(lines) == 2


def test_disabled_progress_always_logs_the_final_reading_tick(monkeypatch):
    lines = []
    monkeypatch.setattr(loguru.logger, "info", lambda *a, **k: lines.append((a, k)))
    now = {"t": 0.0}
    monkeypatch.setattr("time.monotonic", lambda: now["t"])

    with _scan_progress(False) as on_progress:
        on_progress(ProgressEvent("reading", 1, 2))  # phase change -> logs
        # current == total: completion, must not be swallowed by throttling
        on_progress(ProgressEvent("reading", 2, 2))

    assert len(lines) == 2
