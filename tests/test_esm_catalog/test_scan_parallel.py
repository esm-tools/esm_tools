"""Tests for :mod:`esm_catalog.scan.parallel`.

Workers must be module-level and picklable to survive being shipped to a
separate process, so every worker used here is defined at module scope.
"""

from __future__ import annotations

import time
from concurrent.futures import TimeoutError as FutureTimeoutError

import pytest

from esm_catalog.scan.parallel import parallel_map


def _square(n: int) -> int:
    """Square one integer (a picklable, module-level worker)."""
    return n * n


def _boom(n: int) -> int:
    """Raise for the input ``3``; otherwise square it."""
    if n == 3:
        raise ValueError("worker refused input 3")
    return n * n


def _hang_on_three(n: int) -> int:
    """Never return for the input ``3`` (simulates a stuck C call); else square."""
    if n == 3:
        time.sleep(60)
    return n * n


def _on_timeout_negate(n: int) -> int:
    """The ``on_timeout`` fallback used by the tests below: ``-n``, so it is
    distinguishable from a real (always non-negative) squared result."""
    return -n


def test_empty_inputs_short_circuit():
    assert parallel_map([], _square) == []


def test_process_map_preserves_order_and_matches_serial():
    inputs = [1, 2, 3, 4]
    result = parallel_map(inputs, _square)
    assert result == [1, 4, 9, 16]
    assert result == [_square(n) for n in inputs]


def test_process_map_with_explicit_jobs():
    inputs = list(range(10))
    result = parallel_map(inputs, _square, jobs=2)
    assert result == [_square(n) for n in inputs]


def test_process_map_result_order_is_input_order_not_completion_order():
    # Reversed input must yield reversed-squared output, proving the result is
    # ordered by input position rather than by completion.
    inputs = [4, 3, 2, 1]
    assert parallel_map(inputs, _square) == [16, 9, 4, 1]


def test_worker_exception_propagates():
    # A worker raising for one input must surface the error, not swallow it.
    with pytest.raises(ValueError, match="worker refused input 3"):
        parallel_map([1, 2, 3, 4], _boom)


def test_timeout_replaces_overrun_input_and_keeps_going():
    inputs = [1, 2, 3, 4, 5]
    start = time.monotonic()
    result = parallel_map(
        inputs, _hang_on_three, jobs=2, timeout=2, on_timeout=_on_timeout_negate
    )
    elapsed = time.monotonic() - start

    assert (
        elapsed < 30
    )  # nowhere near the 60s hang -- proves it was killed, not awaited
    assert result == [1, 4, -3, 16, 25]


def test_timeout_without_on_timeout_reraises():
    with pytest.raises(FutureTimeoutError):
        parallel_map([1, 2, 3], _hang_on_three, jobs=2, timeout=2)


@pytest.mark.slow
def test_dask_local_cluster_backend():
    # Minimal Dask smoke test on a tiny in-process LocalCluster. Guarded as
    # slow so it does not weigh on the default CI run.
    distributed = pytest.importorskip("distributed")
    assert distributed  # imported successfully
    inputs = [1, 2, 3, 4]
    result = parallel_map(inputs, _square, distributed=True, jobs=2)
    assert result == [1, 4, 9, 16]
