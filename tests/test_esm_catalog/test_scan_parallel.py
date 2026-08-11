"""Tests for :mod:`esm_catalog.scan.parallel`.

Workers must be module-level and picklable to survive being shipped to a
separate process, so every worker used here is defined at module scope.
"""

from __future__ import annotations

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


@pytest.mark.slow
def test_dask_local_cluster_backend():
    # Minimal Dask smoke test on a tiny in-process LocalCluster. Guarded as
    # slow so it does not weigh on the default CI run.
    distributed = pytest.importorskip("distributed")
    assert distributed  # imported successfully
    inputs = [1, 2, 3, 4]
    result = parallel_map(inputs, _square, distributed=True, jobs=2)
    assert result == [1, 4, 9, 16]
