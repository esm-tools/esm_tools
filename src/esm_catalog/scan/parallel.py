"""Order-preserving parallel map with a process-pool or Dask backend.

The scan pipeline is embarrassingly parallel: each input is handled
independently, and the results are collected back in input order. This module
provides one function, :func:`parallel_map`, that fans a *worker* out over a
list of *inputs* and returns the results in the **same order** as the inputs
(deterministic — never as-completed).

Two backends live behind that single function:

* **Process pool** (default): ``concurrent.futures.ProcessPoolExecutor``.
  Running each task in a separate process is deliberate — it sidesteps the
  thread-unsafety of HDF5/NetCDF, which corrupts state when driven from
  multiple threads in one process.
* **Dask** (``distributed=True``): either connect to an existing scheduler
  (given a ``tcp://…`` URL) or spin up a local cluster of worker processes.
  Use this to scale a scan across the nodes of an HPC allocation.

.. important::

   The *worker* MUST be a **module-level, picklable callable** — a plain
   ``def`` at module scope, not a lambda, a closure, or a locally-defined or
   instance-bound function. Both backends ship the worker to a separate
   process by pickling it; an unpicklable worker raises ``PicklingError`` (or
   an opaque Dask serialization error) instead of running. Bind per-run
   configuration with :func:`functools.partial` over a module-level function
   rather than capturing it in a closure.

Errors are **not** swallowed. If the worker raises for any input, that
exception propagates out of :func:`parallel_map` (the process-pool backend
re-raises it when the result is collected; the Dask backend re-raises it on
gather). A partial run therefore fails loudly rather than returning a truncated
or sentinel-filled list.
"""

from __future__ import annotations

import logging
from concurrent.futures import ProcessPoolExecutor
from concurrent.futures import TimeoutError as FutureTimeoutError
from typing import Callable, Optional, Sequence, TypeVar

try:
    from loguru import logger as _logger
except ImportError:  # pragma: no cover - loguru is the normal path
    _logger = logging.getLogger(__name__)

Input = TypeVar("Input")
Result = TypeVar("Result")

Worker = Callable[[Input], Result]
"""A module-level, picklable callable mapping one input to one result."""


def parallel_map(
    inputs: list[Input],
    worker: Worker,
    *,
    distributed: bool = False,
    scheduler: Optional[str] = None,
    jobs: Optional[int] = None,
    label: Optional[str] = None,
    on_item: Optional[Callable[[Input], None]] = None,
    initializer: Optional[Callable[..., None]] = None,
    initargs: Sequence[object] = (),
    timeout: Optional[float] = None,
    on_timeout: Optional[Callable[[Input], Result]] = None,
) -> list[Result]:
    """Apply *worker* to every input in parallel, preserving input order.

    Parameters
    ----------
    inputs : list
        The items to process. Each is passed to *worker* on its own.
    worker : Callable
        A **module-level, picklable** function taking one input and returning
        one result. It runs in a separate process, so it must not be a lambda,
        closure, or instance-bound method (see the module docstring).
    distributed : bool, optional
        Select the backend. ``False`` (default) uses a
        ``ProcessPoolExecutor``; ``True`` uses Dask.
    scheduler : str or None, optional
        Only consulted when ``distributed`` is ``True``. A ``tcp://…`` address
        of an existing Dask scheduler to attach to. When ``None``, a local
        cluster of worker processes is created and torn down for this call.
    jobs : int or None, optional
        Worker-process count. For the process pool it is ``max_workers``; for a
        local Dask cluster it is ``n_workers``. ``None`` lets the backend pick
        its default. Ignored when attaching to an existing scheduler.
    label : str or None, optional
        A human-readable tag for progress logging (e.g. ``"scan"``). Purely
        cosmetic; does not affect the result.
    on_item : Callable or None, optional
        Called in the calling process with each input as its result is
        collected — a progress hook. It runs in input order (never
        as-completed) and does not need to be picklable (only *worker* does).
    initializer, initargs : optional
        A module-level callable (and its arguments) run once in **each worker
        process** at startup — the standard ``ProcessPoolExecutor`` hook. Use it
        to set up per-worker process state (e.g. quiet a noisy library's
        logging, which a spawned worker does not inherit from the parent).
    timeout, on_timeout : optional
        **Process-pool backend only** (ignored when ``distributed=True`` --
        Dask has its own scheduling story). When *timeout* (seconds) is given,
        an input whose *worker* call overruns it is abandoned: the whole pool
        is killed and replaced (see :func:`_process_map` for why the *whole*
        pool, not just that one worker), and *on_timeout* is called with the
        overrun input to produce the result recorded in its place. Required
        together -- *on_timeout* with no *timeout* never fires; *timeout* with
        no *on_timeout* raises.

    Returns
    -------
    list
        The results, one per input, in the **same order as** *inputs*.

    Raises
    ------
    Exception
        Any exception raised by *worker* for any input propagates unchanged;
        results are never silently dropped or replaced with sentinels.
    """
    if not inputs:
        return []

    if distributed:
        return _dask_map(
            inputs, worker, scheduler=scheduler, jobs=jobs, label=label, on_item=on_item
        )
    return _process_map(
        inputs,
        worker,
        jobs=jobs,
        label=label,
        on_item=on_item,
        initializer=initializer,
        initargs=initargs,
        timeout=timeout,
        on_timeout=on_timeout,
    )


def _process_map(
    inputs: list[Input],
    worker: Worker,
    *,
    jobs: Optional[int],
    label: Optional[str],
    on_item: Optional[Callable[[Input], None]] = None,
    initializer: Optional[Callable[..., None]] = None,
    initargs: Sequence[object] = (),
    timeout: Optional[float] = None,
    on_timeout: Optional[Callable[[Input], Result]] = None,
) -> list[Result]:
    """Run *worker* over *inputs* on a ``ProcessPoolExecutor``, in order.

    ``Executor.map`` yields results in submission order and re-raises the first
    worker exception when that result is consumed, giving us both the ordering
    and the fail-loud semantics for free. Iterating it (rather than ``list()``)
    lets *on_item* tick once per collected result, still in input order.

    Without *timeout*, this is exactly that. With it, ``executor.map`` cannot
    be used (it has no per-result timeout that lets processing continue), so
    each input is submitted individually and awaited with
    ``future.result(timeout=timeout)`` instead.
    """
    tag = label or "parallel-map"
    _logger.info(
        "{}: mapping {} inputs over process pool (jobs={})", tag, len(inputs), jobs
    )
    if timeout is None:
        with ProcessPoolExecutor(
            max_workers=jobs, initializer=initializer, initargs=tuple(initargs)
        ) as executor:
            results: list[Result] = []
            for item, result in zip(inputs, executor.map(worker, inputs)):
                results.append(result)
                if on_item is not None:
                    on_item(item)
            return results
    return _process_map_with_timeout(
        inputs,
        worker,
        jobs=jobs,
        tag=tag,
        on_item=on_item,
        initializer=initializer,
        initargs=initargs,
        timeout=timeout,
        on_timeout=on_timeout,
    )


def _process_map_with_timeout(
    inputs: list[Input],
    worker: Worker,
    *,
    jobs: Optional[int],
    tag: str,
    on_item: Optional[Callable[[Input], None]],
    initializer: Optional[Callable[..., None]],
    initargs: Sequence[object],
    timeout: float,
    on_timeout: Optional[Callable[[Input], Result]],
) -> list[Result]:
    """The *timeout*-enforcing path of :func:`_process_map`.

    A worker stuck deep in a non-interruptible C call (confirmed live: cfgrib
    reindexing a GRIB file) cannot be stopped by anything run *inside* that
    worker's own process -- a signal handler there does not preempt C code,
    and forcing an exception mid-call can leave a C library's internal state
    corrupted for whatever else that worker reads afterwards. The only
    reliably safe thing to kill is the worker process itself, from outside,
    once it has proven unresponsive. Since a specific pending future cannot be
    mapped back to the OS process running it through the public API, an
    overrun kills and replaces the *entire* pool -- simple, and correct,
    at the cost of re-running whatever else was in flight in that pool.
    """
    results: list[Result] = []
    pending = list(inputs)
    executor = ProcessPoolExecutor(
        max_workers=jobs, initializer=initializer, initargs=tuple(initargs)
    )
    try:
        futures = [executor.submit(worker, item) for item in pending]
        i = 0
        while i < len(pending):
            item = pending[i]
            try:
                result = futures[i].result(timeout=timeout)
            except FutureTimeoutError:
                _logger.warning(
                    "{}: {} exceeded {}s; killing its worker pool",
                    tag, item, timeout,
                )
                # Snapshot before shutdown() -- it clears ._processes to None
                # almost immediately, before the OS processes are reaped.
                stuck_workers = list(getattr(executor, "_processes", {}).values())
                executor.shutdown(wait=False, cancel_futures=True)
                for proc in stuck_workers:
                    if proc.is_alive():
                        proc.kill()
                if on_timeout is None:
                    raise
                result = on_timeout(item)
                remaining = pending[i + 1 :]
                executor = ProcessPoolExecutor(
                    max_workers=jobs, initializer=initializer, initargs=tuple(initargs)
                )
                futures[i + 1 :] = [executor.submit(worker, it) for it in remaining]
            results.append(result)
            if on_item is not None:
                on_item(item)
            i += 1
        return results
    finally:
        executor.shutdown(wait=False, cancel_futures=True)


def _dask_map(
    inputs: list[Input],
    worker: Worker,
    *,
    scheduler: Optional[str],
    jobs: Optional[int],
    label: Optional[str],
    on_item: Optional[Callable[[Input], None]] = None,
) -> list[Result]:
    """Run *worker* over *inputs* on Dask, gathering results in order.

    Attaches to the scheduler at *scheduler* when given; otherwise spins up a
    process-based ``LocalCluster`` and shuts it down before returning. Futures
    are submitted with ``pure=False`` (the worker may have side effects and
    must not be deduplicated by argument), and ``client.gather`` returns them in
    submission — hence input — order.
    """
    from distributed import Client, LocalCluster
    from distributed import as_completed as dask_as_completed

    tag = label or "parallel-map"
    cluster = None
    try:
        if scheduler:
            _logger.info("{}: attaching to Dask scheduler {}", tag, scheduler)
            client = Client(scheduler)
        else:
            _logger.info("{}: starting local Dask cluster (n_workers={})", tag, jobs)
            cluster = LocalCluster(processes=True, n_workers=jobs)
            client = Client(cluster)

        _logger.info("{}: mapping {} inputs over Dask", tag, len(inputs))
        futures = client.map(worker, inputs, pure=False)
        try:
            if on_item is not None:
                input_of = dict(zip(futures, inputs))
                for future in dask_as_completed(futures):
                    on_item(input_of[future])
            return client.gather(futures)
        finally:
            client.close()
    finally:
        # Only tear down a cluster this call created; never an attached one.
        if cluster is not None:
            cluster.close()
