"""
Minimal event hook system for esm-tools.

This module provides a simple, thread-safe hook notification system with
zero dependencies beyond Python's standard library. It's designed to be
stable and maintainable with minimal API surface.
"""

from typing import Callable, Dict, List
import threading
from datetime import datetime


class HookManager:
    """
    A minimal hook manager with thread-safe hook dispatch.

    Hooks are identified by string names. Callbacks are called synchronously
    in the order they were registered when a hook is triggered.
    """

    def __init__(self):
        self._hooks: Dict[str, List[Callable]] = {}
        self._lock = threading.Lock()

    def add_hook(self, hook: str, callback: Callable) -> None:
        """Register a callback for a hook."""
        with self._lock:
            if hook not in self._hooks:
                self._hooks[hook] = []
            self._hooks[hook].append(callback)

    def trigger_hook(self, hook: str, *args, **kwargs) -> None:
        """Trigger a hook, calling all registered callbacks."""
        with self._lock:
            callbacks = self._hooks.get(hook, []).copy()

        for callback in callbacks:
            try:
                callback(*args, **kwargs)
            except Exception:
                # Silently ignore callback errors to prevent one bad
                # callback from breaking the entire hook chain
                pass


# Global singleton instance
_manager = HookManager()


def add_hook(hook: str, callback: Callable) -> None:
    """Register a callback for a hook on the global manager."""
    _manager.add_hook(hook, callback)


def trigger_hook(hook: str, *args, **kwargs) -> None:
    """Trigger a hook on the global manager."""
    _manager.trigger_hook(hook, *args, **kwargs)
