"""
Unit tests for hook system
"""

import pytest
from esm_tools.hooks import HookManager, add_hook, trigger_hook


class TestHookManager:
    """Test HookManager class"""

    def test_add_hook(self):
        """Test basic hook registration"""
        manager = HookManager()
        called = []

        manager.add_hook("test:event", lambda: called.append(1))
        manager.trigger_hook("test:event")

        assert len(called) == 1

    def test_hook_with_args(self):
        """Test hooks receive arguments"""
        manager = HookManager()
        received = []

        manager.add_hook("test:event", lambda x, y: received.append((x, y)))
        manager.trigger_hook("test:event", 1, 2)

        assert received == [(1, 2)]

    def test_hook_with_kwargs(self):
        """Test hooks receive keyword arguments"""
        manager = HookManager()
        received = []

        manager.add_hook("test:event", lambda **kw: received.append(kw))
        manager.trigger_hook("test:event", a=1, b=2)

        assert received == [{"a": 1, "b": 2}]

    def test_multiple_hooks(self):
        """Test multiple callbacks for same hook"""
        manager = HookManager()
        results = []

        manager.add_hook("test:event", lambda: results.append("a"))
        manager.add_hook("test:event", lambda: results.append("b"))
        manager.trigger_hook("test:event")

        assert results == ["a", "b"]

    def test_error_isolation(self):
        """Test one bad callback doesn't break others"""
        manager = HookManager()
        results = []

        manager.add_hook("test:event", lambda: 1/0)  # Will raise
        manager.add_hook("test:event", lambda: results.append("ok"))
        manager.trigger_hook("test:event")

        assert results == ["ok"]

    def test_nonexistent_hook(self):
        """Test triggering nonexistent hook doesn't error"""
        manager = HookManager()
        manager.trigger_hook("nonexistent:hook")
        # Should not raise


class TestGlobalHooks:
    """Test global hook functions"""

    def test_global_add_hook(self):
        """Test global add_hook function"""
        called = []
        add_hook("global:test", lambda: called.append(1))
        trigger_hook("global:test")

        assert len(called) >= 1  # May have been called by other tests

    def test_hook_order(self):
        """Test hooks called in registration order"""
        results = []

        add_hook("order:test", lambda: results.append(1))
        add_hook("order:test", lambda: results.append(2))
        add_hook("order:test", lambda: results.append(3))

        trigger_hook("order:test")

        # Should be called in order [1, 2, 3]
        assert results == [1, 2, 3]


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
