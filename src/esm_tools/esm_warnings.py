"""
Warning categories for ESM-Tools.

Python silences :class:`DeprecationWarning` by default unless it originates in
``__main__`` (see the default entries in :data:`warnings.filters`). ESM-Tools
raises its deprecations from inside library packages, so a plain
``DeprecationWarning`` would never reach the scientist running a runscript.

To keep our deprecations both *semantically correct* (they really are
deprecations) and *actually visible*, we:

1. give ESM-Tools its own :class:`EsmToolsDeprecationWarning` base class that
   still inherits from :class:`DeprecationWarning`, and
2. install a filter scoped to that base class only, so our warnings are shown
   without un-silencing every stdlib / third-party ``DeprecationWarning``.
"""

import inspect
import warnings


class EsmToolsDeprecationWarning(DeprecationWarning):
    """Base class for all deprecations emitted by ESM-Tools.

    Subclasses put the *human* explanation in their class docstring and the
    *specifics* in ``__init__``. :meth:`__str__` then composes the emitted
    message from a one-line, subclass-supplied summary followed by that
    docstring, so the explanatory prose lives in exactly one place.
    """

    def __str__(self):
        """Compose the warning text: specific summary + class docstring."""
        summary = self.summary()
        doc = inspect.getdoc(type(self)) or ""
        return f"{summary}\n{doc}" if doc else summary

    def summary(self):
        """One-line, situation-specific lead. Overridden by subclasses."""
        # Fall back to whatever positional args were passed to __init__.
        return " ".join(str(a) for a in self.args)


class ConfigKeyRenamedWarning(EsmToolsDeprecationWarning):
    """The old spelling is still accepted for now (translated automatically)
    but is deprecated and will be removed in a future release.
    """

    def __init__(self, old, new):
        self.old = old
        self.new = new
        super().__init__(old, new)

    def summary(self):
        return f"Config key '{self.old}' is deprecated; use '{self.new}'."


# Show our own deprecations once per call site. "default" prints the first
# occurrence for each (message, category, lineno); because our messages embed
# the offending key, each distinct key warns once. Scoping the filter to
# EsmToolsDeprecationWarning leaves stdlib/third-party DeprecationWarnings
# silenced as usual. filterwarnings inserts at the front of warnings.filters,
# so this wins over the default ``ignore::DeprecationWarning`` entry.
warnings.filterwarnings("default", category=EsmToolsDeprecationWarning)


__all__ = [
    "EsmToolsDeprecationWarning",
    "ConfigKeyRenamedWarning",
]
