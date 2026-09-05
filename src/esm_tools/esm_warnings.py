"""Make ESM-Tools deprecation warnings visible.

Deprecations are emitted with the community-standard :mod:`deprecation`
package (``deprecation.DeprecatedWarning``, which carries ``deprecated_in`` /
``removed_in`` version information). Those subclass :class:`DeprecationWarning`,
which Python silences by default outside ``__main__`` (see the default entries
in :data:`warnings.filters`) -- so a scientist running a runscript from inside a
library package would never see them.

Importing this module installs a filter, scoped to
``deprecation.DeprecatedWarning`` only, that shows those warnings without
un-silencing every stdlib / third-party ``DeprecationWarning``. ``filterwarnings``
inserts at the front of ``warnings.filters``, so this wins over the default
``ignore::DeprecationWarning`` entry. The ``default`` action prints the first
occurrence per (message, category, line), and our messages embed the offending
key, so each key warns once per call site.
"""

import warnings

import deprecation

warnings.filterwarnings("default", category=deprecation.DeprecatedWarning)
