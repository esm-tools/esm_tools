"""Pluggable per-format readers.

Importing this package imports each reader module, which registers it on the
reader registry (see ``esm_catalog.scan.reader``). The scan core imports this so
the registry is populated; worker processes import it transitively too.
"""

from esm_catalog.scan.readers import netcdf as _netcdf  # noqa: F401  (registers)
from esm_catalog.scan.readers import grib as _grib  # noqa: F401  (registers)
