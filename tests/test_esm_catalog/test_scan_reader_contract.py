"""The reader packages' public contract.

Pins what a reader (however its internals are split across submodules) must
expose: it satisfies the Reader protocol and is registered for its format.
"""

from __future__ import annotations

import esm_catalog.scan.readers  # noqa: F401  (imports register the built-in readers)
from esm_catalog.scan.format import FileFormat
from esm_catalog.scan.reader import READERS, Reader, reader_for
from esm_catalog.scan.readers.netcdf import NetCDFReader


def test_netcdf_reader_satisfies_protocol():
    assert isinstance(NetCDFReader(), Reader)


def test_netcdf_format_is_registered():
    assert isinstance(reader_for(FileFormat.netcdf), Reader)


def test_every_registered_reader_satisfies_protocol():
    assert READERS
    assert all(isinstance(reader, Reader) for reader in READERS.values())
