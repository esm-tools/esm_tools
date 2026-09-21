"""The reader packages' public contract.

Pins what a reader (however its internals are split across submodules) must
expose: it satisfies the Reader protocol and is registered for its format via
the ``get_reader`` pluggy hookspec (see esm_catalog.scan.readers.plugins).
"""

from __future__ import annotations

from esm_catalog.scan.format import FileFormat
from esm_catalog.scan.reader import Reader, reader_for
from esm_catalog.scan.readers.netcdf import NetCDFReader
from esm_catalog.scan.readers.plugins import get_reader_plugin_manager


def test_netcdf_reader_satisfies_protocol():
    assert isinstance(NetCDFReader(), Reader)


def test_netcdf_format_is_registered():
    assert isinstance(reader_for(FileFormat.netcdf), Reader)


def test_every_registered_reader_satisfies_protocol():
    pm = get_reader_plugin_manager()
    readers = [
        pm.hook.get_reader(file_format=file_format)
        for file_format in FileFormat
    ]
    readers = [reader for reader in readers if reader is not None]
    assert readers
    assert all(isinstance(reader, Reader) for reader in readers)


def test_unhandled_format_raises_lookup_error():
    # sentinel: no plugin claims this file_format, and reader_for() must not
    # silently return something -- every registered reader's get_reader
    # correctly returns None for formats it doesn't own.
    assert get_reader_plugin_manager().hook.get_reader(file_format="not-a-real-format") is None
