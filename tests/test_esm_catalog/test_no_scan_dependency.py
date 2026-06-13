"""Regression guard: the STAC model must not depend on the scan layer.

If this fails, someone reintroduced a scan import into stac/ — breaking the
ability to ship the STAC foundation as a standalone, reviewable unit.
"""

from __future__ import annotations

import ast
import pathlib

STAC_DIR = pathlib.Path(__file__).resolve().parent.parent / "stac"


def _imports(py_file: pathlib.Path) -> set[str]:
    tree = ast.parse(py_file.read_text())
    names: set[str] = set()
    for node in ast.walk(tree):
        if isinstance(node, ast.ImportFrom) and node.module:
            names.add(node.module)
        elif isinstance(node, ast.Import):
            names.update(a.name for a in node.names)
    return names


def test_stac_has_no_scan_imports():
    offenders = {}
    for py in STAC_DIR.rglob("*.py"):
        bad = {m for m in _imports(py) if m.startswith("esm_catalog.scan")}
        if bad:
            offenders[str(py.relative_to(STAC_DIR))] = bad
    assert not offenders, f"stac/ imports scan: {offenders}"


def test_stac_imports_without_scan(monkeypatch):
    import builtins

    real_import = builtins.__import__

    def blocked(name, *args, **kwargs):
        if name.startswith("esm_catalog.scan"):
            raise ImportError(f"scan import blocked: {name}")
        return real_import(name, *args, **kwargs)

    monkeypatch.setattr(builtins, "__import__", blocked)
    import importlib
    import esm_catalog.stac.item
    import esm_catalog.stac.collection
    import esm_catalog.stac.extensions.namelist
    import esm_catalog.stac.extensions.hpc
    import esm_catalog.stac.extensions.contacts
    import esm_catalog.stac.extensions.datacube
    import esm_catalog.stac.extensions.paleo
    importlib.reload(esm_catalog.stac.item)
    importlib.reload(esm_catalog.stac.collection)
    importlib.reload(esm_catalog.stac.extensions.namelist)
    importlib.reload(esm_catalog.stac.extensions.hpc)
    importlib.reload(esm_catalog.stac.extensions.contacts)
    importlib.reload(esm_catalog.stac.extensions.datacube)
    importlib.reload(esm_catalog.stac.extensions.paleo)
