"""The native pplan workflow engine: ``plan`` (read-only models) and
``execution`` (dispatching a resolved Workflow's work).

Deliberately no wildcard re-exports here -- import ``from .pplan import
plan``/``from .pplan import execution`` explicitly. See
esm_runscripts/__init__.py's own wildcard-import chain, and
simulation_config.py's BatchSystem workaround, for why a package that
wildcard-re-exports its submodules can end up shadowing its own
attributes once real imports resolve.
"""
