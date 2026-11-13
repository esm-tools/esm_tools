"""The native P-Plan model layer: read-only, ontology-mapped shapes for a
resolved workflow graph (``Plan``, ``MultiStep``, ``Step``, ``Workflow``).

Deciding *how* a resolved workflow's work actually runs -- dispatch,
launching a runscript, submitting to a scheduler -- lives in
``esm_runscripts.execution``, not here. Deliberately no wildcard
re-exports here either -- import ``from .pplan import plan`` explicitly.
See esm_runscripts/__init__.py's own wildcard-import chain, and
simulation_config.py's ``BatchSystem`` workaround, for why a package that
wildcard-re-exports its submodules can end up shadowing its own
attributes once real imports resolve.
"""
