# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

ESM-Tools is a Python toolkit for Earth System Modeling. It provides integrated tools for downloading, configuring, compiling, and running Earth System Model simulations on HPC systems, including model coupling via OASIS3-MCT.

**Documentation**: https://esm-tools.readthedocs.io/

## Common Commands

```bash
# Install in development mode
pip install -e .

# Run tests
pytest tests/
pytest tests/test_esm_parser/ -v  # specific module

# Lint code
flake8 src/

# Build docs locally
cd docs && sphinx-build -b html . _build/html

# Version bump (maintainers)
bumpversion patch  # or minor/major
git push && git push --tags
```

## Architecture

### Package Structure

All Python packages live in `src/` with CLI entry points defined in `setup.py`:

| Package | CLI Command | Purpose |
|---------|-------------|---------|
| esm_parser | - | YAML configuration parsing with variables, choose statements, includes |
| esm_runscripts | esm_runscripts | Generates HPC job scripts (largest module, 41 files) |
| esm_master | esm_master | Model compilation and installation |
| esm_tools | esm_tools | Core package, config loading, installation detection |
| esm_environment | - | HPC environment detection and setup |
| esm_archiving | esm_archive | Data archiving |
| esm_database | esm_database | Simulation tracking |

### Configuration System

YAML-based configs with custom parsing features (similar to Ansible):
- `configs/components/` - Model-specific configs (echam, fesom, oifs, icon, nemo)
- `configs/setups/` - Coupled model setups (awicm, awiesm)
- `configs/machines/` - HPC machine configs (levante, ollie, blogin)
- `namelists/` - Fortran namelist templates

### Key Patterns

**Error Handling** (`src/esm_tools/error_handling.py`):
```python
from esm_tools.error_handling import user_error, user_note
user_error("ErrorType", "message", exit_code=1, hints=["hint1"])
```

**Logging**: Uses `loguru`. Debug with `DEBUG_ESM_TOOLS=1` environment variable.

**Installation Detection**: Code handles both standard and editable (`pip -e`) installs differently. See `src/esm_tools/__init__.py` for config path resolution.

**Config Resolution**: Use `SimulationSetup` from `esm_runscripts.sim_objects` to build a fully resolved config.

### ESM-Tests (`src/esm_tests/`)

Testing framework for compilation scripts and simulation runs. Reference data lives in a git submodule (`src/esm_tests/resources/` → `esm_tests_info` repo) with `runscripts/` and `last_tested/` directories.

## Docstring Conventions

Use **numpy-style** docstrings throughout the codebase.

- Multi-line docstrings: opening `"""` on its own line, closing `"""` on its own line.
- One-line docstrings (for trivial functions): `"""Single line description."""` on one line.
- Include `Parameters`, `Returns`, and `Raises` sections when the function signature is non-trivial.
- Use `Note` / `Notes` sections for important caveats.
- Reference config keys with double backticks: `` ``key_name`` ``.

Example:

```python
def my_function(config, name):
    """
    Short summary of what the function does.

    Longer description if needed, explaining behavior, side effects,
    or important details.

    Parameters
    ----------
    config : dict
        The simulation configuration dictionary.
    name : str
        Name of the component.

    Returns
    -------
    result : bool
        Whether the operation succeeded.
    """
```

## Git Workflow

- **release** branch: stable releases
- **develop** branch: latest development
- Feature branches: `feat/your-feature-name`

PRs should pass `flake8 src/` before submission.

## Supported Models

ECHAM, FESOM 2.x, OpenIFS, ICON, NEMO, JSBACH, LPJG, RECOM with OASIS3-MCT coupling.
