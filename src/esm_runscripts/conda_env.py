import os
import yaml

from pathlib import Path


def get_conda_info():
    """
    Read conda environment information from the current shell environment.

    Inspects the ``CONDA_PREFIX`` and ``CONDA_EXE`` environment variables set
    by conda when a conda environment is active.

    Returns
    -------
    conda_env : str or None
        Full path to the active conda environment (value of ``CONDA_PREFIX``),
        or ``None`` if no conda environment is active.
    conda_root : str or None
        Root installation path of conda (two levels above ``CONDA_EXE``),
        or ``None`` if ``CONDA_EXE`` is not set.
    """
    conda_env = os.getenv("CONDA_PREFIX")
    conda_exe = os.getenv("CONDA_EXE")
    conda_root = str(Path(conda_exe).parent.parent) if conda_exe else None

    return conda_env, conda_root


def set_launched_with_conda(config):
    """
    Record whether ESM-Tools was launched from inside a conda environment.

    Sets ``config["computer"]["launched_with_conda"]`` to ``True`` if a conda
    environment is currently active (i.e. ``CONDA_PREFIX`` is set), or
    ``False`` otherwise. This flag is later used to decide whether to inject
    conda activation commands into the job script, and can also be used in
    as a switch for ``choose_`` blocks.

    Parameters
    ----------
    config : dict
        The simulation configuration dictionary. Modified in-place.

    Returns
    -------
    config : dict
        The updated configuration dictionary.
    """
    conda_env, _ = get_conda_info()
    config["computer"]["launched_with_conda"] = True if conda_env else False

    return config


def get_conda_info_from_file(config):
    """
    Read conda environment information previously saved by :func:`write_conda_info_file`.

    Looks for ``conda_info.yaml`` in ``config["general"]["thisrun_config_dir"]``
    (written during the ``prepexp`` phase) and returns the stored conda
    environment path and root. This allows the compute job script to activate
    the same conda environment that was used to launch ESM-Tools, even when the
    compute job starts in a fresh shell without the original environment active.

    Parameters
    ----------
    config : dict
        The simulation configuration dictionary. Must contain
        ``config["general"]["thisrun_config_dir"]``.

    Returns
    -------
    conda_env : str or None
        Full path to the conda environment, or ``None`` if the file does not
        exist or the key is absent.
    conda_root : str or None
        Root installation path of conda, or ``None`` if not stored.
    """
    conda_info_file = f'{config["general"]["thisrun_config_dir"]}/conda_info.yaml'
    if not os.path.isfile(conda_info_file):
        return None, None

    with open(conda_info_file, "r") as f:
        conda_info = yaml.safe_load(f)

    return conda_info.get("conda_env"), conda_info.get("conda_root")


def write_conda_info_file(config):
    """
    Stores the active conda environment information to a file in the run directory.

    Called during the ``prepexp`` recipe phase (``execution_mode == "run"``).
    Writes ``conda_info.yaml`` into ``config["general"]["thisrun_config_dir"]``
    so that the compute job script can later read it via
    :func:`get_conda_info_from_file` and activate the correct conda environment.

    Does nothing if no conda environment is active or if ``execution_mode`` is
    not ``"run"`` (e.g. during compilation).

    Parameters
    ----------
    config : dict
        The simulation configuration dictionary. Must contain
        ``config["general"]["thisrun_config_dir"]`` and
        ``config["general"]["execution_mode"]``.

    Returns
    -------
    config : dict
        The unchanged configuration dictionary (returned for recipe chaining).
    """
    if config["general"].get("execution_mode") != "run":
        return config

    conda_env, conda_root = get_conda_info()

    if not conda_env:
        return config

    conda_info = {"conda_env": conda_env, "conda_root": conda_root}

    conda_info_file = f'{config["general"]["thisrun_config_dir"]}/conda_info.yaml'
    with open(conda_info_file, "w") as f:
        yaml.dump(conda_info, f)

    return config
