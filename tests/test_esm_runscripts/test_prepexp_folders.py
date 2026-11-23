"""Tests for prepexp.py's pathlib-based directory creation.

Guards against the bug reported for ``--wf-engine pplan``: prepcompute's
``create_new_files`` does a bare ``os.mkdir`` on
``.../run_.../config/<model>`` and assumes the parent (``.../config``)
already exists. ``_create_folders`` is what's supposed to create that
whole chain ahead of time -- it must create every missing parent, not
just the leaf directory.
"""

from esm_runscripts.prepexp import _create_component_folders, _create_folders


def test_create_folders_makes_the_full_parent_chain(tmp_path):
    # Neither "experiment_config_dir" nor its parent exists yet -- a bare
    # os.makedirs-without-exist_ok or os.mkdir would fail here.
    config = {
        "experiment_config_dir": str(tmp_path / "nested" / "does" / "not" / "exist" / "config"),
        "thisrun_config_dir": str(tmp_path / "run_20000101-20000101" / "config"),
    }

    _create_folders(config, ["config"])

    assert (tmp_path / "nested" / "does" / "not" / "exist" / "config").is_dir()
    assert (tmp_path / "run_20000101-20000101" / "config").is_dir()


def test_create_folders_is_idempotent(tmp_path):
    config = {
        "experiment_config_dir": str(tmp_path / "config"),
        "thisrun_config_dir": str(tmp_path / "run" / "config"),
    }

    _create_folders(config, ["config"])
    _create_folders(config, ["config"])  # must not raise on re-run

    assert (tmp_path / "config").is_dir()
    assert (tmp_path / "run" / "config").is_dir()


def test_create_folders_skips_ignore_and_experiment_dir_for_work(tmp_path):
    config = {
        "thisrun_work_dir": str(tmp_path / "run" / "work"),
    }

    # "work" has no experiment_work_dir entry at all -- must not be looked up.
    _create_folders(config, ["work", "ignore"])

    assert (tmp_path / "run" / "work").is_dir()


def test_create_component_folders_creates_every_valid_model(tmp_path):
    config = {
        "general": {
            "valid_model_names": ["fesom", "oifs"],
            "all_model_filetypes": ["config"],
        },
        "fesom": {
            "experiment_config_dir": str(tmp_path / "fesom" / "config"),
            "thisrun_config_dir": str(tmp_path / "run" / "fesom" / "config"),
        },
        "oifs": {
            "experiment_config_dir": str(tmp_path / "oifs" / "config"),
            "thisrun_config_dir": str(tmp_path / "run" / "oifs" / "config"),
        },
    }

    _create_component_folders(config)

    assert (tmp_path / "run" / "fesom" / "config").is_dir()
    assert (tmp_path / "run" / "oifs" / "config").is_dir()
