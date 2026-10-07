"""CLI scaffold tests: every command is registered and shows help; 'status'
reports the local catalog state without contacting the server.
"""

from __future__ import annotations

import json
from datetime import datetime, timezone

import pystac
import pytest
from click.testing import CliRunner
from upath import UPath

from esm_catalog.cli import main
from esm_catalog.scan.workspace import WorkspaceState, catalog_dir, save_state
from esm_catalog.storage.geoparquet import write_shard


@pytest.fixture
def runner():
    return CliRunner()


def test_help_lists_all_commands(runner):
    result = runner.invoke(main, ["--help"])
    assert result.exit_code == 0
    for command in ("auth", "scan", "status", "server"):
        assert command in result.output


def test_help_hides_push_and_distributed(runner):
    # 'push' is a deprecated alias for 'put'; 'distributed' is flagged as
    # out-of-scope for this CLI (SLURM/Dask orchestration). Both still work
    # (hidden=True), just not advertised.
    result = runner.invoke(main, ["--help"])
    assert result.exit_code == 0
    assert "push" not in result.output
    assert "distributed" not in result.output


def test_push_still_invocable_as_hidden_alias(runner, tmp_path):
    coll = tmp_path / "coll.json"
    coll.write_text(json.dumps({"type": "Collection", "id": "c"}))
    result = runner.invoke(main, ["push", "--help"])
    assert result.exit_code == 0
    assert "deprecated" in result.output.lower()


def test_auth_subcommands_registered(runner):
    result = runner.invoke(main, ["auth", "--help"])
    assert result.exit_code == 0
    assert "login" in result.output
    assert "logout" in result.output


def test_scan_missing_run_reports_clean_error(runner):
    """A directory with no finished_config yields a one-line error, no traceback."""
    result = runner.invoke(main, ["scan", "--exp-root", "/nonexistent/xyz"])
    assert result.exit_code != 0
    assert "scan requires a completed ESM-Tools run" in result.output
    assert "Traceback" not in result.output
    # The SourcingError was translated to a clean ClickException, not propagated.
    assert not isinstance(result.exception, Exception) or isinstance(
        result.exception, SystemExit
    )


# --------------------------------------------------------------------------- #
# get / delete (CRUD against the server)
# --------------------------------------------------------------------------- #


@pytest.fixture
def fake_connect(monkeypatch):
    """Stub out auth + StacClient construction; return the stub client class.

    Tests configure the stub's methods per-case and read back calls made.
    """
    import esm_catalog.auth as auth_mod
    import esm_catalog.cli as cli_mod

    monkeypatch.setattr(auth_mod, "get_bearer_token", lambda settings: "tok")
    monkeypatch.setenv("ESM_CATALOG_SERVER_URL", "https://stac.example.org")

    calls: list[tuple] = []

    class FakeClient:
        def __init__(self, *a, **kw):
            pass

        def __enter__(self):
            return self

        def __exit__(self, *exc):
            return False

        def list_collections(self, limit=None):
            calls.append(("list_collections", limit))
            return {"collections": [{"id": "c1"}, {"id": "c2"}]}

        def get_collection(self, cid):
            calls.append(("get_collection", cid))
            return {"id": cid, "type": "Collection"}

        def delete_collection(self, cid):
            calls.append(("delete_collection", cid))

        def list_items(self, cid, limit=None):
            calls.append(("list_items", cid, limit))
            return {"features": [{"id": "i1", "collection": cid}], "numberMatched": 1}

        def get_item(self, cid, iid):
            calls.append(("get_item", cid, iid))
            return {"id": iid, "collection": cid, "type": "Feature"}

        def delete_item(self, cid, iid):
            calls.append(("delete_item", cid, iid))

    monkeypatch.setattr(cli_mod, "StacClient", FakeClient, raising=False)
    import esm_catalog.client as client_mod

    monkeypatch.setattr(client_mod, "StacClient", FakeClient)
    return calls


def test_get_collections_no_id_emits_jsonl(runner, fake_connect):
    result = runner.invoke(main, ["server", "get", "collections", "--json"])
    assert result.exit_code == 0, result.output
    lines = [json.loads(line) for line in result.output.strip().splitlines()]
    assert lines == [{"id": "c1"}, {"id": "c2"}]


def test_get_collections_with_id_emits_single_json(runner, fake_connect):
    result = runner.invoke(main, ["server", "get", "collections", "c1", "--json"])
    assert result.exit_code == 0, result.output
    assert json.loads(result.output) == {"id": "c1", "type": "Collection"}


def test_get_collections_passes_limit_through(runner, fake_connect):
    result = runner.invoke(main, ["server", "get", "collections", "--json", "--limit", "5"])
    assert result.exit_code == 0, result.output
    assert ("list_collections", 5) in fake_connect


def test_get_items_requires_collection_id(runner, fake_connect):
    result = runner.invoke(main, ["server", "get", "items", "c1", "--json"])
    assert result.exit_code == 0, result.output
    assert json.loads(result.output.strip()) == {"id": "i1", "collection": "c1"}


def test_get_items_with_item_id_emits_single(runner, fake_connect):
    result = runner.invoke(main, ["server", "get", "items", "c1", "i1", "--json"])
    assert result.exit_code == 0, result.output
    assert json.loads(result.output) == {
        "id": "i1", "collection": "c1", "type": "Feature",
    }  # fmt: skip


def test_delete_collections_aborts_without_confirmation(runner, fake_connect, monkeypatch):
    import questionary

    monkeypatch.setattr(questionary, "confirm", lambda *a, **kw: _FakeAsk(False))
    result = runner.invoke(main, ["server", "delete", "collections", "c1"])
    assert result.exit_code == 0, result.output
    assert not any(c[0] == "delete_collection" for c in fake_connect)
    assert "aborted" in result.output.lower()


def test_delete_collections_yes_flag_skips_confirmation(runner, fake_connect):
    result = runner.invoke(main, ["server", "delete", "collections", "c1", "--yes"])
    assert result.exit_code == 0, result.output
    assert ("delete_collection", "c1") in fake_connect


def test_delete_collections_confirm_prompt_shows_item_count(runner, fake_connect, monkeypatch):
    import questionary

    seen = {}

    def fake_confirm(message, **kw):
        seen["message"] = message
        return _FakeAsk(True)

    monkeypatch.setattr(questionary, "confirm", fake_confirm)
    result = runner.invoke(main, ["server", "delete", "collections", "c1"])
    assert result.exit_code == 0, result.output
    assert "1" in seen["message"]
    assert "c1" in seen["message"]
    assert ("delete_collection", "c1") in fake_connect


def test_delete_items_yes_flag_skips_confirmation(runner, fake_connect):
    result = runner.invoke(main, ["server", "delete", "items", "c1", "i1", "--yes"])
    assert result.exit_code == 0, result.output
    assert ("delete_item", "c1", "i1") in fake_connect


# --------------------------------------------------------------------------- #
# server queryables
# --------------------------------------------------------------------------- #


def test_queryables_get_emits_json(runner, monkeypatch):
    import esm_catalog.push as pushmod

    monkeypatch.setenv("ESM_CATALOG_SERVER_URL", "https://stac.example.org")
    monkeypatch.setattr(
        pushmod,
        "get_queryables",
        lambda api_url, verify_tls: {"a": {"type": "string"}},
    )
    result = runner.invoke(main, ["server", "queryables", "get", "--json"])
    assert result.exit_code == 0, result.output
    assert json.loads(result.output) == {"a": {"type": "string"}}


def test_queryables_print_recipe_reads_file(runner, tmp_path):
    delta = tmp_path / "delta.json"
    delta.write_text(json.dumps({"properties": {"a": {}, "b": {}}}))
    result = runner.invoke(
        main,
        ["server", "queryables", "print-recipe", str(delta), "--server", "https://stac.example.org"],
    )
    assert result.exit_code == 0, result.output
    assert "2 new queryable field(s)" in result.output
    assert "ssh stac.example.org" in result.output
    assert str(delta) in result.output


def test_queryables_print_recipe_reads_stdin(runner):
    result = runner.invoke(
        main,
        ["server", "queryables", "print-recipe", "--server", "https://stac.example.org"],
        input=json.dumps({"properties": {"a": {}}}),
    )
    assert result.exit_code == 0, result.output
    assert "1 new queryable field(s)" in result.output


def test_queryables_print_recipe_empty_delta_says_nothing_to_register(runner):
    result = runner.invoke(
        main,
        ["server", "queryables", "print-recipe"],
        input=json.dumps({"properties": {}}),
    )
    assert result.exit_code == 0, result.output
    assert "nothing to register" in result.output.lower()


class _FakeAsk:
    """Mimics questionary's Question.ask() API without a real prompt_toolkit loop."""

    def __init__(self, value):
        self._value = value

    def ask(self):
        return self._value


# --------------------------------------------------------------------------- #
# status
# --------------------------------------------------------------------------- #


def test_status_before_scan_says_so(runner, tmp_path, monkeypatch):
    monkeypatch.delenv("ESM_CATALOG_SERVER_URL", raising=False)
    result = runner.invoke(main, ["status", "--exp-root", str(tmp_path)])
    assert result.exit_code == 0
    assert "Not yet scanned" in result.output
    assert "not configured" in result.output


def test_status_after_scan_reports_catalog_contents(runner, tmp_path, monkeypatch):
    monkeypatch.setenv("ESM_CATALOG_SERVER_URL", "https://stac.example.org")
    catalog = catalog_dir(UPath(tmp_path))
    items_dir = catalog / "items"
    items_dir.mkdir(parents=True)

    save_state(
        catalog,
        WorkspaceState(experiment_id="exp-alpha", scanned={"a.nc": "abc123"}),
    )
    (catalog / "collection.json").write_text(
        json.dumps({"type": "Collection", "id": "exp-alpha"})
    )
    item = pystac.Item(
        id="tas.2000",
        geometry={"type": "Point", "coordinates": [0.0, 0.0]},
        bbox=[0.0, 0.0, 0.0, 0.0],
        datetime=datetime(2000, 1, 1, tzinfo=timezone.utc),
        properties={},
        collection="exp-alpha",
    )
    item.add_asset("data", pystac.Asset(href="file:///tas.2000.nc"))
    write_shard([item], items_dir / "exp-alpha_stac_1.parquet")
    (catalog / "queryables.json").write_text(
        json.dumps({"properties": {"namelist__a__b__c__d": {"type": "number"}}})
    )


# --------------------------------------------------------------------------- #
# validate-cmip6
# --------------------------------------------------------------------------- #


def _write_collection_with_summaries(catalog_root, summaries: dict) -> None:
    catalog = catalog_dir(UPath(catalog_root))
    catalog.mkdir(parents=True, exist_ok=True)
    (catalog / "collection.json").write_text(
        json.dumps({"type": "Collection", "id": "exp-alpha", "summaries": summaries})
    )


def test_validate_cmip6_before_scan_says_so(runner, tmp_path):
    result = runner.invoke(main, ["validate-cmip6", "--exp-root", str(tmp_path)])
    assert result.exit_code != 0
    assert "no collection.json" in result.output


def test_validate_cmip6_noop_without_declared_facets(runner, tmp_path):
    _write_collection_with_summaries(tmp_path, {})
    result = runner.invoke(main, ["validate-cmip6", "--exp-root", str(tmp_path)])
    assert result.exit_code == 0
    assert "nothing to validate" in result.output


def _cmip6_cv_installed() -> bool:
    try:
        import esgvoc.api as esgvoc_api

        esgvoc_api.valid_term_in_collection("CMIP6", "cmip6", "mip_era")
        return True
    except Exception:  # noqa: BLE001 -- any failure means "not installed/reachable"
        return False


def test_validate_cmip6_reports_invalid_facet(runner, tmp_path):
    if not _cmip6_cv_installed():
        pytest.skip("CMIP6 CV not installed locally (run: esgvoc use cmip6@latest)")
    _write_collection_with_summaries(
        tmp_path, {"cmip6:source_id": ["AWI-CM-3"], "cmip6:mip_era": ["CMIP6"]}
    )
    result = runner.invoke(main, ["validate-cmip6", "--exp-root", str(tmp_path)])
    assert result.exit_code != 0
    assert "cmip6:source_id='AWI-CM-3' is not a registered term" in result.output


def test_validate_cmip6_confirms_valid_facets(runner, tmp_path):
    if not _cmip6_cv_installed():
        pytest.skip("CMIP6 CV not installed locally (run: esgvoc use cmip6@latest)")
    _write_collection_with_summaries(
        tmp_path, {"cmip6:source_id": ["AWI-CM-1-1-MR"], "cmip6:mip_era": ["CMIP6"]}
    )
    result = runner.invoke(main, ["validate-cmip6", "--exp-root", str(tmp_path)])
    assert result.exit_code == 0
    assert "2 declared cmip6 facet(s) are valid" in result.output


# --------------------------------------------------------------------------- #
# distributed render-scripts
# --------------------------------------------------------------------------- #

_REQUIRED_FLAGS = [
    "--job-prefix", "catalog",
    "--scratch-dir", "/scratch/catalog",
    "--image-tag", "v6.68.0-rc.1-test-0.1.11",
    "--exp-root", "/exp/pi-ctrl-001",
    "--catalog-dir", "/exp/pi-ctrl-001/catalog",
    "--n-workers", "500",
]  # fmt: skip


def test_distributed_subcommand_registered(runner):
    result = runner.invoke(main, ["distributed", "--help"])
    assert result.exit_code == 0
    assert "render-scripts" in result.output


def test_render_scripts_missing_required_vars_reports_all_at_once(runner, tmp_path):
    result = runner.invoke(
        main, ["distributed", "render-scripts", "--out-dir", str(tmp_path)]
    )
    assert result.exit_code != 0
    for name in ("scratch_dir", "image_tag", "exp_root", "catalog_dir", "n_workers"):
        assert name in result.output


def test_render_scripts_writes_four_scripts_and_prints_submit_order(runner, tmp_path):
    result = runner.invoke(
        main,
        ["distributed", "render-scripts", "--out-dir", str(tmp_path), *_REQUIRED_FLAGS],
    )
    assert result.exit_code == 0, result.output
    for name in ("sched.sbatch", "worker.sbatch", "driver.sbatch", "cleanup.sbatch"):
        assert (tmp_path / name).exists()
    assert "SCHED_JOBID=$(sbatch --parsable" in result.output
    assert "--dependency=after:$SCHED_JOBID" in result.output
    assert "--dependency=afterany:$DRIVER_JOBID" in result.output


def test_render_scripts_worker_array_matches_n_workers(runner, tmp_path):
    runner.invoke(
        main,
        ["distributed", "render-scripts", "--out-dir", str(tmp_path), *_REQUIRED_FLAGS],
    )
    worker = (tmp_path / "worker.sbatch").read_text()
    assert "#SBATCH --array=1-500%500" in worker


def test_render_scripts_no_push_block_by_default(runner, tmp_path):
    runner.invoke(
        main,
        ["distributed", "render-scripts", "--out-dir", str(tmp_path), *_REQUIRED_FLAGS],
    )
    driver = (tmp_path / "driver.sbatch").read_text()
    assert "esm-catalog push" not in driver


def test_render_scripts_push_after_scan_flag_adds_push_block(runner, tmp_path):
    runner.invoke(
        main,
        [
            "distributed", "render-scripts", "--out-dir", str(tmp_path),
            *_REQUIRED_FLAGS,
            "--push-after-scan", "--server-url", "https://stac-dev.awi.de",
        ],  # fmt: skip
    )
    driver = (tmp_path / "driver.sbatch").read_text()
    assert "esm-catalog push" in driver
    assert "https://stac-dev.awi.de" in driver


def test_render_scripts_cli_flag_overrides_vars_file(runner, tmp_path):
    vars_file = tmp_path / "vars.yaml"
    vars_file.write_text(
        "\n".join(
            [
                "job_prefix: fromfile",
                "scratch_dir: /scratch/fromfile",
                "image_tag: v6.68.0-rc.1-test-0.1.11",
                "exp_root: /exp/fromfile",
                "catalog_dir: /exp/fromfile/catalog",
                "n_workers: 100",
            ]
        )
    )
    out_dir = tmp_path / "rendered"
    result = runner.invoke(
        main,
        [
            "distributed", "render-scripts", str(vars_file),
            "--out-dir", str(out_dir), "--n-workers", "999",
        ],  # fmt: skip
    )
    assert result.exit_code == 0, result.output
    worker = (out_dir / "worker.sbatch").read_text()
    assert "#SBATCH --array=1-999%999" in worker
    sched = (out_dir / "sched.sbatch").read_text()
    assert "fromfile-sched" in sched
    assert "/scratch/fromfile" in sched


def test_render_scripts_log_dir_defaults_from_state_dir(runner, tmp_path, monkeypatch):
    # state_dir() wraps platformdirs, whose XDG_STATE_HOME honouring is a Unix-only
    # convention (macOS ignores it by design) -- monkeypatch the function itself
    # rather than the env var, so this test is portable across dev machines.
    monkeypatch.setattr(
        "esm_catalog.xdg.state_dir", lambda: tmp_path / "state" / "esm-catalog"
    )
    out_dir = tmp_path / "rendered"
    result = runner.invoke(
        main,
        ["distributed", "render-scripts", "--out-dir", str(out_dir), *_REQUIRED_FLAGS],
    )
    assert result.exit_code == 0, result.output
    sched = (out_dir / "sched.sbatch").read_text()
    assert f"{tmp_path}/state/esm-catalog/logs" in sched


def test_render_scripts_defaults_to_singularity(runner, tmp_path):
    out_dir = tmp_path / "rendered"
    result = runner.invoke(
        main,
        ["distributed", "render-scripts", "--out-dir", str(out_dir), *_REQUIRED_FLAGS],
    )
    assert result.exit_code == 0, result.output
    sched = (out_dir / "sched.sbatch").read_text()
    assert "module load singularity" in sched
    assert "singularity exec" in sched
    assert "SINGULARITY_CACHEDIR" in sched
    assert "apptainer" not in sched


def test_render_scripts_container_bin_override_switches_cache_env_var(runner, tmp_path):
    out_dir = tmp_path / "rendered"
    result = runner.invoke(
        main,
        [
            "distributed", "render-scripts", "--out-dir", str(out_dir),
            "--container-bin", "apptainer", *_REQUIRED_FLAGS,
        ],  # fmt: skip
    )
    assert result.exit_code == 0, result.output
    sched = (out_dir / "sched.sbatch").read_text()
    assert "module load apptainer" in sched
    assert "apptainer exec" in sched
    assert "APPTAINER_CACHEDIR" in sched


def test_render_scripts_repeated_bind_path_flags(runner, tmp_path):
    out_dir = tmp_path / "rendered"
    result = runner.invoke(
        main,
        [
            "distributed", "render-scripts", "--out-dir", str(out_dir),
            "--bind-path", "/work", "--bind-path", "/scratch", *_REQUIRED_FLAGS,
        ],  # fmt: skip
    )
    assert result.exit_code == 0, result.output
    sched = (out_dir / "sched.sbatch").read_text()
    assert "-B /work -B /scratch" in sched
    assert "/albedo" not in sched  # explicit flags replace the default, don't add to it


def test_render_scripts_bind_path_defaults_to_albedo(runner, tmp_path):
    out_dir = tmp_path / "rendered"
    result = runner.invoke(
        main,
        ["distributed", "render-scripts", "--out-dir", str(out_dir), *_REQUIRED_FLAGS],
    )
    assert result.exit_code == 0, result.output
    sched = (out_dir / "sched.sbatch").read_text()
    assert "-B /albedo" in sched


def test_render_scripts_account_flag_adds_sbatch_account(runner, tmp_path):
    out_dir = tmp_path / "rendered"
    result = runner.invoke(
        main,
        [
            "distributed", "render-scripts", "--out-dir", str(out_dir),
            "--account", "ab1234", *_REQUIRED_FLAGS,
        ],  # fmt: skip
    )
    assert result.exit_code == 0, result.output
    for name in ("sched.sbatch", "worker.sbatch", "driver.sbatch", "cleanup.sbatch"):
        assert "#SBATCH --account=ab1234" in (out_dir / name).read_text()


def test_render_scripts_no_account_by_default(runner, tmp_path):
    out_dir = tmp_path / "rendered"
    result = runner.invoke(
        main,
        ["distributed", "render-scripts", "--out-dir", str(out_dir), *_REQUIRED_FLAGS],
    )
    assert result.exit_code == 0, result.output
    sched = (out_dir / "sched.sbatch").read_text()
    assert "--account" not in sched
    assert "note: no --account set" in result.output


def test_render_scripts_throttle_overrides_array_percent(runner, tmp_path):
    out_dir = tmp_path / "rendered"
    result = runner.invoke(
        main,
        [
            "distributed", "render-scripts", "--out-dir", str(out_dir),
            "--throttle", "20", *_REQUIRED_FLAGS,
        ],  # fmt: skip
    )
    assert result.exit_code == 0, result.output
    worker = (out_dir / "worker.sbatch").read_text()
    assert "#SBATCH --array=1-500%20" in worker


def test_render_scripts_throttle_defaults_to_n_workers(runner, tmp_path):
    out_dir = tmp_path / "rendered"
    result = runner.invoke(
        main,
        ["distributed", "render-scripts", "--out-dir", str(out_dir), *_REQUIRED_FLAGS],
    )
    assert result.exit_code == 0, result.output
    worker = (out_dir / "worker.sbatch").read_text()
    assert "#SBATCH --array=1-500%500" in worker


def test_render_scripts_notes_defaulted_qos_and_partition(runner, tmp_path):
    out_dir = tmp_path / "rendered"
    result = runner.invoke(
        main,
        ["distributed", "render-scripts", "--out-dir", str(out_dir), *_REQUIRED_FLAGS],
    )
    assert result.exit_code == 0, result.output
    assert "note: using Albedo defaults not overridden: qos=12h, partition=smp" in result.output


def test_render_scripts_no_defaults_note_when_all_overridden(runner, tmp_path):
    out_dir = tmp_path / "rendered"
    result = runner.invoke(
        main,
        [
            "distributed", "render-scripts", "--out-dir", str(out_dir),
            "--qos", "normal", "--partition", "compute", "--account", "ab1234",
            *_REQUIRED_FLAGS,
        ],  # fmt: skip
    )
    assert result.exit_code == 0, result.output
    assert "note:" not in result.output


def test_dump_vars_template_honours_account_and_throttle_flags(runner):
    result = runner.invoke(
        main,
        [
            "distributed", "render-scripts", "--dump-vars-template",
            "--account", "ab1234", "--throttle", "20",
        ],  # fmt: skip
    )
    assert result.exit_code == 0, result.output
    assert "account: ab1234" in result.output
    assert "throttle: 20" in result.output
    assert "# throttle: 20" not in result.output


def test_render_scripts_multinode_mode_renders_srun_worker(runner, tmp_path):
    out_dir = tmp_path / "rendered"
    flags = [
        "--job-prefix", "catalog",
        "--scratch-dir", "/scratch/catalog",
        "--image-tag", "v6.68.0-rc.1-test-0.1.11",
        "--exp-root", "/exp/pi-ctrl-001",
        "--catalog-dir", "/exp/pi-ctrl-001/catalog",
        "--worker-mode", "multinode",
        "--n-nodes", "4",
    ]  # fmt: skip
    result = runner.invoke(
        main,
        ["distributed", "render-scripts", "--out-dir", str(out_dir), *flags],
    )
    assert result.exit_code == 0, result.output
    worker = (out_dir / "worker.sbatch").read_text()
    assert "#SBATCH -N4" in worker
    assert "srun --ntasks=512 --ntasks-per-node=128" in worker
    assert "singularity exec" in worker


def test_render_scripts_multinode_missing_n_nodes_errors(runner, tmp_path):
    flags = [
        "--job-prefix", "catalog",
        "--scratch-dir", "/scratch/catalog",
        "--image-tag", "v6.68.0-rc.1-test-0.1.11",
        "--exp-root", "/exp/pi-ctrl-001",
        "--catalog-dir", "/exp/pi-ctrl-001/catalog",
        "--worker-mode", "multinode",
    ]  # fmt: skip
    result = runner.invoke(
        main,
        ["distributed", "render-scripts", "--out-dir", str(tmp_path), *flags],
    )
    assert result.exit_code != 0
    assert "n_nodes" in result.output


def test_render_scripts_multinode_cores_per_node_override(runner, tmp_path):
    out_dir = tmp_path / "rendered"
    flags = [
        "--job-prefix", "catalog",
        "--scratch-dir", "/scratch/catalog",
        "--image-tag", "v6.68.0-rc.1-test-0.1.11",
        "--exp-root", "/exp/pi-ctrl-001",
        "--catalog-dir", "/exp/pi-ctrl-001/catalog",
        "--worker-mode", "multinode",
        "--n-nodes", "2",
        "--cores-per-node", "64",
    ]  # fmt: skip
    result = runner.invoke(
        main,
        ["distributed", "render-scripts", "--out-dir", str(out_dir), *flags],
    )
    assert result.exit_code == 0, result.output
    worker = (out_dir / "worker.sbatch").read_text()
    assert "srun --ntasks=128 --ntasks-per-node=64" in worker


def test_dump_vars_template_prints_yaml_and_skips_rendering(runner):
    result = runner.invoke(
        main,
        ["distributed", "render-scripts", "--dump-vars-template"],
    )
    # No required flags were passed -- exit_code == 0 here (rather than the
    # "missing required variable(s)" ClickException) proves --dump-vars-template
    # short-circuited before validation/rendering ran at all.
    assert result.exit_code == 0, result.output
    assert "job_prefix: catalog" in result.output
    assert "CHANGE_ME" in result.output


def test_dump_vars_template_honours_worker_mode_flags(runner):
    result = runner.invoke(
        main,
        [
            "distributed", "render-scripts", "--dump-vars-template",
            "--worker-mode", "multinode", "--n-nodes", "4", "--partition", "compute",
        ],  # fmt: skip
    )
    assert result.exit_code == 0, result.output
    assert "worker_mode: multinode" in result.output
    assert "n_nodes: 4" in result.output
    assert "partition: compute" in result.output
    assert "# worker_mode: array" in result.output
    assert "# n_workers: 3000" in result.output
