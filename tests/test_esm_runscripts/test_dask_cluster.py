import json
import os
import tempfile
import unittest
from unittest.mock import MagicMock, patch

from esm_runscripts.dask_cluster import (
    DaskStatus,
    get_dask_cluster_status,
    ini_dask_cluster,
    initialize_dask_cluster,
    uses_dask,
    wait_for_dask_status,
)


def write_scheduler_json(address="tcp://127.0.0.1:8786"):
    """Write a temporary scheduler JSON file and return its path."""
    f = tempfile.NamedTemporaryFile(mode="w", suffix=".json", delete=False)
    json.dump({"address": address}, f)
    f.flush()
    f.close()
    return f.name


def make_mock_client(
    workers=None, submit_result=None, submit_error=None, info_error=None
):
    """
    Create a mock dask Client with configurable workers and submit behavior.

    Parameters
    ----------
    workers : dict or None
        Workers dict for scheduler_info (e.g. ``{"w1": {}, "w2": {}}``).
        ``None`` means empty.
    submit_result : object or None
        Return value for ``client.submit(...).result()``.
    submit_error : Exception or None
        Exception raised by ``client.submit(...).result()``.
    info_error : Exception or None
        Exception raised by ``client.scheduler_info()``.
    """
    client = MagicMock()
    if info_error:
        client.scheduler_info.side_effect = info_error
    else:
        client.scheduler_info.return_value = {"workers": workers or {}}
    if submit_error:
        future = MagicMock()
        future.result.side_effect = submit_error
        client.submit.return_value = future
    elif submit_result is not None:
        future = MagicMock()
        future.result.return_value = submit_result
        client.submit.return_value = future
    return client


class TestDaskStatus(unittest.TestCase):
    """Test the DaskStatus enum ordering."""

    def test_ordering(self):
        self.assertLess(DaskStatus.MISSING_JSON, DaskStatus.SCHEDULER_ERROR)
        self.assertLess(DaskStatus.SCHEDULER_ERROR, DaskStatus.WORKERS_ERROR)
        self.assertLess(DaskStatus.WORKERS_ERROR, DaskStatus.NO_WORKERS)
        self.assertLess(DaskStatus.NO_WORKERS, DaskStatus.RUNNING)
        self.assertLess(DaskStatus.RUNNING, DaskStatus.TESTED)

    def test_running_is_ready(self):
        self.assertGreaterEqual(DaskStatus.RUNNING, DaskStatus.RUNNING)
        self.assertGreaterEqual(DaskStatus.TESTED, DaskStatus.RUNNING)
        self.assertLess(DaskStatus.NO_WORKERS, DaskStatus.RUNNING)


class TestUsesDask(unittest.TestCase):
    """Test the uses_dask config check."""

    def test_no_dask_section(self):
        config = {"general": {}}
        self.assertFalse(uses_dask(config))

    def test_empty_actions(self):
        config = {"general": {}, "dask": {"actions": []}}
        self.assertFalse(uses_dask(config))

    def test_action_not_set_to_dask(self):
        config = {
            "general": {"parallel_file_movements": "threads"},
            "dask": {"actions": ["parallel_file_movements"]},
        }
        self.assertFalse(uses_dask(config))

    def test_action_set_to_dask(self):
        config = {
            "general": {"parallel_file_movements": "dask"},
            "dask": {"actions": ["parallel_file_movements"]},
        }
        self.assertTrue(uses_dask(config))

    def test_multiple_actions_one_dask(self):
        config = {
            "general": {
                "parallel_file_movements": "dask",
                "some_other_action": "threads",
            },
            "dask": {"actions": ["parallel_file_movements", "some_other_action"]},
        }
        self.assertTrue(uses_dask(config))

    def test_multiple_actions_none_dask(self):
        config = {
            "general": {
                "parallel_file_movements": "threads",
                "some_other_action": "threads",
            },
            "dask": {"actions": ["parallel_file_movements", "some_other_action"]},
        }
        self.assertFalse(uses_dask(config))


class TestGetDaskClusterStatus(unittest.TestCase):
    """Test get_dask_cluster_status with mocked filesystem and dask client."""

    def setUp(self):
        self.tmp_path = None

    def tearDown(self):
        if self.tmp_path and os.path.exists(self.tmp_path):
            os.unlink(self.tmp_path)

    def _write_json(self, address="tcp://127.0.0.1:8786"):
        self.tmp_path = write_scheduler_json(address)
        return self.tmp_path

    def test_missing_json_file(self):
        status, n_workers = get_dask_cluster_status("/nonexistent/path.json")
        self.assertEqual(status, DaskStatus.MISSING_JSON)
        self.assertEqual(n_workers, 0)

    def test_valid_json_scheduler_unreachable(self):
        path = self._write_json("tcp://127.0.0.1:99999")
        status, n_workers = get_dask_cluster_status(path, client_timeout=0.01)
        self.assertIn(
            status,
            [DaskStatus.SCHEDULER_ERROR, DaskStatus.WORKERS_ERROR],
        )
        self.assertEqual(n_workers, 0)

    @patch("esm_runscripts.dask_cluster.daskd.Client")
    def test_no_workers(self, mock_client_cls):
        mock_client_cls.return_value = make_mock_client(workers={})
        path = self._write_json()
        status, n_workers = get_dask_cluster_status(path)
        self.assertEqual(status, DaskStatus.NO_WORKERS)
        self.assertEqual(n_workers, 0)

    @patch("esm_runscripts.dask_cluster.daskd.Client")
    def test_with_workers(self, mock_client_cls):
        mock_client_cls.return_value = make_mock_client(
            workers={"w1": {}, "w2": {}},
        )
        path = self._write_json()
        status, n_workers = get_dask_cluster_status(path)
        self.assertEqual(status, DaskStatus.RUNNING)
        self.assertEqual(n_workers, 2)

    @patch("esm_runscripts.dask_cluster.daskd.Client")
    def test_test_mode_success(self, mock_client_cls):
        mock_client_cls.return_value = make_mock_client(
            workers={"w1": {}},
            submit_result=True,
        )
        path = self._write_json()
        status, n_workers = get_dask_cluster_status(path, test=True)
        self.assertEqual(status, DaskStatus.TESTED)
        self.assertEqual(n_workers, 1)

    @patch("esm_runscripts.dask_cluster.daskd.Client")
    def test_test_mode_failure(self, mock_client_cls):
        mock_client_cls.return_value = make_mock_client(
            workers={"w1": {}},
            submit_error=RuntimeError("task failed"),
        )
        path = self._write_json()
        status, n_workers = get_dask_cluster_status(path, test=True)
        self.assertEqual(status, DaskStatus.RUNNING)
        self.assertEqual(n_workers, 1)

    @patch("esm_runscripts.dask_cluster.daskd.Client")
    def test_workers_info_exception(self, mock_client_cls):
        mock_client_cls.return_value = make_mock_client(
            info_error=RuntimeError("conn lost"),
        )
        path = self._write_json()
        status, n_workers = get_dask_cluster_status(path)
        self.assertEqual(status, DaskStatus.WORKERS_ERROR)
        self.assertEqual(n_workers, 0)


class TestWaitForDaskStatus(unittest.TestCase):
    """Test the polling loop in wait_for_dask_status."""

    @patch("esm_runscripts.dask_cluster.time.sleep")
    @patch("esm_runscripts.dask_cluster.get_dask_cluster_status")
    def test_immediate_success(self, mock_status, mock_sleep):
        mock_status.return_value = (DaskStatus.RUNNING, 2)
        status, n_workers = wait_for_dask_status(
            "/fake.json",
            DaskStatus.RUNNING,
            timeout=5,
            poll_interval=0.5,
            description="test",
        )
        self.assertEqual(status, DaskStatus.RUNNING)
        self.assertEqual(n_workers, 2)
        mock_sleep.assert_not_called()

    @patch("esm_runscripts.dask_cluster.time.sleep")
    @patch("esm_runscripts.dask_cluster.get_dask_cluster_status")
    def test_eventual_success(self, mock_status, mock_sleep):
        mock_status.side_effect = [
            (DaskStatus.NO_WORKERS, 0),
            (DaskStatus.NO_WORKERS, 0),
            (DaskStatus.RUNNING, 3),
        ]
        status, n_workers = wait_for_dask_status(
            "/fake.json",
            DaskStatus.RUNNING,
            timeout=5,
            poll_interval=0.5,
            description="test",
        )
        self.assertEqual(status, DaskStatus.RUNNING)
        self.assertEqual(n_workers, 3)
        self.assertEqual(mock_sleep.call_count, 2)

    @patch("esm_runscripts.dask_cluster.time.sleep")
    @patch("esm_runscripts.dask_cluster.get_dask_cluster_status")
    def test_timeout(self, mock_status, mock_sleep):
        mock_status.return_value = (DaskStatus.MISSING_JSON, 0)
        status, n_workers = wait_for_dask_status(
            "/fake.json",
            DaskStatus.RUNNING,
            timeout=1.0,
            poll_interval=0.5,
            description="test",
        )
        self.assertEqual(status, DaskStatus.MISSING_JSON)
        self.assertEqual(n_workers, 0)
        self.assertEqual(mock_sleep.call_count, 2)


class TestInitializeDaskCluster(unittest.TestCase):
    """Test the initialize_dask_cluster entry point."""

    @patch(
        "esm_runscripts.dask_cluster.esm_parser.determine_computer_and_node_from_hostname"
    )
    def test_skips_on_login_node(self, mock_hostname):
        mock_hostname.return_value = ("levante", "login_nodes")
        config = {
            "general": {"parallel_file_movements": "dask"},
            "dask": {"actions": ["parallel_file_movements"]},
        }
        result = initialize_dask_cluster(config)
        self.assertIs(result, config)

    @patch(
        "esm_runscripts.dask_cluster.esm_parser.determine_computer_and_node_from_hostname"
    )
    def test_skips_when_dask_not_used(self, mock_hostname):
        mock_hostname.return_value = ("levante", "compute")
        config = {"general": {}, "dask": {"actions": []}}
        result = initialize_dask_cluster(config)
        self.assertIs(result, config)

    @patch("esm_runscripts.dask_cluster.ini_dask_cluster")
    @patch(
        "esm_runscripts.dask_cluster.esm_parser.determine_computer_and_node_from_hostname"
    )
    def test_calls_ini_on_compute_node(self, mock_hostname, mock_ini):
        mock_hostname.return_value = ("levante", "compute")
        config = {
            "general": {"parallel_file_movements": "dask"},
            "dask": {"actions": ["parallel_file_movements"]},
        }
        initialize_dask_cluster(config)
        mock_ini.assert_called_once_with(config)


class TestIniDaskCluster(unittest.TestCase):
    """Test ini_dask_cluster with mocked subprocess and status checks."""

    def _make_config(self, tmp_dir):
        return {
            "general": {"thisrun_log_dir": tmp_dir},
            "computer": {"nnodes_envvar": "SLURM_NNODES"},
            "dask": {
                "scheduler_json": os.path.join(tmp_dir, "scheduler.json"),
                "init_scheduler_cmd": "dask scheduler",
                "init_workers_cmd": "dask worker --nprocs @nodes@",
                "scheduler_timeout": 1,
                "workers_timeout": 1,
                "poll_interval": 0.1,
            },
        }

    @patch("esm_runscripts.dask_cluster.subprocess.Popen")
    @patch("esm_runscripts.dask_cluster.wait_for_dask_status")
    @patch("esm_runscripts.dask_cluster.get_dask_cluster_status")
    def test_starts_scheduler_and_workers(self, mock_status, mock_wait, mock_popen):
        with tempfile.TemporaryDirectory() as tmp_dir:
            config = self._make_config(tmp_dir)
            mock_status.side_effect = [
                (DaskStatus.MISSING_JSON, 0),
                (DaskStatus.RUNNING, 2),
            ]
            mock_wait.return_value = (DaskStatus.NO_WORKERS, 0)
            result = ini_dask_cluster(config)
            self.assertIs(result, config)
            self.assertEqual(mock_popen.call_count, 2)

    @patch("esm_runscripts.dask_cluster.subprocess.Popen")
    @patch("esm_runscripts.dask_cluster.get_dask_cluster_status")
    def test_skips_when_already_running(self, mock_status, mock_popen):
        with tempfile.TemporaryDirectory() as tmp_dir:
            config = self._make_config(tmp_dir)
            mock_status.return_value = (DaskStatus.RUNNING, 4)
            result = ini_dask_cluster(config)
            self.assertIs(result, config)
            mock_popen.assert_not_called()

    @patch("esm_runscripts.dask_cluster.subprocess.Popen")
    @patch("esm_runscripts.dask_cluster.wait_for_dask_status")
    @patch("esm_runscripts.dask_cluster.get_dask_cluster_status")
    def test_substitutes_nodes_placeholder(self, mock_status, mock_wait, mock_popen):
        with tempfile.TemporaryDirectory() as tmp_dir:
            config = self._make_config(tmp_dir)
            mock_status.side_effect = [
                (DaskStatus.MISSING_JSON, 0),
                (DaskStatus.RUNNING, 1),
            ]
            mock_wait.return_value = (DaskStatus.NO_WORKERS, 0)
            with patch.dict(os.environ, {"SLURM_NNODES": "4"}):
                ini_dask_cluster(config)
            workers_cmd = mock_popen.call_args_list[1][0][0]
            self.assertIn("--nprocs 4", workers_cmd)
            self.assertNotIn("@nodes@", workers_cmd)


if __name__ == "__main__":
    unittest.main()
