import copy

import pytest

from esm_runscripts.workflow import (WorkflowError,
                                     collect_all_workflow_information,
                                     complete_clusters, init_total_workflow,
                                     merge_if_possible,
                                     merge_single_entry_if_possible,
                                     order_clusters, prepend_newrun_job,
                                     should_skip_cluster)


def test_should_skip_cluster_last_run_in_chunk():
    config = {
        "general": {
            "workflow": {
                "clusters": {"test_cluster": {"run_only": "last_run_in_chunk"}}
            },
            "last_run_in_chunk": False,
        }
    }
    assert should_skip_cluster("test_cluster", config) is True


def test_should_skip_cluster_first_run_in_chunk():
    config = {
        "general": {
            "workflow": {
                "clusters": {"test_cluster": {"run_only": "first_run_in_chunk"}}
            },
            "first_run_in_chunk": False,
        }
    }
    assert should_skip_cluster("test_cluster", config) is True


def test_should_skip_cluster_skip_chunk_number():
    config = {
        "general": {
            "workflow": {"clusters": {"test_cluster": {"skip_chunk_number": 1}}},
            "chunk_number": 1,
        }
    }
    assert should_skip_cluster("test_cluster", config) is True


def test_should_skip_cluster_skip_run_number():
    config = {
        "general": {
            "workflow": {"clusters": {"test_cluster": {"skip_run_number": 1}}},
            "run_number": 1,
        }
    }
    assert should_skip_cluster("test_cluster", config) is True


def test_should_skip_cluster_no_skip():
    config = {
        "general": {
            "workflow": {"clusters": {"test_cluster": {}}},
            "last_run_in_chunk": True,
            "first_run_in_chunk": True,
            "chunk_number": 2,
            "run_number": 2,
        }
    }
    assert should_skip_cluster("test_cluster", config) is False


def test_init_total_workflow_minimal_config():
    config = {
        "general": {"valid_model_names": [], "workflow": {}},
        "computer": {"partitions": {"compute": {"name": "default_queue"}}},
    }
    expected = {
        "general": {
            "valid_model_names": [],
            "workflow": {
                "clusters": {},
                "jobs": {
                    "prepcompute": {"nproc": 1, "run_before": "compute"},
                    "compute": {
                        "nproc": 0,
                        "run_before": "tidy",
                        "submit_to_batch_system": True,
                        "run_on_queue": "default_queue",
                    },
                    "tidy": {"nproc": 1, "run_after": "compute"},
                },
                "last_task_in_queue": "tidy",
                "first_task_in_queue": "prepcompute",
                "next_run_triggered_by": "tidy",
            },
        },
        "computer": {"partitions": {"compute": {"name": "default_queue"}}},
    }
    result = init_total_workflow(config)
    assert result == expected


def test_init_total_workflow_config_with_nproc():
    config = {
        "general": {"valid_model_names": ["model1"], "workflow": {}},
        "model1": {"nproc": 4},
        "computer": {"partitions": {"compute": {"name": "default_queue"}}},
    }
    expected = {
        "general": {
            "valid_model_names": ["model1"],
            "workflow": {
                "clusters": {},
                "jobs": {
                    "prepcompute": {"nproc": 1, "run_before": "compute"},
                    "compute": {
                        "nproc": 4,
                        "run_before": "tidy",
                        "submit_to_batch_system": True,
                        "run_on_queue": "default_queue",
                    },
                    "tidy": {"nproc": 1, "run_after": "compute"},
                },
                "last_task_in_queue": "tidy",
                "first_task_in_queue": "prepcompute",
                "next_run_triggered_by": "tidy",
            },
        },
        "model1": {"nproc": 4},
        "computer": {"partitions": {"compute": {"name": "default_queue"}}},
    }
    result = init_total_workflow(config)
    assert result == expected


def test_init_total_workflow_config_with_nproca_nprocb():
    config = {
        "general": {"valid_model_names": ["model1"], "workflow": {}},
        "model1": {"nproca": 2, "nprocb": 3},
        "computer": {"partitions": {"compute": {"name": "default_queue"}}},
    }
    expected = {
        "general": {
            "valid_model_names": ["model1"],
            "workflow": {
                "clusters": {},
                "jobs": {
                    "prepcompute": {"nproc": 1, "run_before": "compute"},
                    "compute": {
                        "nproc": 6,
                        "run_before": "tidy",
                        "submit_to_batch_system": True,
                        "run_on_queue": "default_queue",
                    },
                    "tidy": {"nproc": 1, "run_after": "compute"},
                },
                "last_task_in_queue": "tidy",
                "first_task_in_queue": "prepcompute",
                "next_run_triggered_by": "tidy",
            },
        },
        "model1": {"nproca": 2, "nprocb": 3},
        "computer": {"partitions": {"compute": {"name": "default_queue"}}},
    }
    result = init_total_workflow(config)
    assert result == expected


def test_init_total_workflow_config_with_nprocar_nprocbr():
    config = {
        "general": {"valid_model_names": ["model1"], "workflow": {}},
        "model1": {"nproca": 2, "nprocb": 3, "nprocar": 1, "nprocbr": 2},
        "computer": {"partitions": {"compute": {"name": "default_queue"}}},
    }
    expected = {
        "general": {
            "valid_model_names": ["model1"],
            "workflow": {
                "clusters": {},
                "jobs": {
                    "prepcompute": {"nproc": 1, "run_before": "compute"},
                    "compute": {
                        "nproc": 8,
                        "run_before": "tidy",
                        "submit_to_batch_system": True,
                        "run_on_queue": "default_queue",
                    },
                    "tidy": {"nproc": 1, "run_after": "compute"},
                },
                "last_task_in_queue": "tidy",
                "first_task_in_queue": "prepcompute",
                "next_run_triggered_by": "tidy",
            },
        },
        "model1": {"nproca": 2, "nprocb": 3, "nprocar": 1, "nprocbr": 2},
        "computer": {"partitions": {"compute": {"name": "default_queue"}}},
    }
    result = init_total_workflow(config)
    assert result == expected


def test_prepend_newrun_job():
    config = {
        "general": {
            "workflow": {
                "first_task_in_queue": "cluster1",
                "last_task_in_queue": "cluster2",
                "clusters": {
                    "cluster1": {
                        "submission_type": "OtherTask",
                        "called_from": None,
                        "next_submit": [],
                    },
                    "cluster2": {
                        "submission_type": "OtherTask",
                        "called_from": None,
                        "next_submit": ["cluster1"],
                    },
                },
                "jobs": {},
            }
        }
    }

    expected_config = {
        "general": {
            "workflow": {
                "first_task_in_queue": "newrun",
                "last_task_in_queue": "cluster2",
                "clusters": {
                    "cluster1": {
                        "submission_type": "OtherTask",
                        "called_from": "newrun",
                        "next_submit": [],
                    },
                    "cluster2": {
                        "submission_type": "OtherTask",
                        "called_from": None,
                        "next_submit": ["newrun"],
                    },
                    "newrun": {
                        "called_from": "cluster2",
                        "run_before": "cluster1",
                        "next_submit": ["cluster1"],
                        "jobs": ["newrun_general"],
                        "submission_type": "sim_object",
                    },
                },
                "jobs": {
                    "newrun_general": {
                        "nproc": 1,
                        "called_from": "cluster2",
                        "run_before": "cluster1",
                        "next_submit": ["cluster1"],
                        "job_cluster": "newrun",
                    }
                },
            }
        }
    }

    result = prepend_newrun_job(config)
    assert result == expected_config


def test_prepend_newrun_job_no_change():
    config = {
        "general": {
            "workflow": {
                "first_task_in_queue": "cluster1",
                "last_task_in_queue": "cluster2",
                "clusters": {
                    "cluster1": {
                        "submission_type": "sim_object",
                        "called_from": None,
                        "next_submit": [],
                    },
                    "cluster2": {
                        "submission_type": "OtherTask",
                        "called_from": None,
                        "next_submit": ["cluster1"],
                    },
                },
                "jobs": {},
            }
        }
    }

    expected_config = config.copy()

    result = prepend_newrun_job(config)
    assert result == expected_config


def test_merge_single_entry_if_possible_entry_in_source_not_in_target():
    sourceconf = {"key1": "value1"}
    targetconf = {}
    entry = "key1"
    result = merge_single_entry_if_possible(entry, sourceconf, targetconf)
    assert result == {"key1": "value1"}


def test_merge_single_entry_if_possible_entry_in_both_matching():
    sourceconf = {"key1": "value1"}
    targetconf = {"key1": "value1"}
    entry = "key1"
    result = merge_single_entry_if_possible(entry, sourceconf, targetconf)
    assert result == {"key1": "value1"}


def test_merge_single_entry_if_possible_entry_in_both_mismatching():
    sourceconf = {"key1": "value1"}
    targetconf = {"key1": "value2"}
    entry = "key1"
    with pytest.raises(WorkflowError):
        merge_single_entry_if_possible(entry, sourceconf, targetconf)


def test_merge_if_possible_no_conflict():
    source = {"a": 1, "b": 2}
    target = {"c": 3, "d": 4}
    expected = {"c": 3, "d": 4, "a": 1, "b": 2}
    result = merge_if_possible(source, target)
    assert result == expected


def test_merge_if_possible_with_conflict():
    source = {"a": 1, "b": 2}
    target = {"a": 2, "c": 3}
    with pytest.raises(WorkflowError):
        merge_if_possible(source, target)


def test_merge_if_possible_partial_conflict():
    source = {"a": 1, "b": 2}
    target = {"a": 1, "c": 3}
    expected = {"a": 1, "c": 3, "b": 2}
    result = merge_if_possible(source, target)
    assert result == expected


@pytest.fixture
def sample_config():
    return {
        "model1": {
            "workflow": {
                "clusters": {
                    "cluster1": {"key1": "value1"},
                },
                "jobs": {
                    "jobA": {"run_before": "jobB"},
                    "jobB": {"run_after": "jobA", "run_before": "jobC"},
                    "jobC": {"run_after": "jobB"},
                },
                "next_run_triggered_by": "trigger1",
            }
        },
        "general": {
            "workflow": {
                "clusters": {
                    "cluster1": {"key1": "general_value1"},
                },
                "jobs": {
                    "job2": {},
                    "job3": {},
                },
                "next_run_triggered_by": "tidy",
            }
        },
    }


def test_collect_all_workflow_information(sample_config):
    config = copy.deepcopy(sample_config)
    updated_config = collect_all_workflow_information(config)

    # Test clusters merging
    assert (
        updated_config["general"]["workflow"]["clusters"]["cluster1"]["key1"]
        == "value1"
    )

    # Test jobs renaming and copying
    assert "job1_model1" in updated_config["general"]["workflow"]["jobs"]
    assert "job1" not in updated_config["general"]["workflow"]["jobs"]

    # Test run_after and run_before renaming
    assert (
        updated_config["general"]["workflow"]["jobs"]["job1_model1"]["run_after"]
        == "job2"
    )
    assert (
        updated_config["general"]["workflow"]["jobs"]["job1_model1"]["run_before"]
        == "job3"
    )

    # Test job_cluster assignment
    assert (
        updated_config["general"]["workflow"]["jobs"]["job1_model1"]["job_cluster"]
        == "job1"
    )

    # Test next_run_triggered_by
    assert updated_config["general"]["workflow"]["next_run_triggered_by"] == "trigger1"


def test_collect_all_workflow_information_mismatch(sample_config):
    config = copy.deepcopy(sample_config)
    config["model1"]["workflow"]["next_run_triggered_by"] = "mismatch_trigger"

    with pytest.raises(SystemExit):
        collect_all_workflow_information(config)


def test_order_clusters_minimal_config():
    config = {
        "general": {
            "workflow": {
                "clusters": {},
                "jobs": {},
                "first_task_in_queue": "first_task",
                "last_task_in_queue": "last_task",
            }
        }
    }
    expected = {
        "general": {
            "workflow": {
                "clusters": {},
                "jobs": {},
                "first_task_in_queue": "first_task",
                "last_task_in_queue": "last_task",
            }
        }
    }
    result = order_clusters(config)
    assert result == expected


def test_order_clusters_with_run_after():
    config = {
        "general": {
            "workflow": {
                "clusters": {
                    "cluster1": {"run_after": "cluster2"},
                    "cluster2": {},
                },
                "jobs": {},
                "first_task_in_queue": "cluster2",
                "last_task_in_queue": "cluster1",
            }
        }
    }
    expected = {
        "general": {
            "workflow": {
                "clusters": {
                    "cluster1": {
                        "run_after": "cluster2",
                        "called_from": "cluster2",
                        "next_submit": [],
                    },
                    "cluster2": {"next_submit": ["cluster1"]},
                },
                "jobs": {},
                "first_task_in_queue": "cluster2",
                "last_task_in_queue": "cluster1",
            }
        }
    }
    result = order_clusters(config)
    assert result == expected


def test_order_clusters_with_run_before():
    config = {
        "general": {
            "workflow": {
                "clusters": {
                    "cluster1": {"run_before": "cluster2"},
                    "cluster2": {},
                },
                "jobs": {},
                "first_task_in_queue": "cluster1",
                "last_task_in_queue": "cluster2",
            }
        }
    }
    expected = {
        "general": {
            "workflow": {
                "clusters": {
                    "cluster1": {"run_before": "cluster2", "next_submit": ["cluster2"]},
                    "cluster2": {"called_from": "cluster1"},
                },
                "jobs": {},
                "first_task_in_queue": "cluster1",
                "last_task_in_queue": "cluster2",
            }
        }
    }
    result = order_clusters(config)
    assert result == expected


def test_order_clusters_with_run_after_and_run_before():
    config = {
        "general": {
            "workflow": {
                "clusters": {
                    "cluster1": {"run_after": "cluster2"},
                    "cluster2": {"run_before": "cluster3"},
                    "cluster3": {},
                },
                "jobs": {},
                "first_task_in_queue": "cluster2",
                "last_task_in_queue": "cluster1",
            }
        }
    }
    expected = {
        "general": {
            "workflow": {
                "clusters": {
                    "cluster1": {
                        "run_after": "cluster2",
                        "called_from": "cluster2",
                        "next_submit": [],
                    },
                    "cluster2": {
                        "run_before": "cluster3",
                        "next_submit": ["cluster1", "cluster3"],
                    },
                    "cluster3": {"called_from": "cluster2"},
                },
                "jobs": {},
                "first_task_in_queue": "cluster2",
                "last_task_in_queue": "cluster1",
            }
        }
    }
    result = order_clusters(config)
    assert result == expected


def test_complete_clusters():
    config = {
        "general": {
            "workflow": {
                "jobs": {
                    "newexp": {
                        "job_cluster": "cluster0",
                        "nproc": 1,
                        "submit_to_batch_system": False,
                        "submission_type": "sim_object",
                    },
                    "prepexp": {
                        "job_cluster": "cluster0",
                        "nproc": 1,
                        "submit_to_batch_system": False,
                        "submission_type": "sim_object",
                    },
                    "prepcompute": {
                        "job_cluster": "cluster0",
                        "nproc": 1,
                        "submit_to_batch_system": False,
                        "submission_type": "sim_object",
                    },
                    "write_next_cluster": {
                        "job_cluster": "cluster0",
                        "nproc": 1,
                        "submit_to_batch_system": False,
                        "submission_type": "sim_object",
                    },
                    "compute": {
                        "job_cluster": "cluster1",
                        "nproc": 288,
                        "submit_to_batch_system": True,
                        "submission_type": "batch",
                    },
                    "tidy": {
                        "job_cluster": "cluster1",
                        "nproc": 1,
                        "submit_to_batch_system": True,
                        "submission_type": "sim_object",
                    },
                    "prepcompute": {
                        "job_cluster": "cluster1",
                        "nproc": 1,
                        "submit_to_batch_system": False,
                        "submission_type": "sim_object",
                    },
                },
                "clusters": {},
            }
        }
    }

    expected_config = {
        "general": {
            "workflow": {
                "jobs": {
                    "job1": {
                        "job_cluster": "cluster1",
                        "nproc": 2,
                        "submit_to_batch_system": True,
                        "script": True,
                    },
                    "job2": {
                        "job_cluster": "cluster1",
                        "nproc": 4,
                        "submit_to_batch_system": False,
                        "script": True,
                    },
                    "job3": {
                        "job_cluster": "cluster2",
                        "nproc": 1,
                        "submit_to_batch_system": False,
                        "script": False,
                    },
                },
                "clusters": {
                    "cluster1": {
                        "jobs": ["job1", "job2"],
                        "submission_type": "batch",
                        "submit_to_batch_system": True,
                        "order_in_cluster": "sequential",
                        "nproc": 4,
                    },
                    "cluster2": {
                        "jobs": ["job3"],
                        "submission_type": "sim_object",
                        "submit_to_batch_system": False,
                        "order_in_cluster": "sequential",
                        "nproc": 1,
                    },
                },
            }
        }
    }

    result = complete_clusters(config)
    assert result == expected_config
