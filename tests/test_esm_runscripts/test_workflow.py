#!/usr/bin/env python

""" Test for ``esm_runscripts.workflow``"""

from esm_runscripts import workflow
import pytest
import esm_parser


@pytest.fixture()
def test_config():
    """Setup a test config dictionary."""
    config = {
        'computer': {'partitions': {'compute': {'name': 'test'}}},
        'fesom': {
            'nproc': 128},
        'rnfmap': {
            'nproc': 128},
        'oasis3mct': {
            'nproc': 128},
        'xios': {
            'nproc': 128},
        'general': {
            'valid_model_names': ['fesom', 'oifs', 'rnfmap', 'oasis3mct', 'xios'],
            'jobtype': 'unknown',
            'command_line_config': {
                'jobtype': None
            },
            "defaults.yaml": {
                'workflow': {
                    'first_task_in_queue': 'prepcompute',
                    'last_task_in_queue': 'tidy',
                    'next_run_triggered_by': 'tidy',
                    'phases': {
                        'compute': {
                            'called_from': 'prepcompute',
                            'cluster': 'compute',
                            'name': 'compute',
                            'next_submit': ['tidy'],
                            'nproc': 'None',
                            'order_in_cluster': 'sequential',
                            'run_after': 'prepcompute',
                            'run_before': 'tidy',
                            'run_on_queue': 'compute',
                            'submit_to_batch_system': True},
                        'prepcompute': {
                            'batch_or_shell': 'SimulationSetup',
                            'called_from': 'tidy',
                            'cluster': 'prepcompute',
                            'name': 'prepcompute',
                            'next_submit': ['compute'],
                            'nproc': 1,
                            'order_in_cluster': 'sequential',
                            'run_after': 'tidy',
                            'run_before': 'compute',
                            'submit_to_batch_system': False},
                        'tidy': {
                            'batch_or_shell': 'SimulationSetup',
                            'called_from': 'compute',
                            'cluster': 'tidy',
                            'name': 'tidy',
                            'next_submit': ['prepcompute'],
                            'nproc': 1,
                            'order_in_cluster': 'sequential',
                            'run_after': 'compute',
                            'run_before': 'prepcompute',
                            'submit_to_batch_system': False,
                            'trigger_next_run': True}
                    }
                }
            },
            'workflow': {
                'my_new_subjob_general': {
                    'batch_or_shell': 'batch',
                    'order_in_cluster': 'concurrent',
                    'run_on_queue': 'compute',
                    'nproc': 1,
                    'run_after': 'tidy',
                    'script_dir': '/work/ab0995/a270089/myrunscripts/',
                    'script': 'hallowelt.sh',
                    'submit_to_batch_system': True}
            }
        },
        'flow': {
            'workflow': {
                'phases': {
                    'my_new_subjob_flow': {
                        'batch_or_shell': 'batch',
                        'order_in_cluster': 'concurrent',
                        'cluster': 'test_cluster',
                        'run_on_queue': 'compute',
                        'nproc': 1,
                        'run_after': 'tidy',
                        'script_dir': '/work/ab0995/a270089/myrunscripts/',
                        'script': 'hallowelt.sh',
                        'submit_to_batch_system': True,
                        'trigger_next_run': True}
                }
            }
        },
        'oifs': {
            'workflow': {
                'phases': {
                    'my_new_subjob_oifs': {
                        'batch_or_shell': 'batch',
                        'nproc': 1,
                        'order_in_cluster': 'concurrent',
                        'cluster': 'test_cluster',
                        'run_after': 'tidy',
                        'run_on_queue': 'compute',
                        'script': 'helloworld.sh',
                        'script_dir': '/work/ab0995/a270089/myrunscripts/',
                        'submit_to_batch_system': True}
                }
            }
        },
    }
    return config


@pytest.fixture()
def test_default_config_example():
    """Setup a test config dictionary."""
    config = {
        'computer': {'partitions': {'compute': {'name': 'test'}}},
        'fesom': {
            'nproc': 128},
        'oifs': {
            'nproc': 128},
        'rnfmap': {
            'nproc': 128},
        'oasis3mct': {
            'nproc': 128},
        'xios': {
            'nproc': 128},
        'general': {
            'valid_model_names': ['fesom', 'oifs', 'rnfmap', 'oasis3mct', 'xios'],
            'jobtype': 'unknown',
            'command_line_config': {
                'jobtype': None
            },
            "defaults.yaml": {
                'workflow': {
                    'first_task_in_queue': 'prepcompute',
                    'last_task_in_queue': 'tidy',
                    'next_run_triggered_by': 'tidy',
                    'phases': {
                        'compute': {
                            'called_from': 'prepcompute',
                            'cluster': 'compute',
                            'name': 'compute',
                            'next_submit': ['tidy'],
                            'nproc': 'None',
                            'order_in_cluster': 'sequential',
                            'run_after': 'prepcompute',
                            'run_before': 'tidy',
                            'run_on_queue': 'compute',
                            'submit_to_batch_system': True},
                        'prepcompute': {
                            'batch_or_shell': 'SimulationSetup',
                            'called_from': 'tidy',
                            'cluster': 'prepcompute',
                            'name': 'prepcompute',
                            'next_submit': ['compute'],
                            'nproc': 1,
                            'order_in_cluster': 'sequential',
                            'run_after': 'tidy',
                            'run_before': 'compute',
                            'submit_to_batch_system': False},
                        'tidy': {
                            'batch_or_shell': 'SimulationSetup',
                            'called_from': 'compute',
                            'cluster': 'tidy',
                            'name': 'tidy',
                            'next_submit': ['prepcompute'],
                            'nproc': 1,
                            'order_in_cluster': 'sequential',
                            'run_after': 'compute',
                            'run_before': 'prepcompute',
                            'submit_to_batch_system': False,
                            'trigger_next_run': True}
                    }
                }
            }
        }
    }
    return config


# Test scenarios
# 0. Default workflow
@pytest.mark.example
def test_example_0(test_default_config_example):
    test_default_config_example = workflow.assemble_workflow(test_default_config_example)
    order = workflow.display_workflow_sequence(test_default_config_example, display=False)
    assumption = "prepcompute ['prepcompute'] ->  compute ['compute'] ->  tidy ['tidy'] ->  prepcompute ['prepcompute']"
    assert order == assumption


# 1. Add one single phase at the end of the default workflow (Example 1 in documentation)
@pytest.mark.example
def test_example_1(test_default_config_example):
    test_default_config_example["general"]["workflow"] = {
        'phases': {
            'my_postprocessing': {
                'script': 'helloworld.sh',
                'script_dir': '/work/ab0995/a270089/myrunscripts/'}
        }
    }
    assumption = "prepcompute ['prepcompute'] ->  compute ['compute'] ->  tidy ['tidy'] ->  prepcompute ['prepcompute'] and my_postprocessing ['my_postprocessing']"
    test_default_config_example = workflow.assemble_workflow(test_default_config_example)
    order = workflow.display_workflow_sequence(test_default_config_example, display=False)
    assert order == assumption


# 2. Prepend new phase at the beginning of workflow
@pytest.mark.example
def test_example_2(test_default_config_example):
    test_default_config_example["general"]["workflow"] = {
        'phases': {
            'my_preprocessing': {
                'run_before': 'prepcompute',
                'script': 'helloworld.sh',
                'script_dir': '/work/ab0995/a270089/myrunscripts/'}
        }
    }
    assumption = "newrun ['newrun'] ->  my_preprocessing ['my_preprocessing'] ->  prepcompute ['prepcompute'] ->  compute ['compute'] ->  tidy ['tidy'] ->  newrun ['newrun']"
    test_default_config_example = workflow.assemble_workflow(test_default_config_example)
    order = workflow.display_workflow_sequence(test_default_config_example, display=False)
    assert order == assumption


# 3. Append new phase at the beginning of workflow
@pytest.mark.example
def test_example_3(test_default_config_example):
    test_default_config_example["general"]["workflow"] = {
        'phases': {
            'my_new_last_phase': {
                'script': 'helloworld.sh',
                'script_dir': '/work/ab0995/a270089/myrunscripts/',
                'trigger_next_run': True}
        }
    }
    assumption = "prepcompute ['prepcompute'] ->  compute ['compute'] ->  tidy ['tidy'] ->  my_new_last_phase ['my_new_last_phase'] ->  prepcompute ['prepcompute']"
    test_default_config_example = workflow.assemble_workflow(test_default_config_example)
    order = workflow.display_workflow_sequence(test_default_config_example, display=False)
    assert order == assumption


# 4. Append two new phases in the same cluster
@pytest.mark.example
def test_example_4(test_default_config_example):
    test_default_config_example["general"]["workflow"] = {
        'phases': {
            'my_new_last_phase': {
                'script': 'helloworld.sh',
                'script_dir': '/work/ab0995/a270089/myrunscripts/',
                'submit_to_batch_system': True,
                'run_on_queue': 'compute',
                'order_in_cluster': 'concurrent',
                'cluster': 'my_own_new_cluster'},
            'my_second_new_phase': {
                'script': 'halloworld.sh',
                'script_dir': '/work/ab0995/a270089/myrunscripts/',
                'submit_to_batch_system': True,
                'run_on_queue': 'compute',
                'order_in_cluster': 'concurrent',
                'cluster': 'my_own_new_cluster'}
        }
    }
    assumption = "prepcompute ['prepcompute'] ->  compute ['compute'] ->  tidy ['tidy'] ->  prepcompute ['prepcompute'] and my_own_new_cluster ['my_new_last_phase', 'my_second_new_phase']"
    test_default_config_example = workflow.assemble_workflow(test_default_config_example)
    order = workflow.display_workflow_sequence(test_default_config_example, display=False)
    assert order == assumption


# 5. Append two new phases in the same cluster, one of them triggers the next run
@pytest.mark.example
def test_example_5(test_default_config_example):
    test_default_config_example["general"]["workflow"] = {
        'phases': {
            'my_new_last_phase': {
                'script': 'helloworld.sh',
                'script_dir': '/work/ab0995/a270089/myrunscripts/',
                'submit_to_batch_system': True,
                'run_on_queue': 'compute',
                'order_in_cluster': 'concurrent',
                'cluster': 'my_own_new_cluster',
                'trigger_next_run': True},
            'my_second_new_phase': {
                'script': 'halloworld.sh',
                'script_dir': '/work/ab0995/a270089/myrunscripts/',
                'submit_to_batch_system': True,
                'run_on_queue': 'compute',
                'order_in_cluster': 'concurrent',
                'cluster': 'my_own_new_cluster'}
        }
    }
    assumption = "prepcompute ['prepcompute'] ->  compute ['compute'] ->  tidy ['tidy'] ->  my_own_new_cluster ['my_new_last_phase', 'my_second_new_phase'] ->  prepcompute ['prepcompute']"
    test_default_config_example = workflow.assemble_workflow(test_default_config_example)
    order = workflow.display_workflow_sequence(test_default_config_example, display=False)
    assert order == assumption

# 6. Append two new phases in the same cluster at the beginning of run
# @pytest.mark.example
# def test_example_6(test_default_config_example):
#    test_default_config_example["general"]["workflow"] = {
#        'phases': {
#            'my_new_last_phase': {
#                'script': 'helloworld.sh',
#                'run_before': 'prepcompute',
#                'script_dir': '/work/ab0995/a270089/myrunscripts/',
#                'submit_to_batch_system': True,
#                'run_on_queue': 'compute',
#                'cluster': 'my_own_new_cluster'},
#            'my_second_new_phase': {
#                'script': 'halloworld.sh',
#                'script_dir': '/work/ab0995/a270089/myrunscripts/',
#                'run_before': 'prepcompute',
#                'submit_to_batch_system': True,
#                'run_on_queue': 'compute',
#                'cluster': 'my_own_new_cluster'}
#        }
#    }
#    assumption = "prepcompute ['prepcompute'] ->  compute ['compute'] ->  tidy ['tidy'] ->  my_own_new_cluster ['my_new_last_phase', 'my_second_new_phase'] ->  prepcompute ['prepcompute']"
#    test_default_config_example = workflow.assemble_workflow(test_default_config_example)
#    order = workflow.display_workflow_sequence(test_default_config_example, display=False)
#    assert order == assumption


# Test exceptions
# 1. If still a workflow keyword is set by user.
@pytest.mark.exceptions
def test_exception_test_workflow_keyword(test_config):
    test_config['flow']['workflow']['next_run_triggered_by'] = 'my_new_subjob_general'
    with pytest.raises(SystemExit):
        test_config = workflow.assemble_workflow(test_config)


# 2. If an invalid phase keyword is set.
@pytest.mark.exceptions
def test_exception_invalid_phase_keyword(test_config):
    test_config['flow']['workflow']['phases']['my_new_subjob_flow']['wrong_keyword'] = 'wrong_value'
    with pytest.raises(SystemExit):
        test_config = workflow.assemble_workflow(test_config)


# 3. If an unknown phase is called for , e.g. in 'run_after'
@pytest.mark.exceptions
def test_exception_unknown_phase(test_config):
    test_config['flow']['workflow']['phases']['my_new_subjob_flow']['run_after'] = 'trudy'
    with pytest.raises(SystemExit):
        test_config = workflow.assemble_workflow(test_config)


# 4. If a user phase has the same name as a default phase.
@pytest.mark.exceptions
def test_if_user_phase_has_default_phase_name(test_config):
    test_config['flow']['workflow']['phases']['compute'] = {
        'batch_or_shell': 'batch',
        'order_in_cluster': 'concurrent',
        'cluster': 'test_cluster',
        'run_on_queue': 'compute',
        'nproc': 1,
        'run_after': 'tidy',
        'script_dir': '/work/ab0995/a270089/myrunscripts/',
        'script': 'hallowelt.sh',
        'submit_to_batch_system': True}
    with pytest.raises(SystemExit):
        test_config = workflow.assemble_workflow(test_config)


# 5. If two user phases have the same name and are defined in different models/setups.
@pytest.mark.exceptions
def test_if_two_user_phase_have_the_same_name(test_config):
    test_config['oifs']['workflow']['phases']['my_new_subjob_flow'] = {
        'batch_or_shell': 'batch',
        'order_in_cluster': 'concurrent',
        'cluster': 'test_cluster',
        'run_on_queue': 'compute',
        'nproc': 1,
        'run_after': 'tidy',
        'script_dir': '/work/ab0995/a270089/myrunscripts/',
        'script': 'hallowelt.sh',
        'submit_to_batch_system': True}
    with pytest.raises(SystemExit):
        test_config = workflow.assemble_workflow(test_config)


# 6. If no queue is given for a phase that should be run on sbatch system.
@pytest.mark.exceptions
def test_if_queue_is_missing(test_config):
    test_config['oifs']['workflow']['phases']['my_new_subjob_oifs'] = {
        'batch_or_shell': 'batch',
        'order_in_cluster': 'concurrent',
        'cluster': 'test_cluster',
        # 'run_on_queue': 'compute',
        'nproc': 1,
        'run_after': 'tidy',
        'script_dir': '/work/ab0995/a270089/myrunscripts/',
        'script': 'hallowelt.sh',
        'submit_to_batch_system': True}
    with pytest.raises(SystemExit):
        test_config = workflow.assemble_workflow(test_config)


# 7. If more than one phase trigger_next_run.
@pytest.mark.exceptions
def test_if_trigger_next_run_unclear(test_config):
    test_config['oifs']['workflow']['phases']['my_new_subjob_oifs']['trigger_next_run'] = True
    with pytest.raises(SystemExit):
        test_config = workflow.assemble_workflow(test_config)


# 8. If no default phases are defined in defaults.yaml.
@pytest.mark.exceptions
def test_if_no_default_phases(test_config):
    test_config['general']['defaults.yaml']['workflow'].pop('phases', None)
    with pytest.raises(SystemExit):
        test_config = workflow.assemble_workflow(test_config)


# 9. If no default workflow is defined in defaults.yaml.
@pytest.mark.exceptions
def test_inf_no_default_workflow(test_config):
    test_config['general']['defaults.yaml'].pop('workflow', None)
    with pytest.raises(SystemExit):
        test_config = workflow.assemble_workflow(test_config)


def test_get_workflow_commands_for_run():
    config = esm_parser.yaml_file_to_dict('config3.yaml')
    config = workflow.get_workflow_commands_for_run(config)
    # assert order == assumption
    assert 1 == 2

