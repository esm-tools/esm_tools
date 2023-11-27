#!/usr/bin/env python

""" Test for ``esm_runscripts.workflow``"""

from esm_runscripts import workflow
import pytest
import esm_parser

@pytest.fixture()
def test_default_phases_dict():
    phases_dict = {
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
            'submit_to_batch_system': False}
    }
    return phases_dict

@pytest.fixture()
def test_workflow_object():
    workflow_dict = {
        'first_task_in_queue': 'prepcompute',
        'last_task_in_queue': 'tidy',
        'next_run_triggered_by': 'tidy'
    }
    test_workflow = workflow.Workflow(workflow_dict)
    return test_workflow

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
        'oifs': {
            'workflow': {
#                'next_run_triggered_by': 'tidy',
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
                        'submit_to_batch_system': True}}}},
        'general': {
            'valid_model_names': ['fesom', 'oifs', 'rnfmap', 'oasis3mct', 'xios'],
            'workflow': {
#                'next_run_triggered_by': 'tidy',
                'phases': {
                    'my_new_subjob_general': {
                        'batch_or_shell': 'batch',
                        'order_in_cluster': 'concurrent',
                        'run_on_queue': 'compute',
                        'nproc': 1,
                        'run_after': 'tidy',
                        'script_dir': '/work/ab0995/a270089/myrunscripts/',
                        'script': 'hallowelt.sh',
                        'submit_to_batch_system': True}}}},
        'flow': {
            'workflow': {
#                'next_run_triggered_by': 'tidy',
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
                        'submit_to_batch_system': True}
                }
            }
        }
    }
    return config

@pytest.fixture()
def test_config_2():
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
                            'submit_to_batch_system': False}
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

def test_num_phases(test_workflow_object, test_default_phases_dict, test_config):
    for phase in test_default_phases_dict:
        test_workflow_object.phases.append(workflow.WorkflowPhase(test_default_phases_dict[phase]))
    assert test_workflow_object.num_phases == 3

def test_check_user_workflow_dependency(test_workflow_object, test_default_phases_dict, test_config):
    for phase in test_default_phases_dict:
        test_workflow_object.phases.append(workflow.WorkflowPhase(test_default_phases_dict[phase]))
    test_workflow_object = test_workflow_object.collect_all_user_phases(test_config)
    independent = test_workflow_object.check_user_workflow_dependency()
    assert independent

def test_check_user_workflow_dependency_2(test_workflow_object, test_config):
    test_config['flow']['workflow']['phases']['my_new_subjob_flow']['run_after'] = 'my_new_subjob_oifs'
#    test_workflow_object = test_workflow_object.init_default_workflow(test_config)
    test_workflow_object = test_workflow_object.collect_all_user_phases(test_config)
    independent = test_workflow_object.check_user_workflow_dependency()
    assert not independent

def test_check_unknown_phases(test_workflow_object, test_config):
    test_config['flow']['workflow']['phases']['my_new_subjob_flow']['run_after'] = 'my_new_subjob'
#    test_workflow_object = test_workflow_object.init_default_workflow(test_config)
    test_workflow_object = test_workflow_object.collect_all_user_phases(test_config)
    unknown_phases = test_workflow_object.check_unknown_phases()
    assert unknown_phases

def test_assemble_workflow():
    pytest.fail("something wrong")

def test_collect_all_user_workflow(test_config):
    pytest.fail("something wrong")

def test_calc_number_of_tasks():
    pytest.fail("something wrong")

def test_order_phases_and_clusters(test_workflow_object, test_default_phases_dict, test_config):
    for phase in test_default_phases_dict:
        test_workflow_object.phases.append(workflow.WorkflowPhase(test_default_phases_dict[phase]))
    test_config['flow']['workflow']['phases']['my_new_subjob_flow']['run_after'] = 'my_new_subjob_general'
#    test_config['flow']['workflow']['next_run_triggered_by'] = 'my_new_subjob_flow'
#    test_config['oifs']['workflow']['next_run_triggered_by'] = 'my_new_subjob_general'
    #test_config['flow']['workflow']['phases']['my_new_subjob_flow']['run_before'] = 'my_new_subjob_oifs'
    test_workflow_object = test_workflow_object.collect_all_user_phases(test_config)
    test_workflow_object = test_workflow_object.order_phases_and_clusters()
    pytest.fail("something wrong")

def test_complete_clusters(test_workflow_object, test_default_phases_dict, test_config):
    for phase in test_default_phases_dict:
        test_workflow_object.phases.append(workflow.WorkflowPhase(test_default_phases_dict[phase]))
    test_workflow_object = test_workflow_object.collect_all_user_phases(test_config)
    test_workflow_object = test_workflow_object.order_phases_and_clusters()
    pytest.fail("something wrong")

def test_prepend_newrun_job(test_workflow_object, test_default_phases_dict, test_config):
    for phase in test_default_phases_dict:
        test_workflow_object.phases.append(workflow.WorkflowPhase(test_default_phases_dict[phase]))
    test_workflow_object = test_workflow_object.collect_all_user_phases(test_config)
    test_workflow_object = test_workflow_object.order_phases_and_clusters()
    test_workflow_object = test_workflow_object.prepend_newrun_job()
    pytest.fail("something wrong")

def test_write_to_config(test_workflow_object, test_default_phases_dict, test_config):
    for phase in test_default_phases_dict:
        test_workflow_object.phases.append(workflow.WorkflowPhase(test_default_phases_dict[phase]))
    test_workflow_object = test_workflow_object.set_default_nproc(test_config)
    test_workflow_object = test_workflow_object.collect_all_user_phases(test_config)
    test_workflow_object = test_workflow_object.order_phases_and_clusters()
    test_workflow_object = test_workflow_object.prepend_newrun_job()
    config = test_workflow_object.write_to_config(test_config)
    pytest.fail("something wrong")

# Test scenarios
# 1. Add one single phase at the end of the default workflow (Example 1 in documentation)
def test_example_1(test_config_2):
    test_config_2 = workflow.assemble_workflow(test_config_2)
    workflow.display_workflow_sequence(test_config_2)
#    esm_parser.pprint_config(test_config_2)

    pytest.fail("something wrong")

# Test exceptions
# 1. If still a workflow keyword is set by user.
def test_exception_test_workflow_keyword(test_config_2):
    test_config_2['flow']['workflow']['next_run_triggered_by'] = 'my_new_subjob_general'
    with pytest.raises(SystemExit):
        test_config_2 = workflow.assemble_workflow(test_config_2)

# 2. If an invalid phase keyword is set.
def test_exception_invalid_phase_keyword(test_config_2):
    test_config_2['flow']['workflow']['phases']['my_new_subjob_flow']['wrong_keyword'] = 'wrong_value'
    with pytest.raises(SystemExit):
        test_config_2 = workflow.assemble_workflow(test_config_2)

# 3. If an unknown phase is called for , e.g. in 'run_after'
def test_exception_unknown_phase(test_config_2):
    test_config_2['flow']['workflow']['phases']['my_new_subjob_flow']['run_after'] = 'trudy'
    with pytest.raises(SystemExit):
        test_config_2 = workflow.assemble_workflow(test_config_2)

# 4. If a user phase has the same name as a default phase.
def test_if_user_phase_has_default_phase_name(test_config_2):
    test_config_2['flow']['workflow']['phases']['compute'] = {
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
        test_config_2 = workflow.assemble_workflow(test_config_2)

# 5. If two user phases have the same name and are defined in different models/setups.
def test_if_two_user_phase_have_the_same_name(test_config_2):
    test_config_2['oifs']['workflow']['phases']['my_new_subjob_flow'] = {
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
        test_config_2 = workflow.assemble_workflow(test_config_2)

# 6. If no queue is given for a phase that should be run on sbatch system.
def test_if_queue_is_missing(test_config_2):
    test_config_2['oifs']['workflow']['phases']['my_new_subjob_oifs'] = {
        'batch_or_shell': 'batch',
        'order_in_cluster': 'concurrent',
        'cluster': 'test_cluster',
        #'run_on_queue': 'compute',
        'nproc': 1,
        'run_after': 'tidy',
        'script_dir': '/work/ab0995/a270089/myrunscripts/',
        'script': 'hallowelt.sh',
        'submit_to_batch_system': True}
    with pytest.raises(SystemExit):
        test_config_2 = workflow.assemble_workflow(test_config_2)

# 7. If more than one phase trigger_next_run.
def test_if_trigger_next_run_unclear(test_config_2):
    test_config_2['oifs']['workflow']['phases']['my_new_subjob_oifs']['trigger_next_run'] = True
    with pytest.raises(SystemExit):
        test_config_2 = workflow.assemble_workflow(test_config_2)

# 8. If no default phases are defined in defaults.yaml.
def test_if_no_default_phases(test_config_2):
    test_config_2['general']['defaults.yaml']['workflow'].pop('phases', None)
    with pytest.raises(SystemExit):
        test_config_2 = workflow.assemble_workflow(test_config_2)

# 9. If no default workflow is defined in defaults.yaml.
def test_inf_no_default_workflow(test_config_2):
    test_config_2['general']['defaults.yaml'].pop('workflow', None)
    with pytest.raises(SystemExit):
        test_config_2 = workflow.assemble_workflow(test_config_2)
