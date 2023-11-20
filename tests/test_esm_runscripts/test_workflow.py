#!/usr/bin/env python

""" Test for ``esm_runscripts.workflow``"""

from esm_runscripts import workflow
import pytest

@pytest.fixture()
def test_workflow_object():
    test_workflow = workflow.Workflow(["prepcompute","compute","tidy"],always_run_with=["prepare","prepexp"])
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
                'next_run_triggered_by': 'tidy',
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
                'next_run_triggered_by': 'tidy',
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
                        'submit_to_batch_system': True}}}}}
    return config

def test_num_phases(test_workflow_object, test_config):
    test_workflow_object = test_workflow_object.init_default_workflow(test_config)
    assert test_workflow_object.num_phases == 3

def test_check_user_workflow_dependency(test_workflow_object, test_config):
    test_workflow_object = test_workflow_object.init_default_workflow(test_config)
    test_workflow_object = test_workflow_object.collect_all_user_workflows(test_config)
    independent = test_workflow_object.check_user_workflow_dependency()
    assert independent

def test_check_user_workflow_dependency_2(test_workflow_object, test_config):
    test_config['flow']['workflow']['phases']['my_new_subjob_flow']['run_after'] = 'my_new_subjob_oifs'
    test_workflow_object = test_workflow_object.init_default_workflow(test_config)
    test_workflow_object = test_workflow_object.collect_all_user_workflows(test_config)
    independent = test_workflow_object.check_user_workflow_dependency()
    assert not independent

def test_check_unknown_phases(test_workflow_object, test_config):
    test_config['flow']['workflow']['phases']['my_new_subjob_flow']['run_after'] = 'my_new_subjob'
    test_workflow_object = test_workflow_object.init_default_workflow(test_config)
    test_workflow_object = test_workflow_object.collect_all_user_workflows(test_config)
    unknown_phases = test_workflow_object.check_unknown_phases()
    assert unknown_phases

def test_assemble_workflow():
    pytest.fail("something wrong")

def test_collect_all_user_workflow(test_config):
    pytest.fail("something wrong")

def test_calc_number_of_tasks():
    pytest.fail("something wrong")

def test_order_phases(test_workflow_object, test_config):
    test_config['flow']['workflow']['phases']['my_new_subjob_flow']['run_after'] = 'my_new_subjob_general'
#    test_config['flow']['workflow']['next_run_triggered_by'] = 'my_new_subjob_flow'
#    test_config['oifs']['workflow']['next_run_triggered_by'] = 'my_new_subjob_general'
    #test_config['flow']['workflow']['phases']['my_new_subjob_flow']['run_before'] = 'my_new_subjob_oifs'
    test_workflow_object = test_workflow_object.init_default_workflow(test_config)
    test_workflow_object = test_workflow_object.collect_all_user_workflows(test_config)
    test_workflow_object = test_workflow_object.order_phases()
    pytest.fail("something wrong")

def test_complete_clusters(test_workflow_object, test_config):
    test_workflow_object = test_workflow_object.init_default_workflow(test_config)
    test_workflow_object = test_workflow_object.collect_all_user_workflows(test_config)
    test_workflow_object = test_workflow_object.order_phases()
    subjob_clusters = test_workflow_object.complete_clusters(test_config)
    pytest.fail("something wrong")

def test_prepend_newrun_job(test_workflow_object, test_config):
    test_workflow_object = test_workflow_object.init_default_workflow(test_config)
    test_workflow_object = test_workflow_object.collect_all_user_workflows(test_config)
    test_workflow_object = test_workflow_object.order_phases()
    subjob_clusters = test_workflow_object.complete_clusters(test_config)
    [test_workflow_object, subjob_clusters] = workflow.prepend_newrun_job(test_workflow_object, test_config, subjob_clusters)
    pytest.fail("something wrong")

def test_write_to_config(test_workflow_object, test_config):
    test_workflow_object = test_workflow_object.init_default_workflow(test_config)
    test_workflow_object = test_workflow_object.collect_all_user_workflows(test_config)
    test_workflow_object = test_workflow_object.order_phases()
    subjob_clusters = test_workflow_object.complete_clusters(test_config)
    [test_workflow_object, subjob_clusters] = workflow.prepend_newrun_job(test_workflow_object, test_config, subjob_clusters)
    config = test_workflow_object.write_to_config(test_config)
    pytest.fail("something wrong")

def test_write_subjob_clusters_to_config(test_workflow_object, test_config):
    test_workflow_object = test_workflow_object.init_default_workflow(test_config)
    test_workflow_object = test_workflow_object.collect_all_user_workflows(test_config)
    test_workflow_object = test_workflow_object.order_phases()
    test_workflow_object = test_workflow_object.prepend_newrun_job(test_config)
    test_config = test_workflow_object.write_to_config(test_config)
    test_workflow_object = test_workflow_object.complete_clusters(test_config)

def test_prepend_newrun_job(test_workflow_object, test_config):
    test_workflow_object = test_workflow_object.init_default_workflow(test_config)
    test_workflow_object = test_workflow_object.collect_all_user_workflows(test_config)
    test_workflow_object = test_workflow_object.prepend_newrun_job(test_config)
    pytest.fail("something wrong")
