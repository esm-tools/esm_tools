import dpath.util
import pytest

from . import esm_parser

DUMMY_CONFIG_SIMPLE = {
    "general": {
        "foo": "bar",
        "baz": "qux",
        "quux": "corge",
    },
    "model_A": {
        "foo": "bar",
        "baz": "${general.foo}",
        "quux": "corge",
        "grault": "${model_B.foo}",
    },
    "model_B": {
        "foo": "qux",
        "baz": "bar",
        "quux": "corge",
        "component_A": {
            "foo": "bar",
            "baz": "${model_A.foo}",
            "quux": "${model_A.baz}",
        },
    },
    "model_C": {
        "foo": "bar",
        "baz": "qux",
        "quux": "corge",
        "component_A": {
            "foo": "bar",
            "baz": "${model_A.foo}",
            "quux": "${model_A.baz}",
        },
        "component_B": {
            "foo": "qux",
            "baz": "${model_B.component_A.baz}",
            "quux": "${model_B.baz}",
        },
    },
}

DOUBLE_VARIABLE_CONFIG = {
    "general": {
        "foo": "bar",
        "baz": "qux",
        "quux": "corge",
    },
    "model_A": {
        "foo": "bar_too",
        "baz": "${general.foo}_${model_A.foo}",
    },
}


@pytest.fixture
def prepare_dummy_config_simple():
    config = esm_parser.EsmConfig(config=DUMMY_CONFIG_SIMPLE)
    parser = esm_parser.EsmParser()
    return config, parser


@pytest.fixture
def prepare_double_variable_config():
    config = esm_parser.EsmConfig(config=DOUBLE_VARIABLE_CONFIG)
    parser = esm_parser.EsmParser()
    return config, parser


def test_find_variables(prepare_dummy_config_simple):
    config, parser = prepare_dummy_config_simple
    variables = parser.find_variables(config)
    for variable in variables:
        assert variable.should_resolve_from in dpath.util.get(
            DUMMY_CONFIG_SIMPLE, str(variable.address), separator="."
        )


def test_find_multiple_variables(prepare_double_variable_config):
    config, parser = prepare_double_variable_config
    variables = parser.find_variables(config)
    assert len(variables) == 2
    for variable in variables:
        assert variable.name == "baz"


def test_resolve_variable(prepare_dummy_config_simple):
    config, parser = prepare_dummy_config_simple
    variable = esm_parser.ConfigVariable(
        name="baz",
        address=esm_parser.ConfigAddress("model_A.baz"),
        should_resolve_from="general.foo",
    )
    assert variable.is_resolved is False
    parser.resolve_variable(config, variable)
    assert variable.is_resolved is True
    assert variable.value == "bar"
