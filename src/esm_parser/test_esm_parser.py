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


@pytest.fixture
def prepare_config():
    config = esm_parser.EsmConfig(config=DUMMY_CONFIG_SIMPLE)
    parser = esm_parser.EsmParser()
    return config, parser


def test_find_variables(prepare_config):
    config, parser = prepare_config
    variables = parser.find_variables(config)
    for variable in variables:
        assert variable.should_resolve_from == dpath.util.get(
            DUMMY_CONFIG_SIMPLE, str(variable.address), separator="."
        )


def test_resolve_variable(prepare_config):
    config, parser = prepare_config
    variable = esm_parser.ConfigVariable(name="foo", address="general.foo", value="bar")
