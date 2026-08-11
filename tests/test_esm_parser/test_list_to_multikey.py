import pytest

from esm_parser.esm_parser import do_math_in_entry, list_to_multikey


@pytest.fixture
def simple_config():
    """Return a simple configuration dict for testing."""
    return {
        "echam": {
            "streams": ["echam", "accw", "co2"],
            "outdata_sources": {
                "[[streams-->STREAM]]": "a_string_STREAM",
            },
        },
        "fesom": {
            "awicm3_fields": ["ist_feom", "sia_feom"],
            "coupling_fields": {"[[awicm3_fields-->FIELD]]": {"grid": "feom"}},
        },
        "lpj_guess": {
            "proc_list": [0, 1, 2],
            "input_in_work": {
                "VegInit_[[proc_list-->PROC_NUM]]": "run$(( PROC_NUM + 1 ))/vegin.nc",
            },
        },
        "general": {
            "model_name": "awicm3",
            "include_models": ["echam", "fesom"],
            "[[include_models-->MODEL]]_list": ["MODEL_1", "MODEL_2"],
        },
    }


class TestListToMultikey:
    def test_basic_string_expansion(self, simple_config):
        """Test basic expansion of string with multikey format."""
        tree = ["echam", "outdata_sources", "[[streams-->STREAM]]"]
        rhs = "value"
        result = list_to_multikey(tree, rhs, simple_config, [], False)

        expected = {
            "echam": "value",
            "accw": "value",
            "co2": "value",
        }

        assert result == expected

    def test_with_string_replacement(self, simple_config):
        """Test with string that needs replacements of STREAM placeholders."""
        tree = ["echam", "outdata_sources", "[[streams-->STREAM]]"]
        rhs = simple_config["echam"]["outdata_sources"]["[[streams-->STREAM]]"]
        result = list_to_multikey(tree, rhs, simple_config, [], False)

        expected = {
            "echam": "a_string_echam",
            "accw": "a_string_accw",
            "co2": "a_string_co2",
        }

        assert result == expected

    def test_with_list_rhs(self, simple_config):
        """Test with a list as the right-hand side value."""
        tree = ["general", "[[include_models-->MODEL]]_list"]
        rhs = simple_config["general"]["[[include_models-->MODEL]]_list"]
        result = list_to_multikey(tree, rhs, simple_config, [], False)

        # The function adds all items for each entry in the list
        expected = {
            "echam_list": ["echam_1", "fesom_1", "echam_2", "fesom_2"],
            "fesom_list": ["echam_1", "fesom_1", "echam_2", "fesom_2"],
        }

        assert result == expected

    def test_with_dict_rhs(self, simple_config):
        """Test with a dictionary as the right-hand side value."""
        tree = ["fesom", "coupling_fields", "[[awicm3_fields-->FIELD]]"]
        rhs = simple_config["fesom"]["coupling_fields"]["[[awicm3_fields-->FIELD]]"]
        result = list_to_multikey(tree, rhs, simple_config, [], False)

        expected = {
            "ist_feom": {"grid": "feom"},
            "sia_feom": {"grid": "feom"},
        }

        assert result == expected

    def test_without_multikey_syntax(self, simple_config):
        """Test with a normal string without multikey syntax."""
        tree = ["general", "model_name"]
        rhs = simple_config["general"]["model_name"]
        result = list_to_multikey(tree, rhs, simple_config, [], False)

        expected = {"model_name": "awicm3"}

        assert result == expected

    def test_multikey_with_eval(self, simple_config):
        """Test evaluation of math expressions within multikey expansion."""
        tree = ["lpj_guess", "input_in_work", "VegInit_[[proc_list-->PROC_NUM]]"]

        # First, get the raw value that contains the math expression
        rhs = simple_config["lpj_guess"]["input_in_work"][
            "VegInit_[[proc_list-->PROC_NUM]]"
        ]

        # Step 1: First evaluate the math expressions in the raw string
        # This should convert '$(( PROC_NUM + 1 ))' to something like 'eval( PROC_NUM + 1 )'
        evaluated_rhs = do_math_in_entry(tree, rhs, simple_config)

        # Step 2: Now pass the evaluated string through list_to_multikey
        # Update the config to use the evaluated expression
        updated_config = simple_config.copy()
        updated_config["lpj_guess"]["input_in_work"][
            "VegInit_[[proc_list-->PROC_NUM]]"
        ] = evaluated_rhs

        # Now expand the multikey
        result = list_to_multikey(tree, evaluated_rhs, updated_config, [], False)

        # The expected result after both math evaluation and multikey expansion
        expected = {
            "VegInit_0": "run1/vegin.nc",
            "VegInit_1": "run2/vegin.nc",
            "VegInit_2": "run3/vegin.nc",
        }

        # Verify the results match the expected values
        assert result == expected
