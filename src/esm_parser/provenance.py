class DictWithProvenance(dict):
    def __init__(self, dictionary, provenance):
        super().__init__(dictionary)

        self.provenance = {}
        self._set_provenance(provenance)

    def _set_provenance(self, provenance, dictionary=None):
        if not dictionary:
            root = True
            dictionary = self
        else:
            root = False

        for key, val in dictionary.items():
            if isinstance(val, dict):
                dictionary[key] = DictWithProvenance(val, provenance)
            else:
                dictionary[key] = val
                dictionary.provenance[key] = provenance

    def __setitem__(self, key, val):
        if isinstance(val, DictWithProvenance):
            return super().__setitem__(key, val)
        elif isinstance(val, dict):
            return super().__setitem__(key, DictWithProvenance(val, provenance=None))
        # elif isinstance(self, DictWithProvenance) and not isinstance(val, dict):
        #    self.provenance = {key: None}
        #    return super().__setitem__(key, val)
        else:
            self.provenance[key] = None
            return super().__setitem__(key, val)

    def provenance_tree(self, dictionary=None):
        provenance_dict = {}
        if not dictionary:
            dictionary = self
        for key, val in dictionary.items():
            if isinstance(val, dict):
                provenance_dict[key] = self.provenance_tree(val)
            else:
                provenance_dict[key] = dictionary.provenance[key]

        return provenance_dict


# The following can be converted into unit tests:
config_dict = {
    "echam": {
        "type": "atmosphere",
        "files": {
            "greenhouse": {"kind": "input", "path_in_computer": "/my/path/in/computer"}
        },
    }
}

my_provenance = {
    "from_file": None,
    "type": None,
}

config = DictWithProvenance(config_dict, my_provenance)

# Test 1 (should give you a provenance of None for the key ["fesom"]["asd"])
config["fesom"] = {"asd": 0}
assert config.provenance_tree() == {
    "echam": {
        "type": {"from_file": None, "type": None},
        "files": {
            "greenhouse": {
                "kind": {"from_file": None, "type": None},
                "path_in_computer": {"from_file": None, "type": None},
            }
        },
    },
    "fesom": {"asd": None},
}

# Test 2 (should give you a provenance of None for the key "computer")
config["computer"] = 0
check_provenance = {
    "echam": {
        "type": {"from_file": None, "type": None},
        "files": {
            "greenhouse": {
                "kind": {"from_file": None, "type": None},
                "path_in_computer": {"from_file": None, "type": None},
            }
        },
    },
    "fesom": {"asd": None},
    "computer": None,
}
assert config.provenance_tree() == check_provenance

# Test 3 (should give you a provenance of 2 for the leaf keys inside "fesom")
config_fesom = DictWithProvenance({"fesom": {"asd": 0, "model": "ocean"}}, 2)
config.update(config_fesom)
check_provenance = {'echam': {'type': {'from_file': None, 'type': None}, 'files': {'greenhouse': {'kind': {'from_file': None, 'type': None}, 'path_in_computer': {'from_file': None, 'type': None}}}}, 'fesom': {'asd': 2, 'model': 2}, 'computer': None}
assert config.provenance_tree() == check_provenance

# Test 4 (should give you a provenance of None for the keye True)
config[True] = "boolean"
check_provenance = {'echam': {'type': {'from_file': None, 'type': None}, 'files': {'greenhouse': {'kind': {'from_file': None, 'type': None}, 'path_in_computer': {'from_file': None, 'type': None}}}}, 'fesom': {'asd': 2, 'model': 2}, 'computer': None, True: None}
assert config.provenance_tree() == check_provenance
