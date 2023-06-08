class DictWithProvenance(dict):

    def __init__(self, dictionary, provenance=None):

        super().__init__(dictionary)

        self.provenance = {}

        if provenance:
            self._set_provenance(provenance)

    def _set_provenance(self, provenance, dictionary=None):
        if not dictionary:
            root = True
            dictionary = self
            this_provenance = self.provenance
        else:
            root = False
            this_provenance = {}

        for key, val in dictionary.items():
            if isinstance(val, dict):
                this_provenance[key] = self._set_provenance(provenance, dictionary=val)
            else:
                this_provenance[key] = provenance

        if not root:
            return this_provenance

    def __getitem__(self, key):
        return DictWithProvenance(self[key], self.provenance[key])

#        if isinstance(super().__getitem__(key), dict):
#            return DictWithProvenance(super().__getitem__(key), self.provenance[key])
#            return super().__getitem__(key)
#        else:
#            return super().__getitem__(key)

    #def __setitem__(self, key, val):
    #    super().__setitem__(self, key, val)
    #    self.provenance["key", "val"] = "asd"

class ItemWithProvenance(object):

    def __init__(self, val, provenance=None):

        super().__init__(dictionary)

        self.provenance = {}

        if provenance:
            self._set_provenance(provenance)

        self.data = {"value": value, "provenance": provenance}

    def _set_provenance(self, provenance, dictionary=None):
        if not dictionary:
            root = True
            dictionary = self
            this_provenance = self.provenance
        else:
            root = False
            this_provenance = {}


config_dict = {
    "echam": {
        "type": "atmosphere",
        "files": {
            "greenhouse": {
                "kind": "input",
                "path_in_computer": "/my/path/in/computer"
            }
        }
    }
}

my_provenance = {
    "from_file": None,
    "type": None,
}

config = DictWithProvenance(config_dict, my_provenance)

