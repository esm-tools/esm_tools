class DictWithProvenance(dict):
    """
    A dictionary subclass that contains a ``provenance`` attribute. This attribute is
    a ``dict`` that contains those `keys` of the original dictionary whose `values`
    **are not a** ``dict`` (leaves of the dictionary tree), and a provenance value
    defined during the instancing of the object. The ``provenance`` attribute is
    applied recursively within the nested dictionaries during instancing or when the
    ``self.set_provenance(<my_provenance>)`` is used.

    Example
    -------
    After instancing the object:

        .. code-block:: python

            dict_with_provenance = DictWithProvenance(config_dict, {"file": "echam.yaml"})

    where ``config_dict`` is defined as:

        .. code-block:: python

            config_dict = {
                "echam": {
                    "type": "atmosphere",
                    "files": {
                        "greenhouse": {"kind": "input", "path_in_computer": "/my/path/in/computer"}
                    },
                }
            }

    then ``config_dict["echam"].provenance`` will take the following values:

        .. code-block:: python

            >>> config_dict["echam"].provenance
            {'type': {'file': 'echam.yaml'}}

    Note that the `key` ``"files"`` does not exist as the value for that key in the
    ``config_dict`` is a dictionary (**it is not a leaf of the dictionary tree**).

    The `provenance value` can be defined to be any python object. The ``provenance``
    attribute is inherited when merging dictionaries with the ``update`` method
    when merging two ``DictWithProvenance`` objects, with the same rewriting strategy
    as for the keys in the dictionary, and ``provenance`` is also inherited when
    redefining a `value` to contain a ``DictWithProvenance``.

    Use
    ---
    Instance a new ``DictWithProvenance`` object::

        dict_with_provenance = DictWithProvenance(<a_dictionary>, <my_provenance>)

    Redefine the provenance of an existing ``key``::

        dict_with_provenance["<a_key>"].set_provenance(<new_provenance>)

    Set the provenace of a specific leaf within a nested dictionary::

        dict_with_provenance["key1"]["key1"].provenance["leaf_key"] = <new_provenance>

    Get the ``provenance`` representation of the dictionary::

        provenance_dict = dict_with_provenance.get_provenance()
    """


    def __init__(self, dictionary, provenance):
        """
        Instanciates the ``dictionary`` as an object of ``DictWithProvenance`` and
        defines its ``provenance`` attribute recursively with ``set_provenance``.

        Parameters
        ----------
        dictionary : dict
            The ``dict`` that needs to be converted to a ``DictWithProvenance`` object
        provenance : any
            The provenance that will be recursively assigned to all leaves of the
            dictionary tree
        """

        super().__init__(dictionary)

        self.provenance = {}
        self.set_provenance(provenance)


    def set_provenance(self, provenance, dictionary=None):
        """
        Defines recursively the ``provenance`` of the ``DictWithProvenance`` object
        ``self`` or it's nested ``dictionary``.

        Parameters
        ----------
        provenance : any
            New `provenance value` to be set
        dictionary : dict
            Dictionary for which the ``provenance`` is to be set. When a value is not
            given, the ``dictionary`` takes the value of ``self``. Only for recursion
            within nested ``DictWithProvenance``, do not use it outside of this method.
        """

        if not dictionary:
            dictionary = self

        for key, val in dictionary.items():
            if isinstance(val, dict):
                dictionary[key] = DictWithProvenance(val, provenance)
            else:
                dictionary[key] = val
                dictionary.provenance[key] = provenance


    def get_provenance(self, dictionary=None):
        """
        Returns a ``dictionary`` containing the all the nested provenance information
        of the current ``DictWithProvenance`` with a structure and `keys` equivalent to
        the ``self`` dictionary, but with `values` of the `key` leaves those of the
        provenance.

        Parameters
        ----------
        dictionary : dict
            Dictionary for which the provenance needs to be extracted. When a value is
            not given, the ``dictionary`` takes the value of ``self``. Only for
            recursion within nested ``DictWithProvenance``, do not use it outside of
            this method.

        Returns
        -------
        provenance_dict : dict
            A dictionary with a structure and `keys` equivalent to the ``self``
            dictionary, but with `values` of the `key` leaves those of the provenance
        """

        provenance_dict = {}
        if not dictionary:
            dictionary = self

        for key, val in dictionary.items():
            if isinstance(val, dict):
                provenance_dict[key] = self.get_provenance(val)
            else:
                provenance_dict[key] = dictionary.provenance[key]

        return provenance_dict


    def __setitem__(self, key, val):
        """
        Rewrites the ``dict.__setitem__`` method (used for example in when defining a
        `key's value` ``my_dict[key] = val``) to additionally inherit the provenance of
        the new ``val``. There are three cases in which the ``provenance`` can be
        inherited:

        1. ``val`` is a ``DictWithProvenance``: If the value is already an object of
            ``DictWithProvenance`` we use the standard ``__setitem__`` method from
            ``dict`` because that preserves all object's methods and properties

        2. ``val`` is a ``dict``: If ``val`` is a ``dict`` but not a
            ``DictWithProvenance`` sets the ``val`` as an instance of
            ``DictWithProvenance`` setting the ``provenance`` as ``None``

        3. ``val`` is not a ``dict``: If the ``val`` is not a ``dict`` (it's a leaf of
            the dictionary tree) defines ``self.provenance[key]`` as ``None``. Leaves
            of nested ``DictWithProvenance`` cannot contain attribute as we cannot
            subclass python objects such as ``bool`` and therefore, we keep the
            provenance information one level above in our ``dict`` subclass, within
            the attribute ``self.provenance`` for the ``key`` associated to that
            ``val``.

        Parameters
        ----------
        key : string
            Key of a ``dict``
        val : any
            New value of the key

        Returns
        -------
        NoneType :
            Returns the same as ``dict.__setitem__(key, val)`` method would return
            with the exception that the ``val`` will be redefined based on options 1,
            2 or 3
        """

        # If the value is already an object of ``DictWithProvenance`` we can use the
        # standard ``__setitem__`` method from ``dict`` because that preserves all
        # object's methods and properties
        if isinstance(val, DictWithProvenance):
            return super().__setitem__(key, val)
        # If ``val`` is a ``dict`` but not a ``DictWithProvenance`` set the ``val``
        # as an instance of ``DictWithProvenance`` setting the ``provenance`` as
        # ``None``
        elif isinstance(val, dict):
            return super().__setitem__(key, DictWithProvenance(val, provenance=None))
        # If the ``val`` is not a ``dict`` (it's a leaf of the dictionary tree) defines
        # ``self.provenance[key]`` as ``None``
        else:
            self.provenance[key] = None
            return super().__setitem__(key, val)


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
assert config.get_provenance() == {
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
assert config.get_provenance() == check_provenance

# Test 3 (should give you a provenance of 2 for the leaf keys inside "fesom")
config_fesom = DictWithProvenance({"fesom": {"asd": 0, "model": "ocean"}}, 2)
config.update(config_fesom)
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
    "fesom": {"asd": 2, "model": 2},
    "computer": None,
}
assert config.get_provenance() == check_provenance

# Test 4 (should give you a provenance of None for the keye True)
config[True] = "boolean"
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
    "fesom": {"asd": 2, "model": 2},
    "computer": None,
    True: None,
}
assert config.get_provenance() == check_provenance

# Test 5 (reset the provenance of all ``echam`` leaves to "a_string")
config["echam"].set_provenance("a_string")
check_provenance = {'echam': {'type': 'a_string', 'files': {'greenhouse': {'kind': 'a_string', 'path_in_computer': 'a_string'}}}, 'fesom': {'asd': 2, 'model': 2}, 'computer': None, True: None}
assert config.get_provenance() == check_provenance

# Test 6 (reset the provenance of a leaf)
config["echam"]["files"]["greenhouse"].provenance["kind"] = "a_new_string"
check_provenance = {'echam': {'type': 'a_string', 'files': {'greenhouse': {'kind': 'a_new_string', 'path_in_computer': 'a_string'}}}, 'fesom': {'asd': 2, 'model': 2}, 'computer': None, True: None}
assert config.get_provenance() == check_provenance
