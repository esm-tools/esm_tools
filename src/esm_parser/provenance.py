import esm_parser

from esm_calendar import Date


class ProvenanceClassForTheUnsubclassable:
    """
    A class to reproduce the methods of the unclassable ``bool`` and ``NoneType``
    classes, needed to add the ``provenance`` attribute to those versions of the types.

    This class stores 2 attributes, one is the ``value`` of the target object (a
    ``bool`` or a ``NoneType``) and the other is the ``provenance``.
    """

    def __init__(self, value, provenance):
        """
        Parameters
        ----------
        value : bool, None
            Value of the object
        provenance : any
            The provenance information
        """
        self.value = value
        self.provenance = provenance

    def __repr__(self):
        return f"{self.value}"

    def __bool__(self):
        return bool(self.value)

    def __eq__(self, other):
        if isinstance(other, BoolWithProvenance):
            return self.value == other.value

        return self.value == other

    def __hash__(self):
        return hash(self.value)


class BoolWithProvenance(ProvenanceClassForTheUnsubclassable):
    @property
    def __class__(self):
        """
        This is here for having ``isinstance(<my_bool_with_provenance>, bool)`` return ``True``
        """
        return bool


class NoneWithProvenance(ProvenanceClassForTheUnsubclassable):
    @property
    def __class__(self):
        """
        This is here for having ``isinstance(<my_bool_with_provenance>, None)`` return ``True``
        """
        return type(None)


def wrapper_with_provenance_factory(value, provenance=None):
    """
    A function to subclass and instanciate all types of subclassable objects in the
    ESM-Tools ``config`` and add the ``provenance`` attribute to them. It uses the
    ``WrapperWithProvenance`` class defined within the function for that purpose. For classes
    that are not subclassable (``bool`` and ``NoneType``) intanciates an object that
    mimics their behaviour but also contains the ``provenance`` attribute.

    Objects of type ``esm_calendari.esm_calendar.Date`` are not subclass (and the
    ``provenance`` attribute is simply added to them, because they fail to be subclassed
    with in the ``WrapperWithProvenance`` with the following error::

        __new__ method giving error object.__new__() takes exactly one argument
        (the type to instantiate)

    Parameters
    ----------
    value : any
        Value of the object to be subclassed and reinstanciated
    provenance : any
        The provenance information

    Returns
    -------
    WrapperWithProvenance, esm_calendar.esm_calendar.Date, BoolWithProvenance,
    NoneWithProvenance
        The new instance with the ``provenance`` attribute
    """

    if type(value) == bool:
        return BoolWithProvenance(value, provenance)

    elif value == None:
        return NoneWithProvenance(value, provenance)

    elif type(value) == Date:
        value.provenance = provenance
        return value

    else:

        class WrapperWithProvenance(type(value)):
            """
            Dynamically create a subclass of the type of the given value
            """

            def __new__(cls, value, *args, **kwargs):
                return super(WrapperWithProvenance, cls).__new__(cls, value)

            def __init__(self, value, provenance=None):
                self.provenance = provenance

            # Instantiate the subclass with the given value and provenance
        return WrapperWithProvenance(value, provenance)


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

        self.put_provenance(provenance)

    def put_provenance(self, provenance):
        """
        Defines recursively the ``provenance`` of the ``DictWithProvenance`` object
        ``self`` or it's nested ``dictionary``.

        Parameters
        ----------
        provenance : any
        dictionary : dict
            Dictionary for which the ``provenance`` is to be set. When a value is not
            given, the ``dictionary`` takes the value of ``self``. Only for recursion
            within nested ``DictWithProvenance``, do not use it outside of this method.
        """

        for key, val in self.items():
            if isinstance(val, dict):
                self[key] = DictWithProvenance(val, provenance.get(key, {}))
            elif isinstance(val, list):
                self[key] = ListWithProvenance(val, provenance.get(key, {}))
            else:
                self[key] = wrapper_with_provenance_factory(
                    val, provenance.get(key, None)
                )

    def set_provenance(self, provenance):
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
        print("set_provenance in dict: ",provenance)
        for key, val in self.items():
            if isinstance(val, dict):
                self[key] = DictWithProvenance(val, {})
                self[key].set_provenance(provenance)
            elif isinstance(val, list):
                self[key] = ListWithProvenance(val, {})
                self[key].set_provenance(provenance)
            else:
                self[key] = wrapper_with_provenance_factory(val, provenance)

    def get_provenance(self):
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

        for key, val in self.items():
            if isinstance(val, PROVENANCE_MAPPINGS):
                provenance_dict[key] = val.get_provenance()
            elif hasattr(val, "provenance"):
                provenance_dict[key] = val.provenance
            else:
                # The DictWithProvenance object might have dictionaries inside that
                # are not instances of that class (i.e. a dictionary added in the
                # backend). The provenance in this method is then defined as None
                provenance_dict[key] = None

        return provenance_dict

    def set_leaf_id_provenance(self, key):
        """
        Stores the last-leaf provenance information in the class level
        variable ``leaf_id_provenance``.

        This method gets the ``id`` value (unique Python object counter), which
        is used as a key in the `leaf_id_provenance`. The value becomes the
        provenance of that key, or defaults to ``None``

        Parameters
        ----------
        key : Any
           The key of the "inner-most" leaf to store provenance information for
        """
        # If it's a leaf
        if not isinstance(super().__getitem__(key), DictWithProvenance):
            val_id = id(super().__getitem__(key))
            # Stores the provenance in a class variable, under an id key
            DictWithProvenance.leaf_id_provenance[val_id] = self.provenance.get(
                key, None
            )


class ListWithProvenance(list):
    def __init__(self, mylist, provenance):
        super().__init__(mylist)

        self.put_provenance(provenance)

    def put_provenance(self, provenance):
        for c, elem in enumerate(self):
            if isinstance(elem, dict):
                self[c] = DictWithProvenance(elem, provenance[c])
            elif isinstance(elem, list):
                self[c] = ListWithProvenance(elem, provenance[c])
            else:
                self[c] = wrapper_with_provenance_factory(elem, provenance[c])

    def set_provenance(self, provenance):
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
        print("set_provenance in list: ",provenance)
        for c, elem in enumerate(self):
            if isinstance(elem, dict):
                self[c] = DictWithProvenance(elem, {})
                self[c].set_provenance(provenance)
            elif isinstance(elem, list):
                self[c] = ListWithProvenance(elem, [])
                self[c].set_provenance(provenance)
            else:
                self[c] = wrapper_with_provenance_factory(elem, provenance)

    def get_provenance(self):
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
        provenance_list : dict
            A dictionary with a structure and `keys` equivalent to the ``self``
            dictionary, but with `values` of the `key` leaves those of the provenance
        """

        provenance_list = []

        for elem in self:
            if isinstance(elem, PROVENANCE_MAPPINGS):
                provenance_list.append(elem.get_provenance())
            elif hasattr(elem, "provenance"):
                provenance_list.append(elem.provenance)
            else:
                # The DictWithProvenance object might have dictionaries inside that
                # are not instances of that class (i.e. a dictionary added in the
                # backend). The provenance in this method is then defined as None
                provenance_list.append(None)

        return provenance_list


PROVENANCE_MAPPINGS = (DictWithProvenance, ListWithProvenance)


def keep_provenance(func):
    """
    Decorator for recursive functions in ``esm_parser`` to preserve
    provenance.

    Recursive run functions in ``esm_parser`` are generally called on the innermost
    leaf. Here, we still run the function, but additionally store the output of the
    function into the `leaf_id_provenance` container so that provenance can be added
    to the result of the function call.

    Parameters
    ----------
    func : Callable
        The function to decorate
    """

    def inner(tree, rhs, *args, **kwargs):
        output = func(tree, rhs, *args, **kwargs)

        if hasattr(rhs, "provenance"):
            provenance = rhs.provenance
            # Value was modified
            if type(rhs) != type(output) or rhs != output:
                # If the new value has an inherited provenance, keep it (i.e. variable
                # was called: rhs = ${fesom.namelist_dir}, output =
                # /actual/path/with/provenance/to/be/kept})
                if hasattr(output, "provenance"):
                    append_modified_by_to_provenance(output.provenance, func)
                # If the rhs.provenance is not None and output has no provenance, keep
                # the old proveance
                elif provenance != None:
                    provenance = append_modified_by_to_provenance(provenance, func)
                    output = wrapper_with_provenance_factory(output, provenance)

        return output

    return inner


def append_modified_by_to_provenance(provenance, func):
    if "modified_by" not in provenance:
        provenance["modified_by"] = [str(func)]
    else:
        provenance["modified_by"].append(str(func))

    return provenance


if __name__ == "__main__":
    mydict = {
        "person": {"name": "Paul Gierz"},
        "a_string": "hello world",
        "my_var": "MY_VAR",
        "my_other_var": ["a", "b", "c"],
        "my_bolean": True,
        "my_float": 12.1,
        "my_int": 42,
        "list_with_dict_inside": [1, 2, {"my_dict": {"foo": [1, 2, {"foo": "bar"}]}}],
    }

    myprov = {
        "person": {"name": 1},
        "a_string": 2,
        "my_var": 3,
        "my_other_var": [4, 5, 6],
        "my_bolean": 7,
        "my_float": 8,
        "my_int": 9,
        "list_with_dict_inside": [10, 11, {"my_dict": {"foo": [12, 13, {"foo": 14}]}}],
    }

    asd = DictWithProvenance(mydict, myprov)

    print(asd)
    print(asd.get_provenance())

    print(asd["a_string"], asd["a_string"].provenance)
    print(asd["list_with_dict_inside"], asd["list_with_dict_inside"])
