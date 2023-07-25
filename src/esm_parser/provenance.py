import esm_parser


class BoolWithProvenance:
    def __init__(self, value, provenance):
        #for attribute in dir(bool(value)):
        #    try:
        #        self.__setattr__(attribute, bool(value).__getattribute__(attribute))
        #        print(attribute)
        #    except (TypeError, AttributeError):
        #        pass

        self.value = value
        self.provenance = provenance

    def __repr__(self):
        return f"{self.value}"

    def __bool__(self):
        return bool(self.value)

    def __eq__(self, other):

        if isinstance(other, BoolWithProvenance):
            return self.value == other.value

        return self.value==other

    def __hash__(self):
        return hash(self.value)


def ValueWrapperFactory(value, provenance=None):

    if type(value) == bool:
        return BoolWithProvenance(value, provenance)

    else:


        # Dynamically create a subclass of the type of the given value
        class ValueWrapper(type(value)):
            def __new__(cls, value, *args, **kwargs):
                return super(ValueWrapper, cls).__new__(cls, value)
            #def __new__(cls, value, provenance=None):
            #    return super().__new__(cls)

            def __init__(self, value, provenance=None):
            #    self.value = value
                self.provenance = provenance

        # Instantiate the subclass with the given value and provenance
        return ValueWrapper(value, provenance)


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

    leaf_id_provenance = {}
    """
    dict : 
        This is a class-level variable to hold IDs of objects in memory. It is
        at the class level because this information needs to be passed around
        between various instances
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
                self[key] = ValueWrapperFactory(val, provenance.get(key, None))


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

        for key, val in self.items():
            if isinstance(val, dict):
                self[key] = DictWithProvenance(val, provenance)
            if isinstance(val, list):
                self[key] = ListWithProvenance(val, provenance)
            else:
                self[key] = ValueWrapperFactory(val, provenance)


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
            if isinstance(val, PROVENANCE_CLASSES):
                provenance_dict[key] = val.get_provenance()
            elif hasattr(val, "provenance"):
                provenance_dict[key] = val.provenance
            else:
                # The DictWithProvenance object might have dictionaries inside that
                # are not instances of that class (i.e. a dictionary added in the
                # backend). The provenance in this method is then defined as None
                provenance_dict[key] = None

        return provenance_dict


    #def __getitem__(self, key, *args, **kwargs):
    #    self.set_leaf_id_provenance(key)

    #    return super().__getitem__(key, *args, **kwargs)


    #def __setitem__(self, key, val, *args, **kwargs):
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
        #if isinstance(val, DictWithProvenance):
        #    return super().__setitem__(key, val, *args, **kwargs)
        # If ``val`` is a ``dict`` but not a ``DictWithProvenance`` set the ``val``
        # as an instance of ``DictWithProvenance`` setting the ``provenance`` as
        # ``None``
        #elif isinstance(val, dict):
        #    return super().__setitem__(key, DictWithProvenance(val, provenance=None))
        # If the ``val`` is not a ``dict`` (it's a leaf of the dictionary tree) defines
        # ``self.provenance[key]`` as ``None``
        #else:
        #    val_id = id(val)
        #    self.provenance[key] = DictWithProvenance.leaf_id_provenance.get(val_id, None)
        #    return super().__setitem__(key, val, *args, **kwargs)


    #def __delitem__(self, key, *args, **kwargs):

    #    if isinstance(self, DictWithProvenance) and not isinstance(super().__getitem__(key), dict):
    #        del self.provenance[key]

    #    super().__delitem__(key, *args, **kwargs)


    #def update(self, dictionary, *args, **kwargs):
    #    super().update(dictionary, *args, **kwargs)

    #    for key, val in dictionary.items():
    #        val_id = id(val)
    #        if isinstance(dictionary, DictWithProvenance) and not isinstance(val, dict):
    #            self.provenance[key] = dictionary.provenance.get(key,
    #                DictWithProvenance.leaf_id_provenance.get(val_id, None)
    #            )
    #        elif isinstance(dictionary, dict) and not isinstance(val, dict):
    #            self.provenance[key] = DictWithProvenance.leaf_id_provenance.get(val_id, None)


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
            DictWithProvenance.leaf_id_provenance[val_id] = self.provenance.get(key, None)


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
                self[c] = ValueWrapperFactory(elem, provenance[c])


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
            if isinstance(elem, PROVENANCE_CLASSES):
                provenance_list.append(elem.get_provenance())
            elif hasattr(elem, "provenance"):
                provenance_list.append(elem.provenance)
            else:
                # The DictWithProvenance object might have dictionaries inside that
                # are not instances of that class (i.e. a dictionary added in the
                # backend). The provenance in this method is then defined as None
                provenance_list.append(None)

        return provenance_list


PROVENANCE_CLASSES = (DictWithProvenance, ListWithProvenance)

def keep_id_provenance(func):
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
        #rhs_id = id(rhs)
        output = func(tree, rhs, *args, **kwargs)
        #output_id = id(output)

        #if rhs_id in DictWithProvenance.leaf_id_provenance:
        #    DictWithProvenance.leaf_id_provenance[output_id] = DictWithProvenance.leaf_id_provenance[rhs_id]

        return output

    return inner

if __name__=="__main__":
    mydict = {
        "person": {
            "name": "Paul Gierz"
        },
        "a_string": "hello world",
        "my_var": "MY_VAR",
        "my_other_var": ["a", "b", "c"],
        "my_bolean": True,
        "my_float": 12.1,
        "my_int": 42,
        "list_with_dict_inside": [1, 2, {
            "my_dict": {
                "foo": [1,2, {"foo": "bar"}]
            }
        }]
    }

    myprov = {
        "person": {
            "name": 1
        },
        "a_string": 2,
        "my_var": 3,
        "my_other_var": [4, 5, 6],
        "my_bolean": 7,
        "my_float": 8,
        "my_int": 9,
        "list_with_dict_inside": [10, 11, {
            "my_dict": {
                "foo": [12,13, {"foo": 14}]
            }
        }]
    }

    asd = DictWithProvenance(mydict, myprov)

    print(asd)
    print(asd.get_provenance())

    print(asd["a_string"], asd["a_string"].provenance)
    print(asd["list_with_dict_inside"],asd["list_with_dict_inside"])
