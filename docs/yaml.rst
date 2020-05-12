.. highlight:: shell

================
YAML File Syntax
================

What Is YAML?
=============

`YAML` is a structured data format oriented to human-readability. Because of this property,
it is the chosen format for configuration and runscript files in `ESM-Tools`. These
`YAML` files are read by the `esm_parser` and then converted into a Python dictionary.
The functionality of the `YAML` files is further expanded through the `esm_parser` and
other `ESM-Tools` packages (i.e. calendar math through the `esm_calendar`). The
idea behind the implementation of the `YAML` format in `ESM-Tools` is that the user only
needs to create or edit easy-to-write `YAML` files to run a model or a coupled setup,
speeding up the configuration process, avoiding bugs and complex syntax.
The same should apply to developers that would like to implement their models
in `ESM-Tools`: the implementation consist on the configuration of a few `YAML` files.

YAML-Specific Syntax
~~~~~~~~~~~~~~~~~~~~

The main `YAML` **elements** relevant to `ESM-Tools` are:

  * **Scalars**: numbers, strings and booleans, defined by a `key` followed by ``:`` and a
    `value`, i.e.::

      model: fesom
      version: "2.0"
      time_step: 1800

  * **Lists**: a collection of elements defined by a `key` followed by ``:`` and an indented
    list of `elements` (numbers, strings or booleans) starting with ``-``, i.e.::

      namelists:
              - namelist.config
              - namelist.forcing
              - namelist.oce

   or a list of the same `elements` separated by ``,`` inside square brackets ``[elem1, elem2]``::

       namelists: [namelist.config, namelist.forcing, namelist.oce]

  * **Dictionaries**: a collection of `scalars`, `lists` or `dictionaries` indented inside a
    general `key`, i.e.::

      config_files:
              config:  config
              forcing: forcing
              ice:     ice

Some **properties** of the ``YAML`` format are:

  * Indentation can be used to structure information in as many levels as required, i.e. a dictionary
    ``choose_resolution`` that contains a list of dictionaries (``T63``, ``T31`` and ``T127``)::

      choose_resolution:
              T63:
                      levels: "L47"
                      time_step: 450
                      [ ... ]
              T31:
                      levels: "L19"
                      time_step: 450
                      [ ...]
              T127:
                      levels: "L47"
                      time_step: 200
                      [ ... ]

  * This data can be easily imported as `Python` dictionaries, which is part of what the `esm_parser`
    does.

  * ``:`` should always be **followed** by a `white space`.

  * **Strings** can be written both **inside quotes** (``key: "string"`` or ``key: 'string'``) **or 
    unquoted** (``key: string``).

  * ``YAML`` format is **case sensitive**.

  * It is possible to add **comments** to ``YAML`` files using ``#`` before the comment (same as in
    Python).

Extended YAML Syntax
====================

As previously mentioned `ESM-Tools` offers through `esm_parser` 

Configuration File
==================

The following keys are supported inside configuration files for models and coupled setups
(``<PATH>/esm_tools/configs/<model_or_setup>``).

.. csv-table::
   :header: Key, Description
   :widths: 15, 85

   model,       Name of the model.
   repository,  Address of the model's repository.
   metadata,    "List to incude descriptive information about the model (i.e. ``Authors``, ``Institute``, ``Publications``, etc.) used to produce the content of :ref:`Supported_Models:Supported Models`. This information should be organized in nested `keys` followed by the corresponding description. Nested `keys` do not receive a special treatment meaning that you can include here any kind of information about the model. Only the `Publications` `key` is treated in a particular way: it can consist of a single element or a `list`, in which each element contains a link to the publication inside ``<>`` (i.e. ``- Title, Authors, Journal, Year. <https://doi.org/...>``)."

Math Operations
===============

Some math operations are supported in `yaml` files:

* **Addition and subtraction of dates**: a key of a `yaml` file can be define as the result
  of an addition or subtraction of times and dates with the format ``key:
  $(( ${time1}unit1 operator ${time2}unit2 operator ... ))``, where the unit specify
  the time unit for every time in the operation.
  The units available are `seconds`, ... . Example for a ``runtime`` definition::

        runtime: $(( ${end_date} - ${time_step}seconds ))


