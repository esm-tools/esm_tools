.. highlight:: shell
.. The next sets up red text for commenting the document. DELETE before merging inito release
.. role:: red

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

  * **Dictionaries**: a collection of `scalars`, `lists` or `dictionaries` nested inside a
    general `key`, i.e.::

      config_files:
              config:  config
              forcing: forcing
              ice:     ice

Some relevant **properties** of the ``YAML`` format are:

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
                      [ ... ]
              T127:
                      levels: "L47"
                      time_step: 200
                      [ ... ]

  * This data can be easily imported as `Python` dictionaries, which is part of what the `esm_parser`
    does.

  * ``:`` should always be **followed** by a `white space`.

  * **Strings** can be written both **inside quotes** (``key: "string"`` or ``key: 'string'``) **or
    unquoted** (``key: string``).

  * `YAML` format is **case sensitive**.

  * It is possible to add **comments** to ``YAML`` files using ``#`` before the comment (same as in
    Python).

==============================
ESM-Tools Extended YAML Syntax
==============================

.. warning::
   Work in progress. This chapter might be incomplete. Red statements might be imprecise or not true.

`ESM-Tools` offers extended functionality of the `YAML` files through the
`esm_parser`. The following :ref:`yaml:Extended Syntax` subsection lists the extended `ESM-Tools`
syntax for `YAML` files including calendar and math operations (see
:ref:`yaml:Math and Calendar Operations`).
The :ref:`yaml:YAML Elements` subsection lists the `YAML` elements needed for configuration files and
runscripts.

Extended Syntax
===============

Variable calls and ESM-Tools variables
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Variables defined in a `YAML` file can be invoked later on the same file or in oder files
(provided that the file defining the variable has been previously read :red:`(is that true?)`).
The syntax for calling an already defined variable is::

  "${name_of_the_variable}"

ESM-Tools provide a set of variables that can be called from `YAML` files without a previous
declaration:

.. csv-table::
   :header: Key, Description
   :widths: 15, 85

   start_date,          Model's start date.
   end_date,            Model's end date.
   initial_date,        :red:`I don't understand the diference between the start_date and initial_date and so on`
   final_date,          
   parent_date,         
   current_date,        Current date.
   next_date,           :red:`Following time step's date?`
   time_step,           Time step of the model.
   expid,               ID of the experiment.
   parent_expid,        Parent ID.
   esm_namelist_dir,    "Absolute path to the namelists folder (``<PATH>/esm_tools/namelists``)."
   esm_runscript_dir,   "Absolute path to the runscripts folder (``<PATH>/esm_tools/runscripts``)."
   model_dir,           Absolute path of the model directory (where it was installed by `esm_master`).

Lists starting with choose\_
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Lists named as ``choose_<name_of_a_property>`` can be used to nest ``configurations`` under a
``configuration_key`` that can be then invoked from the ``property`` itself::

  property_1: configuration_key_2

  choose_property_1:
                configuration_key_1:
                                configuration_1: value
                                configuration_2: value
                                [ ... ]
                configuration_key_2:
                                configuration_1: value
                                configuration_2: value
                                [ ... ]

An example that can better illustrate this general description is the `FESOM 2.0` resolution
configuration in ``<PATH>/esm_tools/configs/fesom/fesom-2.0.yaml``::

  resolution: CORE2

  choose_resolution:
          CORE2:
                  nx: 126858
                  mesh_dir: "${pool_dir}/meshes/mesh_CORE2_final/"
                  nproc: 288
          GLOB:
                  nx: 830305

Here we are selecting the ``CORE2`` as default configuration set for the ``resolution`` property,
but we could choose the ``GLOB`` configuration in another `YAML` file (i.e. a runscript), to override
this default choice.

Math and Calendar Operations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The following math and calendar operations are supported in `YAML` files:

* **Arithmetic operations**: an element of a `YAML` file can be defined as the result
  of the addition, subtraction, multiplication or division of variables with the format::

    key: "$(( ${variable_1} operator ${variable_2} operator ... ${variable_n} ))"

  The `esm_parser` supports calendar operations through `esm_calendar`. When performing calendar
  operations, variables that are not given in date format need to be followed by their ``unit`` for
  the resulting variable to be also in date format, i.e.::

    runtime: $(( ${end_date} - ${time_step}seconds ))

  ``time_step`` is a variable that is not given in date format, therefore, it is necessary to use
  ``seconds`` for ``runtime`` to be in date format. Another example is to subtract one day from
  the variable ``end_date``::

    $(( ${end_date} - 1days ))

  The units available are:

  ===================== ==================
  Units supported by arithmetic operations
  ========================================
  calendar units        | seconds
                        | minutes
                        | days
                        | months
                        | years
  ===================== ==================

* **Extraction of date components from a date**: it is possible to extract date components from a
  `date variable`. The syntax for such an operation is::

     "${variable!date_component}"

  An example to extract the year from the ``initial_time`` variable::

    yearnew: "${initial_date!syear}"

  The date components available are:

  ========= ======================================
  Date components
  ================================================
  ssecond   Second from a given date.
  sminute   Minute from a given date.
  shour     Hour from a given date.
  sday      Day from a given date.
  smonth    Month from a given date.
  syear     Year from a given date.
  sdoy      Day of the year, counting from Jan. 1.
  ========= ======================================

Globbing
~~~~~~~~

List loops
~~~~~~~~~~

This functionality allows for basic looping through a `YAML list`. The syntax for this is::

  "[[list_to_loop_through-->ELEMENT_OF_THE_LIST]]"

where ``ELEMENT_OF_THE_LIST`` can be used in the same line as a variable. This is
particularly useful to handle files which names contain common strings (i.e. `outdata` and
`restart` files).

The following example uses the list loop functionality inside the ``fesom-2.0.yaml``
configuration file to specify which files need to be copied from the `work` directory
of runs into the general experiment `outdata` directory. The files to be copied for runs
modeling a couple of months in year 2001 are ``a_ice.fesom.2001.nc``, ``alpha.fesom.2001.nc``,
``atmice_x.fesom.2001.nc``, etc. The string ``.fesom.2001.nc`` is present in all files so we
can use the list loop functionality together with calendar operations (:ref:`yaml:Math and Calendar
Operations`) to have a cleaner and more generalized configure file. First, you need to declare the
list of unshared names::

  outputs: [a_ice,alpha,atmice_x, ... ]

Then, you need to declare the ``outdata_sources`` dictionary::

  outdata_sources:
        "[[outputs-->OUTPUT]]": OUTPUT.fesom.${start_date!syear}.nc

Here, ``"[[outputs-->OUTPUT]]":`` provides the `keys` for this dictionary as ``a_ice``, ``alpha``,
``atmice_x``, etc., and ``OUTPUT`` is later used in the `value` to construct the complete file name
(``a_ice.fesom.2001.nc``, ``alpha.fesom.2001.nc``, ``atmice_x.fesom.2001.nc``, etc.).

Finally, ``outdata_targets`` dictionary can be defined to give different names to `outdata` files
from different runs using `calendar operations`::

  outdata_targets:
        "[[outputs-->OUTPUT]]": OUTPUT.fesom.${start_date!syear!smonth}.${start_date!sday}.nc

The values for the `keys` ``a_ice``, ``alpha``, ``atmice_x``, ..., will be
``a_ice.fesom.200101.01.nc``, ``alpha.fesom.200101.01.nc``, ``atmice_x.fesom.200101.01.nc``, ...,
for a January run, and ``a_ice.fesom.200102.01.nc``, ``alpha.fesom.200102.01.nc``,
``atmice_x.fesom.200102.01.nc``, ..., for a February run.

YAML Elements
=============

The `esm_parser` is used to read the multiple types of `YAML` files contained in `ESM-Tools`
(i.e. model and coupling configuration files, machine configurations, runscripts, etc.). Each of
these `YAML` files can contain two type of `YAML` elements:

  * **Tool-specific elements**: `YAML-scalars`, `lists` or `dictionaries` that include instructions and
    information used by `ESM-Tools`. These elements are predefined inside the `esm_parser` :red:`(are
    they really defined in esm_master?)` or other
    packages inside `ESM-Tools` and are used to control the `ESM-Tools` functionality.

  * **Model-specific elements**: `YAML-scalars`, `lists` of `dictionaries` that contain information
    specific to a given model, coupled setup, or machine. Thus, these elements have no meaning to
    the `esm_parser` itself and have no impact in the `ESM-Tools` behavior. :red:`Here, there
    should be something about what is done with the model-specific elements (i.e. written into BASH
    files, converted into model input, ... ?)`.

The following subsections list and describe the **Tool-specific elements** used to operate `ESM-Tools`
from different files.

Configuration Files
~~~~~~~~~~~~~~~~~~~

The following keys should be provided inside configuration files for models and coupled setups
(``<PATH>/esm_tools/configs/<model_or_setup>``):

.. csv-table::
   :header: Key, Description
   :widths: 15, 85

   model,               Name of the model.
   version,             Version of the model.
   repository,          Address of the model's repository.
   metadata,            "List to incude descriptive information about the model (i.e. ``Authors``, ``Institute``, ``Publications``, etc.) used to produce the content of :ref:`Supported_Models:Supported Models`. This information should be organized in nested `keys` followed by the corresponding description. Nested `keys` do not receive a special treatment meaning that you can include here any kind of information about the model. Only the `Publications` `key` is treated in a particular way: it can consist of a single element or a `list`, in which each element contains a link to the publication inside ``<>`` (i.e. ``- Title, Authors, Journal, Year. <https://doi.org/...>``)."
   restart_rate,        
   restart_unit,        
   resolution,          "Name for the desired resolution configuration defined inside the ``choose_resolution`` list."
   pool_dir,            Absolute path of the pool directory.
   setup_dir,           Absolute path of the setup directory.
   bin_dir,             Absolute path of the binary folder containing the model binaries.
   namelist_dir,        Absolute path of the namelists directory for the model.
   namelists,           "List of namelist files required for the model, and contained in ``namelist_dir`` folder."
   executable,          Name of the model executable file.
   choose_resolution,   List of dictionaries containing different resolution configurations.
   namelist_changes,    
   choose_lresume,      
   coupling_fields,     List of coupling field dictionaries containing coupling field variables.
   grids,               List of grid dictionaries containing grid parameters.

File Dictionaries
-----------------

File dictionaries are a special type of `YAML` elements that are useful to handle input, output,
forcing and restart files, and that are normally defined inside the `configuration files` of the
model. File dictionary's `keys` are composed by a file dictionary ``type`` followed by ``_`` and
an ``option``, and the `elements` consist of a list of ``file_tags`` as `keys` with their respective
``file_paths`` as `values`::

  type_option:
        - file_tag1: file_path1
        - file_tag2: file_path2

The ``file_tags`` need to be consistent throughout the different ``options`` for files to be
correctly handled by ESM-Tools. Exceptionally, ``sources`` files can be tagged differently but
then the option ``files`` is required to link sources tags to general tags used by the other
options (see `File dictionary options` table below).

**File dictionary types**

.. csv-table::
   :header: Key, Description
   :widths: 15, 85

   bin,                 Binary files.
   config,              Configure sources.
   ignore,              ":red:`Files to be ignored?`"
   forcing,             Forcing files. An example is described at the end of this section.
   log,                 Log files.
   outdata,             "Output configuration files. A concised example is described in :ref:`yaml:List Loops`."
   restart_in,          
   restart_out,         

**File dictionary options**

.. csv-table::
   :header: Key, Description
   :widths: 15, 85

   sources,             "Source file paths or source file names to be copied to the target path. **Without this option no files will be handled by ESM-Tools**. If ``targets`` option is not defined, the files are copied into the default `target` directory with the same name as in the `source` directory. In that case, if two files have the same name they are both renamed to end in the dates corresponding to their run (``file_name.extension_YYYYMMDD_YYYYMMDD``)."
   files,               "Links the general file tags (`key`) to the `source` elements defined in ``sources``. ``files`` **is optional**. If not present, all `source` files are copied to the `target` directory, and the `source tags` need to be the same as the ones in ``in_work`` and ``targets``. If present, only the `source` files included in ``files`` will be copied (see the `ECHAM` forcing files example below)."
   in_work,             "Files inside the `work` directory of a run (``<base_dir>/<experiment_name>/run_date1_date2/work``) to be transferred to the `target` directory. ``in_work`` **is optional**."
   targets,             "Paths and new names to be given to files transferred from the `sources` directory to the `target` directory. A concised example is described in :ref:`yaml:List Loops`. ``targets`` **is optional**."

File paths can be absolute, but most of the ``type_option`` combinations have a default folder
assigned, so that you can choose to specify only the file name. The default folders are:

.. csv-table::
   :header: Default folders, sources, in_work, targets
   :widths: 10, 30, 30, 30

   **bin**,             
   **config**,          
   **ignore**,          
   **forcing**,         
   **log**,             
   **outdata**,         ``<base_dir>/<experiment_name>/run_date1_date2/work``,          ``<base_dir>/<experiment_name>/run_date1_date2/work``,          ``<base_dir>/<experiment_name>/outdata/<model>``
   **restart_in**,      
   **restart_out**,     

**Example for ECHAM forcing files**



Runscripts
~~~~~~~~~~

The following keys should be provided inside runscripts
(``<PATH>/esm_tools/runscripts/<model>/<runscript.yaml>``):

.. csv-table::
   :header: Key, Description
   :widths: 15, 85

