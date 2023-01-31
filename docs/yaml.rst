.. highlight:: shell
.. The next sets up red text for commenting the document. DELETE before merging inito release
.. role:: red

================
YAML File Syntax
================

What Is YAML?
=============

`YAML` is a structured data format oriented to human-readability. Because of this property,
it is the chosen format for configuration and runscript files in `ESM-Tools` and the
recommended format for runscripts (though bash runscripts are still supported). These
`YAML` files are read by the `esm_parser` and then converted into a Python dictionary.
The functionality of the `YAML` files is further expanded through the `esm_parser` and
other `ESM-Tools` packages (i.e. calendar math through the `esm_calendar`). The
idea behind the implementation of the `YAML` format in `ESM-Tools` is that the user only
needs to create or edit easy-to-write `YAML` files to run a model or a coupled setup,
speeding up the configuration process, avoiding bugs and complex syntax.
The same should apply to developers that would like to implement their models
in `ESM-Tools`: the implementation consists on the configuration of a few `YAML` files.

.. warning::
   `Tabs` are not allowed as `yaml` indentation, and therefore, `ESM-Tools` will return an
   error every time a `yaml` file with `tabs` is invoked (e.g. `runscripts` and `config`
   files need to be `'tab-free'`).

YAML-Specific Syntax
~~~~~~~~~~~~~~~~~~~~

The main `YAML` **elements** relevant to `ESM-Tools` are:

  * **Scalars**: numbers, strings and booleans, defined by a `key` followed by ``:`` and a
    `value`, i.e.:

    .. code-block:: yaml

       model: fesom
       version: "2.0"
       time_step: 1800

  * **Lists**: a collection of elements defined by a `key` followed by ``:`` and an indented
    list of `elements` (numbers, strings or booleans) starting with ``-``, i.e.:

    .. code-block:: yaml

       namelists:
               - namelist.config
               - namelist.forcing
               - namelist.oce

    or a list of the same `elements` separated by ``,`` inside square brackets ``[elem1, elem2]``:

    .. code-block:: yaml

       namelists: [namelist.config, namelist.forcing, namelist.oce]

  * **Dictionaries**: a collection of `scalars`, `lists` or `dictionaries` nested inside a
    general `key`, i.e.:

    .. code-block:: yaml

       config_files:
               config:  config
               forcing: forcing
               ice:     ice

Some relevant **properties** of the ``YAML`` format are:

  * Only **white spaces** can be used for indentation. **Tabs are not allowed**.

  * Indentation can be used to structure information in as many levels as required, i.e. a dictionary
    ``choose_resolution`` that contains a list of dictionaries (``T63``, ``T31`` and ``T127``):

    .. code-block:: yaml

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

ESM-Tools Extended YAML Syntax
==============================

.. warning::
   Work in progress. This chapter might be incomplete. Red statements might be imprecise or not true.

`ESM-Tools` offers extended functionality of the `YAML` files through the
`esm_parser`. The following subsections list the extended `ESM-Tools`
syntax for `YAML` files including calendar and math operations (see
:ref:`yaml:Math and Calendar Operations`).
The :ref:`yaml:YAML Elements` section lists the `YAML` elements needed for configuration files and
runscripts.

Variable Calls
~~~~~~~~~~~~~~

Variables defined in a `YAML` file can be invoked on the same file or in oder files
provided that the file where it is defined is read for the given operation.
The syntax for calling an already defined variable is:

.. code-block:: yaml

   "${name_of_the_variable}"

Variables can be nested in sections. To define a variable using the value of another one that is
nested on a section the following syntax is needed:

.. code-block:: yaml

   "${<section>.<variable>}"

When using `esm_parser`, variables in components, setups, machine files, general information, etc.,
are grouped under sections of respective names (i.e. ``general``, ``ollie``, ``fesom``, ``awicm``, ...).
To access a variable from a different file than the one in which it is declared it is necessary to
reference the file name or label as it follows:

.. code-block:: yaml

   "${<file_label>.<section>.<variable>}"

**Example**

Lets take as an example the variable ``ini_parent_exp_id`` inside the ``general`` section in the
`FESOM-REcoM` runscript ``runscripts/fesom-recom/fesom-recom-ollie-restart-daily.yaml``:

.. code-block:: yaml

   general:
           setup_name: fesom-recom
           [ ... ]
           ini_parent_exp_id: restart_test
           ini_restart_dir: /work/ollie/mandresm/esm_yaml_test/${ini_parent_exp_id}/restart/
           [ ... ]

Here we use ``ini_parent_exp_id`` to define part of the restart path ``ini_restart_dir``.
``general.ini_restart_dir`` is going to be called from the `FESOM-REcoM` configuration file
``configs/setups/fesom-recom/fesom-recom.yaml`` to define the restart directory for `FESOM`
``fesom.ini_restart_dir``:

.. code-block:: yaml

   [ ... ]
   ini_restart_dir: "${general.ini_restart_dir}/fesom/"
   [ ... ]

Note that this line adds the subfolder ``/fesom/`` to the subdirectory.

If we would like to invoke from the same runscript some of the variables defined in another file,
for example the ``useMPI`` variable in ``configs/machines/ollie.yaml``, then we would need to use:

.. code-block:: yaml

   a_new_variable: "${ollie.useMPI}"

Bare in mind that these examples will only work if both `FESOM` and `REcoM` are involved in the
`ESM-Tool` task triggered and if the task is run in `Ollie` (i.e. it will work for
``esm_runscripts fesom-recom-ollie-restart-daily.yaml -e <experiment_id> ...``).

Switches (``choose_``)
~~~~~~~~~~~~~~~~~~~~~~

A `YAML` list named as ``choose_<variable>`` function as a `switch` that evaluates the given ``variable``.
The nested element `keys` inside the ``choose_<variable>`` act as `cases` for the switch and the `values` of
this elements are only defined outside of the ``choose_<variable>`` if they belong to the selected
``case_key``:

.. code-block:: yaml

   variable_1: case_key_2

   choose_variable_1:
           case_key_1:
                   configuration_1: value
                   configuration_2: value
                   [ ... ]
           case_key_2:
                   configuration_1: value
                   configuration_2: value
                   [ ... ]
           "*":
                   configuration_1: value
                   configuration_2: value
                   [ ... ]

The key ``"*"`` or ``*`` works as an `else`.

**Example**

An example that can better illustrate this general description is the `FESOM 2.0` resolution
configuration in ``<PATH>/esm_tools/configs/fesom/fesom-2.0.yaml``:

.. code-block:: yaml

   resolution: CORE2

   choose_resolution:
           CORE2:
                   nx: 126858
                   mesh_dir: "${pool_dir}/meshes/mesh_CORE2_final/"
                   nproc: 288
           GLOB:
                   nx: 830305

Here we are selecting the ``CORE2`` as default configuration set for the ``resolution`` variable,
but we could choose the ``GLOB`` configuration in another `YAML` file (i.e. a runscript), to override
this default choice.

In the case in which ``resolution: CORE2``, then ``nx``, ``mesh_dir`` and ``nproc`` will take the values
defined inside the ``choose_resolution`` for ``CORE2`` (``126858``, 
``runscripts/fesom-recom/fesom-recom-ollie-restart-daily.yaml``, and ``288`` respectively), once
resolved by the `esm_parser`, at the same **nesting level** of the ``choose_resolution``.

.. Note::
   ``choose_versions`` inside configuration files is treated in a special way by the `esm_master`. To
   avoid conflicts in case an additional ``choose_versions`` is needed, include the compilation information
   inside a ``compile_infos`` section (including the ``choose_versions`` switch containning compilation
   information). Outside of this exception, it is possible to use as many ``choose_<variable>`` repetitions
   as needed.

Append to an Existing List (``add_``)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Given an existing list ``list1`` or dictionary:

.. code-block:: yaml

   list1:
       - element1
       - element2

it is possible to add members to this list/dictionary by using the following syntax:

.. code-block:: yaml

   add_list1:
       - element3
       - element4

so that the variable ``list1`` at the end of the parsing will contain
``[element1, element2, element3, element4]``. This is not only useful when you need to
build the list piecewise (i.e. and expansion of a list inside a ``choose_`` switch) but
also as the :ref:`yaml_hierarchy:YAML File Hierarchy` will cause repeated variables to
be overwritten. Adding a nested dictionary in this way merges the ``add_<dictionary>``
content into the ``<dictionary>`` with priority to ``add_<dictionary>`` elements inside
the same file, and following the :ref:`yaml_hierarchy:YAML File Hierarchy` for
different files.

**Properties**

  * It is possible to have multiple ``add_`` for the same variable in the same or even in different
    files. That means that all the elements contained in the multiple ``add_`` will be added to the
    list after the parsing.

**Exceptions**

Exceptions to ``add_`` apply only to the environment and namelist ``_changes`` (see
:ref:`yaml:Environment and Namelist Changes (\`\`_changes\`\`)`). For variables of the type ``_changes``,
an ``add_`` is only needed if the same ``_changes`` block repeats inside the same file. Otherwise, the
``_changes`` block does not overwrite the same ``_changes`` block in other files, but their elements
are combined.

**Example**

In the configuration file for `ECHAM` (``configs/components/echam/echam.yaml``) the list
``input_files`` is declared as:

.. code-block:: yaml

   [ ... ]

   input_files:
       "cldoptprops": "cldoptprops"
       "janspec": "janspec"
       "jansurf": "jansurf"
       "rrtmglw": "rrtmglw"
       "rrtmgsw": "rrtmgsw"
       "tslclim": "tslclim"
       "vgratclim": "vgratclim"
       "vltclim": "vltclim"

   [ ... ]

However different `ECHAM` scenarios require additional input files, for example the ``HIST`` scenario
needs a ``MAC-SP`` element to be added and we use the ``add_`` functionality to do that:

.. code-block:: yaml

   [ ... ]
   choose_scenario:
       [ ... ]
       HIST:
           forcing_files:
               [ ... ]
           add_input_files:
               MAC-SP: MAC-SP
       [ ... ]

An example for the ``_changes`` **exception** can be also found in the same ``ECHAM`` configuration file.
Namelist changes necessary for `ECHAM` are defined inside this file as:

.. code-block:: yaml

   [ ... ]

   namelist_changes:
       namelist.echam:
           runctl:
               out_expname: ${general.expid}
               dt_start:
                   - ${pseudo_start_date!year}
                   - ${pseudo_start_date!month}
                   [ ... ]

This changes specified here will be combined with changes in other files (i.e. ``echam.namelist_changes``
in the coupled setups `AWICM` or `AWIESM` configuration files), not overwritten. However, `ECHAM`'s
version ``6.3.05p2-concurrent_radiation`` needs of further namelist changes written down in the same
file inside a ``choose_`` block and for that we need to use the ``add_`` functionality:

.. code-block:: yaml

   [ ... ]

   choose_version:
       [ ... ]
       6.3.05p2-concurrent_radiation:
           [ ... ]
           add_namelist_changes:
               namelist.echam:
                   runctl:
                       npromar: "${npromar}"
                   parctl:

   [ ... ]


Remove Elements from a List/Dictionary (``remove_``)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It is possible to remove elements inside list or dictionaries by using the
``remove_`` functionality which syntax is:

.. code-block:: yaml

   remove_<dictionary>: [<element_to_remove1>, <element_to_remove2>, ... ]

or:

.. code-block:: yaml

   remove_<dictionary>:
           - <element_to_remove1>
           - <element_to_remove2>
           -  ...

You can also remove specific nested elements of a dictionary separating the `keys` for
the path by ``.``:

.. code-block:: yaml

   remove_<model>.<dictionary>.<subkey1>.<subkey2>: [<element_to_remove1>, <element_to_remove2>, ... ]


Math and Calendar Operations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The following math and calendar operations are supported in `YAML` files:

Arithmetic Operations
---------------------
An element of a `YAML` file can be defined as the result
of the addition, subtraction, multiplication or division of variables with the format:

.. code-block:: yaml

   key: "$(( ${variable_1} operator ${variable_2} operator ... ${variable_n} ))"

The `esm_parser` supports calendar operations through `esm_calendar`. When performing calendar
operations, variables that are not given in date format need to be followed by their ``unit`` for
the resulting variable to be also in date format, i.e.:

.. code-block:: yaml

   runtime: $(( ${end_date} - ${time_step}seconds ))

``time_step`` is a variable that is not given in date format, therefore, it is necessary to use
``seconds`` for ``runtime`` to be in date format. Another example is to subtract one day from
the variable ``end_date``:

.. code-block:: yaml

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

Extraction of Date Components from a Date
-----------------------------------------
It is possible to extract date components from a `date variable`. The syntax for such an operation
is:

.. code-block:: yaml

   "${variable!date_component}"

An example to extract the year from the ``initial_time`` variable:

.. code-block:: yaml

   yearnew: "${initial_date!syear}"

If ``initial_date`` was 2001-01-01T00:00:00, then ``yearnew`` would be 2001.

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

Globbing allows to use ``*`` as a wildcard in filenames for restart, input and output files.
With this feature files can be copied from/to the work directory whose filenames are not
completely known. The syntax needed is:

.. code-block:: yaml

   file_list: common_pathname*common_pathname

Note that this also works together with the :ref:`yaml:List Loops`.

**Example**

The component `NEMO` produces one restart file per processor, and the part of the file name
relative to the processor is not known. In order to handle copying of restart files under
this circumstances, globbing is used in `NEMO`'s configuration file
(``configs/components/nemo/nemo.yaml``):

.. code-block:: yaml

   [ ... ]

   restart_in_sources:
       restart_in: ${expid}_${prevstep_formatted}_restart*_${start_date_m1!syear!smonth!sday}_*.nc
   restart_out_sources:
       restart_out: ${expid}_${newstep_formatted}_restart*_${end_date_m1!syear!smonth!sday}_*.nc

   [ ... ]

This will include inside the ``restart_in_sources`` and ``restart_out_sources`` lists, all the files
sharing the specified common name around the position of the ``*`` symbol, following the same rules
used by the Unix shell.


Environment and Namelist Changes (``_changes``)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The functionality ``_changes`` is used to control environment, namelist and coupling
changes. This functionality can be used from config files, but also runscripts. If
the same type of ``_changes`` is used both in config files and a runscript for a
simulation, the dictionaries are merged following the hierarchy specified in the
:ref:`yaml_hierarchy:YAML File Hierarchy` chapter.

Environment Changes
-------------------

Environment changes are used to make changes to the default environment defined in the
machine files (``esm_tools/configs/machines/<name_of_the_machine>.yaml``). There are
three types of environment changes:

.. csv-table::
   :header: Key, Description
   :widths: 15, 85

   ``environment_changes``,             Changes for both the compilation and the runtime environments.
   ``compiletime_environment_changes``, Changes to the environment applied only during compilation.
   ``runtime_environment_changes``,     Changes to the environment applied only during runtime.

Two types of `yaml` elements can be nested inside an environment changes:
``add_module_actions`` and ``add_export_vars``.

* Use ``add_module_actions`` to include one `module` command or a list of them. The
  shell command ``module`` is already invoked by `ESM-Tools`, therefore you only need to
  list the options (i.e. ``load/unload <module_name>``).

* Use ``add_export_vars`` to export one or a list of environment variables. Shell
  command ``export`` is not needed here, just define the variable as
  ``VAR_NAME: VAR_VALUE`` or as a nested dictionary.

For more information about ``esm_environment`` package, please check
:ref:`esm_environment:ESM Environment`.

**Example**

.. tabs::
   .. tab:: fesom.yaml

      The model `FESOM` needs some environment changes for compiling in `Mistral` and
      `Blogin` HPCs, which are included in `FESOM`'s configuration file
      (``esm_tools/configs/components/fesom/fesom.yaml``):

      .. code-block:: yaml

         [ ... ]

         compiletime_environment_changes:
                 add_export_vars:
                         takenfrom:      fesom1
         choose_computer.name:
                 mistral:
                         add_compiletime_environment_changes:
                                 add_module_actions:
                                         - "unload gcc"
                                         - "load gcc/4.8.2"
                 blogin:
                         add_compiletime_environment_changes:
                                 add_export_vars:
                                         - "NETCDF_DIR=/sw/dataformats/netcdf/intel.18/4.7.3/skl/"
                                         - "LD_LIBRARY_PATH=$NETCDF_DIR/lib/:$LD_LIBRARY_PATH"
                                         - "NETCDF_CXX_INCLUDE_DIRECTORIES=$NETCDF_DIR/include"
                                         - "NETCDF_CXX_LIBRARIES=$NETCDF_DIR/lib"
                                         - "takenfrom='fesom1'"

         runtime_environment_changes:
                 add_export_vars:
                         AWI_FESOM_YAML:
                                 output_schedules:
                                         -
                                                 vars: [restart]
                                                 unit: ${restart_unit}
                                                 first: ${restart_first}
                                                 rate: ${restart_rate}
                                         -
                                                 [ ... ]

      Independently of the computer, ``fesom.yaml`` exports always the ``takenfrom``
      variable for compiling. Because ``compiletime_environment_changes`` is already
      defined for that purpose, any ``compiletime_environment_changes`` in a
      ``choose_`` block needs to have an ``add_`` at the beginning. Here we see that a
      ``choose_`` block is used to select which changes to apply compile environment
      (``add_compiletime_environment_changes``) depending on the HPC system we are in
      (`Mistral` or `Blogin`). For more details on how to use the ``choose_`` and
      ``add_`` functionalities see :ref:`yaml:Switches (\`\`choose_\`\`)` and
      :ref:`yaml:Append to an Existing List (\`\`add_\`\`)`.

      We also see here how ``runtime_environment_changes`` is used to add nested
      information about the output schedules for `FESOM` into an
      ``AWI_FESOM_YAML`` variable that will be exported to the runtime environment.

Changing Namelists
------------------

It is also possible to specify namelist changes to a particular section of a namelist:


.. code-block:: yaml

    echam:
            namelist_changes:
                    namelist.echam:
                            runctl:
                                    l_orbvsop87: false
                            radctl:
                                    co2vmr: 217e-6
                                    ch4vmr: 540e-9
                                    n2ovmr: 245e-9
                                    cecc: 0.017
                                    cobld: 23.8
                                    clonp: -0.008
                                    yr_perp: "remove_from_namelist"

In the example above, the `namelist.echam` file is changed in two specific chapters, first the section ``runctrl`` parameter ``l_orbsvop87`` is set to ``false``, and appropriate gas values and orbital values are set in ``radctl``. Note that the special entry ``"remove_from_namelist`` is used to delete entries. This would translate the following fortran namelist (trucated):

.. code-block:: fortran

        &runctl
            l_orbvsop87 = .false.
        /

        &radctl
            co2vmr = 0.000217
            ch4vmr = 5.4e-07
            n2ovmr = 2.45e-07
            cecc = 0.017
            cobld = 23.8
            clonp = -0.008
        /

Note that, although we set ``l_orbsvop87`` to be ``false``, it is translated to the
namelist as a fortran boolean (``.false.``). This occurs because `ESM-Tools`
"understands" that it is writing a fortan namelist and transforms the `yaml` booleans
into fortran.

For more examples, check the recipe in the cookbook
(:ref:`cookbook:Changing Namelist Entries from the Runscript`).

Coupling changes
----------------

Coupling changes (``coupling_changes``) are typically invoked in the coupling files
(``esm_tools/configs/couplings/``), executed before compilation of coupled setups,
and consist of a list of shell commands to modify the configuration and make files of
the components for their correct compilation for coupling.

For example, in the ``fesom-1.4+echam-6.3.04p1.yaml`` used in `AWICM-1.0`,
``coupling_changes`` lists two ``sed`` commands to apply the necessary changes to the
``CMakeLists.txt`` files for both `FESOM` and `ECHAM`:

.. code-block:: yaml

   components:
   - echam-6.3.04p1
   - fesom-1.4
   - oasis3mct-2.8
   coupling_changes:
   - sed -i '/FESOM_COUPLED/s/OFF/ON/g' fesom-1.4/CMakeLists.txt
   - sed -i '/ECHAM6_COUPLED/s/OFF/ON/g' echam-6.3.04p1/CMakeLists.txt


List Loops
~~~~~~~~~~

This functionality allows for basic looping through a `YAML list`. The syntax for this is:

.. code-block:: yaml

   "[[list_to_loop_through-->ELEMENT_OF_THE_LIST]]"

where ``ELEMENT_OF_THE_LIST`` can be used in the same line as a variable. This is
particularly useful to handle files which names contain common strings (i.e. `outdata` and
`restart` files, see :ref:`yaml:File Dictionaries`).

The following example uses the list loop functionality inside the ``fesom-2.0.yaml``
configuration file to specify which files need to be copied from the `work` directory
of runs into the general experiment `outdata` directory. The files to be copied for runs
modeling a couple of months in year 2001 are ``a_ice.fesom.2001.nc``, ``alpha.fesom.2001.nc``,
``atmice_x.fesom.2001.nc``, etc. The string ``.fesom.2001.nc`` is present in all files so we
can use the list loop functionality together with calendar operations (:ref:`yaml:Math and Calendar
Operations`) to have a cleaner and more generalized configure file. First, you need to declare the
list of unshared names:

.. code-block:: yaml

   outputs: [a_ice,alpha,atmice_x, ... ]

Then, you need to declare the ``outdata_sources`` dictionary:

.. code-block:: yaml

   outdata_sources:
         "[[outputs-->OUTPUT]]": OUTPUT.fesom.${start_date!syear}.nc

Here, ``"[[outputs-->OUTPUT]]":`` provides the `keys` for this dictionary as ``a_ice``, ``alpha``,
``atmice_x``, etc., and ``OUTPUT`` is later used in the `value` to construct the complete file name
(``a_ice.fesom.2001.nc``, ``alpha.fesom.2001.nc``, ``atmice_x.fesom.2001.nc``, etc.).

Finally, ``outdata_targets`` dictionary can be defined to give different names to `outdata` files
from different runs using `calendar operations`:

.. code-block:: yaml

   outdata_targets:
         "[[outputs-->OUTPUT]]": OUTPUT.fesom.${start_date!syear!smonth}.${start_date!sday}.nc

The values for the `keys` ``a_ice``, ``alpha``, ``atmice_x``, ..., will be
``a_ice.fesom.200101.01.nc``, ``alpha.fesom.200101.01.nc``, ``atmice_x.fesom.200101.01.nc``, ...,
for a January run, and ``a_ice.fesom.200102.01.nc``, ``alpha.fesom.200102.01.nc``,
``atmice_x.fesom.200102.01.nc``, ..., for a February run.

File Dictionaries
~~~~~~~~~~~~~~~~~

File dictionaries are a special type of `YAML` elements that are useful to handle input, output,
forcing, logging, binary and restart files among others (see :ref:`yaml:File Dictionary Types` table),
and that are normally defined inside the `configuration files` of models. File dictionary's `keys`
are composed by a file dictionary ``type`` followed by ``_`` and an ``option``, and the `elements`
consist of a list of ``file_tags`` as `keys` with their respective ``file_paths`` as `values`:

.. code-block:: yaml

   type_option:
         file_tag1: file_path1
         file_tag2: file_path2

The ``file_tags`` need to be consistent throughout the different ``options`` for files to be
correctly handled by ESM-Tools. Exceptionally, ``sources`` files can be tagged differently but
then the option ``files`` is required to link sources tags to general tags used by the other
options (see :ref:`yaml:File Dictionary Options` table below).

File Dictionary Types
---------------------

.. csv-table::
   :header: Key, Description
   :widths: 15, 85

   analysis,            User's files for their own analysis tools (i.e. to be used in the pre-/postprocessing).
   bin,                 Binary files.
   config,              Configure sources.
   couple,              Coupling files.
   ignore,              Files to be ignored in the copying process.
   forcing,             Forcing files. An example is described at the end of this section.
   log,                 Log files.
   mon,                 Monitoring files.
   outdata,             "Output configuration files. A concise example is described in :ref:`yaml:List Loops`."
   restart_in,          "Restart files to be copied from the **experiment directory** into the **run directory** (see :ref:`esm_runscripts:Experiment Directory Structure`), during the beginning of the `computing phase` (e.g. to copy restart files from the previous step into the new run folder)."
   restart_out,         "Restart files to be copied from the **run directory** into the **experiment directory** (see :ref:`esm_runscripts:Experiment Directory Structure`), during the `tidy and resubmit phase` (e.g. to copy the output restart files from a finished run into the **experiment directory** for later use the next run)."
   viz,                 Files for the visualization tool.

File Dictionary Options
-----------------------

.. csv-table::
   :header: Key, Description
   :widths: 15, 85

   sources,             "Source file paths or source file names to be copied to the target path. **Without this option no files will be handled by ESM-Tools**. If ``targets`` option is not defined, the files are copied into the default `target` directory with the same name as in the `source` directory. In that case, if two files have the same name they are both renamed to end in the dates corresponding to their run (``file_name.extension_YYYYMMDD_YYYYMMDD``)."
   files,               "Links the general file tags (`key`) to the `source` elements defined in ``sources``. ``files`` **is optional**. If not present, all `source` files are copied to the `target` directory, and the `source tags` need to be the same as the ones in ``in_work`` and ``targets``. If present, only the `source` files included in ``files`` will be copied (see the `ECHAM` forcing files example below)."
   in_work,             "Files inside the `work` directory of a run (``<base_dir>/<experiment_name>/run_date1_date2/work``) to be transferred to the `target` directory. This files copy to the `target` path even if they are not included inside the ``files`` option. ``in_work`` **is optional**."
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

The `ECHAM` configuration file (``<PATH>/configs/echam/echam.yaml``) allows for choosing different
scenarios for a run. These scenarios depend on different combinations of forcing files. File sources
for all cases are first stored in ``echam.datasets.yaml`` (a ``further_reading`` file) as:

.. code-block:: yaml

   forcing_sources:
         # sst
         "amipsst":
                 "${forcing_dir}/amip/${resolution}_amipsst_@YEAR@.nc":
                         from: 1870
                         to: 2016
         "pisst": "${forcing_dir}/${resolution}${ocean_resolution}_piControl-LR_sst_1880-2379.nc"

         # sic
         "amipsic":
                 "${forcing_dir}/amip/${resolution}_amipsic_@YEAR@.nc":
                         from: 1870
                         to: 2016
         "pisic": "${forcing_dir}/${resolution}${ocean_resolution}_piControl-LR_sic_1880-2379.nc"

         [ ... ]

Here ``forcing_sources`` store **all the sources** necessary for all `ECHAM` scenarios, and tag
them with source `keys` (``amipsst``, ``pisst``, ...). Then, it is possible to choose among
these source files inside the scenarios defined in ``echam.yaml`` using ``forcing_files``:

.. code-block:: yaml

   choose_scenario:
         "PI-CTRL":
                 forcing_files:
                         sst: pisst
                         sic: pisic
                         aerocoarse: piaerocoarse
                         aerofin: piaerofin
                         aerofarir: piaerofarir
                         ozone: piozone
         PALEO:
                 forcing_files:
                         aerocoarse: piaerocoarse
                         aerofin: piaerofin
                         aerofarir: piaerofarir
                         ozone: piozone
         [ ... ]

This means that for a scenario ``PI-CTRL`` the files that are handled by ESM-Tools will be
**exclusively** the ones specified inside ``forcing_files``, defined in the
``forcing_sources`` as ``pisst``, ``pisic``, ``piaerocoarse``, ``piaerofin``, ``piaerofarir``
and ``piozone``, and they are tagged with new general `keys` (``sst``, ``sic``, ...) that
are common to all scenarios. The source files not included in ``forcing_files`` won't be
used.

File movements
--------------

Inside the file dictionaries realm, it is possible to specify the type of movement you
want to carry out (among ``copy``, ``link`` and ``move``), for a specific file or file
type, and for a given direction. By default all files are ``copied`` in all directions.

The syntax for defining a file movement for a given file type is:

.. code-block:: yaml

   <model>:
       file_movements:
           <file_type>:
               <direction1>: <copy/link/move>
               <direction2>: <copy/link/move>
               [ ... ]

where the ``file_type`` in one among the :ref:`yaml:File Dictionary Types`, and the
``direction`` one of the following ones:

=================== ===================================================================
Movement file directions
=======================================================================================
``init_to_exp``     Initial files to the corresponding general folder
------------------- -------------------------------------------------------------------
``exp_to_run``      From general to the corresponding run folder
------------------- -------------------------------------------------------------------
``run_to_work``     From run to the work folder on that run
------------------- -------------------------------------------------------------------
``work_to_run``     From the work folder to the corresponding run folder
------------------- -------------------------------------------------------------------
``all_directions``  Directions not specifically defined, use this movement
=================== ===================================================================

It is also possible to do the same for specific files instead of for all files inside
a ``file_type``. The syntax logic is the same:

.. code-block:: yaml

   <model>:
       file_movements:
           <file_key>:
               <direction1>: <copy/link/move>
               <direction2>: <copy/link/move>
               [ ... ]

where ``file_key`` is the key you used to identify your file inside the
``<file_type>_files``, having to add to it ``_in`` or ``_out`` if the file is a
restart, in order to specify in which direction to apply this.

Movements specific to files are still compatible with the ``file_type`` option, and
only the moves specifically defined for files in the ``file_movements`` will differ
from those defined using the ``file_type``.

Accessing Variables from the Previous Run (``prev_run``)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It is possible to use the ``prev_run`` dictionary, in order to access values of
variables from the previous run, in the current run. The idea behind this functionality
is that variables from the previous run can be called from the yaml files with a very
similar syntax to the one that would be used for the current run.

The syntax for that is as follows:

.. code-block:: yaml

   <your_var>: ${prev_run.<path>.<to>.<the>.<var>.<in>.<the>.<previous>.<run>}

For example, let's assume we want to access the `time_step` from the previous run
of a `FESOM` simulation and store it in a variable called `prev_time_step`:

.. code-block:: yaml

   prev_time_step: ${prev_run.fesom.time_step}

.. Note:: Only the single previous simulation loaded

.. Warning:: Use this feature only when there is no other way of accessing the
   information needed. Note that, for example, dates of the previous run are
   already available in the current run, under variables such as
   ``last_start_date``, ``parent_start_date``, etc.

Branchoff experiments with ``prev_run``
---------------------------------------

If you use ``prev_run`` variables in your model configuration files, ``esm_runscripts``
will require that you define a ``prev_run_config_file`` variable in your runscript
**when you try to run a branchoff experiment**. As a branchoff is a way of restarting,
``esm_runscripts`` needs to know which file should use to load the ``prev_run``
information, but (contrary to the regular restarts within the same experiment) finding
that file name is a non-trivial task: being a different experiment, the datestamps and
restart frequency can differ from the parent experiment to the branchoff experiment. To
overcome this problem the user needs to specify the **full path** to the
``finished_config.yaml`` to be used on the first run of the branchoff experiment:

.. code-block:: yaml

    prev_run_config_file: "/<basedir>/<expid>/config/<expid>_finished_config.yaml_<DATE>-<DATE>"

Error-handling and warning syntax
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This syntax allows for error-handling and raising of warnings from the configuration
files (i.e. `yaml` files in ``esm_tools/configs``). For including an error or a warning
under a given condition (e.g. ``choose_`` block for a given selection) use the key
words ``error`` or ``warning`` respectively (if more than one error/warning is present
in the section of your file, use ``add_error/warning`` to combine them).

The syntax in the yaml files for triggering warnings or errors is as follows:

.. code-block:: yaml

   warning/error:
       <name>: # Name for the specific warning or error
           message: "the message of the warning/error"
           esm_tools_version: ">/</=/!=/version_number" # trigger it under certain ESM-Tools version conditions
           ask_user_to_continue: True/False # Ask user about continuing or stopping the process, only for warnings, errors always kill the process

* ``<name>``: what is displayed on the title of the error/warning
* ``message``: the detailed message of the error/warning. You can use `ESM-Tools`
  variables here (``${<variable>}``)
* ``esm_tools_version``: only trigger this error/warning under given `ESM-Tools`
  versions
* ``ask_user_to_continue``: if true, it asks the user whether they want to continue,
  after displaying the warning. Only works for warnings as errors halt the simulation
  without asking

**Example**

.. code-block:: yaml

   recom:
       choose_scenario:
           HIST:
               [ ... ]
           PI-CTRL:
               [ ... ]
           "*":
               add_warning:
                   "wrong scenario type":
                       message: "The scenario you specified (``${recom.scenario}``) is not supported!"
                       ask_user_to_continue: True

If you then define ``recom.scenario: hist`` instead of ``HIST`` then you'll get the
following:

.. code-block::

   wrong scenario type WARNING
   ---------------------------
   Section: recom

   Wrong scenario, scenario hist does not exist

   ? Do you want to continue (set general.ignore_config_warnings: False to avoid quesitoning)?
