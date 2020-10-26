Using your own namelist
=======================

.. use = for sections, ~ for subsections and - for subsubsections

**Feature available since version:** 4.2

.. warning:: This feature is only recommended if the number of changes that need to be applied to the default
   namelist is very large, otherwise we recommend to use the feature ``namelis_changes`` (see
   :ref:`cookbook:Changing Namelist Entries from the Runscript`). You can check the default namelists `here
   <https://github.com/esm-tools/esm_tools/tree/release/namelists>`_.


In your runscript, you can instruct `ESM-Tools` to substitute a given default namelist by `ESM-Tools` by a
namelist of your choice.

1. Search for the ``config_sources`` variable inside the configuration file of the model you are trying to run,
   and then, identify the "key" containing the path to the namelist to modify.

2. In your runscript, indented in the corresponding model section, add an ``add_config_sources`` section,
   containing a variable which "key" is the one of step 1, and the value is the path of the new namelist.

3. Bare in mind, that namelists are first loaded by `ESM-Tools`, and then modified by the default
   ``namelist_changes`` in the configuration files. If you want to ignore all those changes for the your new
   namelist you'll need to add ``remove_namelist_changes: [<name_of_your_namelist>]``.

In dot notation both steps will look like:
``<model_name>.<add_config_sources>.<key_of_the_namelist>: <path_of_your_namelist>``
``<model_name>.<remove_namelis_changes>: [<name_of_your_namelist>]``


Example
~~~~~~~



.. yaml blocks can be written in yaml format by including them in a code block:
.. code-block:: yaml

    dictionary:
        variable1: 1
        variable2: false

Model/setup specific instructions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. subsection including particular things in the recipe for specific models and coupled setups
   Note: numbering of the general recipe steps can be handy to reference the steps to modify
 
Component 1
-----------

See also
~~~~~~~~

.. links to relevant parts of the documentation

:ref:`yaml:What Is YAML?`
