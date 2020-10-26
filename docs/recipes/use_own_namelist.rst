Using your own namelist
=======================

.. use = for sections, ~ for subsections and - for subsubsections

**Feature available since version:** 4.2

.. warning:: This feature is only recommended if the number of changes that need to be applied to the default
   namelist is very large, otherwise we recommend to use the feature ``namelist_changes`` (see
   :ref:`cookbook:Changing Namelist Entries from the Runscript`). You can check the default namelists `here
   <https://github.com/esm-tools/esm_tools/tree/release/namelists>`_.


In your runscript, you can instruct `ESM-Tools` to substitute a given default namelist by `ESM-Tools` by a
namelist of your choice.

1. Search for the ``config_sources`` variable inside the configuration file of the model you are trying to run,
   and then, identify the "key" containing the path to the namelist to modify.

2. In your runscript, indented in the corresponding model section, add an ``add_config_sources`` section,
   containing a variable whose "key" is the one of step 1, and the value is the path of the new namelist.

3. Bare in mind, that namelists are first loaded by `ESM-Tools`, and then modified by the default
   ``namelist_changes`` in the configuration files. If you want to ignore all those changes for the your new
   namelist you'll need to add ``remove_namelist_changes: [<name_of_your_namelist>]``.

In dot notation both steps will look like:
``<model_name>.<add_config_sources>.<key_of_the_namelist>: <path_of_your_namelist>``
``<model_name>.<remove_namelis_changes>: [<name_of_your_namelist>]``

.. warning:: Use step 3 at your own risk! Many of the model specific information and functionality is
   transferred to the model through ``namelist_changes``, and therefore, we discourage you from using this
   unless you have a very deep understanding of the configuration file and the model. Following
   :ref:`cookbook:Changing Namelist Entries from the Runscript` would be a safest solution.


Example
~~~~~~~

In this example we show how to use an `ECHAM` ``namelist.echam`` and a `FESOM` ``namelist.ice`` that are not
the default ones and omit the ``namelist_changes`` present in ``echam.yaml`` and ``fesom.yaml``  configuration
files.

.. tabs::
   .. tab:: ECHAM

      Following step 1, search for the ``config_sources`` dictionary inside the ``echam.yaml``:

      .. code-block:: yaml
         # Configuration Files:
         config_sources:
                 "namelist.echam": "${namelist_dir}/namelist.echam"

      In this case the "key" is ``"namelist.echam"`` and the "value" is ``"${namelist_dir}/namelist.echam"``.
      Let's assume your namelist is in the directory ``/home/ollie/<usr>/my_namelists``. Following step 2,
      you will need to include the following in your runscript:

      .. code-block:: yaml
         echam:
                 add_config_sources:
                         "namelist.echam": /home/ollie/<usr>/my_namelists/namelist.echam

      If you want to omit the ``namelist_changes`` in ``echam.yaml`` or any other configuration file
      that your model/couple setup is using, you'll need to add to your runscript
      ``remove_namelist_changes: [namelist.echam]`` (step 3)::

      .. code-block:: yaml
         echam:
                 add_config_sources:
                         "namelist.echam": /home/ollie/<usr>/my_namelists/namelist.echam

                 remove_namelist_changes: [namelist.echam]

      .. warning:: Many of the model specific information and functionality is transferred to the model
         through ``namelist_changes``, and therefore, we discourage you from using this unless you
         have a very deep understanding of the ``echam.yaml`` file and the ECHAM model. For example,
         using ``remove_namelist_changes: [namelist.echam]`` will destroy the following lines in the
         ``echam.yaml``:

         .. code_block:: yaml
            choose_lresume:
                    False:
                            restart_in_modifications:
                                    "[[streams-->STREAM]]":
                                        - "vdate <--set_global_attr-- ${start_date!syear!smonth!sday}"
                                          # - fdate "<--set_dim--" ${year_before_date}
                                          # - ndate "<--set_dim--" ${steps_in_year_before}
                    True:
                            # pseudo_start_date: $(( ${start_date} - ${time_step} ))
                            add_namelist_changes:
                                    namelist.echam:
                                            runctl:
                                                    dt_start: "remove_from_namelist"

         This lines are relevant for correctly performing restarts, so if
         ``remove_namelist_changes`` is used, make sure to have the approrpiate commands on your
         runscript to remove ``dt_start`` from your namelist in case of a restart.


   .. tab:: FESOM

      Following step 1, search for the ``config_sources`` dictionary inside the ``fesom.yaml``:

      .. code-block:: yaml
         config_sources:
                 config:  "${namelist_dir}/namelist.config"
                 forcing: "${namelist_dir}/namelist.forcing"
                 ice:     "${namelist_dir}/namelist.ice"
                 oce:     "${namelist_dir}/namelist.oce"
                 diag:    "${namelist_dir}/namelist.diag"

      In this case the "key" is ``ice`` and the "value" is ``${namelist_dir}/namelist.ice``.
      Let's assume your namelist is in the directory ``/home/ollie/<usr>/my_namelists``. Following step 2,
      you will need to include the following in your runscript:

      .. code-block:: yaml
         fesom:
                 add_config_sources:
                         ice: "/home/ollie/<usr>/my_namelists/namelist.ice"

      If you want to omit the ``namelist_changes`` in ``fesom.yaml`` or any other configuration file
      that your model/couple setup is using, you'll need to add to your runscript
      ``remove_namelist_changes: [namelist.ice]`` (step 3)::

      .. code-block:: yaml
         fesom:
                 add_config_sources:
                         ice: "/home/ollie/<usr>/my_namelists/namelist.ice"

                 remove_namelist_changes: [namelist.ice]

      .. warning:: Many of the model specific information and functionality is transferred to the model
         through ``namelist_changes``, and therefore, we discourage you from using this unless you
         have a very deep understanding of the ``fesom.yaml`` file and the FESOM model.


See also
~~~~~~~~

.. links to relevant parts of the documentation

:ref:`yaml:What Is YAML?`
