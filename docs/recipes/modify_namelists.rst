Changing Namelist Entries from the Runscript
============================================

.. use = for sections, ~ for subsections and - for subsubsections

**Feature available since version:** 4.2


You can modify namelists directly from your user yaml runscript configuration.

1. Identify which namelist you want to modify and ensure that it is in the correct section.
   For example, you can only modify ``ECHAM`` specific namelists from an ``ECHAM`` block.

2. Find the subsection ("chapter") of the namelist you want to edit.

3. Find the setting ("key") you want to edit

4. Add a ``namelist_changes`` block to your configuration, specify next the
   namelist filename you want to modify, then the chapter, then the key, and
   finally the desired value.

In dot notation, this will look like:
``<model_name>.namelist_changes.<namelist_name>.<chapter_name>.<key_name> = <value>``


Example
~~~~~~~

Here are examples for just the relevant YAML change, and for a full runscript using this feature.

.. yaml blocks can be written in yaml format by including them in a code block:
.. tabs::

   .. tab:: Snippet

      In this example, we modify the ``co2vmr`` of the ``radctl`` section of
      ``namelist.echam``.

      .. code-block:: yaml

          echam:
              namelist_changes:
                  namelist.echam:
                      radctl:
                          co2vmr: 1200e-6

   .. tab:: Full Runscript

      In this example, we set up AWI-ESM 2.1 for a 4xCO2 simulation. You can
      see how multiple namelist changes are applied in one block.

      .. code-block:: yaml

          general:
              setup_name: "awiesm"
              compute_time: "02:30:00"
              initial_date: "2000-01-01"
              final_date: "2002-12-31"
              base_dir: "/work/ab0246/a270077/For_Christian/experiments/"
              nmonth: 0
              nyear: 1
              account: "ab0246"

          echam:
              restart_unit: "years"
              nprocar: 0
              nprocbr: 0
              namelist_changes:
                      namelist.echam:
                              radctl:
                                      co2vmr: 1137.e-6
                              parctl:
                                      nprocar: 0
                                      nprocbr: 0
                              runctl:
                                      default_output: True

          awiesm:
              version: "2.1"
              postprocessing: true
              scenario: "PALEO"
              model_dir: "/work/ab0246/a270077/For_Christian/model_codes/awiesm-2.1/"

          fesom:
              version: "2.0"
              res: "CORE2"
              pool_dir: "/pool/data/AWICM/FESOM2"
              mesh_dir: "/work/ba1066/a270061/mesh_CORE2_finaltopo_mean/"
              restart_rate: 1
              restart_unit: "y"
              restart_first: 1
              lresume: 0
              namelist_changes:
                  namelist.config:
                      paths:
                          ClimateDataPath: "/work/ba0989/a270077/AWIESM_2_1_LR_concurrent_rad/nonstandard_input_files/fesom/hydrography/"

          jsbach:
              input_sources:
                  jsbach_1850: "/work/ba1066/a270061/mesh_CORE2_finaltopo_mean/tarfilesT63/input/jsbach/jsbach_T63CORE2_11tiles_5layers_1850.nc"


Practical Usage
~~~~~~~~~~~~~~~

It is generally a good idea to run your simulation once in **check** mode
before actually submitting and examining the resulting namelists::

    $ esm_runscripts <your_config.yaml> -e <expid> -c


The namelists are printed in their final form as part of the log during the job
submission and can be seen on disk in the ``work`` folder of your first
``run_XZY`` folder.

Note that you can have several chapters for one namelist or several namelists
included in one ``namelist_changes`` block, but you can only have one
``namelist_changes`` block per model or component (see
:ref:`yaml:Changing Namelists`).


Unusual Namelists
~~~~~~~~~~~~~~~~~

Some times, you have strange namelists of the form:

.. code-block:: fortran

   sn_tracer(1)   = 'DET'   , 'Detritus                   '  , 'mmole-N/m3' ,  .false.
   sn_tracer(2)   = 'ZOO'   , 'Zooplankton concentration  '  , 'mmole-N/m3' ,  .false.
   sn_tracer(3)   = 'PHY'   , 'Phytoplankton concentration'  , 'mmole-N/m3' ,  .false.

To correctly insert this via ``esm-tools``, you can use:

.. code-block:: yaml

    namelist_changes:
          namelist_top_cfg:
            namtrc:
               sn_tracer: "remove_from_namelist"
               sn_tracer(1)%clsname: DET
               sn_tracer(2)%clsname: ZOO
               sn_tracer(3)%clsname: PHY
               sn_tracer(1)%cllname: "Detritus"
               sn_tracer(2)%cllname: "Zooplankton concentration"
               sn_tracer(3)%cllname: "Phytoplankton concentration"
               sn_tracer(1:3)%clunit: "mmole-N/m3"

See also
~~~~~~~~

.. links to relevant parts of the documentation
- `Default namelists on GitHub <https://github.com/esm-tools/esm_tools/tree/release/namelists>`_
- :ref:`yaml:Changing Namelists`
- :ref:`yaml:What Is YAML?`
