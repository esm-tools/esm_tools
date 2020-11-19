Include a New Forcing/Input File
================================

.. use = for sections, ~ for subsections and - for subsubsections

**Feature available since version:** 4.2

There are several ways of including a new forcing or input file into your experiment
depending on the degree of control you'd like to achieve. An important clarification is
that ``<forcing/input>_sources`` file dictionary specifies the **sources** (paths to
the files in the pools or personal folders, that need to be copied or linked into the
experiment folder). On the other hand ``<forcing/input>_files`` specifies which of these
sources are to be **included in the experiment**. This allows us to have many sources
already available to the user, and then the user can simply choose which of them to use
by chosing from ``<forcing/input>_files``. ``<forcing/input>_in_work`` is used to copy
the files into the work folder (``<base_dir>/<exp_id>/run_<DATE>/work``) if necessary
and change their name. For more technical details see :ref:`yaml:File Dictionaries`.

The next sections illustrate some of the many options to handle forcing and input
files.

Source Path Already Defined in a Config File
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

1. Make sure the source of the file is already specified inside the ``forcing_sources``
   or ``input_sources`` `file dictionaries` in the configuration file of the setup or
   model you are running, or on the ``further_reading`` files.

2. In your runscript, include the `key` of the source file you want to include inside
   the ``forcing_files`` or ``input_files`` section.

.. note::
   Note that the `key` containing the source in the ``forcing_sources`` or
   ``input_sources`` can be different than the key specified in ``forcing_files`` or
   ``input_files``.

Example
-------

.. tabs::
   .. tab:: ECHAM

      In ECHAM, the source and input file paths are specified in a separate file
      (``<PATH>/esm_tools/configs/components/echam/echam.datasets.yaml``) that
      is reached through the ``further_reading`` section of the ``echam.yaml``. This
      file includes a large number of different sources for input and forcing contained
      in the pool directories of the HPC systems Ollie and Mistral. Let's have a look
      at the ``sst`` forcing file options available in this file:

      .. code-block:: yaml

         forcing_sources:
                 # sst
                 "amipsst":
                         "${forcing_dir}/amip/${resolution}_amipsst_@YEAR@.nc":
                                 from: 1870
                                 to: 2016
                 "pisst": "${forcing_dir}/${resolution}${ocean_resolution}_piControl-LR_sst_1880-2379.ncy"

      This means that from our runscript we will be able to select either ``amipsst``
      or ``pisst`` as `sst` forcing files. If you define ``scenario`` in `ECHAM` be
      ``PI-CTRL`` the correct file source (``pisst``) is already selected for you.
      However, if you would like to select this file manually you can just simply add
      the following to your runscript:

      .. code-block:: yaml

         forcing_files:
                 sst: pisst

Modify the Source of a File
~~~~~~~~~~~~~~~~~~~~~~~~~~~

To change the path of the source for a given forcing or input file from your runscript:

1. Include the source path under a `key` inside ``forcing_sources`` or
   ``input_sources`` in your runscript:

   .. code-block:: yaml

      <forcing/input>_sources:
              <key_for_your_file>: <path_to_your_file>

   If the source is not a single file, but there is a file per year use the ``@YEAR@``
   and ``from:`` ``to:`` functionality in the path to copy only the files corresponding
   to that run's year:

   .. code-block:: yaml

      <forcing/input>_sources:
              <key_for_your_source>: <firt_part_of_the_path>@YEAR@<second_part_of_the_path>
                      from: <first_year>
                      to: <last_year>

2. Make sure the `key` for your path is defined in one of the config files that you are
   using, inside of either ``forcing_files`` or ``input_files``. If it is not defined
   anywhere you will have to include it in your runscript:

   .. code-block:: yaml

      <forcing/input>_files:
              <key_for_your_file>: <key_for_your_source>

Copy the file in the work folder and/or rename it
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To copy the files from the forcing/input folders into the work folder
(``<base_dir>/<exp_id>/run_<DATE>/work``) or rename them:

1. Make sure your file and its source is defined somewhere (either in the config files
   or in your runscript) in ``<forcing/input>_sources`` and ``<forcing/input>_files``
   (see subsections :ref:`cookbook:Source Path Already Defined in a Config File`
   and :ref:`cookbook:Modify the Source of a File`).

2. In your runscript, add the `key` to the file you want to **copy** with `value` the
   same as the `key`, inside <forcing/input>_in_work:

   .. code-block:: yaml

      <forcing/input>_in_work:
              <key_for_your_file>: <key_for_your_file>

3. If you want to **rename** the file set the `value` to the desired name:

   .. code-block:: yaml

      <forcing/input>_in_work:
              <key_for_your_file>: <key_for_your_file>

Example
-------

.. tabs::
   .. tab:: ECHAM

      In `ECHAM` the ``sst`` forcing file depends in the scenario defined by the user:

      **esm_tools/config/component/echam/echam.datasets.yaml**

      .. code-block:: yaml

         forcing_sources:
                 # sst
                 "amipsst":
                         "${forcing_dir}/amip/${resolution}_amipsst_@YEAR@.nc":
                                 from: 1870
                                 to: 2016
                 "pisst": "${forcing_dir}/${resolution}${ocean_resolution}_piControl-LR_sst_1880-2379.nc"

      **esm_tools/config/component/echam/echam.yaml**

      .. code-block:: yaml

         choose_scenario:
                 "PI-CTRL":
                         forcing_files:
                                 sst: pisst
                                 [ ... ]

      If ``scenario: "PI-CTRL"`` then the source selected will be
      ``${forcing_dir}/${resolution}${ocean_resolution}_piControl-LR_sst_1880-2379.nc``
      and the name of the file copied to the experiment forcing folder will be
      ``${resolution}${ocean_resolution}_piControl-LR_sst_1880-2379.nc``. However,
      `ECHAM` needs this file in the same folder as the binary (the ``work`` folder)
      under the name ``unit.20``. To copy and rename this file into the ``work`` folder
      the following lines are used in the ``echam.yaml`` configuration file:

      .. code-block:: yaml

         forcing_in_work:
                 sst: "unit.20"

      You can use the same syntax **inside your runscript** to copy into the ``work``
      folder any forcing or input file, and rename it.


See also
~~~~~~~~

.. links to relevant parts of the documentation

- :ref:`yaml:What Is YAML?`
- :ref:`yaml:File Dictionaries`
