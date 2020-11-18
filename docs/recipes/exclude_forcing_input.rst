Exclude a Forcing/Input File
============================

.. use = for sections, ~ for subsections and - for subsubsections

**Feature available since version:** 4.2

To exclude one of the predefined forcing or input files from being copied to your
experiment folder:

1. Find the `key` of the file to be excluded inside the config file,
   ``<forcing/input>_files`` `file dictionary`.

2. In your runscript, use the ``remove_`` functionality to exclude this `key` from the
   ``<forcing/input>_files`` `file dictionary`:

   .. code-block:: yaml

              remove_<input/forcing>_files:
                      - <key_of_the_file1>
                      - <key_of_the_file2>
                      - ...

Example
~~~~~~~

.. tabs::
   .. tab:: ECHAM

      To exclude the ``sst`` forcing file from been copied to the experiment folder
      include the following lines in your runscript:

      .. code-block:: yaml

         remove_forcing_files:
                 - sst

See also
~~~~~~~~

.. links to relevant parts of the documentation

- :ref:`yaml:What Is YAML?`
- :ref:`yaml:Remove Elements from a List/Dictionary (\`\`remove_\`\`)`
- :ref:`yaml:File Dictionaries`
