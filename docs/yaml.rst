.. highlight:: shell

================
YAML File Syntax
================

Model configurations and runscripts can be written as ``YAML`` files. This files (and also
the ``BASH`` runscripts) are read by the `esm_parser` and then converted into a Python
dictionary.

Configuration File
==================

The following keys are supported inside configuration files for models and coupled setups
(``<PATH>/esm_tools/configs/<model_or_setup>``).

.. csv-table::
   :header: Key, Description
   :widths: 20, 80

   model,       Name of the model.
   repository,  Address of the model's repository.

Math Operations
===============

Some math operations are supported in `yaml` files:

* **Addition and subtraction of dates**: a key of a `yaml` file can be define as the result
  of an addition or subtraction of times and dates with the format ``key:
  $(( ${time1}unit1 operator ${time2}unit2 operator ... ))``, where the unit specify
  the time unit for every time in the operation.
  The units available are `seconds`, ... . Example for a ``runtime`` definition::

        runtime: $(( ${end_date} - ${time_step}seconds ))


