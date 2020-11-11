.. _target to branchoff old restart:

How to branch-off FESOM from old spinup restart files
=======================================================

.. use = for sections, ~ for subsections and - for subsubsections

.. **Feature available since version:** <version_num>

When you branch-off from very old FESOM ocean restart files, you may encounter the following runtime error:

.. code-block::

    read ocean restart file
    Error:
    NetCDF: Invalid dimension ID or name


This is because the naming of the NetCDF time dimension variable in the restart file has changed from ``T`` to ``time`` during the development of `FESOM` and the different `FESOM` versions.
Therefore, recent versions of `FESOM` expect the name of the time dimension to be ``time``.

In order to branch-off experiments from spinup restart files that use the old name for the time dimension, you need to rename this dimension before starting the branch-off experiment. 

.. warning:: The following work around will change the restart file permanently. Make sure you do not apply this to the original file.


To rename a dimension variable of a NetCDF file, you can use ``ncrename``:

.. code-block:: bash

    ncrename -d T,time <copy_of_restart_spinup_file>.nc

where ``T`` is the old dimension and ``time`` is the new dimension.

See also
~~~~~~~~

.. links to relevant parts of the documentation

- :ref:`cookbook:How to run a branch-off experiment`
