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
``<model_name>.namelist_changes.<namelist_name>.<chapter_name><key_name> = <value>``


Example
~~~~~~~

In this example, we modify the ``co2vmr`` of the ``radctl`` section of ``namelist.echam``.

.. yaml blocks can be written in yaml format by including them in a code block:
.. code-block:: yaml

    echam:
        namelist_changes:
            namelist.echam:
                radctl:
                    co2vmr: 1200e-6



Practical Usage
~~~~~~~~~~~~~~~

It is generally a good idea to run your simulation once in **check** mode
before actually submitting and examining the resulting namelists::

    $ esm_runscripts <your_config.yaml> -e <expid> -c


The namelists are printed in their final form as part of the log during the job
submission and can be seen on disk in the ``work`` forlder of your first
``run_XZY`` folder.

Note that you can have several chapters for one namelist or several namelists
included in one ``namelist_changes`` block, but you can only have one
``namelist_changes`` block per model or component.

See also
~~~~~~~~

.. todo Maybe we want to include a link here to the default namelists?

.. links to relevant parts of the documentation

:ref:`yaml:What Is YAML?`
