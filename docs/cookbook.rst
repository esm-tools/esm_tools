.. highlight:: shell
.. The next sets up red text for commenting the document. DELETE before merging inito release
.. role:: red

========
Cookbook
========

In this chapter you can find multiple recipes for different ESM-Tools functionalities, such
running a model, adding forcing files, editing defaults in namelists, etc.

If you'd like to contribute with your own recipe, or ask for a recipe, please open a
documentation issue on `our GitHub repository <https://github.com/esm-tools/esm_tools/issues/new?assignees=&labels=documentation&template=doc_request_contribution.md&title=>`_.

.. note::

   Throughout the cookbook, we will sometimes refer to a nested part of a
   configuration via dot notation, e.g. ``a.b.c``. Here, we mean the following
   in a YAML config file:

   .. code-block:: yaml

      a:
        b:
          c: "foo"

   This would indicate that the value of ``a.b.c`` is ``"foo"``. In Python, you
   would access this value as ``a["b"]["c"]``.

.. Include your recipes here.
.. Keep them alphabetical after the template.
.. In vim, you can do:
..
.. V (enter visual line mode)
.. } (Move to the end of the paragraph)
.. : sort (sort the lines you have selected alphabetically)

.. include:: recipes/sbatch_flags.rst
.. include:: recipes/echam_disturbance.rst
.. include:: recipes/modify_namelists.rst
.. include:: recipes/het_par.rst
.. include:: recipes/how_to_run.rst
.. include:: recipes/add_model_setup.rst
.. include:: recipes/add_machine.rst
.. include:: recipes/add_forcing_input.rst
.. include:: recipes/exclude_forcing_input.rst
.. include:: recipes/use_own_namelist.rst
.. include:: recipes/branchoff_from_old_spinups.rst
.. include:: recipes/batch_mail.rst
