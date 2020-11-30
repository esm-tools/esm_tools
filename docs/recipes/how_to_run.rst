How to setup runscripts for different kind of experiments
=========================================================

.. use = for sections, ~ for subsections and - for subsubsections

.. **Feature available since version:** <version_num>

This recipe describes how to setup a runscript for the following different kinds of experiments. Besides the variables described in :ref:esm_variables, add the following variables to your runscript, as described below.

* **Initial run**: An experiment from initial model conditions.


.. yaml blocks can be written in yaml format by including them in a code block:
.. code-block:: yaml
        
    general:
        lresume: 0

* **Restart**: An experiment that restarts from a previous experiment with the same experiment id.

.. code-block:: yaml

    general:
        lresume: 1

* **Branching off**: An experiment that restarts from a previous experiment but with a different experiment id.

.. code-block:: yaml

    general:
        lresume: 1
        ini_parent_exp_id: <old-experiment-id>
        ini_restart_dir: <path-to-restart-dir-of-old-experiment>/restart/

* **Branching off and redate**: An experiment that restarts from a previous experiment with a different experiment id and if this experiment should be continued with a diiferent start date.

.. code-block:: yaml

    lresume: 1
        ini_parent_exp_id: <old-experiment-id>
        ini_restart_dir: <path-to-restart-dir-of-old-experiment>/restart/
        first_initial_year: <year>


.. Note: make sure to leave vertical spaces below the lines starting with ``.. tab::``
   and ``.. code-block::``. If you don't, sphinx will complain about excess of inputs for
   these functions.


See also
~~~~~~~~

.. links to relevant parts of the documentation

- :ref:'esm_variables'
- :ref:`yaml:What Is YAML?`
