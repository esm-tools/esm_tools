Recipe Title
============

.. use = for sections, ~ for subsections and - for subsubsections

**Feature available since version:** <version_num>

General description here.

1. Step 1
   make sure you indent correctly inside steps
2. Step 2
   make sure you indent correctly inside steps

.. yaml blocks can be written in yaml format by including them in a code block:
.. code-block:: yaml

    dictionary:
        variable1: 1
        variable2: false

Example
~~~~~~~

.. subsection including examples with particular things in the recipe for specific models
   and coupled setups.
   Recommendation: use ``tabs`` for including examples for different models
   Note: numbering of the general recipe steps can be handy to reference the steps to modify

.. tabs::
   .. tab:: MODEL 1

      Your text here

      .. code-block:: yaml

         Your yaml code here

   .. tab:: MODEL 2

      Your text here

      .. code-block:: yaml

         Your yaml code here

.. Note: make sure to leave vertical spaces below the lines starting with ``.. tab::``
   and ``.. code-block::``. If you don't, sphinx will complain about excess of inputs for
   these functions.


See also
~~~~~~~~

.. links to relevant parts of the documentation

- `Text of your link <actual url for the link>`_
- :ref:`yaml:What Is YAML?`
