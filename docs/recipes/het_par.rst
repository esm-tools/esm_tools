Heterogeneous Parallelization Run (MPI/OpenMP)
==============================================

.. use = for sections, ~ for subsections and - for subsubsections

**Feature available since version:** 5.1

In order to run a simulation with hybrid MPI/OpenMP parallelization include the
following in your runscript:

1. Add ``heterogenous_parallelization: true`` in the ``computer`` section of your
   runscript. If the ``computer`` section does not exist create one.
2. Add ``omp_num_threads: <number>`` to the sections of the components you'd like to
   have OpenMP parallelization.


Example
~~~~~~~

.. subsection including examples with particular things in the recipe for specific models
   and coupled setups.
   Recommendation: use ``tabs`` for including examples for different models
   Note: numbering of the general recipe steps can be handy to reference the steps to modify

.. tabs::
   .. tab:: AWICM3

      In `AWICM3` we have 3 components: `FESOM-2`, `OpenIFS` and `RNFMAP`. We want to
      run `OpenIFS` with 8 OpenMP threads, `RNFMAP` with 48, and `FESOM-2` with 1.
      Then, the following lines need to be added to our runscript:

      .. code-block:: yaml

         general:
             [ ... ]
         computer:
             heterogeneous_parallelization: true
             [ ... ]
         fesom:
             omp_num_threads: 1
             [ ... ]
         oifs:
             omp_num_threads: 8
             [ ... ]
         rnfmap:
             omp_num_threads: 48
             [ ... ]

..   .. tab:: AWIEM-2.?

..      Your text here

..      .. code-block:: yaml

..         Your yaml code here

.. Note: make sure to leave vertical spaces below the lines starting with ``.. tab::``
   and ``.. code-block::``. If you don't, sphinx will complain about excess of inputs for
   these functions.


See also
~~~~~~~~

.. links to relevant parts of the documentation

- :ref:`esm_variables:Runtime variables`
