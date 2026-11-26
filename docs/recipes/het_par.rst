Heterogeneous Parallelization Run (MPI/OpenMP)
==============================================

.. use = for sections, ~ for subsections and - for subsubsections

**Feature available since version:** 5.1

In order to run a simulation with hybrid MPI/OpenMP parallelization include the
following in your runscript:

1. Add ``heterogeneous_parallelization: true`` in the ``computer`` section of your
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


Rank placement with taskset
~~~~~~~~~~~~~~~~~~~~~~~~~~~

On Slurm machines, ``computer.taskset: true`` (the default in some setups, e.g.
`AWICM3` and `AWIESM3`) places every MPI rank explicitly: ESM-Tools decides the node
and the cores of each rank, and pins the rank's threads to those cores with
``taskset``.

How ranks are placed
--------------------

- Components keep their rank order (the order of the components in the setup).
- Components are laid out one after another: a component starts on the next free core
  after the previous one, so two components share the node where they meet.
- Every rank starts on a multiple of its ``omp_num_threads`` and never crosses a node
  boundary. With 1, 2, 4 or 8 threads this keeps each rank inside one L3 cache group
  on AMD EPYC nodes. If ``omp_num_threads`` does not divide the cores of a node, the
  first core of a rank is not always a multiple of its threads inside the node.
- The number of nodes requested in the ``#SBATCH`` header is the number of nodes the
  placement uses.

The placement can be tuned per component with two variables:

``interleave_into`` (with ``ranks_per_node``)
   Puts the ranks of a component into the nodes of other components (hosts) instead
   of nodes of its own. When a host starts a new node, the end of that node is
   reserved for the next ``ranks_per_node`` ranks of the guest (default ``1``), and
   the host fills the cores before them. Once all guest ranks are placed, the
   remaining host nodes are used only by the host. With several hosts, the guest
   starts with the host that comes first in rank order and continues in the next one.

   .. code-block:: yaml

      xios:
          interleave_into: [oifs]
          ranks_per_node: 2

   .. note:: A host node that carries guest ranks gets fewer host ranks. Components
      whose decomposition depends on the ranks per node (e.g. FESOM's ``n_part``)
      have to match that.

``ranks_per_node`` (without ``interleave_into``)
   Caps the ranks per node of a component. The component starts on a new node and its
   ranks are spread evenly over the cores of each node, leaving the cores in between
   idle, which gives the busy cores more memory bandwidth, cache and clock headroom.
   The next component starts on the next node. For example, on 128-core nodes, 96
   FESOM ranks per node use three of every four cores:

   .. code-block:: yaml

      fesom:
          ranks_per_node: 96

   A component that hosts other components cannot set ``ranks_per_node``.

ESM-Tools stops with an error if the layout does not fit (e.g. no room for the guest
ranks on the host nodes, or ``ranks_per_node`` times ``omp_num_threads`` larger than
the cores of a node).

The placement file
------------------

The placement is written to ``<run>/work/rank_placement`` when the job script is
prepared, with one ``<node_index> <first_core>`` line per MPI rank, in rank order
(``node_index`` counts the nodes of the job, starting at ``0``). The job script builds
the Slurm host file (``SLURM_HOSTFILE``) from it, and the ``prog_<component>.sh``
wrappers read their rank's first core from it for ``taskset``. Check this file to see
where each rank runs. ``esm_tests`` compares it with the last tested state.


See also
~~~~~~~~

.. links to relevant parts of the documentation

- :ref:`esm_variables:Runtime variables`
