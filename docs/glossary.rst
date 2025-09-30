========
Glossary
========

.. glossary::

    chunk
        A chunk is a set of :term:`runs<run>` that are grouped together for execution. This allows
        to group runs for offline coupling.
    experiment
        The whole simulation and computations performed under the same :term:`workflow<workflow>`.
    job
        A job is a unit of work inside a :term:`workflow<workflow>`. Sometimes also referred as a phase.
    run
        A run the set of computations and :term:`jobs<job>` until a restart is performed, a cycle of within a :term:`experiment<experiment>`.
    workflow
        A workflow is a series of :term:`jobs<job>` that are executed during a run and their dependencies.
