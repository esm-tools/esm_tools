==============
ESM Runscripts
==============


Job Phases
----------

The following table summarizes the job phases and gives a brief description.

....

Running only part of a job
--------------------------

It's possible to run only part of a job. This is particularly interesting for
development work; when you might only want to test a specific phase without
having to run a whole simulation.

As an example; let's say you only want to run the ``tidy`` phase of a
particular job; which will move things from the particular run folder to the
overall experiment tree. In this example; the experiment will be called ``test001``::

        $ esm_runscripts ${PATH_TO_USER_CONFIG} -t tidy_and_resubmit
