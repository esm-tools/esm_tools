==============
ESM Runscripts
==============


Job Phases
----------

The following table summarizes the job phases and gives a brief description.

....

Yaml Runscripts
---------------

Runscripts can be written as a yaml file.

Math Operations
~~~~~~~~~~~~~~~

Some math operations are supported in yaml files:

* **Addition and subtraction of dates**: a key of a yaml file can be define as the result
  of an addition or subtraction of times and dates with the format ``key:
  $(( ${time1}unit1 operator ${time2}unit2 operator ... ))``, where the unit specify
  the time unit for every time in the operation.
  The units available are `seconds`, ... . Example for a ``runtime`` definition::

        runtime: $(( ${end_date} - ${time_step}seconds ))

Running only part of a job
--------------------------

It's possible to run only part of a job. This is particularly interesting for
development work; when you might only want to test a specific phase without
having to run a whole simulation.

As an example; let's say you only want to run the ``tidy`` phase of a
particular job; which will move things from the particular run folder to the
overall experiment tree. In this example; the experiment will be called ``test001``::

        $ esm_runscripts ${PATH_TO_USER_CONFIG} -t tidy_and_resubmit
