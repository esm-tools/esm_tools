.. highlight:: shell

=========
ESM-Tests
=========

.. note:: This is a feature aimed for advance users and developers who work in preparing
   default configurations of experiments, or who implement a model/coupled-setup in
   ESM-Tools.

`ESM-Tests` is the integration testing suite from `ESM-Tools`. Its aim is to test a set
of selected runscripts and model builds just by using one single command:
``esm_tests``. It can also perform dry actions (i.e. check compilations and check
runs).

ESM-Tests cookbook
------------------

How to approve changes on a GitHub Pull-Request
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

1. In the pull-request, if all the tests passed you don't need to approve any
   changes, you can jump directly to step 4.
2. If any of the tests labelled as ``esm_tests`` failed (click on the triangles to
   expand screen captures):

   .. collapse:: Click on Details

      .. image:: images/esm_tests1.png

   .. collapse:: Find the names of the runscripts with differences (in yellow)

      .. image:: images/esm_tests2.png

   .. collapse:: Scroll up and expand the lines starting with COMPILE/SUBMITTING (for
      compilation and runtime checks respectively) followed by the script with
      differences

      .. image:: images/esm_tests3.png

   .. collapse:: Review the differences with special attention to namelists and
      namcouple files

      .. image:: images/esm_tests4.png


3. If there are no problematic differences, and the pull-request has been already
   reviewed and is just ready to be merged, write a message on the PR containing
   ``#approve-changes``. This will commit the new files from the tests as the
   ``last-state``, in the ``esm_tests_info`` repository.

   .. warning:: Currently, ``#approve-changes`` does not update the test status
      on GitHub, once the operation finishes. If you want to see whether
      ``#approve_changes`` finished or not you have to navigate to the ``Actions``
      tab in GitHub. If you want to see all tests green, wait until
      ``#approve-changes`` finishes, and relaunch the tests for the last
      failed set of tests in the PR. Miguel - I know this is a pain, but I could not
      figure out how to do all this automatically (I wasted enough time on GitHub
      Actions for years to come).

4. Bump the version and wait that the bumpversion commit shows up.

5. You can now merge.
