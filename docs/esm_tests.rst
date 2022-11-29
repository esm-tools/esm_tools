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

Glossary
--------

.. glossary::

   esm_tests_info
    The repository where the files of the ``last-state`` and the ``runscripts`` for
    testing are stored. This repository is clone as a submodel of `ESM-Tools` whenever
    ``esm_tests`` command is run for the first time. The repository is cloned locally
    into the ``esm_tools/src/esm_tests/resources`` folder. You can activate the submodule
    manually via ``git submodule init`` followed by ``git submodule sync``.

   last-state
    Set of configuration files, both from compilation and runtime, that represent the
    last approved configuration of the testing runscripts. This set of files is kept
    for comparison with the equivalent files of future pull-requests, to ensure the
    stability of the configurations. `ESM-Tests` always compares the new files to the
    ``last-state`` files automatically, both in actual compilation/runs or check
    compilation/runs. The files stored in the ``last-state`` are:
        - compilation scripts (``comp-*.sh``)
        - namelists
        - namcouple
        - finished_config
        - batch scripts (``.run``)

Usage
-----

::

    esm_tests [-h] [-n] [-c] [-u] [-d] [-s SAVE] [-t] [-o] [-b] [-g] [-e]

Arguments
---------

====================================================== ==========================================================
Optional arguments                                     Description
====================================================== ==========================================================
  -h, --help                                           Show this help message and exit
  -n, --no-user                                        Avoid loading user config (for check tests in GitHub)
  -c, --check                                          Check mode on (does not compile or run, but produces
                                                       some files that can be compared to previous existing
                                                       files in ``last_tested`` folder)
  -u, --update                                         Updates the resources with the release branch,
                                                       including runscriptsand last_tested files
  -d, --delete                                         Delete previous tests
  -s SAVE, --save SAVE                                 Save files for comparisson in ``last_tested`` folder
  -t, --state                                          Print the state stored in ``state.yaml``
  -o, --hold                                           Hold before operation, to give time to check the output
  -b, --bulletpoints                                   Bullet points for printing the state and copy/paste as
                                                       markdown text
  -g, --github                                         Use this flag when running in GitHub servers (i.e.
                                                       adds syntax for collapsing compare sections of the
                                                       output for GitHub Actions)
  -e, --system-exit-on-errors                          Trigger a system exit on errors or file differences
                                                       so that GitHub actions can catch that as a failing test
====================================================== ==========================================================

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
