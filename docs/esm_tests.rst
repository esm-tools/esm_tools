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

   actual vs check test
    An `actual test` is a test where the model has been compiled and run in one of the
    supported HPCs. A `check test` is a dry test, meaning, no compilation or run takes
    place, but instead, the configuration files and scripts are generated. Both
    `actual` and `check` tests compare their output configuration files and scripts
    to the `last-state`, and offer the possibility to update the `last-state` of those
    files at the end of the test.

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
    compilation/runs.

   runscripts
    The runscripts to run the tests. Runscripts define the test simulation details as
    in regular `ESM-Tools` runscripts, but are also used for `ESM-Tests` to understand
    what needs to be compiled. Runscripts are part of the ``esm_tests_info`` submodule,
    and can be found (if the submodule was initiated via ``esm_tests -u``) in
    ``esm_tools/src/esm_tests/resources/runscripts``. Runscripts need to be generalized
    (i.e. ``choose_computer.name``) for the different HPCs where you want to run the
    tests.

   ``state.yaml``
    In this document some times referred only as state, is a `YAML` file that includes
    information about the status of the tests (actual tests or check tests) in
    different computers. It also includes the date of the last actual test.

Usage
-----

`ESM-Tests` is design to compile and run tests **just with one single command**, without
additional arguments: ``esm_tests``, so that launching a suite of tests in a supported
HPC is straight forward. Higher granularity in the control of the tests is enabled via:
- :ref:`esm_tests:Arguments`
- Runscripts via the usual ``esm_parser`` syntax (e.g. ``choose_computer.name``)
- :ref:`esm_tests:Model control files (\`\`config.yaml\`\`)`
- :ref:`esm_tests:Local test configuration (\`\`test_config.yaml\`\`)`
The basic usage of `ESM-Tests` -> configure files

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
  -s SAVE, --save SAVE                                 Save files for comparisson in ``last_tested`` folder.
                                                       The values can be ``True``/``False``
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

Last-state
----------

The ``last-state`` files are https://github.com/esm-tools/esm_tets_info repository, in
the ``release`` branch. The files stored in the ``last-state`` are:
- compilation scripts (``comp-*.sh``)
- namelists
- namcouple
- finished_config
- batch scripts (``.run``)

Check test status
-----------------

As a user, you can check the ``last-state`` status (the online one of the
``esm_tests_info`` repo, ``release`` branch) by running::

    esm_tools --test-state

This will give you a summary of the state of compilation and running tests for
different models, in different computers, and also a date of when the latest actual
compilation and run tests were carried out.

If you are testing locally in an HPC, you can get the same information about your local
state by running::

    esm_tests -t

Model control files (``config.yaml``)
-------------------------------------

**File location:** ``esm_tools/src/esm_tests/resources/runscripts/<model>/config.yaml``
**Versioned**: Yes, distributed with ``esm_tests_info``

Something here

Local test configuration (``test_config.yaml``)
-----------------------------------------------

**File location:** ``esm_tools/src/esm_tests/test_config.yaml``
**Versioned**: No, user specific, git-ignored

This file gives you control on which tests ``esm_tests`` will run in the current
machine, independently of what tests are defined in the `Model control files`. The
current machine needs to be included in the `Model control file` for the test to run
(i.e. ``test_config.yaml`` runs only the tests included there and supported on the
current platform). The syntax is as follows:

    .. code-block:: yaml

       <model1>:
           - <runscript1_name>.yaml
           - <runscript2_name>.yaml
           - [ ... ]
       <model2>: all
       [ ... ]

The ``model`` sections need to be named after the folders in
``esm_tools/src/esm_tests/resources/runscripts``. If you want to run all the suported
runscripts for a model in this platform, make the ``model`` section have the value
``all``. If you want to select a set of **supported runscripts** make the ``model``
be a list of runscripts (this runscripts need to be in
``esm_tools/src/esm_tests/resources/runscripts/<model>/``). If you want to run all the
supported runscripts for all supported models in this platform, but still keep this
file around (i.e. commented most of the contents), make the file content be an empty
dictionary (``{}``).

**Example**

    .. code-block:: yaml

       #{}
       awiesm: #all
           - all_awiesm-2.1-recom.yaml
       #    - awiesm-2.1_icebergs.yaml
           - bootstrap.yaml
           - pico.yaml
           - PI_ctrl_awiesm-2.1-wiso.yaml
           - pi.yaml
           - pi-wiso.yaml
       echam: all
       fesom: all
       awicm: all
       #    - awicm1-CMIP6-initial-monthly.yaml
       #    - awicm2-initial-monthly.yaml
       fesom-recom:
          - fesom-recom1.4-initial-daily.yaml
       awicm3: all
       #    - awicm3-v3.1-TCO95L91-CORE2_initial
       #    - awicm3-frontiers-TCO159L91-CORE2_initial.yaml
       #oifsamip: all
       #vilma-pism: all

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
