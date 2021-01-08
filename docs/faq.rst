==========================
Frequently Asked Questions
==========================

Installation
------------

1. **Q**: My organization is not in the pull-down list I get when trying the Federated Login to gitlab.awi.de.

   **A**: Then maybe your institution just didn't join the DFN-AAI. You can check that at https://tools.aai.dfn.de/entities/.

2. **Q**: I am trying to use the Federated Login, and that seems to work fine. When I should be redirected to the gitlab server though, I get the error that my uid is missing.

   **A**: Even though your organization joined the DFN-AAI, gitlab.awi.de needs your organization to deliver information about your institutional e-mail address as part of the identity provided. Please contact the person responsible for shibboleth in your organization.


ESM Runscripts 
--------------

1. **Q**: I get the error: ``load_all_functions: not found [No such file or directory]`` when calling my runscript like this::
  
        $ ./my_run_script.sh -e some_expid 
   
   **A**: You are trying to call your runscript the old-fashioned way that worked with the shell-script version, until revision 3. With the new python version, you get a new executable ``esm_runscripts`` that should be in your PATH already. Call your runscript like this::

        $ esm_runscripts my_run_script.sh -e some_expid

   All the command line options still apply. By the way, "load_all_function" doesn't hurt to have in the runscript, but can savely be removed.

2. **Q**: What should I put into the variable ``FUNCTION_PATH`` in my runscript, I can't find the folder ``functions/all`` it should point to.

   **A**: You can safely forget about ``FUNCTION_PATH``, which was only needed in the shell script version until revision 3. Either ignore it, or better remove it from the runscript.

3. **Q**: When I try to branch-off from a spinup experiment using `FESOM`, I get the following runtime error::

    read ocean restart file
    Error:
    NetCDF: Invalid dimension ID or name


   **A**: See :ref:`How to branch-off FESOM from old spinup restart files <target to branchoff old restart>`.

ESM Master
----------

1. **Q**: How can I define different environments for different models / different versions of the same model?

   **A**: You can add a choose-block in the models yaml-file (``esm_tools/configs/model_name.yaml``), e.g.:

   .. code-block:: yaml

      choose_version:
              40r1:
                      environment_changes:
                              add_export_vars:
                                      - 'MY_VAR="something"'
                              add_module_actions:
                                      - load my_own_module

              43r3:
                      environment_changes:
                              add_export_vars:
                                      - 'MY_VAR="something_else"'
2. **Q**: How can I add a new model, setup, and coupling strategy to the esm_master tool?

   **A**: Add your configuration in the file configs/esm_master/setups2models.yaml

Frequent Errors
---------------

1. **Q**: When I try to install `ESM-Tools` or use ``esm_versions`` I get the following error::

       RuntimeError: Click will abort further execution because Python 3 was configured to use ASCII as encoding for the environment. Consult https://click.palletsprojects.com/en/7.x/python3/ for mitigation steps.

or something on the following lines::

    ERROR: Command errored out with exit status 1:
    command: /sw/rhel6-x64/conda/anaconda3-bleeding_edge/bin/python -c 'import sys, setuptools, tokenize; sys.argv[0] = '"'"'/tmp/pip-install-0y687gmq/esm-master/setup.py'"'"'; _file__='"'"'/tmp/pip-install-0y687gmq/esm-master/setup.py'"'"';f=getattr(tokenize, '"'"'open'"'"', open)(__file__);code=f.read().replace('"'"'\r\n'"'"', '"'"'\n'"'"');f.close();exec(compile(code, _file__, '"'"'exec'"'"'))' egg_info --egg-base /tmp/pip-install-0y687gmq/esm-master/pip-egg-info
    cwd: /tmp/pip-install-0y687gmq/esm-master/
    Complete output (7 lines):
    Traceback (most recent call last):
      File "<string>", line 1, in <module>
      File "/tmp/pip-install-0y687gmq/esm-master/setup.py", line 8, in <module>
        readme = readme_file.read()
      File "/sw/rhel6-x64/conda/anaconda3-bleeding_edge/lib/python3.6/encodings/ascii.py", line 26, in decode
        return codecs.ascii_decode(input, self.errors)[0]
    UnicodeDecodeError: 'ascii' codec can't decode byte 0xf0 in position 1468: ordinal not in range(128)
        ----------------------------------------
    ERROR: Command errored out with exit status 1: python setup.py egg_info Check the logs for full command output.

   **A**: Some systems have ``C.UTF-8`` as locale default (i.e. ``$LC_ALL``, ``$LANG``). This issue is solved by setting up the locales respectively to ``en_US`` and ``en_US.UTF-8``, either manually or adding them to the local bash configuration file (i.e. ``~/.bash_profile``)::

        $ export LC_ALL=en_US
        $ export LANG=en_US.UTF-8

2. **Q**: How can I add a new model, setup, and coupling strategy to the esm_master tool?

   **A**: Add your configuration in the file ``configs/esm_master/setups2models.yaml`` (see :ref:`contributing:Implementing a New Model` and :ref:`cookbook:Implement a New Coupled Setup`)
