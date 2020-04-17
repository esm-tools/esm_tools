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
  
        $> ./my_run_script.sh -e some_expid 
   
   **A**: You are trying to call your runscript the old-fashioned way that worked with the shell-script version, until revision 3. With the new python version, you get a new executable ``esm_runscripts`` that should be in your PATH already. Call your runscript like this::

        $> esm_runscripts my_run_script.sh -e some_expid

   All the command line options still apply. By the way, "load_all_function" doesn't hurt to have in the runscript, but can savely be removed.

2. **Q**: What should I put into the variable ``FUNCTION_PATH`` in my runscript, I can't find the folder ``functions/all`` it should point to.

   **A**: You can safely forget about ``FUNCTION_PATH``, which was only needed in the shell script version until revision 3. Either ignore it, or better remove it from the runscript.


ESM Master 
----------

1. **Q**: How can I define different environments for different models / different versions of the same model?
   
   **A**: You can add a choose-block in the models yaml-file (``esm_tools/configs/model_name.yaml``), e.g.::
  
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

Frequent Errors
---------------

1. **Q**: When I use ``esm_versions`` I get the following error::

       RuntimeError: Click will abort further execution because Python 3 was configured to use ASCII as encoding for the environment. Consult https://click.palletsprojects.com/en/7.x/python3/ for mitigation steps.

   **A**: Some systems have ``C.UTF-8`` as locale default (i.e. ``$LC_ALL``, ``$LANG``). This issue is solved by setting up the locales to ``en_US.utf-8`` either manually or adding them to the local bash configuration file (i.e. ``~/.bash_profile``)::

        $> export LC_ALL=en_US.utf-8
        $> export LANG=en_US.utf-8

