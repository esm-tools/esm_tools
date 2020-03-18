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
