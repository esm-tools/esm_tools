=========================
ESM-Versions (deprecated)
=========================
.. note::
    .. deprecated:: 6.0.0
       This feature is deprecated since 2022 (version 6.0.0). If you are using a more recent version of ESM-Tools, please use ``esm_tools --version`` instead, to get the version of `ESM-Tools`. See also section :ref:`esm_tools:ESM Tools` and :ref:`esm_tools:Usage: Getting the version`. Please also find further information for e.g. upgrading ESM-Tools here ....

Above version 3.1.5 and below 6.0.0, you will find an executable in your path called ``esm_version``. This was added by Paul Gierz to help the user / developer to keep track of / upgrade the python packages belonging to ESM Tools.

Usage
~~~~~

It doesn't matter from which folder you call ``esm_versions``. You have two subcommands::

        esm_versions check                      gives you the version number of each 
                                                installed esm python package
        esm_versions upgrade                    upgrades all installed esm python 
                                                packages to the newest version
                                                of the release branch

Notice that you can also upgrade single python packages, e.g.::

        esm_versions upgrade esm_parser         upgrades only the package esm_parser 
                                                to the newest version of the release
                                                branch

And yes, ``esm_versions`` can upgrade itself.
