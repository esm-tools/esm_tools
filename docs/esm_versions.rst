============
ESM-Versions
============

New with the Tools version 3.1.5, you will find an executable in your path called ``esm_version``. This was added by Paul Gierz to help the user / developer to keep track of / upgrade the python packages belonging to ESM Tools.

Usage
-----

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


Getting ESM-Versions 
--------------------

As was said before, if you have the Tools with a version newer than 3.1.4, you should already have ``esm_versions`` in your path. In case you are on an older version of the Tools, or it is missing because of problems, you need to remove the installed python packages by hand one last time, and then reinstall them using the installer:

1. Make sure to push all your local changes to branches of the repos, or save them otherwise!

2. Remove the installed python libs::

    $> rm -rf ~/.local/lib/python-whatever_your_version/site-packages/esm*

3. Remove the installed executables::

    $> rm -rf ~/.local/bin/esm*

4. Upgrade the repository esm_tools::

    $> cd path/to/esm_tools
    $> git checkout release
    $> git pull

5. Re-install the python packages::

    $> ./install.sh

You should now be on the most recent released version of the tools, and ``esm_versions`` should be in your ``PATH``.



