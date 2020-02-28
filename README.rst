=========
ESM Tools
=========


Installing
----------

To use the new version of the esm-tools, now rewritten in Python, clone this repository::

    git clone https://gitlab.awi.de/esm_tools/esm_tools.git

Then, run the ``install.sh``::

    ./install.sh

You should now have the command line tools ``esm_master`` and ``esm_runscripts``, which replace the old version.

You may have to add the installation path to your ``PATH`` variable::

    export PATH=~/.local/bin:$PATH


Usage: esm-master
-----------------

To use the command line tool ``esm_master``, just enter at a prompt::

    esm_master

The tool may ask you to configure your settings; which are stored in your home folder under ``${HOME}/.esmtoolsrc``. A list of avaiable commands and models are printed to the screen. To download, compile, and install ``awicm-2.0``; you can say::

    esm_master install-awicm-2.0

This will trigger a download, if needed a configuration, and a compilation process. Similarly, you can recompile with ``recomp-XXX``, clean with ``clean-XXX``, or do individual steps, e.g. ``get``, ``configure``, ``comp``.

The download and installation will always occur in the **current working directory**.

You can get further help with::

    esm_master --help
