==========
ESM Master
==========

Usage: esm_master
-----------------

To use the command line tool ``esm_master``, just enter at a prompt::

    $ esm_master

The tool may ask you to configure your settings; which are stored in your home folder under ``${HOME}/.esmtoolsrc``. A list of avaiable models, coupled setups, and available operations are printed to the screen, e.g.::

    setups: 
       awicm: 
          1.0: ['comp', 'clean', 'get', 'update', 'status', 'log', 'install', 'recomp']
          CMIP6: ['comp', 'clean', 'get', 'update', 'status', 'log', 'install', 'recomp']
          2.0: ['comp', 'clean', 'get', 'update', 'status', 'log', 'install', 'recomp']
    [...]

As can be seen in this example, ``esm_master`` supports operations on the coupled setup awicm in the versions 1.0, CMIP6 and 2.0; and what the tool can do with that setup. You execute ``esm_master`` by calling::

$ esm_master operation-software-version,

e.g.::

$ esm_master install-awicm-2.0



By default, ``esm_master`` supports the following operations:

**get**:
        Cloning the software from a repository, currently supporting git and svn
**conf**:
        Configure the software (only needed by mpiesm and icon at the moment)
**comp**:
        Compile the software. If the software includes libraries, these are compiled first. After compiling the binaries can be found in the subfolders ``bin`` and ``lib``.
**clean**:
        Remove all the compiled object files.
**install**:
        Shortcut to get, then conf, then comp.
**recomp**:
        Shortcut to conf, then clean, then comp.
**update**:
        Get the newest commit of the software from the repository.
**status**:
        Get the state of the local database of the software (e.g. ``git status``)
**log**:
        Get a list of the last commits of the local database of the software (e.g. ``git log``)


To download, compile, and install ``awicm-2.0``; you can say::

    $ esm_master install-awicm-2.0

This will trigger a download, if needed a configuration, and a compilation process. Similarly, you can recompile with ``recomp-XXX``, clean with ``clean-XXX``, or do individual steps, e.g. ``get``, ``configure``, ``comp``.

The download and installation will always occur in the **current working directory**.

You can get further help with::

    $ esm_master --help
