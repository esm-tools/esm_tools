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
    
 
Logging
-------

``esm_master`` uses Loguru for console logging. To increase verbosity call ``esm_master`` with the ``-v`` or ``--verbose`` flag.

Configuring esm-master for Compile-Time Overrides
-------------------------------------------------

It is possible that some models have special compile-time settings that need to be included, overriding the machine defaults. Rather than placing these changes in ``configs/machines/NAME.yaml``, they can be instead placed in the ``computer`` section of the component or model configurations using a ``choose_general.execution_mode`` block, e.g.:

.. code-block:: yaml
    
    computer:
        choose_general.execution_mode:
            compile:
                export_vars:
                    SPECIAL_COMPILE_VAR: "compile_value"        

See :ref:`esm_environment:ESM Environment` for more details on environment configuration options.

CLI Configuration Overrides
----------------------------

``esm_master`` supports Spack-style ``key=value`` overrides directly on the command line, so you can
change any configuration value without creating a YAML file::

    $ esm_master install-awiesm-2.6.2 computer.mpi_implementation=openmpi_2026

Keys use dot notation to address nested sections (``section.key``). Multiple overrides can be
passed at once::

    $ esm_master install-awiesm-2.6.2 computer.mpi_implementation=openmpi_2026 computer.fc=ifort

Values are parsed as YAML scalars, so strings, integers, booleans, and ``null`` all work::

    $ esm_master install-fesom-2.0 computer.omp_num_threads=4

.. note::

   Some configuration keys are themselves named with a dot (e.g. ``namelist.echam`` inside
   ``add_namelist_changes``). In those cases, change the separator with ``--separator`` so the dot
   in the key name is not interpreted as a level separator::

      $ esm_master install-awiesm-2.6.2 --separator , general,some.dotted.key=value

The overrides are applied after the target/model config is loaded but **before** ``choose_``
blocks are resolved, so they are visible to ``choose_mpi_implementation`` and similar switch
blocks in the machine and component configs.

.. note::

   This feature is only available for ``esm_master``. In ``esm_runscripts`` you always need
   a runscript YAML so modifications to the config should always be done there.
