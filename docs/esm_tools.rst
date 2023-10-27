=========
ESM Tools
=========

`ESM-Tools` includes also a command line tool named ``esm_tools``. It can be used to interact with several parts of our software.
``esm_tools`` can be used with two options and two subcommands:

Options
^^^^^^^

+-----------------------------------+-----------------------------------------------------------+
| ``esm_tools`` options             | Description                                               |
+===================================+===========================================================+
| --version                         | Shows the version of the currently used ``esm-tools``.    |
+-----------------------------------+-----------------------------------------------------------+
| --help                            | Prints usage information about ``esm_tools``              |
+-----------------------------------+-----------------------------------------------------------+

Subcommands
^^^^^^^^^^^

+-----------------------------------+--------------------------------------------------+---------------+-------------------------------------------------------+
| ``esm_tools`` subcommand          | Options                                          | Arguments     | Descritpion                                           +
+===================================+==================================================+===============+=======================================================+
| create-new-config                 | -- help                                          | NAME          | Opens your $EDITOR and creates a new file for NAME    |
|                                   | -t, --type [component (default)|setup|machine]   |               |                                                       |
+-----------------------------------+--------------------------------------------------+---------------+-------------------------------------------------------+
| test-state                        | -- help                                          |               | Prints the state of the last tested experiments.      |
+-----------------------------------+--------------------------------------------------+---------------+-------------------------------------------------------+

Usage: Top level command
------------------------

To show all top-level command options and subcommands::

    $ esm_tools --help
    Usage: esm_tools [OPTIONS] COMMAND [ARGS]...

    Options:
      --version  Show the version and exit.
      --help     Show this message and exit.

    Commands:
      create-new-config  Opens your $EDITOR and creates a new file for NAME
      test-state         Prints the state of the last tested experiments.

.. _esm-tools_version:
Usage: Getting the Version
--------------------------

You can get the version number of the currently used ``esm_tools`` installation with::

    $ esm_tools --version
    esm_tools, version 6.20.1

Usage: Checking current testing state
-------------------------------------

You can get the current state of our automatic tests with::

    $ esm_tools test-state

Usage: Create a new configuration
---------------------------------

You can get a pre-generated template to add a new component with::

    $ esm_tools create-new-config <MY_NEW_CONFIG_NAME>
    Creating a new component configuration for my_new_thing

    ...EDITOR OPENS....

    Thank you! The new configuration has been saved. Please commit it (and get in touch with the
    esm-tools team if you need help)!

You can also specify if you want to create a new ``setup``, ``component`` or ``machine`` configuration file by giving the option ``-t`` or ``--type`` to the subcommand::

  $ esm_tools create-new-config --type setup <MY_NEW_SETUP_NAME>

Note however that there is (as of this writing) no template available for setups!
