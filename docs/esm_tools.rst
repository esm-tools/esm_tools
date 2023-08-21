=========
ESM Tools
=========

The command line interface ``esm_tools`` also has a top-level program you can
use to interact with several parts of our software.

Usage: Top level commands
-------------------------

To show all top-level commands::

    $ esm_tools --help
    Usage: esm_tools [OPTIONS] COMMAND [ARGS]...

    Options:
      --version  Show the version and exit.
      --help     Show this message and exit.

    Commands:
      create-new-config  Opens your $EDITOR and creates a new file for NAME
      test-state         Prints the state of the last tested experiments.

Usage: Getting the Version
--------------------------

You can get the version number you currently have with::

    $ esm_tools --version
    esm_tools, version 6.20.1

Usage: Checking current testing state
-------------------------------------

You can get the current state of our automatic tests with::

    $ esm_tools test-state

Usage: Making a new component
-----------------------------

You can get a pre-generated template to add a new component with::

    $ esm_tools create-new-config <MY_NEW_CONFIG_NAME>
    Creating a new component configuration for my_new_thing

    ...EDITOR OPENS....

    Thank you! The new configuration has been saved. Please commit it (and get in touch with the
    esm-tools team if you need help)!

You can also specify if you are creating a new ``setup`` or a new ``component`` with::

  $ esm_tools create-new-config --type setup <MY_NEW_SETUP_NAME>

Note however that there is (as of this writing) no template available for setups!
