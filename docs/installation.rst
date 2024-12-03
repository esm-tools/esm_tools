.. highlight:: shell

============
Installation
============

Prerequisite
------------

Make sure you have installed or loaded a python version that is 3.7 or later but not newer than 3.10 (see also :ref:`before_you_continue:Before you continue`). Use a pip version that is up-to-date (to update run ``pip install -U pip``). Also make sure that the location to which the python binaries of `ESM-Tools` will be installed (which is ``~/.local/bin`` by default) is in your ``PATH``. For that purpose, add the following lines to one of your login or profile files, i.e. ``~/.bash_profile``, ``~/.bashrc``, ``~/.profile``, etc.::

    $ export PATH=$PATH:~/.local/bin
    $ export LC_ALL=en_US.UTF-8
    $ export LANG=en_US.UTF-8

Installing in local environment
-------------------------------

After downloading the `ESM-Tools` software (see :ref:`get_esm-tools:Get ESM-Tools`) you have a new folder named ``esm_tools``. In this new folder you find the script to install `ESM-Tools` on the HPC your are running this install script on. 

To change into the new folder and execute the instalation script, execute the following commands::

     $ cd esm_tools
     $ ./install.sh

This should install all necessary python packages of `ESM-Tools`. If you wonder where they end up, take a look at ``~/.local/lib/python%versionnumber%/site-packages``.

To see where `ESM-Tools` are installed, run the following command::

    $ which esm_tools

A possible (default) output can be ``~/.local/bin/esm_tools``.

..
    Installing using pip
    --------------------

    There is a version of `ESM-Tools` available that can be installed using ``pip``:
    
    - Update ``pip`` and ``wheel`` python packages::
    
        $ pip install -U pip wheel
    
    - Install `ESM-Tools`::
    
        $ pip install esm-tools 

    .. note::
    The version that is available via ``pip`` is not the most recent version. We strongly recommend to install the most recent version of `ESM-Tools` using the install script described above (see also :ref:`get_esm-tools:Get ESM-Tools` and :ref:`installation:Installing in local environment`).

Installing in a conda environment
-----------------------------------

Work in progress. We are still testing it.


Installing in an encapsulated environment using ``direnv``
----------------------------------------------------------
    
Based on an alternative installation procedure, that provides an `ESM-Tools` installation employing direnv (https://direnv.net/), you can now install various encapsulated versions of `ESM-Tools` alongside each other. These different installations do not impact each other's configuration. Consequently, they can coexist in peaceful harmony. In the suggested alternative installation method all configurations will reside within the base folder of a specific `ESM-Tools` version that you install. There is no dependency on configurations outside the installation directory of a specific `ESM-Tools` version, mitigating the potential for side effects if another version of `ESM-Tools` is installed in parallel. To install `ESM-Tools` as suggested here, just follow the procedure outlined below. The steps to create the installation involve preparation of ``direnv``, including setting up an environment that encapsulates potentially version-specific settings, creating a dedicated directory to which a specific version of `ESM-Tools` will be installed.
    
To set up such an installation in an encapsulated environent, please do the followings steps:
    
- Download and install ``direnv`` (if not yet done) with the following command::
    
    $ curl -sfL https://direnv.net/install.sh | bash 
  

  This will install ``direnv`` binary locally under ``~/.local/bin/direnv``. 
    
- Create and enter a new folder that should hold the new encapsulated environment::
    
    $ mkdir esm_tools_v6.1.10 #adjust version number as appropriate
    $ cd esm_tools_v6.1.10/
    
- Set up ``direnv`` hooks according to your used shell (see also https://direnv.net/docs/hook.html). For the ``bash`` shell add the following line at the end of your ``.bashrc`` and/or ``.bash_profile`` file::
    
    eval "$(direnv hook bash)"
    
- Create ``.envrc`` file::
    
    $ direnv edit . 
    
  This command will create the new file ``.envrc`` and opens it in the default editor. You can also create the file and open it in your favorate editor.
    
- Add the following lines to the created ``.envrc`` file and save it::
    
      module load python3
      module load git
      layout python
      module load gcc

  Please also have a look at :ref:`before_you_continue:Before you continue` for more details about necessary modules on the different HPCs. Make sure you keep the line ``layout python``.
    
- Allow this ``.envrc`` file to be used by ``direnv``::
    
    $ direnv allow .
    
This environment setup will be automatically applied each time you enter this folder.

It enables us now to install `ESM-Tools` within this specific environment (see also :ref:`get_esm-tools:Get ESM-Tools` and :ref:`installation:Installing in local environment`)::

    $ git clone https://github.com/esm-tools/esm_tools.git
    $ cd esm_tools
    $ ./install.sh

.. note::

    Please note, that all calls of `ESM-Tools` commands for this particular installed version needs to be done within the folder that holds the ``direnv`` environment.


Update ESM-Tools
----------------

If you installed in editable mode as described above, you can update `ESM-Tools` by using ``git``::

    $ cd esm_tools
    $ git pull origin release
    
..
    Upgrade ESM-Tools
    -----------------

    To upgrade all the `ESM-Tools` packages you can run::
    
    $ esm_versions upgrade
    
    This will only upgrade the packages that are not installed in editable mode. Those,
    installed in editable mode will need to be upgraded using ``git``.
    
    You can also choose to upgrade specific packages by adding the package name to the
    previous command, i.e. to upgrade ``esm_master``::
    
    $ esm_versions upgrade esm_parser
    
    .. Note:: If there are version conflicts reported back at this point with some of the
       Python modules (i.e. ``pkg_resources.ContextualVersionConflict: (<package name>)``),
       try reinstalling that package:
       ``pip install <package> --upgrade --ignore-installed``.

Uninstall ESM-Tools
-------------------

We are sorry to see you go! You can uninstall your current `ESM-Tools` installation in two different ways depending slightly on how you installed it.

Using pip
^^^^^^^^^

Make sure you have the most recent version of
``pip`` available for your system::

$ python3 -m pip install -U pip

If you are using `ESM-Tools` version 6.0.0 or higher you can use the following command to uninstall all `ESM-Tools` packages::

$ pip uninstall [--user] esm-tools

The ``--user`` flag may be required when using ``pip`` if you are not uninstalling in either a virtual environment or a global install (you would need to be root in that case).

If you are using a version of `ESM-Tools` that is older than 6.0.0 use ``pip`` to uninstall as follows::

$ pip freeze | grep esm | xargs pip uninstall -y

Manually
^^^^^^^^

If you have installed `ESM-Tools` with the ``install.sh`` script or using ``pip`` with ``user`` mode, please follow the following steps to uninstall the software manually.

- Delete the ``esm_*`` executables::

    $ rm -ri ~/.local/bin/esm*

- Remove the installed Python packages::

  $ rm -ri ~/.local/lib/python3.<version>/site-packages/esm*

Note that you may have a different Python version, so the second command might need to be adapted.
