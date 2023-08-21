.. highlight:: shell

============
Installation
============


Downloading
-----------

``esm_tools`` is hosted on https://github.com/esm-tools. To get access to the software you need to be able to log into GitHub.

Then you can start by cloning the repository ``esm_tools.git``::

$ git clone https://github.com/esm-tools/esm_tools.git

This gives you a collection of `yaml` configuration files containing all the information on models, coupled setups, machines, etc. in the subfolder ``config``, default namelists in the folder ``namelists``, example runscripts for a large number of models on different HPC systems in subfolder ``runscripts``, and this documention in ``docs``. Also you will find the installer ``install.sh`` used to install the python packages.

Installing in an encapuslated environment
-----------------------------------------

Based on an alternative installation procedure, that provides an esm-tools installation employing direnv (https://direnv.net/), you can now install various encapsulated versions of esm-tools alongside each other. These different installations do not impact each others' configuration. Consequently, they can coexist in peaceful harmony. In the suggested alternative installation method all configurations will reside within the base folder of a specific esm-tools version that you install. There is no dependency on configurations outside the installation directory of a specific esm-tools version, mitigating the potential for side effects if another version of esm tools is installed in parallel. To install esm-tools as suggested here, just follow the procedure outlined below. The steps to create the installation involve preparation of direnv, including setting up an environment that encapsulates potentially version-specific settings, creating a dedicated directory to which a specific version of esm-tools will be installed, and installing the the esm-tools via pip.
The commands to be executed are (note comments for further explanation)::

    $ curl -sfL https://direnv.net/install.sh | bash # install direnv if not yet done - this enables encapsulation and parallel use of different esm-tools versions
    $ mkdir esm_tools_v6.1.10 #adjust version number as appropriate
    $ cd esm_tools_v6.1.10/
      #create .envrc (e.g. via direnv edit .) and add information matching the result of the cat command below
    $ cat .envrc
      module load python3
      layout python
      module load gcc
    $ pip install -U pip wheel
    $ pip install esm-tools


Accessing components in DKRZ server
-----------------------------------

Some of the ``esm_tools`` components are hosted in the gitlab.dkrz.de servers. To be able to reach these components you will need:

1. A DKRZ account (https://www.dkrz.de/up/my-dkrz/getting-started/account/DKRZ-user-account).

2. Become a member of the group ``esm_tools``. Either look for the group and request membership, or directly contact dirk.barbi@awi.de.

3. Request access from the corresponding author of the component. Feel free to contact us if you don't know who the model developers are or check the :ref:`Supported_Models:Supported Models` section.

.. include:: ../README.rst


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

We are sorry to see you go! To uninstall your current installation make sure you have the most recent version of
``pip`` available for your system::

$ python3 -m pip install -U pip

Then, you can use use the following command to uninstall all `ESM-Tools` packages::

$ pip freeze | grep esm | xargs pip uninstall -y

You can also choose to manually uninstall. In order to do that, remove the installed Python packages and delete the ``esm_*`` executables. The following commands will do the trick if you installed with the ``install.sh`` script or installed using ``pip`` with ``user`` mode ::

$ rm -ri ~/.local/bin/esm*
$ rm -ri ~/.local/lib/python3.<version>/site-packages/esm*

Note that you may have a different Python version, so the second command might need to be adapted. You may also use ``pip`` to uninstall any of the packages::

$ pip uninstall [--user] esm-tools

The ``--user`` flag may be required when using ``pip`` if you are not uninstalling in either a virtual environment or a global install (you would need to be root in that case).

