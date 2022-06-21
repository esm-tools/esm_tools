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

To uninstall your current installation make sure you have the most recent version of
``pip`` available for your system::

$ python3 -m pip install -U pip

Then, you can use use the following command to uninstall all `ESM-Tools` packages::

$ esm_versions clean

You can also choose to manually uninstall. In order to do that, remove the installed Python packages and delete the ``esm_*`` executables. The following commands will do the trick if you installed with the ``install.sh`` script or installed using ``pip`` with ``user`` mode ::

$ rm -ri ~/.local/bin/esm*
$ rm -ri ~/.local/lib/python3.<version>/site-packages/esm*

Note that you may have a different Python version, so the second command might need to be adapted. You may also use ``pip`` to uninstall any of the packages::

$ pip uninstall [--user] esm-tools

The ``--user`` flag may be required when using ``pip``.

