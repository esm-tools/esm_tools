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

Configuration
-------------

If you have installed ``esm_tools`` you need to configure it before the first use to setup the hidden file ``$HOME/.esmtoolsrc`` correctly. This configuration will set required user information that are needed by both ``esm_master`` and ``esm_runscripts`` to work correctly. Such information are your user accounts on the different software repositories, your account on the machines you want to compute on, and some basic settings for the esm_runscripts.

To configure esm_master you should run the executable::

$ esm_master

Running it for the first time after installation, you will be asked to type in your user settings. This interactive configuration includes the following steps::

$ Please enter your username for gitlab.dkrz.de (default: anonymous)
$ Please enter your username for swrepo1.awi.de (default: anonymous)

Note that you will need to manually edit the file ``~/.esmtoolsrc``, if you mistakenly spelled any of the user names required for accessing the repositories, or you selected the default user name (``anonymous``).

Uninstall ESM-tools
-------------------

To uninstall your current installation of `ESM-Tools` you can use the following command::

$ esm_versions clean

You can also choose to manually uninstall. In order to do that, remove the installed Python packages and delete the ``esm_*`` executables. The following commands will do the trick if you installed with the ``install.sh`` script or installed using ``pip`` with ``user`` mode ::

$ rm -rf ~/.local/bin/esm*
$ rm -rf ~/.local/lib/python3.6/site-packages/esm*

Note that you may have a different Python version, so the second command might need to be adapted. You may also use ``pip`` to uninstall any of the packages::

$ pip uninstall [--user] esm-tools

The ``--user`` flag may be required when using ``pip``.

