.. highlight:: shell

=============
Get ESM-Tools
=============

Downloading
-----------

`ESM-Tools` is hosted on https://github.com/esm-tools/esm_tools. To get access to the software you need to be able to log into https://github.com.

Then you can start by cloning the repository ``esm_tools.git``. Type into a terminal of the HPC or computer of your choice the following command line::

$ git clone https://github.com/esm-tools/esm_tools.git

This will create a local repository of `ESM-Tools` in a folder named ``esm_tools``. This repository includes a collection of `yaml` configuration files containing all the information on models, coupled setups, machines, etc. in the subfolder ``config``, default namelists in the folder ``namelists``, example runscripts for a large number of models on different HPC systems in subfolder ``runscripts``, and this documention in ``docs``. Also you will find the installer ``install.sh`` used to install the python packages.

More information on the installation of `ESM-Tools` can be found in the section :ref:`installation:Installation` or also in the section :ref:`ten_steps:Ten Steps to a Running Model`.

Accessing components in DKRZ server
-----------------------------------

Some of the `ESM-Tools` components are hosted in the gitlab.dkrz.de servers. To be able to reach these components you will need:

1. A DKRZ account (https://www.dkrz.de/up/my-dkrz/getting-started/account/DKRZ-user-account).

2. Become a member of the group ``esm_tools``. Either look for the group and request membership, or directly contact dirk.barbi@awi.de.

3. Request access from the corresponding author of the component. Feel free to contact us if you don't know who the model developers are or check the :ref:`Supported_Models:Supported Models` section.



