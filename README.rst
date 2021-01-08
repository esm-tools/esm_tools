=========
ESM Tools
=========

.. image:: https://readthedocs.org/projects/esm-tools/badge/?version=latest

For our complete documentation, please check https://esm-tools.readthedocs.io/en/latest/index.html.


Before you continue
-------------------

You will need python 3 (possibly version 3.6 or newer), a version of git that is not ancient (everything newer than 2.10 should be good), and up-to-date pip (``pip install -U pip``) to install the `esm_tools`. That means that on the supported machines, you could for example use the following settings:

ollie.awi.de::

    $ module load git
    $ module load python3

mistral.awi.de::

    $ module load git
    $ module load anaconda3

glogin.hlrn.de / blogin.hlrn.de::

    $ module load git
    $ module load anaconda3

juwels.fz-juelich.de::

    $ module load git
    $ module load Python-3.6.8

Note that some machines might raise an error ``conflict netcdf_c`` when loading ``anaconda3``. In that case you will need to swap ``netcdf_c`` with ``anaconda3``::

    $ module swap netcdf_c anaconda3



Installing
----------

First, make sure you add the following lines to one of your login or profile files, i.e. ``~/.bash_profile``, ``~/.bashrc``, ``~/.profile``, etc.::

    $ export PATH=$PATH:~/.local/bin
    $ export LC_ALL=en_US.UTF-8
    $ export LANG=en_US.UTF-8

To use the new version of the esm-tools, now rewritten in Python, clone this repository::

    $ git clone https://github.com/esm-tools/esm_tools.git

Then, run the ``install.sh``::

    $ ./install.sh

You should now have the command line tools ``esm_master`` and ``esm_runscripts``, which replace the old version.

You may have to add the installation path to your ``PATH`` variable::

    $ export PATH=~/.local/bin:$PATH
