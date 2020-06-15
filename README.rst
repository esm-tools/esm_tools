=========
ESM Tools
=========

.. image:: https://readthedocs.org/projects/esm-tools/badge/?version=latest
:target: https://esm-tools.readthedocs.io/en/latest/?badge=latest
:alt: Documentation Status


Before you continue
-------------------

You will need python 3 (possibly version 3.5 or newer) and also a version of git that is not ancient (everything newer than 2.10 should be good) to install the `esm_tools`. That means that on the supported machines, you could for example use the following settings:

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

To use the new version of the esm-tools, now rewritten in Python, clone this repository::

    $ git clone https://github.com/esm-tools/esm_tools.git

Then, run the ``install.sh``::

    $ ./install.sh

You should now have the command line tools ``esm_master`` and ``esm_runscripts``, which replace the old version.

You may have to add the installation path to your ``PATH`` variable::

    $ export PATH=~/.local/bin:$PATH

CI-Status
---------

The following table shows automatic testing for compilation of various models
on Ollie. Automatic testing of actual runs is still under constructions.
Automatic testing for Mistral is not yet available.

+-------------------------------------------------------------------------------------------------------------------|
| Model Installation Ollie                                                                                          |
+===================================================================================================================|
| .. image:: https://github.com/esm-tools/esm_tools/workflows/CI-Ollie-install-awicm-1.0/badge.svg                  |
+-------------------------------------------------------------------------------------------------------------------+
| .. image:: https://github.com/esm-tools/esm_tools/workflows/CI-Ollie-install-awicm-2.0-esm-interface-yac/badge.svg|
+-------------------------------------------------------------------------------------------------------------------+
| .. image:: https://github.com/esm-tools/esm_tools/workflows/CI-Ollie-install-awicm-2.0-esm-interface/badge.svg    |
+-------------------------------------------------------------------------------------------------------------------+
| .. image:: https://github.com/esm-tools/esm_tools/workflows/CI-Ollie-install-awicm-2.0/badge.svg                  |
+-------------------------------------------------------------------------------------------------------------------+
| .. image:: https://github.com/esm-tools/esm_tools/workflows/CI-Ollie-install-awicm-3.0/badge.svg                  |
+-------------------------------------------------------------------------------------------------------------------+
| .. image:: https://github.com/esm-tools/esm_tools/workflows/CI-Ollie-install-awicm-3.1/badge.svg                  |
+-------------------------------------------------------------------------------------------------------------------+
| .. image:: https://github.com/esm-tools/esm_tools/workflows/CI-Ollie-install-awicm-CMIP6/badge.svg                |
+-------------------------------------------------------------------------------------------------------------------+
| .. image:: https://github.com/esm-tools/esm_tools/workflows/CI-Ollie-install-awicm-recom-1.0/badge.svg            |
+-------------------------------------------------------------------------------------------------------------------+
| .. image:: https://github.com/esm-tools/esm_tools/workflows/CI-Ollie-install-awicmcr-1.0/badge.svg                |
+-------------------------------------------------------------------------------------------------------------------+
| .. image:: https://github.com/esm-tools/esm_tools/workflows/CI-Ollie-install-awicmcr-2.0/badge.svg                |
+-------------------------------------------------------------------------------------------------------------------+
| .. image:: https://github.com/esm-tools/esm_tools/workflows/CI-Ollie-install-awicmcr-CMIP6/badge.svg              |
+-------------------------------------------------------------------------------------------------------------------+
| .. image:: https://github.com/esm-tools/esm_tools/workflows/CI-Ollie-install-awiesm-1.1/badge.svg                 |
+-------------------------------------------------------------------------------------------------------------------+
| .. image:: https://github.com/esm-tools/esm_tools/workflows/CI-Ollie-install-awiesm-1.2/badge.svg                 |
+-------------------------------------------------------------------------------------------------------------------+
| .. image:: https://github.com/esm-tools/esm_tools/workflows/CI-Ollie-install-awiesm-2.1/badge.svg                 |
+-------------------------------------------------------------------------------------------------------------------+
| .. image:: https://github.com/esm-tools/esm_tools/workflows/CI-Ollie-install-awiesm-2.2/badge.svg                 |
+-------------------------------------------------------------------------------------------------------------------+
| .. image:: https://github.com/esm-tools/esm_tools/workflows/CI-Ollie-install-fesom-recom-1.4/badge.svg            |
+-------------------------------------------------------------------------------------------------------------------+
| .. image:: https://github.com/esm-tools/esm_tools/workflows/CI-Ollie-install-fesom-recom-2.0/badge.svg            |
+-------------------------------------------------------------------------------------------------------------------+
| .. image:: https://github.com/esm-tools/esm_tools/workflows/CI-Ollie-install-foci-1.0/badge.svg                   |
+-------------------------------------------------------------------------------------------------------------------+
| .. image:: https://github.com/esm-tools/esm_tools/workflows/CI-Ollie-install-foci-default/badge.svg               |
+-------------------------------------------------------------------------------------------------------------------+
| .. image:: https://github.com/esm-tools/esm_tools/workflows/CI-Ollie-install-focioifs-2.0/badge.svg               |
+-------------------------------------------------------------------------------------------------------------------+
| .. image:: https://github.com/esm-tools/esm_tools/workflows/CI-Ollie-install-focioifs-agrif/badge.svg             |
+-------------------------------------------------------------------------------------------------------------------+
| .. image:: https://github.com/esm-tools/esm_tools/workflows/CI-Ollie-install-focioifs-vvl-o3-awi3/badge.svg       |
+-------------------------------------------------------------------------------------------------------------------+
| .. image:: https://github.com/esm-tools/esm_tools/workflows/CI-Ollie-install-focioifs-vvl-o3/badge.svg            |
+-------------------------------------------------------------------------------------------------------------------+
| .. image:: https://github.com/esm-tools/esm_tools/workflows/CI-Ollie-install-focioifs-vvl-o4/badge.svg            |
+-------------------------------------------------------------------------------------------------------------------+
| .. image:: https://github.com/esm-tools/esm_tools/workflows/CI-Ollie-install-focioifs-vvl/badge.svg               |
+-------------------------------------------------------------------------------------------------------------------+
| .. image:: https://github.com/esm-tools/esm_tools/workflows/CI-Ollie-install-mpiesm-1.2.00p4/badge.svg            |
+-------------------------------------------------------------------------------------------------------------------+
| .. image:: https://github.com/esm-tools/esm_tools/workflows/CI-Ollie-install-mpiesm-1.2.01/badge.svg              |
+-------------------------------------------------------------------------------------------------------------------+
| .. image:: https://github.com/esm-tools/esm_tools/workflows/CI-Ollie-install-mpiesm-1.2.01p1/badge.svg            |
+-------------------------------------------------------------------------------------------------------------------+
| .. image:: https://github.com/esm-tools/esm_tools/workflows/CI-Ollie-install-oifscl-40r1/badge.svg                |
+-------------------------------------------------------------------------------------------------------------------+
| .. image:: https://github.com/esm-tools/esm_tools/workflows/CI-Ollie-install-oifscl-43r3/badge.svg                |
+-------------------------------------------------------------------------------------------------------------------+
