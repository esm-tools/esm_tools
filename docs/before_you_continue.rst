===================
Before you continue
===================

You will need python 3 (possibly version 3.6 or newer), a version of git that is not ancient (everything newer than 2.10 should be good), and up-to-date pip (``pip install -U pip``) to install the `ESM-Tools`. That means that on the supported machines, you could for example use the following settings:

..
    ollie.awi.de::

        $ module load git
        $ module load python3

+-------------------------------+-----------------------------------+
| machine                       | Settings                          |
+===============================+===================================+
| albedo                        | .. code-block:: bash              |
|                               |                                   |
|                               |    $ module load git              |
|                               |    $ module load python3          |
+-------------------------------+-----------------------------------+
| levante.dkrz.de               | .. code-block:: bash              |
|                               |                                   |
|                               |    $ module load git              |
|                               |    $ module load python3          |
+-------------------------------+-----------------------------------+
| glogin.hlrn.de/blogin.hlrn.de | .. code-block:: bash              |
|                               |                                   |
|                               |    $ module load git              |
|                               |    $ module load anaconda3        |
+-------------------------------+-----------------------------------+
| juwels.fz-juelich.de          | .. code-block:: bash              |
|                               |                                   |
|                               |    $ module load Stages/2022      |
|                               |    $ module load git              |
|                               |    $ module load Python/3.9.6     |
+-------------------------------+-----------------------------------+
| aleph                         | .. code-block:: bash              |
|                               |                                   |
|                               |    $ module load git              |
|                               |    $ module load python           |
+-------------------------------+-----------------------------------+

.. note::
   Note that some machines might raise an error ``conflict netcdf_c`` when loading ``anaconda3``. In that case you will need to swap ``netcdf_c`` with ``anaconda3``::

    $ module unload netcdf_c
    $ module load anaconda3
