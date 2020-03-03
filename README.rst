==========
ESM MASTER
==========

ESM Master tool for downloading, configuring and compiling of earth system model components and coupled setups

* Free software: GNU General Public License v2

* Download, configure, and compile various Earth System Models

Installing
----------

Please see the instructions for installation here: https://gitlab.awi.de/esm_tools/esm_tools

Example
-------

To use the command line tool ``esm_master``, just enter at a prompt::

    esm_master

The tool may ask you to configure your settings; which are stored in your home folder under ``${HOME}/.esmtoolsrc.`` A list of avaiable commands and models are printed to the screen. To download, compile, and install awicm-2.0; you can say::

    esm_master install-awicm-2.0

This will trigger a download, if needed a configuration, and a compilation process. Similarly, you can recompile with ``recomp-XXX``, clean with ``clean-XXX``, or do individual steps, e.g. get, configure, comp.
The download and installation will always occur in the current working directory.
You can get further help with::

    esm_master --help

Configuration
-------------

The ``esm_master`` tool is configured via a YAML file, which can be found under your ``esm_tools`` directory: ``configs/esm_master``. This contains two files:

* ``setups2models.yaml``

* ``esm_master.yaml``

The first file stores the configuration for where to download specific versions of each Earth System Model, while the second configurations which commands the ``esm_master`` binary provides.

Credits
-------

This package was created with Cookiecutter_ and the `audreyr/cookiecutter-pypackage`_ project template.

.. _Cookiecutter: https://github.com/audreyr/cookiecutter
.. _`audreyr/cookiecutter-pypackage`: https://github.com/audreyr/cookiecutter-pypackage
