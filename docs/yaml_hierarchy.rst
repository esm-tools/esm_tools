===================
YAML File Hierarchy
===================

Hierarchy of YAML configuration files
-------------------------------------

The following graph illustrates the hierarchy of the different YAML configuration files. 

.. graphviz:: graph/yaml_hierarchy.dot
    :name: yaml hierarchy
    :caption: ESM-Tools configuration files hierarchy
    :alt: ESM-Tools configuration files hierarchy
    :align: center

A parameter defined in, for example, a ``component`` will be overwritten by the same
parameter if it's defined in a ``setup`` or the ``runscript``.

How can I know where a parameter is defined?
--------------------------------------------

One of the first steps in every `ESM-Tools` operation is to load the configuration files
and merge their information following the hierarchy described above. During runtime,
this final dictionary is stored in a yaml file under the following possible paths:

* ``<base_dir>/<expid>/run_<DATE>/configs/<expid>_finished_config.yaml``
* ``<base_dir>/<expid>/configs/<expid>_finished_config.yaml``

One can use this file to check the final value of the desired parameter. The same
information can also be retriece by using inspect command
(:ref:`esm_runscripts:Arguments`):

.. code-block:: bash

    esm_runscripts <your_runscript> -e <expid> --inspect config

Additionally, these files/output also contain information about the provenance of the
value in the form of comments:

.. code-block:: yaml

    fesom:
        model: fesom  # /Users/mandresm/Codes/esm_tools/configs/components/fesom/fesom-2.0.yaml,line:4,col:8
        branch: 2.0.2 # /Users/mandresm/Codes/esm_tools/configs/components/fesom/fesom-2.0.yaml,line:17,col:13
        version: 2 # /Users/mandresm/Codes/esm_tools/configs/setups/awicm3/awicm3.yaml,line:399,col:18
        type: ocean # /Users/mandresm/Codes/esm_tools/configs/components/fesom/fesom-2.0.yaml,line:7,col:7
        comp_command: mkdir -p build; cd build; cmake -DOIFS_COUPLED=ON -DFESOM_COUPLED=ON -DCMAKE_INSTALL_PREFIX=../ ..;   make install -j `nproc --all` # /Users/mandresm/Codes/esm_tools/configs/setups/awicm3/awicm3.yaml,line:414,col:31
        clean_command: rm -rf build CMakeCache.txt # /Users/mandresm/Codes/esm_tools/configs/components/fesom/fesom-2.0.yaml,line:10,col:16
        required_plugins:
        - git+https://github.com/esm-tools-plugins/tar_binary_restarts  # /Users/mandresm/Codes/esm_tools/configs/components/fesom/fesom-2.0.yaml,line:13,col:3
        install_bins: bin/fesom.x  # /Users/mandresm/Codes/esm_tools/configs/components/fesom/fesom-2.0.yaml,line:22,col:19
