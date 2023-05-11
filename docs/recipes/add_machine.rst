Implement a New HPC Machine
===========================

To implement a new HPC machine to esm-tools, two files need to be updated and created, respectvily::
   
   - <PATH>/esm_tools/configs/machines/all_machines.yaml
   - <PATH>/esm_tools/configs/machines/<new_machine>.yaml

1. Add an additional entry for the new machine.

   Use your favourite text editor and open the file ``<PATH>/esm_tools/configs/machines/all_machines.yaml``::

    $ <your_text_editor> <PATH>/esm_tools/configs/machines/all_machines.yaml

   and add a new entry for the new machine (replace placeholders indicated by <...>)

   .. code-block:: yaml

     <new_machine>:
        login_nodes: '<hostname>*'
        compute_nodes: '<compute_notes>'


2. Create a new machine file.

   Use your favourite text editor to create and edit a new machine file ``<new_machine>.yaml`` in the
   ``esm_tools/configs/machines/`` folder::

    $ <your_text_editor> <PATH>/esm_tools/configs/machines/<new_machine>.yaml

   or copy the ``machine_template.yaml`` file and edit the relevant entries::

    $ cp <PATH>/esm_tools/configs/templates/machine_template.yaml  <PATH>/esm_tools/configs/machines/<new_machine>.yaml
    $ <your_text_editor> <PATH>/esm_tools/configs/machines/<new_machine>.yaml


See also
~~~~~~~~

.. links to relevant parts of the documentation

- :ref:`esm_variables:ESM-Tools Variables`
- :ref:`yaml:Switches (\`\`choose_\`\`)`
- :ref:`yaml:What Is YAML?`
