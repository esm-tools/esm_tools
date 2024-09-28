Templating files with Jinja
===========================

``esm_runscripts`` supports ``jinja`` templating. This is desirable because some times
we might want that ``esm_runcripts`` "edits" some source files based on parameters
defined in the configuration yamls or in its current scope.

For example, in the following ``domain_def.xml`` file:

.. code-block:: xml

   <domain_definition>
   <!-- Definition of the native domain of the model -->
   <domain id="reduced_gaussian" long_name="reduced Gaussian grid" type="gaussian" />

   <!-- Definition of regular Gaussian domains -->
   <domain_group id="regular_domains" type="rectilinear" >
      <domain id="regular" long_name="regular grid" ni_glo="{{xios.ni_glo}}" nj_glo="{{xios.nj_glo}}" >
         <generate_rectilinear_domain />
         <interpolate_domain order="1" write_weight="true" />
      </domain>
   </domain_group>
   </domain_definition>

we want that ``ni_glo`` and ``nj_glo`` get the values from the parameters
``xios.ni_glo`` and ``xios.nj_glo``, defined in the
``esm_tools/configs/components/xios/xios.yaml``.

This is achieved in ``esm_runscripts`` by:
1. Having a source file where it's name finishes with ``.j2``, and that uses the syntax
   in the example above (including the ESM-Tools parameters to substitute within double
   curly braces ``{{ }}``), or using any other ``jinja`` syntax (read more about
   ``jinja`` syntax in the
   `jinja's documentation <https://jinja.palletsprojects.com/en/3.1.x/templates/>`_).
2. Including that file as a ``source`` in any of the configuration yamls involved in
   the simulation. For example, to use the
   ``namelists/oifs/43r3/xios/domain_def.xml.j2`` template to generate the
   ``domain_def.xml`` file, we would include it in the
   ``esm_tools/configs/components/xios/xios.yaml`` as follows (the same way we include
   any file that must be copied/moved/linked to the working directory, i.e.,
   :ref:`yaml:File Dictionary`):

   .. code-block:: yaml

      config_sources:
          domain_def: ${xml_dir}/domain_def.xml.j2

      config_in_work:
          domain_def: domain_def.xml

.. warning::
   If you don't name the file ending ``.j2``, it will be copied as is, without any
   substitution.

.. note::
   As with any other :ref:`yaml:File Dictionary`, you can ommit the target (in this case
   ``config_in_work.domain_def``) and the file will be copied to the target directory
   with the same name as the source file, except that for jinja files, the ``.j2`` will
   be removed.