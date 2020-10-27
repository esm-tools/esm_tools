Implement a New Model
=====================

**Feature available since version:** 4.2


1. Upload your model into a repository such us `gitlab.awi.de`, `gitlab.dkrz.de` or `github`.
   Make sure to set up the right access permissions, so that you comply with the licensing of
   the software you are uploading.

2. If you are interested in implementing more than one version of the model, we recommend you
   to commit them to the master branch in the order they were developed, and that you create
   a tag per version. For example:

   a. Clone the empty master branch you just created and add your model files to it::

      $ git clone https://<your_repository>
      $ cp -rf <your_model_files_for_given_version> <your_repository_folder>
      $ git add .

   b. Commit, tag the version and push the changes to your repository::

      $ git commit -m "your comment here"
      $ git tag -a <version_id> -m "your comment about the version"
      $ git push -u origin <your_master_branch>
      $ git push origin <version_id>

   c. Repeat steps `a` and `b` for all the versions that you would like to be present in
      ESM-Tools.

3. Now that you have your model in a repository you are ready to implement it into `esm_tools`.
   First, you will need to create your own branch of `esm_tools`, following the steps 1-4 in
   :ref:`contributing:Contribution to esm_tools Package`. The recommended name for the branch
   would be `feature/<name_of_your_model>`.

4. Then you will need to create a folder for your model inside ``esm_tools/configs/components``
   and create the model's `yaml` file::

    $ mkdir <PATH>/esm_tools/configs/<model>
    $ touch <PATH>/esm_tools/configs/<model>/<model>.yaml

5. Use your favourite text editor to open and edit your ``<model>.yaml`` in the
   ``esm_tools/configs/components/model`` folder::

    $ <your_text_editor> <PATH>/esm_tools/configs/components/<model>.yaml

6. Complete the following information about your model:

   .. code-block:: yaml

      # YOUR_MODEL YAML CONFIGURATION FILE
      #

      model: your_model_name
      type: type_of_your_model      # atmosphere, ocean, etc.
      version: "the_default_version_of_your_model"

7. Include the names of the different versions in the ``available_versions`` section and the compiling
   information for the default version:

   .. code-block:: yaml

      [...]

      available_versions:
      - "1.0.0"
      - "1.0.1"
      - "1.0.2"
      git-repository: "https://your_repository.git"
      branch: your_model_branch_in_your_repo
      install_bins: "path_to_the_binaries_after_comp"
      comp_command: "your_shell_commands_for_compiling"     # You can use the defaults "${defaults.comp_command}"
      clean_command: "your_shell_commands_for_cleaning"     # You can use the defaults "${defaults.clean_command}"

      executable: your_model_command

      setup_dir: "${model_dir}"
      bin_dir: "${setup_dir}/name_of_the_binary"

   In the ``install_bins`` key you need to indicate the path inside your model folder where the
   binaries are compiled to, so that `esm_master` can find them once compiled. The
   ``available_versions`` key is needed for `esm_master` to list the versions of your model.
   The ``comp_command`` key indicates the command needed to compile your model, and can be set as
   ``${defaults.comp_command}`` for a default command
   (``mkdir -p build; cd build; cmake ..;   make install -j `nproc --all```), or you can define your
   own list of compiling commands separated with ``;`` (``"command1; command2"``).

8. At this point you can choose between including all the version information inside the same
   ``<model>.yaml`` file, or to distribute this information among different version files:

   .. tabs::

      .. tab:: Single file

         In the ``<model>.yaml``, use a ``choose_`` switch (see :ref:`yaml:Switches (\`\`choose_\`\`)`)
         to modify the default information that you added in step 7 to meet the requirements for each
         specific version. For example, each different version has its own git branch:

         .. code-block:: yaml

            choose_version:
                    "1.0.0":
                            branch: "1.0.0"
                    "1.0.1":
                            branch: "1.0.1"
                    "1.0.2":
                            branch: "develop"

      .. tab:: Multiple version files

         a. Create a `yaml` file per version or group of versions. The name of these files should
            be the same as the ones in the ``available_versions`` section, in the main
            ``<model>.yaml`` file or, in the case of a file containing a group of versions, the
            shared name among the versions (i.e. ``fesom-2.0.yaml``)::

             $ touch <PATH>/esm_tools/configs/<model>/<model-version>.yaml

         b. Open the version file with your favourite editor and include the version specific
            changes. For example, you want that the version ``1.0.2`` from your model pulls from
            the ``develop`` git branch, instead of from the default branch. Then you add to the
            ``<model>-1.0.2.yaml`` version file:

            .. code-block:: yaml

               branch: "develop"

            Another example is the ``fesom-2.0.yaml``. While ``fesom.yaml`` needs to contain all
            ``available_versions``, the version specific changes are split among ``fesom.yaml``
            (including information about versions 1) and ``fesom-2.0.yaml`` (including 
            information about versions 2):

            .. tabs::

               .. tab:: fesom.yaml

                  .. code-block:: yaml

                     [ ... ]

                     available_versions:
                     - 2.0-o
                     - 2.0-esm-interface
                     - '1.4'
                     - '1.4-recom-mocsy-slp'
                     - 2.0-esm-interface-yac
                     - 2.0-paleodyn
                     - 1.4-recom-awicm
                     - '2.0'
                     - '2.0-r' # OG: temporarily here
                     choose_version:
                       '1.4-recom-awicm':
                         destination: fesom-1.4
                         branch: co2_coupling
                       '1.4-recom-mocsy-slp':
                         branch: fesom-recom-mocsy-slp
                         destination: fesom-1.4

                     [ ... ]

               .. tab:: fesom-2.0.yaml

                  .. code-block:: yaml

                     [ ... ]

                     choose_version:
                       '2.0':
                         branch: 2.0.2
                         git-repository:
                         - https://gitlab.dkrz.de/FESOM/fesom2.git
                         - github.com/FESOM/fesom2.git
                         install_bins: bin/fesom.x
                       2.0-esm-interface:
                         branch: fesom2_using_esm-interface
                         destination: fesom-2.0
                         git-repository:
                         - https://gitlab.dkrz.de/a270089/fesom-2.0_yac.git
                         install_bins: bin/fesom.x

                     [ ... ]


   .. note:: These are just examples of model configurations, but the parser used by `ESM-Tools`
      to read `yaml` files (`esm_parser`) allows for a lot of flexibility in their configuration;
      i.e., imagine that the different versions of your model are in different repositories,
      instead of in different branches, and their paths to the binaries are also different. Then
      you can include the ``git-repository`` and ``install_bins`` variables inside the corresponding
      version case for the ``choose_version``.

9. You can now check if `esm_master` can list and install your model correctly::

    $ esm_master

   This command should return, without errors, a list of available models and versions including yours.
   Then you can actually try installing your model in the desired folder::

    $ mkdir ~/model_codes
    $ cd ~/model_codes
    $ esm_master install-your_model-version

10. If everything works correctly you can check that your changes pass ``flake8``, commit your changes, push
    them to the ``origin`` and submit a pull request through GitHub (see steps 5-7 in
    :ref:`contributing:Contribution to esm_tools Package`).


.. note:: You can include all the compiling information inside a ``compile_infos`` section to avoid
   conflicts with other ``choose_version`` switches present in our configuration file.


See also
~~~~~~~~

.. links to relevant parts of the documentation

- :ref:`yaml:ESM-Tools Variables`
- :ref:`yaml:Switches (\`\`choose_\`\`)`
- :ref:`yaml:What Is YAML?`


Implement a New Coupled Setup
=============================

**Feature available since version:** 4.2


1. Make sure the models, couplers and versions you want to use, are already available for `esm_master`
   to install them (``$ esm_master`` and check the list). If something is missing you will need to
   add it following the instructions in :ref:`cookbook:Implementing a New Model`.

2. Once everything you need is available to `esm_master`, you will need to create your own branch of
   `esm_tools`, following the steps 1-4 in :ref:`contributing:Contribution to esm_tools Package`.

3. Then you will need to create a folder for your coupled setup inside ``esm_tools/configs/setups``
   folder, and create a `yaml` file per version of your setup::

    $ mkdir <PATH>/esm_tools/configs/setups/<your_setup>
    $ touch <PATH>/esm_tools/configs/setups/<your_setup>/<setup>.yamli

4. CONTINUE

   These `yaml` files need to exist for `esm_master` to download and compile your coupled setup, but
   they can be empty. However, you can choose to fill them with a basic configuration::

    # YOUR_SETUP YAML CONFIGURATION FILE
    #

    model: your_setup
    version: "your_setup_version"

4. Use your favourite text editor to open and edit ``setups2models.yaml`` in the ``configs/esm_master/``
   folder::

   $ <your_text_editor> <PATH>/esm_tools/configs/esm_master/setups2models.yaml

5. The information of your coupled setup should be placed inside the ``setups`` chapter of the file and
   be correctly aligned with the other setups. You can use the following example as a template::

    setups:
            [...]
            your_setup:
                         available_versions:
                                 - "1.0.0"
                                 - "1.0.1"

                         choose_version:
                                 "1.0.0":
                                         couplings:
                                                 - "model1-1.0+model2-1.0"
                                 "1.0.1":
                                         couplings:
                                                 - "model1-1.1+model2-1.1"

            [...]

   The ``available_versions`` key is needed for `esm_master` to label and list the versions of your setup
   when called without input (``$ esm_master``). The ``choose_version`` key relates version labels with
   their particular configurations. In this example, each version contains only the parameter
   ``couplings`` that consist of a label that points to a coupling configuration, contained in another
   chapter of ``setups2models.yaml``.

6. Now you need to include the different coupling configurations into the ``couplings`` chapter of the
   ``setups2models.yaml``. You can use the following example as a template::

    couplings:
            "model1-1.0+model2-1.0":
                    components:
                            - "model1-1.0"
                            - "model2-1.0"
                            - "coupler-version"
                    coupling_changes:
                            - "sed -i '/MODEL1_PARAMETER/s/OFF/ON/g' model1-1.0/file_to_change"
                            - "sed -i '/MODEL2_PARAMETER/s/OFF/ON/g' model2-1.0/file_to_change"

            [...]

   Remember to do this with all the couplings required from your setup versions in the ``setups``
   chapter. The ``components`` subsection should list the models and couplers used for the given coupling,
   including their required version, in the same way they are labelled in the ``components`` chapter of
   ``setups2models.yaml``. The ``coupling_changes`` subsection should include a list of commands to make
   the necessary changes in the component's make files, for a correct compilation of the coupled setup.

7. You can now check if `esm_master` can list and install your coupled setup correctly::

    $ esm_master

   This command should return, without errors, a list of available setups and versions including yours.
   Then you can actually try installing your setup in the desire folder::

    $ mkdir ~/model_codes
    $ cd ~/model_codes
    $ esm_master install-your_setup-version

8. If everything works correctly you can check that your changes pass `flake8`, commit your changes, push
   them to the ``origin`` and submit a pull request through GitHub (see steps 5-7 in
   :ref:`contributing:Contribution to esm_tools Package`).


See also
~~~~~~~~

.. links to relevant parts of the documentation

- :ref:`yaml:ESM-Tools Variables`
- :ref:`yaml:Switches (\`\`choose_\`\`)`
- :ref:`yaml:What Is YAML?`
