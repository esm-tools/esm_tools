.. highlight:: shell

============
Contributing
============

Contributions are welcome, and they are greatly appreciated! Every little bit
helps, and credit will always be given.

You can contribute in many ways:

Types of Contributions
----------------------

Report Bugs
~~~~~~~~~~~

Report bugs at https://github.com/esm-tools/esm_tools/issues.

If you are reporting a bug, please include:

* Your operating system name and version.
* Any details about your local setup that might be helpful in troubleshooting.
* Detailed steps to reproduce the bug.

Fix Bugs
~~~~~~~~

Look through the GitHub issues for bugs. Anything tagged with "bug" and "help
wanted" is open to whoever wants to implement it.

Implement Features
~~~~~~~~~~~~~~~~~~

Look through the GitHub issues for features. Anything tagged with "enhancement"
and "help wanted" is open to whoever wants to implement it.

Write Documentation
~~~~~~~~~~~~~~~~~~~

ESM Tools could always use more documentation, whether as part of the
official ESM Tools docs, in docstrings, or even on the web in blog posts,
articles, and such.

Submit Feedback
~~~~~~~~~~~~~~~

The best way to send feedback is to file an issue at https://github.com/esm-tools/esm_tools/issues.

If you are proposing a feature:

* Explain in detail how it would work.
* Keep the scope as narrow as possible, to make it easier to implement.
* Remember that this is a volunteer-driven project, and that contributions
  are welcome :)

Get Started!
------------

Ready to contribute? Here's how to set up `esm-tools` packages for local development (see :ref:`packages:Python Packages` for a list of available packages). Note that the procedure of contributing to the `esm_tools` package (see :ref:`contributing:Contribution to esm_tools Package`) is different from the one to contribute to the other packages (:ref:`contributing:Contribution to other Packages`).

Contribution to esm_tools Package
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

1. Fork the `esm_tools` repo on GitHub.
2. Clone your fork locally::

    $> git clone https://github.com/your_github_username_here/esm_tools.git

   (or whatever subproject you want to contribute to).

3. By default, ``git clone`` will give you the release branch of the project. You might want to consider checking out the development branch, which might not always be as stable, but usually more up-to-date than the release branch::

    $> git checkout develop

4. Create a branch for local development::

    $> git checkout -b name-of-your-bugfix-or-feature

   Now you can make your changes locally.

5. When you're done making changes, check that your changes pass flake8::

    $> flake8 esm_tools

6. Commit your changes and push your branch to GitHub::

    $> git add .
    $> git commit -m "Your detailed description of your changes."
    $> git push origin name-of-your-bugfix-or-feature

7. Submit a pull request through the GitHub website.

Contribution to Other Packages
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

1. Follow steps 1-4 in :ref:`contributing:Contribution to esm_tools Package`
   for the desired package, cloning your fork locally with::

   $> git clone https://github.com/your_github_username_here/<PACKAGE>.git

2. Proceed to do a development install of the package in the package's folder::

   $> cd <package's_folder>
   $> pip install -e .

3. From now on when binaries are called, they will refer to the source code you are working
   on, located in your local package's folder. For example, if you are editing the 
   package `esm_master` located in ``~/esm_master`` and you run ``$> esm_master install-fesom-2.0``
   you'll be using the edited files in ``~/esm_master`` to install FESOM 2.0.

4. Follow steps 5-7 in :ref:`contributing:Contribution to esm_tools Package`.

Implementing a New Model
------------------------

1. Upload your model into a repository such us `gitlab.awi.de`, `gitlab.dkrz.de` or `github`.
   Make sure to set up the right access permissions, so that you comply with the licensing of
   the software you are uploading.

2. If you are interested in implementing more than one version of the model, we recommend you
   to commit them to the master branch chronologically, and that you create a tag per version.
   For example:

   a. Clone the empty master branch you just created and add your model files to it::

      $> git clone https://<your_repository>
      $> cp -rf <your_model_files_for_given_version> <your_repository_folder>
      $> git add .

   b. Commit, tag the version and push the changes to your repository::

      $> git commit -m "your comment here"
      $> git tag -a <version_id> -m "your comment about the version"
      $> git push -u origin <your_master_branch>
      $> git push origin <version_id>

   c. Repeat steps `a` and `b` for all the versions that you would like to be present in
      ESM-Tools.

3. Now that you have your model in a repository you are ready to implement it into `esm_tools`.
   First, you will need to create your own branch of `esm_tools`, following the steps 1-4 in
   :ref:`contributing:Contribution to esm_tools Package`.

4. Then you will need to create a folder for your model inside the ``configs`` folder, and create
   a `yaml` file per version of your model::

    $> mkdir <PATH>/esm_tools/configs/<model>
    $> touch <PATH>/esm_tools/configs/<model>/<model-version>.yaml

   These `yaml` files need to exist for `esm_master` to download and compile your model, but they can
   be empty. However, you can choose to fill them with a basic configuration::

    # YOUR_MODEL YAML CONFIGURATION FILE
    #

    model: your_model
    branch: your_model_branch_in_your_repo
    version: "the_version_of_your_model"

    comp_executable: your_model_bin
    executable: your_model_command

    setup_dir: "${model_dir}"
    bin_dir: "${setup_dir}/the_location_of_your_model_bin"

5. Use your favourite text editor to open and edit ``setups2models.yaml`` in the ``configs/esm_master/``
   folder::

   $> <your_text_editor> <PATH>/esm_tools/configs/esm_master/setups2models.yaml

6. The information of your model should be placed inside the `components` chapter of the file and
   be correctly aligned with the other components. You can use the following template as an
   and example::

    components:
            [...]
            "your_model":
                    install_bins: "path_to_the_binaries"
                    git-repository: "https://your_repository.git"
                    choose_version:
                           "1.0.0":
                                   branch: "1.0.0"
                           "1.0.1":
                                   branch: "1.0.1"
                           "1.0.2":
                                   branch: "develop"
                    available_versions:
                           - "1.0.0"
                           - "1.0.1"
                           - "1.0.2"
                    comp_command: "your_commands_for_compiling"
                    clean_command: "${defaults.clean_command}"

            [...]

   In the ``install_bins`` key you need to indicate the path inside your model folder where the
   binaries are compiled to, so that `esm_master` can find them once compiled. The ``choose_version``
   key relates version labels with their particular configurations, in this case only the ``branch``
   (or tag) where they are located in your repository. The ``available_versions`` key is needed for
   `esm_master` to list the versions of your model when called without input (``$> esm_master``).
   The ``comp_command`` key indicates the command needed to compile your model, and can be set as
   ``${defaults.comp_command}`` for a predefined command 
   (``mkdir -p build; cd build; cmake ..;   make install -j `nproc --all```), or you can define your
   own list of compiling commands separated with ``;``. Note that this is just an example of a model
   configuration, but the parser used by `esm_tools` to read `yaml` files (`esm_parser`) allows for a
   lot of flexibility in their configuration; i.e., imagine that the different versions of your models
   are in different repositories, instead of in different branches, and their path to the binaries
   are also different. Then you can remove the ``git-repository`` and ``install_bins`` sections from
   the general model section (``"your_model"``), and place their particular configuration in their 
   corresponding version inside the ``choose_version`` section.

7. You can now check if `esm_master` can list and install your model correctly::

    $> esm_master

   This command should return, without errors, a list of available models and versions including yours.
   Then you can actually try installing your model::

    $> esm_master install-your_model-version

8. If everything works correctly you can check that your changes pass `flake8`, commit your changes, push
   them to the ``origin`` and submit a pull request through GitHub (see steps 5-7 in 
   :ref:`contributing:Contribution to esm_tools Package`).
 
Pull Request Guidelines
-----------------------

Before you submit a pull request, check that it meets these guidelines:

1. The pull request should include tests.

2. If the pull request adds functionality, the docs should be updated. Put
   your new functionality into a function with a docstring, and add the
   feature to the list in README.rst.

3. The pull request should work for Python 3.5, 3.6, 3.7 and 3.8, and for PyPy. Check
   https://travis-ci.com/dbarbi/esm_tools/pull_requests
   and make sure that the tests pass for all supported Python versions.

Deploying
---------

A reminder for the maintainers on how to deploy.
Make sure all your changes are committed (including an entry in HISTORY.rst).
Then run::

$> bumpversion patch # possible: major / minor / patch
$> git push
$> git push --tags

