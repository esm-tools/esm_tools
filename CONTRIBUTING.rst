.. highlight:: shell

============
Contributing
============

Contributions are welcome, and they are greatly appreciated! Every little bit
helps, and credit will always be given.

You can contribute in many ways:

Types of Contributions
======================

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
============

Ready to contribute? Here's how to set up `esm-tools` packages for local development (see :ref:`packages:Python Packages` for a list of available packages). Note that the procedure of contributing to the `esm_tools` package (see :ref:`contributing:Contribution to esm_tools Package`) is different from the one to contribute to the other packages (:ref:`contributing:Contribution to other Packages`).

Contribution to esm_tools Package
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

1. Fork the `esm_tools` repo on GitHub.
2. Clone your fork locally::

    $ git clone https://github.com/esm-tools/esm_tools.git

   (or whatever subproject you want to contribute to).

3. By default, ``git clone`` will give you the release branch of the project. You might want to consider checking out the development branch, which might not always be as stable, but usually more up-to-date than the release branch::

    $ git checkout develop

4. Create a branch for local development::

    $ git checkout -b name-of-your-bugfix-or-feature

   Now you can make your changes locally.

5. When you're done making changes, check that your changes pass flake8::

    $ flake8 esm_tools

6. Commit your changes and push your branch to GitHub::

    $ git add .
    $ git commit -m "Your detailed description of your changes."
    $ git push origin name-of-your-bugfix-or-feature

7. Submit a pull request through the GitHub website.

Contribution to Other Packages
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

1. Follow steps 1-4 in :ref:`contributing:Contribution to esm_tools Package`
   for the desired package, cloning your fork locally with::

   $ git clone https://github.com/esm-tools/<PACKAGE>.git

2. Proceed to do a development install of the package in the package's folder::

   $ cd <package´s_folder>
   $ pip install -e .

3. From now on when binaries are called, they will refer to the source code you are working
   on, located in your local package's folder. For example, if you are editing the
   package `esm_master` located in ``~/esm_master`` and you run ``$ esm_master install-fesom-2.0``
   you'll be using the edited files in ``~/esm_master`` to install FESOM 2.0.

4. Follow steps 5-7 in :ref:`contributing:Contribution to esm_tools Package`.

Get Back to the Standard Distribution
-------------------------------------

Once finished with the contribution, you might want to get back to the standard
non-editable mode version of the package in the ``release`` branch. To do that
please follow these steps:

1. Uninstall all `ESM-Tools` packages (:ref:`Installation:Uninstall ESM-Tools`). This
   will not remove the folder where you installed the package in editable mode, just
   delete the links to that folder.

2. Navigate to the ``esm_tools`` folder and run the ``./install.sh`` script.

3. Check that your package is now installed in the folder
   ``~/.local/lib/python3.<version>/site-packages/``.

.. Note:: If the package is still shows the path to the editable-mode folder, try
   running ``pip install --use-feature=in-tree-build .`` from ``esm_tools``.

Pull Request Guidelines
=======================

Before you submit a pull request, check that it meets these guidelines:

1. The pull request should include tests.

2. If the pull request adds functionality, the docs should be updated. Put
   your new functionality into a function with a docstring, and add the
   feature to the list in README.rst.

3. The pull request should work for Python 3.5, 3.6, 3.7 and 3.8, and for PyPy. Check
   https://travis-ci.com/dbarbi/esm_tools/pull_requests
   and make sure that the tests pass for all supported Python versions.

Deploying
=========

A reminder for the maintainers on how to deploy.
Make sure all your changes are committed (including an entry in HISTORY.rst).
Then run::

$ bumpversion patch # possible: major / minor / patch
$ git push
$ git push --tags

