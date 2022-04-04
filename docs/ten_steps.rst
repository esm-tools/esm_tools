============================
Ten Steps to a Running Model
============================

1. Make sure you have git installed with version newer than 2.13, that the python version is 3.6 or later (see :ref:`installation:Before you continue`), and that pip is up-to-date (``pip install -U pip``). Also make sure that the location to which the python binaries will be installed (which is ``~/.local/bin`` by default) is in your ``PATH``. For that purpose, add the following lines to one of your login or profile files, i.e. ``~/.bash_profile``, ``~/.bashrc``, ``~/.profile``, etc.::

    $ export PATH=$PATH:~/.local/bin
    $ export LC_ALL=en_US.UTF-8
    $ export LANG=en_US.UTF-8


2. Make sure you have a GitHub account and check our GitHub repository (``https://github.com/esm-tools``).

3. Download the git repository ``esm_tools.git`` from GitHub::

     $ git clone https://github.com/esm-tools/esm_tools.git

4. In the new folder ``esm_tools``, run the installer::

     $ cd esm_tools
     $ ./install.sh

   This should install the python packages of ESM-Tools. If you wonder where they end up, take a look at ``~/.local/lib/python%versionnumber%/site-packages``.

5. Go to the toplevel folder into which you want to install your model codes (commonly used locations are ``$HOME/model_codes/`` as in the example below or ``$HOME/esm/models/``), and run ``esm_master install-``, followed by the name and the version of the model you want to install:: 

    $ mkdir $HOME/model_codes/
    $ cd $HOME/model_codes/

6. Now download and compile your model code. As an example, if we want to install FESOM2::

    $ esm_master install-fesom-2.0

You will be asked for your password to the repository of the model you are trying to install. If you don't have access to that repo yet, ``esm_master`` will not be able to install the model; you will have to contact the model developers to be granted access. Feel free to contact us if you don't know who the model developers are.

7. Check if the installation process worked; if so, you should find the model executable in the subfolder ``bin`` of the model folder. E.g.::

    $ ls fesom-2.0/bin

8. Go back to the ``esm_tools`` folder, and pick a sample runscript from the ``runscripts`` subfolder. These examples are very short and can be easily adapted. Pick one that is for the model you want to run, and maybe already adapted to the HPC system you are working on. Make sure to adapt the paths to your personal settings, e.g. ``model_dir``, ``base_dir`` etc.::

    $ cd /path/to/esm_tools/runscripts/fesom2
    $ (your_favourite_editor) fesom2-ollie-initial-monthly.yaml

9. Run a check of the simulation to see if all needed files are found, and everything works as expected::

    $ esm_runscripts fesom2-ollie-initial-monthly.yaml -e my_first_test -c --open-run

The command line option ``-c`` specifies that this is a check run, which means that all the preparations, file system operations, ... are performed as for a normal simulation, but then the simulation will stop before actually submitting itself to the compute nodes and executing the experiment. You will see a ton of output on the screen that you should check for correctness before continuing, this includes:

      * information about missing files that could not be copied to the experiment folder
      * namelists that will be used during the run
      * the miniature ``.run`` script that is submitted the compute nodes, which also shows the environment that will be used

All the data that is used in the current leg of the run is copied to / linked to ``$BASE_DIR/$EXP_ID/run_xxxxxxxxxxx`` (e.g. ``$BASE_DIR/$EXP_ID/run_20220101-20221231`` for a run covering a one year leg lasting from January 2022 to December 2022).

    * work directory: ``$BASE_DIR/$EXP_ID/run_20220101-20221231/work/``
    * log files: ``$BASE_DIR/$EXP_ID/log/``
    * minimal runscript: ``$BASE_DIR/$EXP_ID/run_20220101-20221231/scripts/``
    
ESM-Tools directory structure is unfortunately still quite complicated. Please take look at the subpage **Understanding ESM-Tools directory structure**
before running a longer experiment.
10. Run the experiment::

     $ esm_runscripts fesom2-ollie-initial-monthly.yaml -e my_first_test

That should really be it. Good luck!
