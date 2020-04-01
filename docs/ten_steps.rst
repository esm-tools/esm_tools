============================
Ten Steps to a Running Model
============================

1. Make sure you have git installed with version newer than 2.13, and python version 3.5 or later. Also make sure that the location to which the python binaries will be installed (which is ``~/.local/bin`` by default) is in your ``PATH``. You might want to add the following line to one of your login or profile files, e.g. ``~/.bashrc``, ``~/.profile``, etc.::

     $> export PATH=$PATH:~/.local/bin 

2. Get access to the ``esm_tools`` group on one of our mirrors, preferably the gitlab server at AWI. You can request access either online at ``https://gitlab.awi.de/esm_tools``, or via e-mail to ``dirk.barbi@awi.de``.

3. Download the git repository ``esm_tools.git``, preferably from the gitlab server at AWI::

     $> git clone https://gitlab.awi.de/esm_tools/esm_tools.git
 
4. In the new folder ``esm_tools``, run the installer::

     $> cd esm_tools
     $> ./install.sh

   This should install the python packages of ESM-Tools. If you wonder where they end up, take a look at ``~/.local/lib/python%versionnumber%/site-packages``.
   Also, a new file called ``~/.esmtoolsrc`` is added to your ``HOME``, which contains some very few details about the installation.

5. Run ``esm_master`` once and answer the questions to setup the tool completely. You should see a long list of available targets if everything works.
   
6. Go to the toplevel folder into which you want to install you model codes, and run ``esm_master install-``, followed by the name and the version of the model you want to install. As an example, if we want to run FESOM2::

    $> mkdir ../model_codes
    $> cd ../model_codes 
    $> esm_master install-fesom-2.0

  You will be asked for your password to the repository of the model you are trying to install. If you don't have access to that repo yet, ``esm_master`` will not be able to install the model; you will have to contact the model developers to be granted access. Feel free to contact us if you don't know who the model developers are.

7. Check if the installation process worked; if so, you should find the model executable in the subfolder ``bin`` of the model folder. E.g.::

    $> ls fesom-2.0/bin

8. Go back to the ``esm_tools`` folder, and pick a sample runscript from the ``runscripts`` subfolder. These examples are very short and can be easily adapted. Pick one that is for the model you want to run, and maybe already adapted to the HPC system you are working on. Make sure to adapt the paths to your personal settings, e.g. ``model_dir``, ``base_dir`` etc.::

    $> cd ../esm_tools/runscripts/fesom2
    $> (your_favourite_editor) fesom2-ollie-initial-monthly.yaml

   Notice that the examples exist with the endings ``.run`` and ``.yaml``. It doesn't matter what you pick. The files ending in ``.run`` are looking more like conventional shell scripts that you might be better used to, the ``.yaml``-files are written as yaml configuration files, which makes things much nicer and more elegant to write down. We strongly encourage you to give the ``yaml``-version a try.

9. Run a check of the simulation to see if all needed files are found, and everything works as expected::

    $> esm_runscripts fesom2-ollie-initial-monthly.yaml -e my_first_test -c

   The command line option ``-c`` specifies that this is a check run, which means that all the preparations, file system operations, ... are performed as for a normal simulation, but then the simulation will stop before actually submitting itself to the compute nodes and executing the experiment. You will see a ton of output on the screen that you should check for correctness before continuing, this includes:

      * information about missing files that could not be copied to the experiment folder
      * namelists that will be used during the run
      * the miniature ``.sad`` script that is submitted the compute nodes, which also shows the environment that will be used

   You can also check directly if the job folder looks like expected. You can find it at ``$BASE_DIR/$EXP_ID`/run_xxxxxxxxxxx``, where ``BASE_DIR`` was set in your runscript, ``EXP_ID``   (probably) on the command line, and ``run_xxxxxxxxxxxxx`` stands for the first chunk of your chain job. You can check the work folder, which is located at ``$BASE_DIR/$EXP_ID/run_xxxxxxxxxxxx/work``, as well as the complete configuration used to generate the simulation, located at ``$BASE_DIR/$EXP_ID/run_xxxxxxxxxxxx/log``.

10. Run the experiment::

     $> esm_runscripts fesom2-ollie-initial-monthly.yaml -e my_first_test

That should really be it. Good luck!   

