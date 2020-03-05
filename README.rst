Before you continue
-------------------

You will need python 3 (possibly version 3.5 or newer) and also a version of git that is not ancient (everything newer than 2.10 should be good) to install the esm_tools. That means that on the supported machines, you could for example use the following settings:

ollie.awi.de::

    module load git
    module load python3

mistral.awi.de::

    module load git
    module load anaconda3

glogin.hlrn.de / blogin.hlrn.de::

    module load git
    module load anaconda3

juwels.fz-juelich.de::

    module load git
    module load Python-3.6.8



Installing
----------

To use the new version of the esm-tools, now rewritten in Python, clone this repository::

    git clone https://gitlab.awi.de/esm_tools/esm_tools.git

Then, run the ``install.sh``::

    ./install.sh

You should now have the command line tools ``esm_master`` and ``esm_runscripts``, which replace the old version.

You may have to add the installation path to your ``PATH`` variable::

    export PATH=~/.local/bin:$PATH


