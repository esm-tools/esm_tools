.. highlight:: shell

============
Installation
============


Downloading
-----------

``esm_tools`` is hosted on https://gitlab.awi.de, with a mirror on https://gitlab.dkrz.de . To get access to the software, you need to be able to login to one of these two servers. 

gitlab.awi.de:
        - DMAWI login: open to all employees of the Alfred Wegener Helmholtz Institute for Polar and Climate Research.   Make sure the DMAWI tab is active, then use your normal AWI LDAP login and password to sign in.
        - Shibboleth Login: open to employees of member organizations of the DFN-AAI (see https://tools.aai.dfn.de/entities/ for a list of participating identity providers). Click "Federated Login" on the right side, and choose your organization from the list. User your affiliations username and password to identify yourself, and navigate through the upcoming questionaire. You should then be directed to the server gitlab.awi.de. Please notice that you are asked to choose a password for this server, so that from that moment on you can use your e-mail address and the new password for login / git access. Shibboleth login should work for hundreds of institues, including e.g. DKRZ and GEOMAR.
       
gitlab.dkrz.de: 
        Open for everyone with a DKRZ account, and activated gitlab acess in the project page.

If you encounter any problems with getting access to gitlab.awi.de, please feel free to contact dirk.barbi@awi.de.


Once you can access the server, you will need to become a member of the group ``esm_tools``. Either look for the group and request membership, or directly contact dirk.barbi@awi.de.

Now that you have access to one of the download servers, and to the ``esm_tools`` group, you can start by cloning the repository ``esm_tools.git``::

$> git clone https://gitlab.awi.de/esm_tools/esm_tools.git

This gives you a collection of yaml configuration files containing all the information on models, coupled setups, machines etc. in the subfolder ``config``, default namelists in the folder ``namelists``, example runscripts for a large number of models on different HPC systems in subfolder ``runscripts``, and this documention in ``docs``. Also you will find the installer ``install.sh`` used to install the python packages.

.. include:: ../README.rst

Configuration
-------------

If you have installed ``esm_tools`` you need to configure ìt before the first use to setup the hidden file ``$HOME/.esmtoolsrc`` correctly. This configuration will set required user information that are needed by both ``esm-master`` and ``esm_runscripts`` to work correctly. Such information are your user accounts on the different software repositories, your account on the machines you want to compute on, and some basic settings for the esm-runscripts.

To configure esm-master you should run the executable::

$> ./esm_master

Running it for the first time after installation, you will be asked to type in your user settings. This interactive configuration includes the following steps::

$> Please enter your username for gitlab.dkrz.de (default: anonymous)
$> Please enter your username for swrepo1.awi.de (default: anonymous)
