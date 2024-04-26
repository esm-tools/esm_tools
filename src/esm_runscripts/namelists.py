"""
``esm-runscripts`` Core Plugins for dealing with Fortran Namelists.

Provides plugins for loading, modifying, deleting, and writing Fortran
Namelists as part of the ``esm-runscripts`` recipe. All plugins are found under
the class Namelist as static methods. A deprecated class ``namelist`` (small "n") is
provided, which warns you when it is used.
"""
import logging
import os
import sys
import warnings

import f90nml

from esm_parser import user_error


class Namelist:
    """Methods for dealing with FORTRAN namelists"""

    @staticmethod
    def nmls_load(mconfig):
        """
        Loads Fortran namelists into the configuration dictionary.

        User Information
        ----------------
        To associate namelists with a specific model, you should have a section
        in your configuration that lists the namelists::

            fesom:
                namelists:
                    - "namelist.config"
                    - "namelist.oce"
                    - "namelist.ice"
                    - "namelist.diag"

        Programmer Information
        ----------------------
        The namelists are represented by f90nml Namelist objects, and are
        stored under::

            mconfig["namelists"]["namelist.echam"]``

        This would point to the ECHAM namelist as a f90nml object, which
        closely resembles a dictionary.

        The actual namelists to load are listed in the raw configuration as a
        list of strings::

            mconfig['namelists'] = ['nml1', 'nml2', 'nml3', ...]

        Namelists are assumed to have been copied to
        ``mconfig["thisrun_config_dir"]``, and are loaded from there.

        If the ``mconfig`` has a key ``"namelist_case"`` equal to "uppercase",
        the uppercase attribute of the f90nml representation of the namelist is
        set to ``True``.

        Parameters
        ----------
        mconfig : dict
            The model (e.g. ECHAM, FESOM, NEMO or OIFS) configuration

        Returns
        -------
        mconfig : dict
            The modified configuration.
        """
        nmls = mconfig.get("namelists", [])
        mconfig["namelists"] = dict.fromkeys(nmls)
        for nml in nmls:
            if os.path.isfile(os.path.join(mconfig["thisrun_config_dir"], nml)):
                logging.debug("Loading %s", nml)
                try:
                    mconfig["namelists"][nml] = f90nml.read(
                        os.path.join(mconfig["thisrun_config_dir"], nml)
                    )
                except (StopIteration, ValueError) as e:
                    user_error(
                        "Namelist format",
                        f"The namelist ``{nml}`` has a formatting issue. Please, "
                        f"fix it before proceeding.\n\n``Hint:`` {e}",
                        dsymbols=["``", "'"],
                    )
            else:
                mconfig["namelists"][nml] = f90nml.namelist.Namelist()
            if mconfig.get("namelist_case") == "uppercase":
                mconfig["namelists"][nml].uppercase = True
        return mconfig

    @staticmethod
    def nmls_check_changes(namelist_changes):
        """
        Checks if namelist changes are given in correct syntax.
        If not, a user_error will be raised and stop the execution.

        Programmer Information
        ----------------------

        Parameters
        ----------
        namelist_changes : nested dict

        If the syntax is correct, namelist_changes should be a nested dict of the following form::

        {'namelist1.nml': {'namelist1': {'variable1': 'value1', 'variable2': 'value2', 'variable3': 'value3'}, 'namelist2': {'variable1': value1}}}

        Returns
        -------
        None

        """

        error_message = (
            "There is a syntax error, probably in your runscript (but potentially in other yaml files), "
            "regarding namelist changes (e.g. in a 'add_namelist_changes' block). "
            "It seems that either 'namelist_file' or "
            "'namelist_group' or 'both' are missing.\n"
            "Please make sure that namelist changes are specified in the correct syntax (see example below)"
            " and rerun your runscript.\n"
        )
        example = (
            "\nExample of a ``correct syntax`` for [add_]namelist_changes:\n"
            "\t [add_]namelist_changes:\n"
            "\t    '<namelist_file>':\n"
            "\t        '<namelist_group>':\n"
            "\t            <variable>: <value>"
        )

        nml_syntax_error = False

        for namelist in list(namelist_changes):
            changes = namelist_changes[namelist]
            # Check if namelist_changes are specified in correct syntax (e.g. in runscript)
            # If correct syntax, changes is always a dict.
            if not isinstance(changes, dict):
                nml_syntax_error = True
                this_is_wrong = (f"There is a syntax error in the following lines:\n\n[add_]namelist_changes:\n    '{namelist}: {changes}'\n...")
                break
            else:
                for change_chapter in list(changes):
                    change_entries = changes[change_chapter]
                    # Check if namelist_changes are specified in correct syntax (e.g. in runscript)
                    # If correct syntax, change_entries is always a dict.
                    if not isinstance(change_entries, dict):
                        nml_syntax_error = True
                        this_is_wrong = (
                            f"There is a syntax error in the following lines:\n\n[add_]namelist_changes:\n    '{namelist}':\n        '{change_chapter}: {change_entries}'\n...")
                        break
        if nml_syntax_error:
            user_error(
                "Syntax error in namelist changes",
                f"{error_message}\n{this_is_wrong}\n{example}",
                dsymbols=["``", "'"],
            )

    @staticmethod
    def nmls_remove(mconfig):
        """
        Removes an element from a namelist chapter.

        User Information
        ----------------
        In the configuration file, assume you have::

            echam:
                namelist_changes:
                    namelist.echam:
                        radctl:
                            co2vmr: "remove_from_namelist"

        In this case, the entry co2vmr would be deleted from the radctl section
        of namelist.echam.

        Programmer Information
        ----------------------
        IDEA(PG): Maybe we can provide examples of how these functions are used
        in the code?

        Parameters
        ----------
        mconfig : dict
            The model (e.g. ECHAM, FESOM, NEMO or OIFS) configuration

        Returns
        -------
        mconfig : dict
            The modified configuration.

        Calls to other methods
        ----------------------
        nmls_check_changes
        """

        namelist_changes = mconfig.get("namelist_changes", {})
        # Check if namelist_changes have correct syntax
        Namelist.nmls_check_changes(namelist_changes)
        namelist_removes = []
        for namelist in list(namelist_changes):
            changes = namelist_changes[namelist]
            logging.debug("Determining remove entires for %s", namelist)
            logging.debug("All changes: %s", changes)
            for change_chapter in list(changes):
                change_entries = changes[change_chapter]
                for key in list(change_entries):
                    value = change_entries[key]
                    if value == "remove_from_namelist":
                        namelist_removes.append((namelist, change_chapter, key))
                        # the key is probably coming from esm_tools config
                        # files or from a user runscript. It can contain lower
                        # case, but the original Fortran namelist could be in
                        # any case combination. Here `original_key` is coming
                        # from the default namelist and may contain mixed case.
                        # `key` is the processed variable from f90nml module and
                        # is lowercase.
                        remove_original_key = False
                        # traverse the namelist chapter and see if a mixed case
                        # variable is also found
                        for key2 in namelist_changes[namelist][change_chapter]:
                            # take care of the MiXeD FORTRAN CaSeS
                            if key2.lower() == key.lower() and key2 != key:
                                original_key = key2
                                remove_original_key = True
                                namelist_removes.append(
                                    (namelist, change_chapter, original_key)
                                )
                        # remove both lowercase and mixed case variables
                        del namelist_changes[namelist][change_chapter][key]
                        if remove_original_key:
                            del namelist_changes[namelist][change_chapter][original_key]
                        # mconfig instead of config, Grrrrr
                        print(
                            f"- NOTE: removing the variable: {key} from the namelist: {namelist}"
                        )

        for remove in namelist_removes:
            namelist, change_chapter, key = remove
            logging.debug("Removing from %s: %s, %s", namelist, change_chapter, key)
            if key in mconfig["namelists"][namelist].get(change_chapter, {}):
                del mconfig["namelists"][namelist][change_chapter][key]
            elif "%" in key:
                namvar, prop = key.split("%")
                del mconfig["namelists"][namelist][change_chapter][namvar][prop]
            else:
                logging.debug(
                    "Unable to remove %s: %s, %s", namelist, change_chapter, key
                )
        return mconfig

    @staticmethod
    def nmls_modify(mconfig):
        """
        Performs namelist changes.

        User Information
        ----------------
        In the configuration file, you should have a section as::

            echam:
                namelist_changes:
                    namelist.echam:
                        radctl:
                            co2vmr: 1200e-6


        This would change the value of the echam namelist (namelist.echam),
        subsection radctl, entry co2vmr to the value 1200e-6.

        Programmer Information
        ----------------------
        IDEA(PG): Maybe we can provide examples of how these functions are used
        in the code?

        Note
        ----
        Actual changes are performed by the f90nml package patch fuction. See
        here: https://tinyurl.com/y4ydz363

        Parameters
        ----------
        mconfig : dict
            The model (e.g. ECHAM, FESOM, NEMO or OIFS) configuration

        Returns
        -------
        mconfig : dict
            The modified configuration.

        Calls to other methods:
        ----------------------
        nmls_check_changes
        """
        namelist_changes = mconfig.get("namelist_changes", {})
        # Check if namelist_changes have correct syntax
        Namelist.nmls_check_changes(namelist_changes)

        for namelist, changes in namelist_changes.items():
            mconfig["namelists"][namelist].patch(changes)
        return mconfig

    @staticmethod
    def echam_transient_forcing(config):
        """
        Allows for ECHAM specific transient orbital and greenhouse gas forcing .

        User Information
        ----------------
        In the configuration file, assume you have::

            echam:
                use_transient_forcing: True
                transient_forcing_table: /some/path/to/a/table

        In this case, the entries for co2vmr, n2ovmr, ch4vmr, cecc, cobl, clonp
        would be extracted from this table according to the current model year,
        and the radctl section of namelist.echam is modified.

        The forcing table should look like this::

            # Year; CO2; CH4; N2O; Eccentricity; Obliquity; Perihelion
            1850; 0.000187; 3.779100e-07; 2.064600e-07; 0.018994; 22.944859; 294.23880

        The esm-tools will first check if the current year exists, extract the
        relevant values for you, and put everything into the radctl section of
        ``namelist.echam``

        Parameters
        ----------
        config : dict
            The config

        Returns
        -------
        config : dict
            The modified configuration.
        """
        if "echam" in config["general"]["valid_model_names"]:
            # Get the echam namelist:
            nml = config["echam"]["namelists"]["namelist.echam"]
            # Get the current radtl chapter or make a new empty one:
            radctl = nml.get("radctl", f90nml.namelist.Namelist())
            if config["echam"].get("use_transient_forcing", False):
                import pandas as pd

                try:
                    forcing_table = pd.read_csv(
                        config["echam"]["transient_forcing_table"], sep=";", index_col=0, header=None, 
                    )
                    co2, n2o, ch4, cecc, cobld, clonp = forcing_table.loc[
                        config["general"]["current_date"].year
                    ]
                    radctl["co2vmr"] = co2
                    radctl["n2ovmr"] = n2o
                    radctl["ch4vmr"] = ch4
                    radctl["cecc"] = cecc
                    radctl["cobld"] = cobld
                    radctl["clonp"] = clonp
                    # Line needed for making sure the changes in radctl make it to nml,
                    # even when ``radctl`` did not exist in ``nml``
                    nml["radctl"] = radctl
                    print(
                        "-------------------------------------------------------------"
                    )
                    print("")
                    print(
                        "              > Applying transient foricng in echam namelist!"
                    )
                    print("")
                    print(
                        "--------------------------------------------------------------"
                    )
                    print("             > The new values are:")
                    print(f"             CO2:  {radctl['co2vmr']}")
                    print(f"             N2O:  {radctl['n2ovmr']}")
                    print(f"             CH4:  {radctl['ch4vmr']}")
                    print(f"             CECC: {radctl['cecc']}")
                    print(f"             COBL: {radctl['cobld']}")
                    print(f"             CLONP:{radctl['clonp']}")
                except Exception as e:
                    # Haha something went wrong. Let's be polite about it though
                    print(
                        "There was a problem with reading in the forcing from the transient forcing table"
                    )
                    print()
                    print("Sorry")
                    print()
                    print("Please be sure to use the correct format of your table!")
                    print("It should be the following:")
                    print(
                        "# Model Year; CO2; N2O; CH4; Eccentricty; Obliquity; Perihelion"
                    )
                    print("Commented lines (with a #) will be ignored in that file")
                    print(
                        "Please note that you need to use a semicolon (;) as a seperator"
                    )
                    print()
                    print("Also, make sure that you set a valid filepath")
                    print("We were looking for the following:")
                    try:
                        print(config["echam"]["transient_forcing_table"])
                    except KeyError:
                        print(
                            "Oops, looks like you didn't specify which forcing table to use!"
                        )
                        print("You need to set in your echam configuration:")
                        print("echam:")
                        print("    transient_forcing_table: /path/to/your/table")
                        sys.exit(1)
                    sys.exit(1)
        return config

    @staticmethod
    def apply_echam_disturbance(config):
        """
        Applies a disturbance to the DYNCTL chapter of the echam namelist via the enstdif

        Relevant configuration entries:
        * disturbance_years (list of int): Which year to apply the disturbance
        * distrubance (float): Value to apply. Default can be found in echam.yaml
        """
        if "echam" in config["general"]["valid_model_names"]:
            # Get the echam namelist:
            nml = config["echam"]["namelists"]["namelist.echam"]
            # Get the current dynctl chapter or make a new empty one:
            dynctl = nml.get("dynctl", f90nml.namelist.Namelist())
            # Determine which years the user wants to have disturbed:
            if os.path.isfile(
                config["general"]["experiment_scripts_dir"] + "/disturb_years.dat"
            ):
                with open(
                    config["general"]["experiment_scripts_dir"] + "/disturb_years.dat"
                ) as f:
                    disturbance_file = [
                        int(line.strip()) for line in f.readlines() if line.strip()
                    ]
                if config["general"]["verbose"]:
                    print(disturbance_file)
            else:
                disturbance_file = None
                if config["general"]["verbose"]:
                    print(
                        "WARNING: "
                        + config["general"]["experiment_scripts_dir"]
                        + "/disturb_years.dat",
                        "was not found",
                    )
            disturbance_years = disturbance_file or config["echam"].get(
                "disturbance_years", []
            )
            current_year = config["general"]["current_date"].year
            if current_year in disturbance_years:
                print("-------------------------------------------------------")
                print("")
                print("              > Applying disturbance in echam namelist!")
                print("")
                print("-------------------------------------------------------")
                dynctl["enstdif"] = config["echam"].get("disturbance", 1.000001)
                nml["dynctl"] = dynctl
            else:
                if config["general"]["verbose"]:
                    print("Not applying disturbance in echam namelist.")
                    print(
                        "Current year",
                        current_year,
                        "disturbance_years",
                        disturbance_years,
                    )
        return config

    @staticmethod
    def nmls_finalize(mconfig, verbose):
        """
        Writes namelists to disk after all modifications have finished.

        User Information
        ----------------
        Part of the main log output will be a section specifing the actual
        namelists that have been used for your simulation, including all
        relevant additions, removals, or changes.

        Programmer Information
        ----------------------
        A copy of the f90nml object representations of the namelists is stored
        under the dictionary key "namelist_objs", as a dictionary of
        ("namelist_name", f90nml_objfect) key/value pairs.

        Warning
        -------
        Removing this step from your recipe might result in a broken run,
        as the namelists will not be present in their desired form! Even if
        your model runs, it might not contain all user-required changes.

        Parameters
        ----------
        mconfig : dict
            The model (e.g. ECHAM, FESOM, NEMO or OIFS) configuration

        Returns
        -------
        mconfig : dict
            The modified configuration.
        """
        all_nmls = {}

        for nml_name, nml_obj in mconfig.get("namelists", {}).items():
            with open(
                os.path.join(mconfig["thisrun_config_dir"], nml_name), "w"
            ) as nml_file:
                nml_obj.write(nml_file)
            all_nmls[nml_name] = nml_obj  # PG: or a string representation?
        mconfig["namelist_objs"] = all_nmls
        if verbose:
            mconfig = Namelist.nmls_output(mconfig)
        return mconfig

    @staticmethod
    def nmls_output(mconfig):
        all_nmls = {}

        for nml_name, nml_obj in mconfig.get("namelists", {}).items():
            all_nmls[nml_name] = nml_obj  # PG: or a string representation?
        for nml_name, nml in all_nmls.items():
            message = f"\nFinal Contents of {nml_name}:"
            print(message)
            print(len(message) * "-")
            nml.write(sys.stdout)
            print("-" * 80)
            print(f"::: end of the contents of {nml_name}\n")
        return mconfig

    @staticmethod
    def nmls_output_all(config):
        print(
            "\n" "- Namelists modified according to experiment specifications..."
        )
        for model in config["general"]["valid_model_names"]:
            config[model] = nmls_output(config[model], config["general"]["verbose"])
        return config


class namelist(Namelist):
    """Legacy class name. Please use Namelist instead!"""

    def __init__(self, *args, **kwargs):
        warnings.warn(
            "Please change your code to use Namelist!",
            DeprecationWarning,
            stacklevel=2,
        )

        super(namelist, self).__init__(*args, **kwargs)
