import datetime
import getpass
import hashlib
import json
import pathlib

import dpath
import intake
import intake_esm  # noqa: F401, import only needed to register intake-esm driver
import pandas as pd
import xarray as xr
from loguru import logger


def create_intake_esm_catalog(config):
    """
    Create an intake-esm catalog based on the configuration.

    This creates an intake-esm compatible catalog based on the simulation
    configuration. The catalog is stored under ``config["intake"]``. Write
    to disk as a separate step, this is not handled here!

    Creation of the catalog can be controlled via the configuration key::

        config["general"]["create_catalog"] = True

    Default is ``True``.


    Parameters
    ----------
    config : dict
        The simulation configuration

    Notes
    -----
    The catalog attributes for each entry define what can be searched for and filtered. The exact
    specifications of what should be included seems to be ambiguous and could be defined however
    we want. Here, we use NextGEMS as a basis, and remove some of the fields which are not obvious
    as a start. To see what is used in the DKRZ NextGems catalogue::

        $ jq .attributes[].column_name /pool/data/Catalogs/dkrz_nextgems_disk.json

    See Also
    --------
    * https://intake.readthedocs.io/en/latest/index.html
    * https://github.com/NCAR/esm-collection-spec/blob/master/collection-spec/collection-spec.md
    * https://tutorials.dkrz.de/tutorial_intake-5-create-esm-collection.html
    """
    if not config["general"].get("create_catalog", True):
        return config
    catalog = config["intake"] = {}
    catalog["esmcat_version"] = "0.1.0"
    attributes = catalog["attributes"] = []
    catalog_attrs = [
        "variable_id",
        "project",
        "institution_id",
        "source_id",
        "experiment_id",
        "realm",
        "time_min",
        "time_max",
    ]
    for attr in catalog_attrs:
        attributes.append(dict(column_name=attr, vocabulary=""))
    assets = catalog["assets"] = dict(column_name="uri", format_column_name="format")
    aggregation_control = catalog["aggregation_control"] = {
        "variable_column_name": "variable_id",
        "groupby_attrs": [
            "project",
            "institution_id",
            "source_id",
            "experiment_id",
            "realm",
        ],
        "aggregations": [
            {"type": "union", "attribute_name": "variable_id", "options": {}},
            {
                "type": "join_existing",
                "attribute_name": "time_min",
                "options": {"dim": "time", "coords": "minimal", "compat": "override"},
            },
        ],
    }
    catalog["id"] = hashlib.sha256(
        f"{config['general']['expid']}_{datetime.datetime.now()}_{getpass.getuser()}".encode()
    ).hexdigest()
    catalog["description"] = str(config["general"].get("description"))
    catalog["title"] = f"Intake-ESM Catalog for Experiment {config['general']['expid']}"
    catalog["last_updated"] = str(datetime.datetime.now())
    catalog_dict = catalog["catalog_dict"] = []
    # Each entry in catalog_dict should correspond to the schema provided
    # in catalog_attrs plus assets
    for model in config["general"]["valid_model_names"]:
        print(f"Cataloguing output of model {model}")
        mconfig = config[model]
        # FIXME(PG): This is not how we should determine which files are in the experiment outdata
        #            since this will list **all** files, not just the ones added during this run.
        for output_file in pathlib.Path(mconfig["experiment_outdata_dir"]).iterdir():
            if mconfig["model"] in ["echam", "jsbach"]:
                if "codes" in output_file.suffix or "idx" in output_file.suffix:
                    logger.debug(
                        "Skipping codes file or already processed grib outputfile"
                    )
                    continue
            # print(f"Cataloguing {output_file}...")
            xarray_engine = "netcdf4" if "nc" in output_file.suffix else "cfgrib"
            # NOTE(PG): Determine which variables are contained in the file, this could be better...
            try:
                var_list = list(
                    xr.open_dataset(output_file, engine=xarray_engine).variables.keys()
                )
            except Exception as e:
                logger.warning(f"Could not determine variables in {output_file}: {e}")
                logger.warning("This file is not added to the catalog!")
                continue
            this_asset = dict(
                project=config["general"].get("project", "esm_tools"),
                institution_id="AWI",
                source_id=f"{config['general']['model']}-{config['general']['version']}",
                experiment_id=str(config["general"]["expid"]),
                realm=str(mconfig.get("type", "UNKNOWN")),
                time_min=str(config["general"]["start_date"]),
                time_max=str(config["general"]["end_date"]),
                uri=f"file://{output_file}",
                # _data_format_=xarray_engine,  # NOTE(PG): I don't like needing this...
                format=xarray_engine,
                variable_id=var_list,
            )
            catalog_dict.append(this_asset)

    catalog_dict = catalog["catalog_dict"]
    catalog_df = pd.DataFrame(catalog_dict)
    # Try to construct the esm_datastore object:
    validated_cat = intake.open_esm_datastore(obj=dict(esmcat=catalog, df=catalog_df))
    config["intake"] = validated_cat
    return config


def write_intake_esm_catalog(config):
    """
    Save the intake catalog to disk.

    This function saves the intake catalog specified in the configuration to a YAML file
    on disk. If a previous catalog exists, it merges the new catalog with the existing one.

    Saving of the catalog can be controlled via the configuration key::

        config["general"]["write_catalog"] = True

    Default is ``True``.

    Parameters
    ----------
    config : dict
        Configuration dictionary containing the catalog and general settings.
        Expected keys are "general" and "intake".

    Returns
    -------
    dict
        The updated configuration dictionary with the merged intake catalog.
    """
    if not config["general"].get("write_catalog", True):
        return config

    cat_file = pathlib.Path(
        f'{config["general"]["experiment_dir"]}/{config["general"]["expid"]}_intake_catalog.json'
    )
    catalog = config["intake"]

    if cat_file.exists():
        with open(cat_file, "r") as f:
            prev_cat = json.load(f)
    else:
        prev_cat = {}

    catalog_name = cat_file.stem
    catalog.serialize(catalog_name, directory=cat_file.parent)
    # catalog.serialize("paul", directory=cat_file.parent)

    backup_file = cat_file.with_suffix(".json_backup")
    if cat_file.exists():
        with open(cat_file, "r") as f:
            with open(backup_file, "w") as backup:
                backup.write(f.read())

    # Merge the new catalog into the previous one
    with open(cat_file, "r") as f:
        new_cat = json.load(f)
    dpath.merge(prev_cat, new_cat)

    # Save the merged catalog back to disk
    with open(cat_file, "w") as f:
        json.dump(prev_cat, f, indent=4)

    return config
