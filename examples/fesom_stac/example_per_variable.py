"""
Example usage of builder_per_variable_collection.py

This demonstrates how to:
1. Build a catalog with one collection per variable
2. Query specific variable collections
3. Load all files for a specific variable
"""

import builder_per_variable_collection as builder

if __name__ == "__main__":
    data_dir = "/albedo/work/user/pgierz/SciComp/Tutorials/AWIESM_Basics/experiments/basic-001/outdata/fesom"
    output_dir = "/albedo/work/user/pasili001/stac_catalog/per_variable/basic-001"

    builder.build_fesom_stac(
        data_dir=data_dir,
        output_dir=output_dir,
        catalog_id="fesom-catalog-per-variable",
    )
