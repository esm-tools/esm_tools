"""
Example of reading multi-level STAC catalog.

This demonstrates how to navigate the hierarchical structure:
Root Catalog → Experiment Catalog → Variable Collection → Items
"""

import intake
import xarray as xr

catalog_path = "/albedo/work/user/pasili001/stac_catalog/multi_level/catalog.json"

# Open the root catalog
root_cat = intake.open_stac_catalog(catalog_path)

print("=" * 70)
print("ROOT CATALOG")
print("=" * 70)
print(f"Catalog ID: {root_cat.name}")
print(f"\nAvailable experiments:")
for exp_id in root_cat:
    print(f"  - {exp_id}")

print("\n" + "=" * 70)
print("EXPERIMENT LEVEL")
print("=" * 70)

# Get the first available experiment (or use a specific one)
available_experiments = list(root_cat)
if not available_experiments:
    print("No experiments found in catalog!")
    exit(1)

# Use 'basic-001' if available, otherwise use the first experiment
experiment_name = (
    "basic-001" if "basic-001" in available_experiments else available_experiments[0]
)

# Access the experiment
exp_cat = root_cat[experiment_name]
print(f"\nExperiment: {exp_cat.name}")
print(f"\nAvailable variables in this experiment:")
for var_id in exp_cat:
    print(f"  - {var_id}")

print("\n" + "=" * 70)
print("VARIABLE COLLECTION LEVEL")
print("=" * 70)

# Access a specific variable collection within the experiment
ssh_collection = exp_cat["ssh-collection"]
print(f"\nCollection: {ssh_collection.name}")

# Get all SSH files from this experiment
ssh_files = []
for item_id in ssh_collection:
    item_source = ssh_collection[item_id]
    stac_item = item_source._stac_obj
    asset = stac_item.assets["data"]
    ssh_files.append(asset.href)

print(f"\nFound {len(ssh_files)} SSH files in experiment '{experiment_name}':")
for f in ssh_files:
    print(f"  - {f}")

print("\n" + "=" * 70)
print("LOADING DATA")
print("=" * 70)

# Load all SSH files from this experiment with xarray
ds = xr.open_mfdataset(ssh_files, combine="by_coords")
print(f"\nCombined SSH dataset for experiment '{experiment_name}':")
print(ds)

print("\n" + "=" * 70)
print("CROSS-EXPERIMENT COMPARISON (if multiple experiments exist)")
print("=" * 70)

# Example: Compare SSH data across multiple experiments
# This would require iterating through multiple experiment catalogs
for exp_id in root_cat:
    try:
        exp = root_cat[exp_id]
        if "ssh-collection" in list(exp):
            ssh_coll = exp["ssh-collection"]
            num_items = len(list(ssh_coll))
            print(f"  {exp_id}: {num_items} SSH files")
    except Exception as e:
        print(f"  {exp_id}: Error accessing SSH collection - {e}")
