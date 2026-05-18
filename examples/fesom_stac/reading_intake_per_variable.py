"""
Example of reading STAC catalog with one collection per variable.

This demonstrates the simpler querying pattern when each variable
has its own collection.
"""

import intake
import xarray as xr

catalog_path = (
    "/albedo/work/user/pasili001/stac_catalog/per_variable/basic-001/catalog.json"
)
cat = intake.open_stac_catalog(catalog_path)

print("Available collections:")
for collection_id in cat:
    print(f"  - {collection_id}")

print("\n" + "=" * 60)
print("Loading all SSH files from ssh-collection")
print("=" * 60)

ssh_collection = cat["ssh-collection"]

ssh_files = []
for item_id in ssh_collection:
    item_source = ssh_collection[item_id]
    stac_item = item_source._stac_obj
    asset = stac_item.assets["data"]
    ssh_files.append(asset.href)

print(f"Found {len(ssh_files)} SSH files:")
for f in ssh_files:
    print(f"  - {f}")

ds = xr.open_mfdataset(ssh_files, combine="by_coords")
print(f"\nCombined dataset:")
print(ds)
