import intake
import xarray as xr

catalog_path = "/albedo/work/user/pasili001/stac_catalog/scratch/basic-001/catalog.json"
cat = intake.open_stac_catalog(catalog_path)
collection = cat["fesom-collection"]

# Collect all SSH file paths
ssh_files = []
for item_id in collection:
    if item_id.startswith("ssh.fesom."):
        item_source = collection[item_id]
        stac_item = item_source._stac_obj
        asset = stac_item.assets["data"]
        ssh_files.append(asset.href)

# Open all files at once with xarray
ds = xr.open_mfdataset(ssh_files, combine="by_coords")
print(ds)
