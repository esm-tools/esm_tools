from pathlib import Path

import pystac
import xarray as xr


def main():
    catalog_path = Path(
        "/albedo/work/user/pasili001/stac_catalog/scratch/basic-001/catalog.json"
    )

    # 1) Read STAC
    catalog = pystac.Catalog.from_file(str(catalog_path))

    # 2) Get the collection (by id)
    collection = catalog.get_child("fesom-collection")

    # 3) Collect NetCDF asset paths from items
    hrefs = []
    for item in collection.get_all_items():
        asset = item.assets.get("data")
        if asset is None:
            continue
        hrefs.append(asset.href)

    # Optional: sort for stable ordering (mostly relevant if you concat by time)
    hrefs = sorted(set(hrefs))

    print(f"Found {len(hrefs)} NetCDF assets")
    print(hrefs[:3])

    ssh_hrefs = [h for h in hrefs if "/ssh.fesom." in h]
    ds_ssh = xr.concat(
        [xr.open_dataset(h, decode_times=True) for h in ssh_hrefs], dim="time"
    )

    print(ds_ssh)


if __name__ == "__main__":
    main()
