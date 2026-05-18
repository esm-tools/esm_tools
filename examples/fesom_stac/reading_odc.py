import pystac
import odc.stac
from pystac_client import Client

catalog_path = "/albedo/work/user/pasili001/stac_catalog/scratch/basic-001/catalog.json"
catalog = pystac.Catalog.from_file(catalog_path)

items = list(catalog.get_all_items())

ds = odc.stac.load(
    items,
    assets=["data"],
    chunks={},  # dask-ready
)
print(ds)
