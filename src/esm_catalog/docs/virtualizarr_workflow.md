# VirtualiZarr Workflow: From STAC Discovery to Virtual Zarr Cube

This guide shows how to take results from an ESM catalog STAC query and open them
as a single lazy, chunked xarray dataset using
[VirtualiZarr](https://github.com/zarr-developers/VirtualiZarr) — without copying
or reformatting any data.

---

## Concept

A STAC search tells you **what files exist** and **where they are**.
VirtualiZarr tells you **how to open all of them at once** as a virtual Zarr cube.

```
STAC API query            →  list of file paths  →  VirtualiZarr manifest
(discovery, esm-catalog)                             (single virtual dataset)
                                                         ↓
                                                    xarray + dask analysis
```

The manifest is a lightweight JSON (or in-memory object) that maps Zarr chunk
coordinates to byte ranges inside the original NetCDF files.  No data is read or
copied until you actually access array values.

---

## Installation

```bash
pip install virtualizarr[hdf]          # for NetCDF4 / HDF5 files (FESOM output)
pip install virtualizarr[kerchunk]     # also needed for NetCDF3 / GRIB
pip install pystac-client              # for querying the STAC API
```

---

## Workflow 1: In-memory virtual cube (no serialization)

This is the simplest path: query the catalog, virtualise the matching files, and
analyse immediately.  The manifest lives only in memory for the duration of the
session.

```python
import xarray as xr
from pystac_client import Client
from virtualizarr import open_virtual_dataset
from virtualizarr.parsers import HDFParser

# ── 1. Query the catalog ────────────────────────────────────────────────────
catalog = Client.open("http://localhost:8000")

items = list(
    catalog.search(
        collections=["basic-001-fesom"],
        filter="variable='ssh'",
        filter_lang="cql2-text",
    ).items()
)
print(f"Found {len(items)} items")

# ── 2. Extract local file paths ──────────────────────────────────────────────
# The API stores hrefs as file:// URIs; strip the prefix for local access.
def href_to_path(href: str) -> str:
    return href.removeprefix("file://")

paths = [href_to_path(item.assets["data"].href) for item in items]

# ── 3. Virtualise each file ──────────────────────────────────────────────────
parser = HDFParser()
virtual_datasets = [
    open_virtual_dataset(p, parser=parser, loadable_variables=["lon", "lat"])
    for p in paths
]

# ── 4. Concatenate into a single virtual cube ────────────────────────────────
# Sort by time before concatenating to ensure correct ordering.
virtual_datasets.sort(key=lambda ds: ds.time.values[0])
vds = xr.concat(virtual_datasets, dim="time")

print(vds)
# <xarray.Dataset>
# Dimensions:  (time: 12000, nod2: 830305)
# Data variables:
#     ssh      (time, nod2) float32 ManifestArray ...
# Coordinates:
#     lon      (nod2) float32 ...
#     lat      (nod2) float32 ...
#     time     (time) datetime64[ns] ...

# ── 5. Analyse lazily ────────────────────────────────────────────────────────
# No data has been read yet. Computation triggers byte-range reads.
ssh_mean = vds["ssh"].mean(dim="time").compute()
```

---

## Workflow 2: Persist the manifest (Kerchunk JSON)

For repeated analyses of the same dataset, save the manifest once and reopen it
instantly without re-scanning all the files.

```python
# After step 4 above — save the manifest
vds.vz.to_kerchunk("basic-001-fesom-ssh.kerchunk.json", format="dict")

# Later: reopen in milliseconds
import fsspec

with fsspec.open("basic-001-fesom-ssh.kerchunk.json") as f:
    import ujson
    refs = ujson.load(f)

ds = xr.open_dataset(
    "reference://",
    engine="zarr",
    backend_kwargs={
        "consolidated": False,
        "storage_options": {"fo": refs, "remote_protocol": "file"},
    },
)
```

The Kerchunk JSON file is small (typically a few MB even for thousands of files)
and can be stored alongside the DuckDB catalog or committed to a git repo.

---

## Workflow 3: Persist to Icechunk (versioned, append-friendly)

[Icechunk](https://icechunk.io) stores manifests in a versioned, transactional
format.  Use this when the dataset grows over time (new model output arrives) or
when multiple users need to access the same virtual cube reliably.

```python
import icechunk
from virtualizarr.writers.icechunk import dataset_to_icechunk

# Create (or open) an Icechunk store
storage = icechunk.local_filesystem_storage("/albedo/home/pasili001/stores/basic-001-fesom-ssh")
store = icechunk.IcechunkStore.create(storage)

# Write the virtual dataset
dataset_to_icechunk(vds, store)
store.commit("initial: basic-001-fesom SSH all timesteps")

# Later: open from the store
ds = xr.open_zarr(store, consolidated=False)
```

---

## Workflow 4: Multi-variable, multi-collection query

```python
# Query multiple collections / variables at once
items = list(
    catalog.search(
        collections=["basic-001-fesom", "basic-001-echam"],
    ).items()
)

# Group by collection × variable
from collections import defaultdict
groups = defaultdict(list)
for item in items:
    key = (item.collection_id, item.properties.get("variable", "unknown"))
    groups[key].append(href_to_path(item.assets["data"].href))

# Virtualise each group into its own cube
cubes = {}
for (collection, variable), paths in groups.items():
    vds_list = [open_virtual_dataset(p, parser=HDFParser()) for p in sorted(paths)]
    cubes[(collection, variable)] = xr.concat(vds_list, dim="time")

# e.g. cubes[("basic-001-fesom", "ssh")], cubes[("basic-001-echam", "temp2")]
```

---

## Tips for HPC (Lustre / tape)

**File accessibility.**
VirtualiZarr reads byte ranges from the original files at analysis time — not at
manifest-creation time.  If a file is on tape (HSM nearline/offline), it must be
recalled before `compute()` is called.  Check the `hpc:state` property on STAC items
before building the virtual cube:

```python
online = [
    item for item in items
    if item.properties.get("hpc:state", "online") == "online"
]
```

**Stripping `file://` prefixes.**
The ESM catalog API stores asset hrefs as `file:///absolute/path`.  VirtualiZarr's
`HDFParser` expects a plain path or `s3://` URI.  Always strip the prefix:

```python
path = item.assets["data"].href.removeprefix("file://")
```

**Large collections.**
For thousands of files, virtualisation can take a few minutes (one `h5py.File` open
per file).  Run once, persist to Kerchunk or Icechunk, and reuse.  The manifest
creation is embarrassingly parallel — consider `joblib.Parallel`:

```python
from joblib import Parallel, delayed

virtual_datasets = Parallel(n_jobs=8)(
    delayed(open_virtual_dataset)(p, parser=HDFParser()) for p in paths
)
```

**NetCDF3 and GRIB files (ECHAM output).**
ECHAM produces GRIB files and some NetCDF3 files.  These require the Kerchunk-based
parsers:

```python
from virtualizarr.parsers.kerchunk import NetCDF3Parser
vds = open_virtual_dataset(path, parser=NetCDF3Parser())
```

---

## Future: Kerchunk manifest as a STAC asset

Once VirtualiZarr manifests become routine, the scan step could generate them
automatically and store the manifest path as a second asset on each STAC item:

```json
"assets": {
  "data": {
    "href": "file:///albedo/.../ssh.fesom.185001.nc",
    "type": "application/x-netcdf"
  },
  "zarr_manifest": {
    "href": "file:///albedo/.../ssh.fesom.185001.kerchunk.json",
    "type": "application/vnd+zarr",
    "roles": ["data", "virtual-reference"],
    "title": "Kerchunk virtual Zarr reference"
  }
}
```

This would allow consumers to open files via the manifest without needing to know the
original format, and would work correctly once files are migrated to cold / object
storage.

---

## See also

- [VirtualiZarr documentation](https://virtualizarr.readthedocs.io)
- [Kerchunk](https://fsspec.github.io/kerchunk/)
- [Icechunk](https://icechunk.io)
- `docs/quickstart.md` — how to build and serve the ESM catalog
- `ARCHITECTURE.md` — VirtualiZarr integration section for the broader context
