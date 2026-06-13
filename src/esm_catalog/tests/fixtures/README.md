# Test Fixtures

## tiny.grib2

A minimal synthetic GRIB2 file (~179 bytes) generated with eccodes for use in
unit tests. It contains a single message:

- **paramId**: 130 (temperature, shortName="t")
- **gridType**: regular_ll (4 x 2 lat-lon grid)
- **Ni/Nj**: 4 x 2
- **lat**: 45.0 to -45.0, **lon**: 0.0 to 90.0
- **dataDate**: 20000101, **dataTime**: 0
- **values**: [280.0] x 8

Generated with:
```python
import eccodes
gid = eccodes.codes_grib_new_from_samples("regular_ll_pl_grib2")
eccodes.codes_set(gid, "Ni", 4)
eccodes.codes_set(gid, "Nj", 2)
eccodes.codes_set(gid, "latitudeOfFirstGridPointInDegrees", 45.0)
eccodes.codes_set(gid, "longitudeOfFirstGridPointInDegrees", 0.0)
eccodes.codes_set(gid, "latitudeOfLastGridPointInDegrees", -45.0)
eccodes.codes_set(gid, "longitudeOfLastGridPointInDegrees", 90.0)
eccodes.codes_set(gid, "iDirectionIncrementInDegrees", 30.0)
eccodes.codes_set(gid, "jDirectionIncrementInDegrees", 90.0)
eccodes.codes_set(gid, "paramId", 130)
eccodes.codes_set(gid, "dataDate", 20000101)
eccodes.codes_set(gid, "dataTime", 0)
eccodes.codes_set_values(gid, [280.0] * 8)
with open("src/esm_catalog/tests/fixtures/tiny.grib2", "wb") as f:
    eccodes.codes_write(gid, f)
eccodes.codes_release(gid)
```

> **Note:** Regenerating this fixture requires the eccodes samples to be discoverable.
> The `regular_ll_pl_grib2.tmpl` sample must be on the eccodes samples path; set
> `GRIB_SAMPLES_PATH` if your eccodes install does not bundle it (the `eccodeslib` wheel does).
