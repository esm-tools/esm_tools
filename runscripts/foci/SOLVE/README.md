example runscripts used in SOLVe project (2025 - 2027)
for reasons currently unknown a temporary fix in echam.datasets.yaml had to be applied
```diff
@@ -177,9 +177,9 @@ echam:
                 # historical and scenario forcing
                 moz_lbc:
                     "${input_dir}/HAMMOZ_${echam.resolution}/hammoz_lbc_17500116-21011216_CMIP6_CCMI2_ssp370_${echam.resolution}.nc":
-                        to: 2014
+                        to: 2013
                     "${input_dir}/HAMMOZ_${echam.resolution}/hammoz_lbc_17500116-21011216_CMIP6_CCMI2_${echam.scenario}_${echam.resolution}.nc":
-                        from: 2015
+                        from: 2014
                 moz_sad_sulf: "${input_dir}/HAMMOZ_${echam.resolution}/moz_sad_sulf_1849-2100_c080220_${echam.resolution}${echam.levels}.nc"
                 moz_epp:
                     "${epp_dir}/ubc_noy_epp_@YEAR@_${echam.resolution}${echam.levels}_20toplevs.nc":

```
Last update: 2026-02-11 by Sebastian Wahl, GEOMAR
