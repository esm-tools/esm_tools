#!/bin/bash
# Rename FESOM regridded XIOS output from the split-suffix pattern
# "<var>.fesom.reg_<ystart>-<yend>.nc" to the legacy-style FESOM pattern
# "<var>.fesom.gr.<year>.nc". Idempotent: skips files already in target form.
#
# The "gr" segment matches the CMIP grid_label for regridded (regular
# lat/lon) output and keeps the disk pattern visually distinct from the
# native files (which stay as "<var>.fesom.<year>.nc").
#
# Disjoint from the native rename glob: this script's input pattern
# "*.fesom.reg_*-*.nc" cannot match files that match the native renamer's
# "*.fesom_*-*.nc" pattern (the latter requires "fesom_" literally
# adjacent, which the reg files never have because of the ".reg_"
# separator), so both renamers can run in any order.
#
# Usage: rename_xios_fesom_reg.sh <experiment_outdata_dir>
# experiment_outdata_dir is the per-component outdata dir (e.g. .../outdata/fesom).

set -eu

outdata_dir="${1:?outdata dir required}"

if [ ! -d "${outdata_dir}" ]; then
    echo "rename_xios_fesom_reg: ${outdata_dir} does not exist, nothing to do"
    exit 0
fi

shopt -s nullglob
for f in "${outdata_dir}"/*.fesom.reg_*-*.nc; do
    base="$(basename "${f}")"
    var="${base%%.fesom.reg_*}"
    suffix="${base#${var}.fesom.reg_}"      # "YYYY-YYYY.nc"
    ystart="${suffix%%-*}"
    new="${outdata_dir}/${var}.fesom.gr.${ystart}.nc"
    if [ -e "${new}" ]; then
        echo "rename_xios_fesom_reg: ${new} exists, skipping"
        continue
    fi
    mv "${f}" "${new}"
    echo "rename_xios_fesom_reg: ${base} -> $(basename "${new}")"
done
