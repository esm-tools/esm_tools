#!/bin/bash
# Rename XIOS-produced FESOM output files from the split-suffix pattern
# "<var>.fesom_<ystart>-<yend>.nc" to the legacy FESOM pattern
# "<var>.fesom.<year>.nc". Idempotent: skips files already in target form.
#
# Usage: rename_xios_fesom.sh <experiment_outdata_dir>
# experiment_outdata_dir is the per-component outdata dir (e.g. .../outdata/fesom).

set -eu

outdata_dir="${1:?outdata dir required}"

if [ ! -d "${outdata_dir}" ]; then
    echo "rename_xios_fesom: ${outdata_dir} does not exist, nothing to do"
    exit 0
fi

shopt -s nullglob
for f in "${outdata_dir}"/*.fesom_*-*.nc; do
    base="$(basename "${f}")"
    var="${base%%.fesom_*}"
    suffix="${base#${var}.fesom_}"          # "YYYY-YYYY.nc"
    ystart="${suffix%%-*}"
    new="${outdata_dir}/${var}.fesom.${ystart}.nc"
    if [ -e "${new}" ]; then
        echo "rename_xios_fesom: ${new} exists, skipping"
        continue
    fi
    mv "${f}" "${new}"
    echo "rename_xios_fesom: ${base} -> $(basename "${new}")"
done
