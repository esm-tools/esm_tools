import shutil
from pathlib import Path
from builder import build_fesom_stac

force_rewrite = True

exp_dir = Path("/albedo/work/user/pgierz/SciComp/Tutorials/AWIESM_Basics/experiments")
outdir_prefix = Path("/albedo/work/user/pasili001/stac_catalog/scratch")

experiments = []

for entry in exp_dir.iterdir():
    if entry.is_dir():
        data_path = entry / "outdata/fesom"
        if data_path.exists():
            files = list(data_path.glob("*.nc"))
            if files:
                experiments.append((entry.stem, data_path))

print(f"Found {len(experiments)} experiments")

for name, data_dir in experiments:
    output_dir = outdir_prefix / name
    if not output_dir.exists():
        build_fesom_stac(data_dir=data_dir, output_dir=output_dir)
    else:
        if force_rewrite == True:
            print(f"Rewriting {output_dir}")
            shutil.rmtree(output_dir)
            build_fesom_stac(data_dir=data_dir, output_dir=output_dir)

"""
model_dir = Path(
    "/albedo/work/user/pgierz/SciComp/Tutorials/AWIESM_Basics/experiments/basic-001"
)
data_dir = f"{model_dir}/outdata/fesom"
output_dir = "basic-001_fesom"

if not Path(output_dir).exists():
    build_fesom_stac(data_dir=data_dir, output_dir=output_dir)
"""
