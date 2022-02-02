import icb_apply_distribution_functions as icb
import sys

print("LA DEBUG: sys.argv = ", str(sys.argv))

ifile = sys.argv[1]
mesh_path = sys.argv[2]
icb_path = sys.argv[3]
basin_file = sys.argv[4]

ib = icb.IcebergCalving(ifile, mesh_path, icb_path, basin_file)
ib.create_dataframe()
ib._icb_generator()
