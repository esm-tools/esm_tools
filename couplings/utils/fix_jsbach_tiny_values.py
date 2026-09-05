#!/usr/bin/env python3
"""
Fix tiny values (1e-10) in jsbach.nc and jsbach restart files.

The jsbach_init_file program generates values of 1e-10 for tiles/variables 
that should have 0 cover. While the sum may be 1.0, these tiny values can 
cause JSBACH's validation to fail.

This script:
1. Sets all values < 1e-8 to exactly 0.0 for fraction-like variables
2. Renormalizes cover_fract to ensure sum = 1.0 on land

Usage:
    python fix_jsbach_tiny_values.py <jsbach_file.nc> [--restart]
    
    --restart: Also fix restart-specific variables (cover_fract_pot, veg_ratio, etc.)
"""

import sys
import numpy as np

try:
    from netCDF4 import Dataset
except ImportError:
    print("ERROR: netCDF4 module not found")
    sys.exit(1)


def fix_variable(nc, var_name, normalize=False):
    """Fix tiny values in a variable, optionally normalize to sum=1"""
    if var_name not in nc.variables:
        return 0
    
    var = nc.variables[var_name][:]
    tiny_mask = (var > 0) & (var < 1e-8)
    n_tiny = np.sum(tiny_mask)
    
    if n_tiny == 0:
        return 0
    
    # Set tiny values to 0
    var[tiny_mask] = 0.0
    
    if normalize:
        # Renormalize so sum = 1.0 along first axis (tiles)
        var_sum = var.sum(axis=0, keepdims=True)
        # Only normalize where sum > 0 (land points)
        var_sum_safe = np.where(var_sum > 0, var_sum, 1.0)
        var = var / var_sum_safe
    
    nc.variables[var_name][:] = var
    return n_tiny


def main():
    if len(sys.argv) < 2:
        print(__doc__)
        sys.exit(1)
    
    jsbach_file = sys.argv[1]
    is_restart = '--restart' in sys.argv
    
    print(f"=== Fix tiny values in {jsbach_file} ===")
    print(f"Mode: {'restart' if is_restart else 'init file'}")
    
    # Variables to fix
    if is_restart:
        # jsbach restart file variables
        vars_to_fix = {
            'cover_fract': True,       # normalize
            'cover_fract_pot': True,   # normalize
            'forest_fract': False,
            'veg_ratio': False,
            'veg_ratio_max': False,
        }
    else:
        # jsbach init file (jsbach.nc) variables
        vars_to_fix = {
            'cover_fract': True,       # normalize
            'natural_veg': False,
            'veg_fract': False,
        }
    
    total_fixed = 0
    
    with Dataset(jsbach_file, 'r+') as nc:
        for var_name, normalize in vars_to_fix.items():
            n_fixed = fix_variable(nc, var_name, normalize)
            if n_fixed > 0:
                print(f"  {var_name}: fixed {n_fixed} tiny values")
                total_fixed += n_fixed
    
    if total_fixed == 0:
        print("No tiny values found. Nothing to do.")
    else:
        print(f"Total: fixed {total_fixed} tiny values")
    
    print("=== Done ===")


if __name__ == '__main__':
    main()
