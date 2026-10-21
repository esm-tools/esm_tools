#!/usr/bin/env python3
"""
Fix act_fpc in veg restart file for deglaciated areas.

When ice melts (deglaciation), act_fpc at those grid points remains 0 because
the vegetation hasn't had time to grow. This causes fpc_to_cover_fract_pot
to produce near-zero cover_fract_pot values, triggering JSBACH errors.

Solution: Initialize act_fpc at deglaciated points using pot_fpc values.

Usage:
    python fix_veg_restart_act_fpc.py veg_restart.nc
"""

import sys
import numpy as np
import netCDF4 as nc
import shutil
from datetime import datetime

def fix_act_fpc(veg_file, dry_run=False):
    """
    Fix act_fpc in veg restart file by initializing deglaciated points
    """
    print(f"=== Fix act_fpc in veg restart file ===")
    print(f"Veg restart file: {veg_file}")
    print(f"Dry run: {dry_run}")
    print()
    
    # Open veg restart
    if not dry_run:
        backup_file = f"{veg_file}.backup_actfpc_{datetime.now().strftime('%Y%m%d%H%M%S')}"
        shutil.copy2(veg_file, backup_file)
        print(f"Backup created: {backup_file}")
    
    veg = nc.Dataset(veg_file, 'r+' if not dry_run else 'r')
    
    act_fpc = veg.variables['act_fpc'][:]
    pot_fpc = veg.variables['pot_fpc'][:]
    
    print(f"act_fpc shape: {act_fpc.shape}")
    print(f"pot_fpc shape: {pot_fpc.shape}")
    
    # Calculate sums
    act_sum = np.sum(act_fpc, axis=0)
    pot_sum = np.sum(pot_fpc, axis=0)
    
    print(f"\nBefore fix:")
    print(f"  act_fpc sum: min={act_sum.min():.6f}, max={act_sum.max():.6f}")
    print(f"  pot_fpc sum: min={pot_sum.min():.6f}, max={pot_sum.max():.6f}")
    
    # Find points where act_fpc sum is very small but pot_fpc sum is near 1
    # These are deglaciated points that need initialization
    need_fix = (act_sum < 0.5) & (pot_sum > 0.5)
    n_fix = np.sum(need_fix)
    
    print(f"\nPoints needing fix (act_fpc_sum < 0.5 and pot_fpc_sum > 0.5): {n_fix}")
    
    if n_fix > 0:
        if not dry_run:
            # Initialize act_fpc using pot_fpc values for deglaciated points
            for i in np.where(need_fix)[0]:
                act_fpc[:, i] = pot_fpc[:, i]
            
            veg.variables['act_fpc'][:] = act_fpc
            veg.sync()
            print(f"act_fpc updated at {n_fix} points")
        else:
            print(f"[DRY RUN] Would update act_fpc at {n_fix} points")
    
    # Verify
    if not dry_run and n_fix > 0:
        act_fpc_new = veg.variables['act_fpc'][:]
        act_sum_new = np.sum(act_fpc_new, axis=0)
        still_bad = np.sum(act_sum_new < 0.5)
        
        print(f"\nAfter fix:")
        print(f"  act_fpc sum: min={act_sum_new.min():.6f}, max={act_sum_new.max():.6f}")
        print(f"  Points still with act_fpc sum < 0.5: {still_bad}")
    
    veg.close()
    
    print("\nDone!")
    return n_fix

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python fix_veg_restart_act_fpc.py veg_restart.nc [--dry-run]")
        sys.exit(1)
    
    veg_file = sys.argv[1]
    dry_run = "--dry-run" in sys.argv
    
    fix_act_fpc(veg_file, dry_run)
