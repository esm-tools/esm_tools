#!/usr/bin/env python3
"""
Fix pot_fpc in veg restart file for deglaciated areas.

When cover_fract in jsbach restart is updated, pot_fpc in veg restart 
must also be updated to match. Otherwise, JSBACH will crash with
memory errors or inconsistent state.

Usage:
    python fix_veg_restart_pot_fpc.py veg_restart.nc jsbach_restart.nc
"""

import sys
import numpy as np
import netCDF4 as nc
import shutil
from datetime import datetime

def fix_pot_fpc(veg_file, jsbach_file, dry_run=False):
    """
    Update pot_fpc in veg restart file to match cover_fract in jsbach restart
    """
    print(f"=== Fix pot_fpc in veg restart file ===")
    print(f"Veg restart file: {veg_file}")
    print(f"JSBACH restart file: {jsbach_file}")
    print(f"Dry run: {dry_run}")
    print()
    
    # Open jsbach restart to get cover_fract
    jsbach = nc.Dataset(jsbach_file, 'r')
    cover_fract = jsbach.variables['cover_fract'][:]
    jsbach.close()
    
    # Open veg restart
    if not dry_run:
        backup_file = f"{veg_file}.backup_{datetime.now().strftime('%Y%m%d%H%M%S')}"
        shutil.copy2(veg_file, backup_file)
        print(f"Backup created: {backup_file}")
    
    veg = nc.Dataset(veg_file, 'r+' if not dry_run else 'r')
    
    pot_fpc_old = veg.variables['pot_fpc'][:]
    
    print(f"pot_fpc shape: {pot_fpc_old.shape}")
    print(f"cover_fract shape: {cover_fract.shape}")
    
    if pot_fpc_old.shape != cover_fract.shape:
        print("ERROR: Shape mismatch! Cannot proceed.")
        veg.close()
        return -1
    
    # FIX 2025-01-25: Improved mismatch detection
    # Find ANY points where pot_fpc and cover_fract differ significantly
    diff = np.abs(pot_fpc_old - cover_fract)
    mismatch_mask = np.any(diff > 1e-6, axis=0)  # Check across all tiles
    n_mismatch = np.sum(mismatch_mask)
    
    # Also specifically check for the common case: glacier in pot_fpc but not in cover_fract
    glacier_mismatch = (pot_fpc_old[0] > 0.5) & (cover_fract[0] < 0.5)
    n_glacier_mismatch = np.sum(glacier_mismatch)
    
    print(f"\nTotal mismatch points (any tile differs > 1e-6): {n_mismatch}")
    print(f"Glacier mismatch points (pot_fpc[0]>0.5 but cover_fract[0]<0.5): {n_glacier_mismatch}")
    
    # Always sync pot_fpc to cover_fract to ensure consistency
    # This is safe because cover_fract is the authoritative source from jsbach.nc
    if not dry_run:
        print(f"\nSyncing pot_fpc to match cover_fract...")
        veg.variables['pot_fpc'][:] = cover_fract
        veg.sync()
        print(f"pot_fpc updated to match cover_fract")
    
    # Verify
    if not dry_run:
        pot_fpc_new = veg.variables['pot_fpc'][:]
        diff_new = np.abs(pot_fpc_new - cover_fract)
        new_mismatch = np.sum(np.any(diff_new > 1e-6, axis=0))
        print(f"\nAfter fix, mismatch points: {new_mismatch}")
        
        # Verify sum = 1
        pot_sum = np.sum(pot_fpc_new, axis=0)
        bad_sum = np.sum(np.abs(pot_sum - 1.0) > 1e-6)
        print(f"Points with pot_fpc sum != 1: {bad_sum}")
    
    veg.close()
    
    print("\nDone!")
    return n_mismatch

if __name__ == "__main__":
    if len(sys.argv) < 3:
        print("Usage: python fix_veg_restart_pot_fpc.py veg_restart.nc jsbach_restart.nc [--dry-run]")
        sys.exit(1)
    
    veg_file = sys.argv[1]
    jsbach_file = sys.argv[2]
    dry_run = "--dry-run" in sys.argv
    
    fix_pot_fpc(veg_file, jsbach_file, dry_run)
