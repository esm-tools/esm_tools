#!/usr/bin/env python3
"""
Fix cover_fract and cover_fract_pot in jsbach restart file for deglaciated areas.

IMPORTANT: This script only updates points where the GLACIER MASK (glac) changed
from 1 to 0. It does NOT touch points that were already non-glacier (like tropical 
evergreen forest which also uses Tile 1).

Usage:
    python fix_jsbach_restart_cover_fract.py restart_jsbach.nc jsbach.nc jsbach_reference.nc
"""

import sys
import numpy as np
import netCDF4 as nc
import shutil
from datetime import datetime

def fix_cover_fract(restart_file, jsbach_new_file, jsbach_ref_file=None, dry_run=False):
    """
    Update cover_fract and cover_fract_pot in restart file for deglaciated areas only.
    
    Deglaciated areas are identified by comparing glac in jsbach_new with jsbach_reference:
    - glac_ref > 0.5 AND glac_new < 0.5 => deglaciated
    """
    print(f"=== Fix cover_fract in jsbach restart file ===")
    print(f"Restart file: {restart_file}")
    print(f"New JSBACH file: {jsbach_new_file}")
    print(f"Reference JSBACH file: {jsbach_ref_file}")
    print(f"Dry run: {dry_run}")
    print()
    
    # Open new jsbach file
    jsbach_new = nc.Dataset(jsbach_new_file, 'r')
    cover_fract_new = jsbach_new.variables['cover_fract'][:]
    glac_new = jsbach_new.variables['glac'][:]
    slm = jsbach_new.variables['slm'][:]
    jsbach_new.close()
    
    # Open reference jsbach file (to identify deglaciated areas)
    if jsbach_ref_file:
        jsbach_ref = nc.Dataset(jsbach_ref_file, 'r')
        glac_ref = jsbach_ref.variables['glac'][:]
        jsbach_ref.close()
    else:
        # If no reference, assume all glac=1 in restart were originally glaciated
        glac_ref = np.ones_like(glac_new)
    
    # Identify truly deglaciated areas (was glacier, now not glacier)
    deglaciated_2d = (glac_ref > 0.5) & (glac_new < 0.5)
    n_deglaciated_2d = np.sum(deglaciated_2d)
    print(f"Deglaciated grid cells (2D): {n_deglaciated_2d}")
    
    if n_deglaciated_2d == 0:
        print("No deglaciated areas found. Nothing to do.")
        return 0
    
    # Open restart file
    if not dry_run:
        backup_file = f"{restart_file}.backup_{datetime.now().strftime('%Y%m%d%H%M%S')}"
        shutil.copy2(restart_file, backup_file)
        print(f"Backup created: {backup_file}")
    
    restart = nc.Dataset(restart_file, 'r+' if not dry_run else 'r')
    
    cover_fract_old = restart.variables['cover_fract'][:]
    cover_fract_pot_old = restart.variables['cover_fract_pot'][:]
    
    # Build land mask mapping (2D -> 1D)
    land_mask_2d = slm > 0.5
    n_land = int(np.sum(land_mask_2d))
    
    print(f"Number of land points: {n_land}")
    print(f"Restart cover_fract shape: {cover_fract_old.shape}")
    print(f"JSBACH cover_fract shape: {cover_fract_new.shape}")
    
    # Map 2D to 1D
    land_idx_map = {}
    land_idx_1d = 0
    for i in range(glac_new.shape[0]):
        for j in range(glac_new.shape[1]):
            if land_mask_2d[i, j]:
                land_idx_map[(i, j)] = land_idx_1d
                land_idx_1d += 1
    
    # Update only deglaciated points
    updated_count = 0
    
    for i in range(glac_new.shape[0]):
        for j in range(glac_new.shape[1]):
            if not land_mask_2d[i, j]:
                continue
            
            if not deglaciated_2d[i, j]:
                continue
            
            land_idx = land_idx_map[(i, j)]
            
            # Get new cover_fract from jsbach.nc
            new_fract = cover_fract_new[:, i, j]
            
            if not dry_run:
                # Update cover_fract
                restart.variables['cover_fract'][:, land_idx] = new_fract
                # Update cover_fract_pot (same as cover_fract for natural vegetation)
                restart.variables['cover_fract_pot'][:, land_idx] = new_fract
            
            updated_count += 1
    
    print(f"\nDeglaciated points updated: {updated_count}")
    
    if not dry_run:
        restart.sync()
    
    restart.close()
    
    print("\nDone!")
    return updated_count

if __name__ == "__main__":
    if len(sys.argv) < 3:
        print("Usage: python fix_jsbach_restart_cover_fract.py restart_jsbach.nc jsbach_new.nc [jsbach_reference.nc] [--dry-run]")
        sys.exit(1)
    
    restart_file = sys.argv[1]
    jsbach_new_file = sys.argv[2]
    jsbach_ref_file = sys.argv[3] if len(sys.argv) > 3 and not sys.argv[3].startswith('--') else None
    dry_run = "--dry-run" in sys.argv
    
    fix_cover_fract(restart_file, jsbach_new_file, jsbach_ref_file, dry_run)
