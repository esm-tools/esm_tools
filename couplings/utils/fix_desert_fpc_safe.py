#!/usr/bin/env python3
"""
Safe script to fix FPC issues in JSBACH veg restart file.

This script directly modifies the veg restart file using netCDF4,
avoiding the pack/unpack process that causes precision issues.

Two fixes are applied:
1. In deglaciated areas (where GLAC changed from 1 to 0):
   - Set desert_fpc = 0.0 (allow vegetation to grow)
   - Set bare_fpc = 1.0 - act_fpc_sum (to maintain sum = 1)

2. For ALL landpoints where total FPC > 1:
   - Reduce desert_fpc to bring total to 1
   - If not enough, also reduce bare_fpc

Usage:
    python fix_desert_fpc_safe.py <veg_restart> <jsbach_reference> <jsbach_new> [--dry-run]
"""

import sys
import numpy as np

try:
    from netCDF4 import Dataset
except ImportError:
    print("ERROR: netCDF4 module not found. Please load the appropriate module:")
    print("       module load python3")
    print("       or use: conda activate <env_with_netcdf4>")
    sys.exit(1)


def get_landpoint_indices_from_2d_mask(mask_2d, landseamask):
    """
    Convert 2D grid mask to 1D landpoint indices.
    
    Parameters:
    -----------
    mask_2d : 2D array (lat, lon) with 1 where condition is true
    landseamask : 2D array (lat, lon) with 1 for land points
    
    Returns:
    --------
    Array of landpoint indices where mask_2d is 1
    """
    # Get all land points in order (row-major, i.e., lat varies slowest)
    land_indices = np.where(landseamask.flatten() > 0.5)[0]
    
    # Get indices where mask is 1
    mask_flat = mask_2d.flatten()
    
    # Find which landpoints have mask = 1
    landpoint_indices = []
    for i, grid_idx in enumerate(land_indices):
        if mask_flat[grid_idx] > 0.5:
            landpoint_indices.append(i)
    
    return np.array(landpoint_indices)


def fix_all_fpc_issues(desert_fpc, bare_fpc, act_fpc_sum, tol=1e-10):
    """
    Fix all landpoints where total FPC differs from 1 by more than tol.
    
    Strategy:
    - If total > 1 + tol:
        - First reduce desert_fpc to bring total to 1
        - If not enough, also reduce bare_fpc
    - If total < 1 - tol:
        - Increase bare_fpc by the deficit (bounded to [0, 1])
    """
    total_fpc = desert_fpc + bare_fpc + act_fpc_sum
    problem_mask = np.abs(total_fpc - 1.0) > tol
    problem_indices = np.where(problem_mask)[0]
    
    n_fixed = 0
    for idx in problem_indices:
        diff = total_fpc[idx] - 1.0
        if diff > tol:
            excess = diff
        
            # First try to reduce desert_fpc
            if desert_fpc[idx] >= excess:
                desert_fpc[idx] -= excess
                n_fixed += 1
            else:
                # Need to also reduce bare_fpc
                remaining_excess = excess - desert_fpc[idx]
                desert_fpc[idx] = 0.0
                
                if bare_fpc[idx] >= remaining_excess:
                    bare_fpc[idx] -= remaining_excess
                    n_fixed += 1
                else:
                    # Fallback: enforce exact closure by setting bare to residual
                    bare_fpc[idx] = max(0.0, 1.0 - act_fpc_sum[idx])
                    desert_fpc[idx] = 0.0
                    n_fixed += 1
        elif diff < -tol:
            deficit = -diff
            # Prefer to fill deficit into bare_fpc (can't exceed 1 - act_fpc_sum)
            max_bare = max(0.0, 1.0 - act_fpc_sum[idx])
            bare_fpc[idx] = min(max_bare, bare_fpc[idx] + deficit)
            # If bare was already at max, adjust desert as residual (rare)
            residual = 1.0 - (act_fpc_sum[idx] + bare_fpc[idx] + desert_fpc[idx])
            if abs(residual) > tol:
                desert_fpc[idx] = min(1.0, max(0.0, desert_fpc[idx] + residual))
            n_fixed += 1
    
    return desert_fpc, bare_fpc, n_fixed


def main():
    if len(sys.argv) < 4:
        print(__doc__)
        sys.exit(1)
    
    veg_restart_file = sys.argv[1]
    jsbach_reference_file = sys.argv[2]
    jsbach_new_file = sys.argv[3]
    dry_run = '--dry-run' in sys.argv
    
    print(f"=== Fix FPC issues in veg restart ===")
    print(f"Veg restart file: {veg_restart_file}")
    print(f"Reference jsbach: {jsbach_reference_file}")
    print(f"New jsbach:       {jsbach_new_file}")
    print(f"Dry run:          {dry_run}")
    print()
    
    # Load the glacier mask difference
    print("Loading jsbach files to identify deglaciated areas...")
    
    with Dataset(jsbach_reference_file, 'r') as ref_nc:
        glac_ref = ref_nc.variables['glac'][:]
        if len(glac_ref.shape) == 3:
            glac_ref = glac_ref[0, :, :]  # Remove time dimension if present
        # Get land sea mask from reference file
        if 'slm' in ref_nc.variables:
            landseamask = ref_nc.variables['slm'][:]
        elif 'landseamask' in ref_nc.variables:
            landseamask = ref_nc.variables['landseamask'][:]
        else:
            raise KeyError("Cannot find land sea mask (slm or landseamask) in reference file")
        if len(landseamask.shape) == 3:
            landseamask = landseamask[0, :, :]
    
    with Dataset(jsbach_new_file, 'r') as new_nc:
        glac_new = new_nc.variables['glac'][:]
        if len(glac_new.shape) == 3:
            glac_new = glac_new[0, :, :]
    
    # Deglaciated areas: was glacier (ref=1), now land (new=0)
    deglaciated_mask = (glac_ref > 0.5) & (glac_new < 0.5)
    n_deglaciated_2d = np.sum(deglaciated_mask)
    print(f"Number of deglaciated grid cells (2D): {n_deglaciated_2d}")
    
    # Load veg restart
    print(f"\nLoading veg restart file: {veg_restart_file}")
    
    with Dataset(veg_restart_file, 'r+' if not dry_run else 'r') as veg_nc:
        # Get current fpc values
        desert_fpc = veg_nc.variables['desert_fpc'][:]
        bare_fpc = veg_nc.variables['bare_fpc'][:]
        
        # Get act_fpc (has tiles dimension)
        act_fpc = veg_nc.variables['act_fpc'][:]
        # Get pot_fpc (has tiles dimension) if present
        pot_fpc = veg_nc.variables['pot_fpc'][:] if 'pot_fpc' in veg_nc.variables else None
        
        # === Fix 0: Remove tiny values from act_fpc ===
        tiny_act = (act_fpc > 0) & (act_fpc < 1e-8)
        n_tiny_act = np.sum(tiny_act)
        if n_tiny_act > 0:
            print(f"\n--- Fix 0: Tiny values in act_fpc ---")
            print(f"  Found {n_tiny_act} tiny values in act_fpc, setting to 0")
            act_fpc[tiny_act] = 0.0
            if not dry_run:
                veg_nc.variables['act_fpc'][:] = act_fpc
        
        # Also fix tiny values in desert_fpc
        tiny_desert = (desert_fpc > 0) & (desert_fpc < 1e-8)
        n_tiny_desert = np.sum(tiny_desert)
        if n_tiny_desert > 0:
            print(f"  Found {n_tiny_desert} tiny values in desert_fpc, setting to 0")
            desert_fpc[tiny_desert] = 0.0
        
        act_fpc_sum = np.sum(act_fpc, axis=0)  # Sum across tiles

        # === Fix 0b: Ensure pot_fpc is valid (sum across tiles must be 1) ===
        # JSBACH uses pot_fpc to derive cover_fract_pot and expects it to be a valid
        # composition vector (sum=1) even when total vegetation fraction is small.
        # Some restart files contain pot_fpc=0 for many landpoints, which triggers:
        #   FATAL ERROR in fpc_to_cover_fract_pot: sum of cover_fract_pot /= 1
        if pot_fpc is not None:
            tol = 1e-10
            pot_sum = np.sum(pot_fpc, axis=0)
            bad = np.where(np.abs(pot_sum - 1.0) > tol)[0]
            if bad.size > 0:
                print(f"\n--- Fix 0b: pot_fpc closure ---")
                print(f"  Found {bad.size} landpoints with |sum(pot_fpc)-1| > {tol:g}")
                # Fix each bad landpoint:
                for idx in bad:
                    s = pot_sum[idx]
                    if s > tol:
                        pot_fpc[:, idx] = pot_fpc[:, idx] / s
                    else:
                        # If pot_fpc is (near) all-zero, use act_fpc composition if available,
                        # otherwise put all potential vegetation into the first tile.
                        a = act_fpc_sum[idx]
                        if a > tol:
                            pot_fpc[:, idx] = act_fpc[:, idx] / a
                        else:
                            pot_fpc[:, idx] = 0.0
                            pot_fpc[0, idx] = 1.0
                # Recompute and report
                pot_sum2 = np.sum(pot_fpc, axis=0)
                bad2 = int(np.sum(np.abs(pot_sum2 - 1.0) > tol))
                print(f"  After fix: {bad2} landpoints still failing pot_fpc closure")
                if not dry_run:
                    veg_nc.variables['pot_fpc'][:] = pot_fpc
        
        # Calculate total FPC
        total_fpc = desert_fpc + bare_fpc + act_fpc_sum
        tol = 1e-10
        n_problems_before = int(np.sum(np.abs(total_fpc - 1.0) > tol))
        print(f"\nBefore fix: {n_problems_before} landpoints with |total FPC - 1| > {tol:g}")
        print(f"  Total FPC: min={total_fpc.min():.12f}, max={total_fpc.max():.12f}")
        
        # === Fix 1: Deglaciated areas ===
        if n_deglaciated_2d > 0:
            deglaciated_landpoints = get_landpoint_indices_from_2d_mask(
                deglaciated_mask, landseamask
            )
            n_deglaciated = len(deglaciated_landpoints)
            print(f"\n--- Fix 1: Deglaciated areas ---")
            print(f"Number of deglaciated landpoints: {n_deglaciated}")
            
            if n_deglaciated > 0:
                # For deglaciated areas:
                # - desert_fpc = 0 (allow vegetation to grow)
                # - Initialize act_fpc to a small value (1% of pot_fpc) if currently 0
                #   This is CRITICAL: JSBACH's fpc_to_cover_fract_pot function fails
                #   when act_fpc=0 for non-glacier points, producing 1e-10 values
                # - bare_fpc = 1.0 - act_fpc_sum (so that total = 1)
                
                # First, initialize act_fpc for points where it's currently 0
                small_init = 0.01  # 1% initial vegetation
                n_act_init = 0
                for idx in deglaciated_landpoints:
                    if act_fpc_sum[idx] < 1e-8 and pot_fpc is not None:
                        # Initialize act_fpc to a small fraction of pot_fpc
                        act_fpc[:, idx] = pot_fpc[:, idx] * small_init
                        n_act_init += 1
                
                # Recalculate act_fpc_sum after initialization
                act_fpc_sum = np.sum(act_fpc, axis=0)
                
                if n_act_init > 0:
                    print(f"  Initialized act_fpc for {n_act_init} deglaciated points (1% of pot_fpc)")
                    if not dry_run:
                        veg_nc.variables['act_fpc'][:] = act_fpc
                
                new_desert_fpc = np.zeros(n_deglaciated)
                new_bare_fpc = 1.0 - act_fpc_sum[deglaciated_landpoints]
                # Do NOT force a positive floor (like 1e-10): it can make total FPC > 1
                new_bare_fpc = np.clip(new_bare_fpc, 0.0, 1.0)
                
                desert_fpc[deglaciated_landpoints] = new_desert_fpc
                bare_fpc[deglaciated_landpoints] = new_bare_fpc
                print(f"  Set desert_fpc=0 and bare_fpc=1-act_fpc for {n_deglaciated} points")
        
        # === Fix 2: All remaining FPC issues ===
        print(f"\n--- Fix 2: All remaining FPC issues ---")
        desert_fpc, bare_fpc, n_fixed = fix_all_fpc_issues(
            desert_fpc, bare_fpc, act_fpc_sum, tol=tol
        )
        print(f"  Fixed {n_fixed} additional landpoints")
        
        # Verify
        total_fpc_after = desert_fpc + bare_fpc + act_fpc_sum
        n_problems_after = int(np.sum(np.abs(total_fpc_after - 1.0) > tol))
        print(f"\nAfter fix: {n_problems_after} landpoints with |total FPC - 1| > {tol:g}")
        print(f"  Total FPC: min={total_fpc_after.min():.12f}, max={total_fpc_after.max():.12f}")
        
        if dry_run:
            print("\n[DRY RUN] No changes made to file.")
        else:
            print("\nApplying changes...")
            veg_nc.variables['desert_fpc'][:] = desert_fpc
            veg_nc.variables['bare_fpc'][:] = bare_fpc
            print("Changes applied successfully!")
    
    print("\n=== Done ===")


if __name__ == '__main__':
    main()
