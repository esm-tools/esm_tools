#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Utility functions for handling ice shelf cavity geometry operations.

This module provides helper functions for:
- Converting between different file formats
- Computing cavity metrics and statistics
- Validating cavity geometry
- Visualizing cavity changes

Author: ESM Tools Team, 2025
"""

import os
import numpy as np
import netCDF4 as nc
import matplotlib.pyplot as plt
import logging

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger('cavity_utils')

def validate_cavity_geometry(draft, bedrock, min_water_column=10.0):
    """
    Validate ice shelf cavity geometry to ensure minimum water column thickness.
    
    Parameters:
    -----------
    draft : np.ndarray
        Ice shelf draft (depth of ice bottom)
    bedrock : np.ndarray
        Bedrock topography
    min_water_column : float
        Minimum allowed water column thickness (m)
        
    Returns:
    --------
    np.ndarray
        Valid cavity mask (True where cavity is valid)
    """
    # Calculate water column thickness
    water_column = draft - bedrock
    
    # Check where water column is too thin
    valid_cavity = water_column >= min_water_column
    
    n_invalid = np.sum(~valid_cavity)
    if n_invalid > 0:
        logger.warning(f"Found {n_invalid} invalid cavity points with water column < {min_water_column}m")
    
    return valid_cavity

def smooth_cavity_geometry(draft, mask, iterations=1):
    """
    Apply smoothing to ice shelf draft to avoid numerical issues.
    
    Parameters:
    -----------
    draft : np.ndarray
        Ice shelf draft to smooth
    mask : np.ndarray
        Mask indicating ice shelf cells (True for ice shelf)
    iterations : int
        Number of smoothing iterations
        
    Returns:
    --------
    np.ndarray
        Smoothed ice draft
    """
    from scipy.ndimage import gaussian_filter
    
    # Only smooth within ice shelf region
    smoothed_draft = draft.copy()
    
    # Apply smoothing iteratively
    for i in range(iterations):
        # Gaussian smoothing (only where mask is True)
        smooth_region = gaussian_filter(
            np.where(mask, smoothed_draft, np.nan), 
            sigma=1.0, 
            mode='constant'
        )
        
        # Replace only in mask region
        smoothed_draft[mask] = smooth_region[mask]
    
    logger.info(f"Smoothed ice shelf draft with {iterations} iterations")
    
    return smoothed_draft

def convert_pism_to_fesom_cavity(pism_file, fesom_mesh_file, output_file):
    """
    Convert PISM cavity geometry to FESOM format.
    
    Parameters:
    -----------
    pism_file : str
        Path to PISM output file
    fesom_mesh_file : str
        Path to FESOM mesh file
    output_file : str
        Path to output FESOM cavity file
        
    Returns:
    --------
    str
        Path to created output file
    """
    from scipy.interpolate import griddata
    
    # Load PISM data
    with nc.Dataset(pism_file, 'r') as ds:
        # Get coordinates
        x = ds.variables['x'][:]
        y = ds.variables['y'][:]
        
        # Get fields
        thickness = ds.variables['thickness'][:]
        bed = ds.variables['bedrock_altitude'][:]
        mask = ds.variables['mask'][:]
        
    # Create 2D grid
    xx, yy = np.meshgrid(x, y)
    
    # Identify ice shelf cells (mask=3 in PISM)
    ice_shelf_mask = (mask == 3)
    
    # Calculate ice draft
    rho_ice = 910.0
    rho_ocean = 1028.0
    sea_level = 0.0
    
    ice_draft = np.zeros_like(thickness)
    ice_draft[ice_shelf_mask] = sea_level - thickness[ice_shelf_mask] * (rho_ice / rho_ocean)
    
    # Load FESOM mesh
    with nc.Dataset(fesom_mesh_file, 'r') as mesh:
        fesom_x = mesh.variables['x'][:]
        fesom_y = mesh.variables['y'][:]
        fesom_z = mesh.variables['z'][:]
        fesom_nodes_2d = mesh.variables['nod2d'][:] - 1  # Convert to 0-indexing
    
    # Interpolate from PISM to FESOM
    pism_points = np.column_stack((xx.flatten(), yy.flatten()))
    
    # Interpolate ice draft and mask
    fesom_draft = griddata(
        pism_points, 
        ice_draft.flatten(),
        np.column_stack((fesom_x, fesom_y)),
        method='linear',
        fill_value=0.0
    )
    
    # Create ice shelf mask on FESOM grid
    shelf_indicator = np.zeros_like(mask.flatten(), dtype=float)
    shelf_indicator[ice_shelf_mask.flatten()] = 1.0
    
    fesom_shelf_mask = griddata(
        pism_points,
        shelf_indicator,
        np.column_stack((fesom_x, fesom_y)),
        method='linear',
        fill_value=0.0
    )
    
    # Threshold to get binary mask
    fesom_shelf_mask = (fesom_shelf_mask > 0.5)
    
    # Create output file for FESOM
    with nc.Dataset(output_file, 'w') as out:
        # Create dimensions
        out.createDimension('nod2d', len(fesom_nodes_2d))
        
        # Create variables
        v_draft = out.createVariable('ice_draft', 'f8', ('nod2d',))
        v_mask = out.createVariable('cavity_mask', 'i4', ('nod2d',))
        
        # Set attributes
        v_draft.long_name = 'Ice shelf draft'
        v_draft.units = 'm'
        v_mask.long_name = 'Ice shelf cavity mask'
        v_mask.units = '1'
        
        # Write data
        v_draft[:] = fesom_draft[fesom_nodes_2d]
        v_mask[:] = fesom_shelf_mask[fesom_nodes_2d].astype(int)
    
    logger.info(f"Converted PISM cavity to FESOM format: {output_file}")
    return output_file

def compute_cavity_metrics(draft, bedrock, mask):
    """
    Compute key metrics for ice shelf cavity.
    
    Parameters:
    -----------
    draft : np.ndarray
        Ice shelf draft
    bedrock : np.ndarray
        Bedrock topography
    mask : np.ndarray
        Ice shelf mask (True for ice shelf)
        
    Returns:
    --------
    dict
        Dictionary with cavity metrics
    """
    # Water column thickness
    water_column = draft - bedrock
    
    # Only consider ice shelf regions
    shelf_indices = np.where(mask)
    
    # Compute metrics
    metrics = {
        'cavity_area': np.sum(mask),
        'mean_water_column': np.mean(water_column[shelf_indices]),
        'min_water_column': np.min(water_column[shelf_indices]),
        'max_water_column': np.max(water_column[shelf_indices]),
        'cavity_volume': np.sum(water_column[shelf_indices]),
        'mean_draft': np.mean(draft[shelf_indices]),
        'min_draft': np.min(draft[shelf_indices]),
        'max_draft': np.max(draft[shelf_indices])
    }
    
    return metrics

def visualize_cavity_changes(old_mask, new_mask, old_draft, new_draft, output_file):
    """
    Visualize changes in cavity geometry between time steps.
    
    Parameters:
    -----------
    old_mask : np.ndarray
        Previous cavity mask
    new_mask : np.ndarray
        New cavity mask
    old_draft : np.ndarray
        Previous ice draft
    new_draft : np.ndarray
        New ice draft
    output_file : str
        Path to output image file
        
    Returns:
    --------
    str
        Path to created image file
    """
    # Calculate changes
    cavity_expanded = np.logical_and(~old_mask, new_mask)
    cavity_retreated = np.logical_and(old_mask, ~new_mask)
    cavity_unchanged = np.logical_and(old_mask, new_mask)
    
    draft_change = new_draft - old_draft
    
    plt.figure(figsize=(12, 10))
    
    # Plot cavity extent changes
    plt.subplot(2, 2, 1)
    plt.imshow(cavity_expanded, cmap='Greens', interpolation='nearest')
    plt.colorbar(label='New cavity')
    plt.title('Cavity Expansion')
    
    plt.subplot(2, 2, 2)
    plt.imshow(cavity_retreated, cmap='Reds', interpolation='nearest')
    plt.colorbar(label='Retreated cavity')
    plt.title('Cavity Retreat')
    
    # Plot draft changes
    plt.subplot(2, 2, 3)
    plt.imshow(np.ma.masked_array(draft_change, mask=~cavity_unchanged), 
              cmap='RdBu_r', interpolation='nearest', vmin=-50, vmax=50)
    plt.colorbar(label='Draft change (m)')
    plt.title('Draft Change in Existing Cavity')
    
    # Plot total draft
    plt.subplot(2, 2, 4)
    plt.imshow(np.ma.masked_array(new_draft, mask=~new_mask), 
              cmap='viridis', interpolation='nearest')
    plt.colorbar(label='Draft (m)')
    plt.title('New Cavity Draft')
    
    plt.tight_layout()
    plt.savefig(output_file, dpi=150)
    plt.close()
    
    # Print some statistics
    n_expanded = np.sum(cavity_expanded)
    n_retreated = np.sum(cavity_retreated)
    n_unchanged = np.sum(cavity_unchanged)
    
    logger.info(f"Cavity changes: {n_expanded} cells expanded, {n_retreated} cells retreated, {n_unchanged} cells unchanged")
    logger.info(f"Mean draft change in existing cavity: {np.mean(draft_change[cavity_unchanged]):.2f} m")
    
    return output_file

def identify_disconnected_cavities(mask, connectivity=2):
    """
    Identify and label disconnected cavity regions.
    
    Parameters:
    -----------
    mask : np.ndarray
        Ice shelf cavity mask
    connectivity : int
        Connectivity for labeling (1=4-connected, 2=8-connected)
        
    Returns:
    --------
    np.ndarray
        Labeled array where each disconnected region has a unique integer value
    """
    from scipy.ndimage import label
    
    # Label connected components
    labeled_array, num_features = label(mask, structure=np.ones((3,3)))
    
    logger.info(f"Identified {num_features} disconnected cavity regions")
    
    return labeled_array, num_features

def calculate_melt_rate(temperature, salinity, draft, pressure_depth=None):
    """
    Calculate basal melt rate using the three-equation parameterization.
    
    This implements a commonly used three-equation model for ice-ocean interactions
    that calculates melt rates based on the difference between ocean temperature
    and the in-situ freezing point.
    
    Parameters:
    -----------
    temperature : np.ndarray
        Ocean temperature at the ice-ocean interface (°C)
    salinity : np.ndarray
        Ocean salinity at the ice-ocean interface (psu)
    draft : np.ndarray
        Ice shelf draft depth (m, positive downward)
    pressure_depth : np.ndarray, optional
        Depth for pressure calculation (m). If None, draft will be used.
        
    Returns:
    --------
    np.ndarray
        Melt rate in meters of ice per year (positive for melting)
    """
    # Physical constants
    rho_ice = 910.0      # Ice density [kg/m^3]
    rho_fw = 1000.0      # Freshwater density [kg/m^3]
    rho_sw = 1028.0      # Seawater density [kg/m^3]
    cp_ocean = 3974.0    # Specific heat capacity of ocean [J/(kg*K)]
    L_fusion = 3.34e5    # Latent heat of fusion for ice [J/kg]
    
    # Transfer coefficients
    gamma_T = 1.0e-4     # Thermal exchange velocity [m/s]
    gamma_S = 5.05e-7    # Salt exchange velocity [m/s]
    
    # Use draft for pressure calculation if pressure_depth not provided
    if pressure_depth is None:
        pressure_depth = np.abs(draft)
    
    # Calculate pressure in dbar (approx 1 dbar per m depth)
    pressure = pressure_depth / 10.0  # Convert from m to bar, then to dbar
    
    # Calculate freezing point (UNESCO equation)
    # T_freeze = a*S + b + c*depth
    a = -0.0575         # Freezing point slope [°C/psu]
    b = 0.0901          # Freezing point offset [°C]
    c = -7.61e-4        # Freezing point depth dependence [°C/m]
    
    freezing_point = a * salinity + b + c * pressure_depth
    
    # Temperature above freezing
    thermal_driving = temperature - freezing_point
    
    # Only melt where ocean is above freezing point
    thermal_driving = np.maximum(thermal_driving, 0.0)
    
    # Calculate melt rate [m/yr]
    melt_factor = (gamma_T * cp_ocean * rho_sw) / (L_fusion * rho_ice)
    melt_rate = melt_factor * thermal_driving
    
    # Convert from m/s to m/yr
    seconds_per_year = 365.25 * 24 * 3600
    melt_rate = melt_rate * seconds_per_year
    
    return melt_rate

def conservative_remap_pism_to_fesom(pism_data, pism_x, pism_y, fesom_x, fesom_y, fesom_elements=None):
    """
    Perform conservative remapping from PISM to FESOM grid.
    
    This function implements a more accurate remapping than simple interpolation
    by preserving the integral of the field over the domain, which is important
    for conserving mass and energy in coupled simulations.
    
    Parameters:
    -----------
    pism_data : np.ndarray
        2D array of data on PISM grid
    pism_x : np.ndarray
        1D array of x-coordinates for PISM grid
    pism_y : np.ndarray
        1D array of y-coordinates for PISM grid
    fesom_x : np.ndarray
        Array of x-coordinates for FESOM nodes
    fesom_y : np.ndarray
        Array of y-coordinates for FESOM nodes
    fesom_elements : np.ndarray, optional
        Array of FESOM element connectivity (for second-order accurate remap)
        
    Returns:
    --------
    np.ndarray
        Data remapped to FESOM grid
    """
    from scipy.interpolate import LinearNDInterpolator, griddata
    import matplotlib.tri as tri
    
    logger.info("Performing conservative remapping from PISM to FESOM grid")
    
    # Calculate PISM cell areas (assuming regular grid)
    dx = np.diff(pism_x)[0]
    dy = np.diff(pism_y)[0]
    cell_area = dx * dy
    
    # First-order approach if no elements provided
    if fesom_elements is None:
        # Create PISM points array
        grid_x, grid_y = np.meshgrid(pism_x, pism_y)
        pism_points = np.column_stack((grid_x.flatten(), grid_y.flatten()))
        
        # Standard interpolation as fallback
        fesom_data = griddata(
            pism_points, 
            pism_data.flatten(),
            np.column_stack((fesom_x, fesom_y)),
            method='linear',
            fill_value=0.0
        )
        
        logger.warning("No FESOM elements provided, using simple interpolation")
        return fesom_data
    
    # Create PISM interpolator
    grid_x, grid_y = np.meshgrid(pism_x, pism_y)
    pism_interpolator = LinearNDInterpolator(
        np.column_stack((grid_x.flatten(), grid_y.flatten())),
        pism_data.flatten()
    )
    
    # Create FESOM triangulation
    triangulation = tri.Triangulation(fesom_x, fesom_y, triangles=fesom_elements)
    
    # Allocate output array
    fesom_data = np.zeros_like(fesom_x)
    
    # For each FESOM node, compute weighted average over surrounding elements
    for i in range(len(fesom_x)):
        # Find triangles containing this node
        triangles_with_node = np.where(np.any(fesom_elements == i, axis=1))[0]
        
        if len(triangles_with_node) == 0:
            continue
            
        # Compute weighted average
        total_weight = 0.0
        weighted_sum = 0.0
        
        for tri_idx in triangles_with_node:
            triangle = fesom_elements[tri_idx]
            
            # Get triangle vertices
            vx = fesom_x[triangle]
            vy = fesom_y[triangle]
            
            # Compute triangle area
            area = 0.5 * np.abs(
                (vx[0]*(vy[1]-vy[2]) + vx[1]*(vy[2]-vy[0]) + vx[2]*(vy[0]-vy[1]))
            )
            
            # Sample PISM data at triangle centroid
            centroid_x = np.mean(vx)
            centroid_y = np.mean(vy)
            value = pism_interpolator(centroid_x, centroid_y)
            
            if np.isnan(value):
                continue
                
            # Add weighted contribution
            weighted_sum += value * area
            total_weight += area
        
        if total_weight > 0:
            fesom_data[i] = weighted_sum / total_weight
    
    logger.info("Conservative remapping complete")
    return fesom_data

def handle_time_series(input_file, variable_name, time_idx=-1):
    """
    Extract data from a time series file at the specified time index.
    
    Parameters:
    -----------
    input_file : str
        Path to NetCDF file containing time series data
    variable_name : str
        Name of variable to extract
    time_idx : int
        Time index to extract (-1 for last time step)
        
    Returns:
    --------
    tuple
        (data, time_value) containing the extracted data and corresponding time
    """
    with nc.Dataset(input_file, 'r') as ds:
        # Check if variable exists
        if variable_name not in ds.variables:
            available_vars = list(ds.variables.keys())
            logger.error(f"Variable '{variable_name}' not found. Available: {available_vars}")
            return None, None
            
        # Get dimensions of variable
        var_dims = ds.variables[variable_name].dimensions
        
        # Find time dimension
        time_dim = None
        for dim in var_dims:
            if dim.lower() in ['time', 't']:
                time_dim = dim
                break
                
        if time_dim is None:
            logger.warning(f"No time dimension found for variable '{variable_name}'")
            data = ds.variables[variable_name][:]
            return data, None
            
        # Get time values if available
        if time_dim in ds.variables:
            time_values = ds.variables[time_dim][:]
            time_value = time_values[time_idx]
        else:
            time_value = None
            
        # Handle different dimensionality
        if time_dim == var_dims[0]:
            # Time is first dimension
            data = ds.variables[variable_name][time_idx, ...]
        else:
            # Time is not first dimension, need to create proper slice
            slice_spec = [slice(None)] * len(var_dims)
            time_idx_in_dims = var_dims.index(time_dim)
            slice_spec[time_idx_in_dims] = time_idx
            data = ds.variables[variable_name][tuple(slice_spec)]
            
    logger.info(f"Extracted data for '{variable_name}' at time index {time_idx}")
    return data, time_value

def detect_cavity_changes(old_cavity_file, new_cavity_file, threshold=0.1):
    """
    Detect significant changes in cavity geometry between time steps.
    
    Parameters:
    -----------
    old_cavity_file : str
        Path to previous cavity geometry file
    new_cavity_file : str
        Path to new cavity geometry file
    threshold : float
        Fractional threshold for significant change (0.1 = 10%)
        
    Returns:
    --------
    dict
        Dictionary with change metrics
    """
    if not os.path.exists(old_cavity_file):
        logger.warning(f"Previous cavity file not found: {old_cavity_file}")
        return None
        
    if not os.path.exists(new_cavity_file):
        logger.warning(f"New cavity file not found: {new_cavity_file}")
        return None
    
    # Load cavity geometry files
    with nc.Dataset(old_cavity_file, 'r') as old, nc.Dataset(new_cavity_file, 'r') as new:
        # Load cavity masks
        if 'cavity_flag' in old.variables:
            old_mask = old.variables['cavity_flag'][:] > 0
        elif 'cavity_mask' in old.variables:
            old_mask = old.variables['cavity_mask'][:] > 0
        else:
            logger.error("Cavity mask variable not found in old file")
            return None
            
        if 'cavity_flag' in new.variables:
            new_mask = new.variables['cavity_flag'][:] > 0
        elif 'cavity_mask' in new.variables:
            new_mask = new.variables['cavity_mask'][:] > 0
        else:
            logger.error("Cavity mask variable not found in new file")
            return None
            
        # Load draft data
        if 'ice_draft' in old.variables:
            old_draft = old.variables['ice_draft'][:]
        else:
            logger.error("Ice draft variable not found in old file")
            return None
            
        if 'ice_draft' in new.variables:
            new_draft = new.variables['ice_draft'][:]
        else:
            logger.error("Ice draft variable not found in new file")
            return None
    
    # Calculate change metrics
    metrics = {}
    
    # Change in cavity area
    old_area = np.sum(old_mask)
    new_area = np.sum(new_mask)
    area_change = (new_area - old_area) / old_area if old_area > 0 else float('inf')
    metrics['area_change_fraction'] = area_change
    
    # Change in mean draft
    old_mean_draft = np.mean(old_draft[old_mask]) if np.any(old_mask) else 0
    new_mean_draft = np.mean(new_draft[new_mask]) if np.any(new_mask) else 0
    draft_change = (new_mean_draft - old_mean_draft) / old_mean_draft if old_mean_draft > 0 else float('inf')
    metrics['draft_change_fraction'] = draft_change
    
    # Areas of advance/retreat
    advance_mask = new_mask & ~old_mask
    retreat_mask = old_mask & ~new_mask
    metrics['advance_area'] = np.sum(advance_mask)
    metrics['retreat_area'] = np.sum(retreat_mask)
    
    # Detect significant changes
    significant_change = (abs(area_change) > threshold) or (abs(draft_change) > threshold)
    metrics['significant_change'] = significant_change
    
    # Log changes
    logger.info(f"Cavity area change: {area_change*100:.2f}%")
    logger.info(f"Mean draft change: {draft_change*100:.2f}%")
    logger.info(f"Cavity advance: {metrics['advance_area']} cells")
    logger.info(f"Cavity retreat: {metrics['retreat_area']} cells")
    logger.info(f"Significant change detected: {significant_change}")
    
    return metrics

def main():
    """Main function to demonstrate utility functions."""
    import argparse
    
    parser = argparse.ArgumentParser(description='Ice Shelf Cavity Utilities')
    parser.add_argument('--pism-file', type=str, help='PISM output file')
    parser.add_argument('--fesom-mesh', type=str, help='FESOM mesh file')
    parser.add_argument('--output', type=str, help='Output file')
    parser.add_argument('--action', type=str, choices=['convert', 'validate', 'visualize'],
                        default='convert', help='Action to perform')
    
    args = parser.parse_args()
    
    if args.action == 'convert' and args.pism_file and args.fesom_mesh and args.output:
        convert_pism_to_fesom_cavity(args.pism_file, args.fesom_mesh, args.output)
    elif args.action == 'validate' and args.pism_file:
        # Simple demonstration
        with nc.Dataset(args.pism_file, 'r') as ds:
            thickness = ds.variables['thickness'][:]
            bed = ds.variables['bedrock_altitude'][:]
            mask = ds.variables['mask'][:] == 3  # PISM convention
            
            # Calculate draft
            rho_ice = 910.0
            rho_ocean = 1028.0
            sea_level = 0.0
            draft = np.zeros_like(thickness)
            draft[mask] = sea_level - thickness[mask] * (rho_ice / rho_ocean)
            
            # Validate
            valid_mask = validate_cavity_geometry(draft, bed)
            
            # Print results
            metrics = compute_cavity_metrics(draft, bed, mask)
            for k, v in metrics.items():
                print(f"{k}: {v}")
    else:
        parser.print_help()
    
if __name__ == "__main__":
    main()
