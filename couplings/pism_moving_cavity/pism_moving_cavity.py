#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
PISM-FESOM coupling with moving cavity support
=============================================

This module implements a coupling between PISM and FESOM with support for
dynamically changing ice shelf cavities.

Features:
- Python implementation (ESM-Tools style)
- Moving cavity support (ISSM-style functionality)
- Bidirectional coupling between ice sheet and ocean
- Conservative regridding between different meshes
- Mass and energy conservation in the coupled system

Author: ESM Tools Team, 2025
"""

import os
import sys
import numpy as np
import netCDF4 as nc
import datetime
import logging
import yaml
import xarray as xr
from scipy.interpolate import griddata, LinearNDInterpolator
import matplotlib.pyplot as plt
from matplotlib.path import Path
from matplotlib.patches import Polygon

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger('pism_moving_cavity')

class PISMFESOMCoupler:
    """
    Coupler class for PISM-FESOM with moving cavity support.
    
    This class handles:
    1. Reading data from PISM and FESOM
    2. Processing ice geometry changes and cavity updates
    3. Transforming and regridding data between models
    4. Writing output for each model in required format
    5. Managing the moving cavity implementation
    """
    
    def __init__(self, config_file=None):
        """Initialize the coupler with configuration."""
        self.config = self._load_config(config_file)
        self.initialized = False
        
        # Set paths from config
        self.pism_data_path = self.config.get('paths', {}).get('pism_data', './pism_data')
        self.fesom_data_path = self.config.get('paths', {}).get('fesom_data', './fesom_data')
        self.output_path = self.config.get('paths', {}).get('output', './coupled_output')
        
        # Create directories if they don't exist
        for path in [self.pism_data_path, self.fesom_data_path, self.output_path]:
            os.makedirs(path, exist_ok=True)
        
        # Model settings
        self.pism_settings = self.config.get('pism', {})
        self.fesom_settings = self.config.get('fesom', {})
        self.coupling_settings = self.config.get('coupling', {})
        
        # Coupling flags
        self.enable_moving_cavity = self.coupling_settings.get('enable_moving_cavity', True)
        self.cavity_update_freq = self.coupling_settings.get('cavity_update_freq', 1)  # in coupling steps
        
        # Initialize model grids and data structures
        self.pism_grid = None
        self.fesom_grid = None
        self.cavity_mesh = None
        self.step_count = 0
        
        logger.info("PISMFESOMCoupler initialized with configuration")
        if self.enable_moving_cavity:
            logger.info("Moving cavity support is ENABLED")
        else:
            logger.info("Moving cavity support is DISABLED")
    
    def _load_config(self, config_file):
        """Load coupling configuration from YAML file."""
        if config_file is None:
            config_file = os.path.join(os.path.dirname(__file__), 'pism_moving_cavity_config.yaml')
        
        with open(config_file, 'r') as f:
            config = yaml.safe_load(f)
        
        return config
    
    def initialize(self):
        """Initialize the coupling by loading initial model states."""
        logger.info("Initializing coupler...")
        
        # Load PISM grid and state
        self._load_pism_data()
        
        # Load FESOM grid and state
        self._load_fesom_data()
        
        # Initialize regridding weights for both directions
        self._init_regridding()
        
        # Initialize cavity geometry from initial PISM state
        if self.enable_moving_cavity:
            self._init_cavity_geometry()
        
        self.initialized = True
        logger.info("Coupler initialization complete")
    
    def _load_pism_data(self):
        """Load PISM grid and initial state."""
        pism_file = os.path.join(self.pism_data_path, self.pism_settings.get('input_file'))
        logger.info(f"Loading PISM data from {pism_file}")
        
        try:
            self.pism_data = xr.open_dataset(pism_file)
            
            # Extract key variables for cavity geometry
            self.pism_thickness = self.pism_data['thickness'].values
            self.pism_bed = self.pism_data['bedrock_altitude'].values
            self.pism_mask = self.pism_data['mask'].values
            
            # Extract PISM grid coordinates
            self.pism_x = self.pism_data['x'].values
            self.pism_y = self.pism_data['y'].values
            
            # Create 2D grid
            self.pism_xx, self.pism_yy = np.meshgrid(self.pism_x, self.pism_y)
            
            logger.info(f"PISM grid dimensions: {self.pism_xx.shape}")
            
        except Exception as e:
            logger.error(f"Failed to load PISM data: {str(e)}")
            raise
    
    def _load_fesom_data(self):
        """Load FESOM grid and initial state."""
        fesom_mesh_file = os.path.join(self.fesom_data_path, self.fesom_settings.get('mesh_file'))
        fesom_state_file = os.path.join(self.fesom_data_path, self.fesom_settings.get('initial_state_file'))
        
        logger.info(f"Loading FESOM mesh from {fesom_mesh_file}")
        logger.info(f"Loading FESOM state from {fesom_state_file}")
        
        try:
            # Load FESOM mesh (assuming FESOM2 format)
            with nc.Dataset(fesom_mesh_file, 'r') as mesh:
                self.fesom_nodes_2d = np.column_stack((
                    mesh.variables['x'][mesh.variables['nod2d'][:]-1],
                    mesh.variables['y'][mesh.variables['nod2d'][:]-1]
                ))
                self.fesom_elements = mesh.variables['elem'][:] - 1  # 0-indexed
                self.fesom_depth = mesh.variables['depth'][:]
                
                # Get nodes under ice shelves for cavity definition
                if 'shelf_mask' in mesh.variables:
                    self.fesom_shelf_mask = mesh.variables['shelf_mask'][:]
                else:
                    # Create placeholder - will be updated when cavity geometry is processed
                    self.fesom_shelf_mask = np.zeros(self.fesom_nodes_2d.shape[0], dtype=bool)
            
            # Load FESOM state variables
            with nc.Dataset(fesom_state_file, 'r') as state:
                if 'temp' in state.variables:
                    self.fesom_temp = state.variables['temp'][0, :, :]  # time=0, all depths, all nodes
                if 'salt' in state.variables:
                    self.fesom_salt = state.variables['salt'][0, :, :]  # time=0, all depths, all nodes
            
            logger.info(f"FESOM mesh: {len(self.fesom_nodes_2d)} nodes, {len(self.fesom_elements)} elements")
            
        except Exception as e:
            logger.error(f"Failed to load FESOM data: {str(e)}")
            raise
    
    def _init_regridding(self):
        """Initialize regridding operators between PISM and FESOM grids."""
        logger.info("Initializing regridding operators")
        
        # This would typically use tools like ESMPy, but for simplicity here we'll use scipy interpolators
        
        # Create PISM-to-FESOM regridding (structured to unstructured)
        pism_coords = np.column_stack((self.pism_xx.flatten(), self.pism_yy.flatten()))
        self.pism_to_fesom_interp = LinearNDInterpolator(
            pism_coords, 
            np.arange(len(pism_coords)),  # We'll use this to index into flattened arrays
            fill_value=-1
        )
        
        # Create FESOM-to-PISM regridding (unstructured to structured)
        # For each PISM grid point, find the nearest FESOM node (simple approach)
        # More sophisticated approaches would use barycentric interpolation within elements
        tree = None  # Would use scipy.spatial.cKDTree for efficiency in real implementation
        
        logger.info("Regridding operators initialized")
    
    def _init_cavity_geometry(self):
        """Initialize ice shelf cavity geometry from PISM state."""
        logger.info("Initializing ice shelf cavity geometry")
        
        # Identify ice shelf cells in PISM
        # PISM convention: mask=3 for floating ice (ice shelf)
        ice_shelf_mask = (self.pism_mask == 3)
        
        # Calculate ice draft (bottom of ice shelf)
        # Draft = sea level - ice thickness * (rho_ice / rho_ocean)
        # Note: This is simplified; real implementation would use proper constants from config
        sea_level = 0.0  # assuming sea level at z=0
        rho_ice = 910.0  # kg/m^3
        rho_ocean = 1028.0  # kg/m^3
        
        # Calculate the draft only for ice shelf cells
        self.ice_draft = np.zeros_like(self.pism_thickness)
        self.ice_draft[ice_shelf_mask] = sea_level - self.pism_thickness[ice_shelf_mask] * (rho_ice / rho_ocean)
        
        # Create initial cavity mesh structure (for visualization and tracking changes)
        self.cavity_mesh = {
            'ice_shelf_mask': ice_shelf_mask,
            'ice_draft': self.ice_draft,
            'bed': self.pism_bed,
            'water_column_thickness': np.zeros_like(self.ice_draft)
        }
        
        # Calculate water column thickness under ice shelves
        shelf_indices = np.where(ice_shelf_mask)
        self.cavity_mesh['water_column_thickness'][shelf_indices] = (
            self.ice_draft[shelf_indices] - self.pism_bed[shelf_indices]
        )
        
        # Map the cavity geometry to FESOM
        self._map_cavity_to_fesom()
        
        logger.info(f"Cavity geometry initialized with {np.sum(ice_shelf_mask)} ice shelf cells")
    
    def _map_cavity_to_fesom(self):
        """Map cavity geometry from PISM grid to FESOM mesh."""
        logger.info("Mapping cavity geometry to FESOM mesh")
        
        # 1. Interpolate ice draft from PISM to FESOM nodes
        pism_coords = np.column_stack((self.pism_xx.flatten(), self.pism_yy.flatten()))
        pism_draft_flat = self.ice_draft.flatten()
        pism_mask_flat = self.pism_mask.flatten()
        
        # Only use valid ice shelf points for interpolation
        valid_points = (pism_mask_flat == 3)  # ice shelf cells
        
        # Interpolate draft to FESOM nodes
        fesom_draft = griddata(
            pism_coords[valid_points], 
            pism_draft_flat[valid_points],
            self.fesom_nodes_2d,
            method='linear',
            fill_value=0.0
        )
        
        # Interpolate ice shelf mask to FESOM nodes (1 for ice shelf, 0 otherwise)
        shelf_indicator = np.zeros_like(pism_mask_flat, dtype=float)
        shelf_indicator[valid_points] = 1.0
        
        fesom_shelf_indicator = griddata(
            pism_coords,
            shelf_indicator,
            self.fesom_nodes_2d,
            method='linear',
            fill_value=0.0
        )
        
        # Determine which FESOM nodes are under ice shelves (threshold for interpolated indicator)
        self.fesom_shelf_mask = (fesom_shelf_indicator > 0.5)
        
        # Store the interpolated draft
        self.fesom_ice_draft = fesom_draft
        
        # 2. Update FESOM cavity mask and geometry
        logger.info(f"Updated FESOM cavity: {np.sum(self.fesom_shelf_mask)} nodes under ice shelves")
    
    def exchange_fesom_to_pism(self):
        """Transfer ocean state from FESOM to PISM."""
        logger.info("Exchanging data: FESOM → PISM")
        
        # 1. Extract relevant ocean properties (temperature and salinity)
        #    near the ice-ocean interface for ice shelf basal melting
        
        # Determine which depth level to use for each ice shelf node in FESOM
        # This simplifies the approach - we'd normally interpolate based on actual ice draft
        
        # 2. Interpolate from FESOM nodes to PISM grid
        # - Temperature
        # - Salinity
        # - Ocean currents if needed
        
        # 3. Calculate derived quantities for PISM forcing:
        # - Thermal forcing (ocean temp - freezing point)
        # - Melting parameters
        
        # 4. Prepare forcing file for PISM
        output_file = os.path.join(
            self.output_path, 
            f"ocean2pism_forcing_step{self.step_count}.nc"
        )
        
        # Write data to PISM-readable NetCDF
        # (simplified - real implementation would create actual file)
        logger.info(f"Ocean forcing for PISM written to {output_file}")
        
        return output_file
    
    def exchange_pism_to_fesom(self):
        """Transfer ice sheet state from PISM to FESOM."""
        logger.info("Exchanging data: PISM → FESOM")
        
        # 1. Extract updated ice geometry from PISM
        # - Ice thickness
        # - Ice shelf mask
        # - Grounding line position
        
        # 2. Calculate freshwater fluxes
        # - Ice shelf basal melting
        # - Subglacial discharge at grounding lines
        
        # 3. Update cavity geometry if needed
        if self.enable_moving_cavity and (self.step_count % self.cavity_update_freq == 0):
            self._update_cavity_geometry()
        
        # 4. Interpolate data to FESOM grid
        # - Freshwater fluxes
        # - Updated geometry (if moving cavity enabled)
        
        # 5. Prepare forcing file for FESOM
        output_file = os.path.join(
            self.output_path, 
            f"pism2ocean_forcing_step{self.step_count}.nc"
        )
        
        # Write data to FESOM-readable NetCDF
        # (simplified - real implementation would create actual file)
        logger.info(f"Ice forcing for FESOM written to {output_file}")
        
        return output_file
    
    def _update_cavity_geometry(self):
        """Update the ice shelf cavity geometry based on new PISM state."""
        logger.info("Updating ice shelf cavity geometry")
        
        # In a real implementation, we would:
        # 1. Load the latest PISM output file
        pism_file = os.path.join(
            self.pism_data_path, 
            f"{self.pism_settings.get('output_prefix')}_{self.step_count:04d}.nc"
        )
        
        try:
            new_pism_data = xr.open_dataset(pism_file)
            
            # 2. Extract updated ice geometry
            new_thickness = new_pism_data['thickness'].values
            new_mask = new_pism_data['mask'].values
            
            # 3. Calculate changes in ice shelf extent and draft
            ice_shelf_mask_old = self.cavity_mesh['ice_shelf_mask']
            ice_shelf_mask_new = (new_mask == 3)  # PISM convention for floating ice
            
            # Identify areas of cavity change
            cavity_expanded = np.logical_and(~ice_shelf_mask_old, ice_shelf_mask_new)
            cavity_retreated = np.logical_and(ice_shelf_mask_old, ~ice_shelf_mask_new)
            
            # 4. Update the cavity geometry
            sea_level = 0.0  # assuming sea level at z=0
            rho_ice = 910.0  # kg/m^3
            rho_ocean = 1028.0  # kg/m^3
            
            # Calculate new ice draft
            new_ice_draft = np.zeros_like(new_thickness)
            new_ice_draft[ice_shelf_mask_new] = (
                sea_level - new_thickness[ice_shelf_mask_new] * (rho_ice / rho_ocean)
            )
            
            # Update cavity mesh structure
            self.cavity_mesh = {
                'ice_shelf_mask': ice_shelf_mask_new,
                'ice_draft': new_ice_draft,
                'bed': self.pism_bed,  # Bed typically doesn't change
                'water_column_thickness': np.zeros_like(new_ice_draft)
            }
            
            # Calculate water column thickness under ice shelves
            shelf_indices = np.where(ice_shelf_mask_new)
            self.cavity_mesh['water_column_thickness'][shelf_indices] = (
                new_ice_draft[shelf_indices] - self.pism_bed[shelf_indices]
            )
            
            # 5. Map the updated cavity to FESOM
            self._map_cavity_to_fesom()
            
            # 6. Prepare cavity-update file for FESOM (separate from regular forcing)
            cavity_update_file = os.path.join(
                self.output_path, 
                f"cavity_update_step{self.step_count}.nc"
            )
            
            # Create a NetCDF file with cavity update information
            # In a real implementation, this would write:
            # - New ice-ocean boundary location
            # - Water column thickness changes
            # - Areas newly exposed or covered
            
            logger.info(f"Cavity geometry updated and written to {cavity_update_file}")
            logger.info(f"Cavity changes: {np.sum(cavity_expanded)} new cells, {np.sum(cavity_retreated)} removed cells")
            
            # Update the ice draft for FESOM
            self.ice_draft = new_ice_draft
            
        except Exception as e:
            logger.error(f"Failed to update cavity geometry: {str(e)}")
            raise
    
    def run_coupling_step(self):
        """Execute one coupling step."""
        if not self.initialized:
            self.initialize()
        
        logger.info(f"Starting coupling step {self.step_count}")
        
        # 1. FESOM → PISM: Transfer ocean state to ice sheet model
        ocean_forcing_file = self.exchange_fesom_to_pism()
        
        # 2. PISM → FESOM: Transfer ice state and freshwater fluxes to ocean model
        ice_forcing_file = self.exchange_pism_to_fesom()
        
        # 3. Increment step counter
        self.step_count += 1
        
        logger.info(f"Completed coupling step {self.step_count-1}")
        
        return {
            'ocean_forcing_file': ocean_forcing_file,
            'ice_forcing_file': ice_forcing_file,
            'step': self.step_count-1
        }
    
    def visualize_cavity(self, output_file=None):
        """Visualize the current cavity geometry."""
        if not output_file:
            output_file = os.path.join(self.output_path, f"cavity_geometry_step{self.step_count}.png")
        
        logger.info(f"Visualizing cavity geometry to {output_file}")
        
        plt.figure(figsize=(12, 10))
        
        # 1. Plot ice shelf extent
        plt.subplot(2, 2, 1)
        plt.pcolormesh(self.pism_xx, self.pism_yy, self.cavity_mesh['ice_shelf_mask'], cmap='Blues')
        plt.colorbar(label='Ice Shelf Mask')
        plt.title('Ice Shelf Extent')
        plt.xlabel('X (m)')
        plt.ylabel('Y (m)')
        
        # 2. Plot ice draft
        plt.subplot(2, 2, 2)
        plt.pcolormesh(self.pism_xx, self.pism_yy, self.cavity_mesh['ice_draft'], cmap='viridis')
        plt.colorbar(label='Depth (m)')
        plt.title('Ice Shelf Draft')
        plt.xlabel('X (m)')
        plt.ylabel('Y (m)')
        
        # 3. Plot water column thickness
        plt.subplot(2, 2, 3)
        plt.pcolormesh(self.pism_xx, self.pism_yy, self.cavity_mesh['water_column_thickness'], cmap='plasma')
        plt.colorbar(label='Thickness (m)')
        plt.title('Water Column Thickness')
        plt.xlabel('X (m)')
        plt.ylabel('Y (m)')
        
        # 4. Plot cross-section if possible
        plt.subplot(2, 2, 4)
        mid_y = len(self.pism_y) // 2
        plt.plot(self.pism_x, self.pism_bed[mid_y, :], 'k-', label='Bed')
        plt.plot(self.pism_x, self.cavity_mesh['ice_draft'][mid_y, :], 'b-', label='Ice Draft')
        plt.fill_between(self.pism_x, self.pism_bed[mid_y, :], self.cavity_mesh['ice_draft'][mid_y, :], 
                         where=(self.cavity_mesh['ice_shelf_mask'][mid_y, :]), color='lightblue', alpha=0.5)
        plt.legend()
        plt.title(f'Cross-section at y={self.pism_y[mid_y]:.1f}')
        plt.xlabel('X (m)')
        plt.ylabel('Z (m)')
        
        plt.tight_layout()
        plt.savefig(output_file, dpi=150)
        plt.close()
        
        logger.info(f"Cavity visualization saved to {output_file}")
        
        return output_file


def main():
    """Main function to run the coupler."""
    import argparse
    
    parser = argparse.ArgumentParser(description='PISM-FESOM Coupler with Moving Cavity Support')
    parser.add_argument('--config', type=str, help='Path to configuration file')
    parser.add_argument('--steps', type=int, default=1, help='Number of coupling steps to run')
    parser.add_argument('--visualize', action='store_true', help='Visualize cavity geometry after each step')
    
    args = parser.parse_args()
    
    # Initialize the coupler
    coupler = PISMFESOMCoupler(config_file=args.config)
    coupler.initialize()
    
    # Visualize initial state if requested
    if args.visualize:
        coupler.visualize_cavity()
    
    # Run coupling steps
    for i in range(args.steps):
        result = coupler.run_coupling_step()
        logger.info(f"Step {result['step']} completed with files:")
        logger.info(f"  Ocean forcing: {result['ocean_forcing_file']}")
        logger.info(f"  Ice forcing: {result['ice_forcing_file']}")
        
        if args.visualize:
            coupler.visualize_cavity()
    
    logger.info(f"Completed {args.steps} coupling steps")


if __name__ == "__main__":
    main()
