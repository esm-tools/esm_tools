"""
Example usage of builder_multi_level.py

This demonstrates how to:
1. Auto-discover experiments from a directory structure
2. Load experiment metadata from YAML configuration
3. Build a multi-level catalog with multiple experiments
4. Organize data hierarchically: Root → Experiment → Variable Collection → Items
"""

import yaml
from pathlib import Path
import builder_multi_level as builder


def load_experiment_config(config_file="experiments_config.yaml"):
    """Load experiment metadata from YAML configuration file.

    Args:
        config_file: Path to YAML configuration file

    Returns:
        dict: Configuration with 'experiments' and 'defaults' keys
    """
    config_path = Path(config_file)
    if config_path.exists():
        with open(config_path, "r") as f:
            return yaml.safe_load(f)
    else:
        print(f"Warning: Config file {config_file} not found. Using defaults.")
        return {"experiments": {}, "defaults": {}}


def discover_experiments(base_dir, config):
    """Auto-discover experiments from directory structure.

    Searches for directories containing 'outdata/fesom/*.nc' files.
    Combines discovered paths with metadata from config.

    Args:
        base_dir: Base directory to search for experiments
        config: Configuration dict from YAML file

    Returns:
        dict: Experiments dict ready for build_multi_level_catalog()
    """
    base_path = Path(base_dir)
    experiments = {}

    # Get defaults from config
    defaults = config.get("defaults", {})
    default_description = defaults.get("description", "FESOM experiment")
    default_metadata = defaults.get("metadata", {})

    # Discover experiments
    for entry in base_path.iterdir():
        if entry.is_dir():
            data_path = entry / "outdata/fesom"
            if data_path.exists():
                files = list(data_path.glob("*.nc"))
                if files:
                    exp_name = entry.stem

                    # Get metadata from config or use defaults
                    exp_config = config.get("experiments", {}).get(exp_name, {})

                    experiments[exp_name] = {
                        "data_dir": str(data_path),
                        "description": exp_config.get(
                            "description", f"{default_description}: {exp_name}"
                        ),
                        "metadata": {
                            **default_metadata,  # Start with defaults
                            **exp_config.get(
                                "metadata", {}
                            ),  # Override with specific config
                        },
                    }

    return experiments


if __name__ == "__main__":
    # Configuration
    exp_base_dir = Path(
        "/albedo/work/user/pgierz/SciComp/Tutorials/AWIESM_Basics/experiments"
    )
    output_dir = Path("/albedo/work/user/pasili001/stac_catalog/multi_level")
    config_file = "experiments_config.yaml"

    # Load experiment metadata from YAML
    print("Loading experiment configuration...")
    config = load_experiment_config(config_file)

    # Auto-discover experiments
    print(f"Discovering experiments in: {exp_base_dir}")
    experiments = discover_experiments(exp_base_dir, config)

    print(f"\nFound {len(experiments)} experiments:")
    for exp_name, exp_info in experiments.items():
        print(f"  - {exp_name}: {exp_info['description']}")
        print(f"    Data: {exp_info['data_dir']}")
        if exp_info["metadata"]:
            print(f"    Metadata: {exp_info['metadata']}")

    if not experiments:
        print("\nNo experiments found! Check the base directory path.")
        exit(1)

    # Build the multi-level catalog
    print(f"\n{'='*70}")
    print("Building multi-level STAC catalog...")
    print(f"{'='*70}")

    builder.build_multi_level_catalog(
        experiments=experiments,
        output_dir=str(output_dir),
        root_catalog_id="fesom-experiments",
        root_description="FESOM model experiments organized by experiment and variable",
    )

    print(f"\n{'='*70}")
    print("Catalog structure:")
    print(f"{'='*70}")
    print(f"Root: {output_dir / 'catalog.json'}")
    for exp_name in experiments.keys():
        print(f"  └── {exp_name}/")
        print(f"      └── catalog.json")
        print(f"      └── <variable-collections>/")
    print(f"\nTo read the catalog:")
    print(f"  python reading_multi_level.py")
