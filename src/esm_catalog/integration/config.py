"""Load and parse ESM-Tools finished_config.yaml."""

from pathlib import Path


def load_config(path: Path | str | None) -> dict | None:
    """Load an ESM-Tools YAML config file.

    Uses ruamel.yaml to preserve comments (important for downstream round-trip
    use by ESM-Tools itself).  Returns None if path is None.
    """
    if path is None:
        return None
    from ruamel.yaml import YAML
    yaml = YAML()
    return yaml.load(Path(path))
