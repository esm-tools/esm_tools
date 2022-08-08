import yaml

from esm_runscripts.filedicts import SimulationFile


def test_filedicts_basics(fs):
    """Tests basic attribute behavior of filedicts"""

    dummy_config = """
    echam:
        files:
            jan_surf:
                name_in_pool: T63CORE2_jan_surf.nc
                name_in_work: unit.24
                filetype: NetCDF
                description: >
                    Initial values used for the simulation, including
                    properties such as geopotential, temperature, pressure
    """
    config = yaml.safe_load(dummy_config)
    fs.create_file("/work/ollie/pool/ECHAM/T63/T63CORE2_jan_surf.nc")
    sim_file = SimulationFile(config["echam"]["files"]["jan_surf"])
    assert sim_file["name_in_work"] == "unit.24"
