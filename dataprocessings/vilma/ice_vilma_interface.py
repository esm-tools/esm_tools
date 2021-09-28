def prepare_environment(config):
    environment_dict = {
        ICE_TO_VILMA: 1,
        BASE_DIR: config["general"]["base_dir"],
        solidearth_ice_thickness_file: "/some/file",
    }
    return environment_dict
