#!/usr/bin/env python

from esm_parser.yaml_to_dict import yaml_file_to_dict
import yaml

def main():
    s2m = yaml_file_to_dict("../configs/esm_master/setups2models.yaml")
    with open("../configs/esm_master/defaults.yaml", "w") as yml:
        yaml.dump(s2m["defaults"], yml)
        del s2m["defaults"]
    with open("../configs/esm_master/esm-software.yaml", "w") as yml:
        yaml.dump(s2m["esm-software"], yml)
        del s2m["esm-software"]
    for thing in ["setups", "components", "couplings"]:
        for component in s2m[thing]:
            with open("../configs/esm_master/"+thing+"/"+component+".yaml", "w") as yml:
                yaml.dump(s2m[thing][component], yml)



if __name__ == "__main__":
    main()
