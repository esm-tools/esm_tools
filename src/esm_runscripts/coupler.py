import sys

known_couplers = ["oasis3mct", "yac"]


class coupler_class:
    def __init__(self, full_config, name):
        self.name = name

        self.process_ordering = full_config[name]["process_ordering"]
        self.coupled_execs = []
        for exe in self.process_ordering:
            self.coupled_execs.append(full_config[exe]["executable"])
        self.runtime = full_config["general"]["runtime"][5]
        self.nb_of_couplings = 0
        if "coupling_target_fields" in full_config[self.name]:
            for restart_file in list(full_config[self.name]["coupling_target_fields"]):
                self.nb_of_couplings += len(
                    list(full_config[self.name]["coupling_target_fields"][restart_file])
                )
        if "coupling_input_fields" in full_config[self.name]:
            for restart_file in list(full_config[self.name]["coupling_input_fields"]):
                self.nb_of_couplings += len(
                    list(full_config[self.name]["coupling_input_fields"])
                )
        if name == "oasis3mct":
            from . import oasis

            # seb-wahl: manual merge from 'oifs' branch as oifs branch contains many whitespace changes
            self.norestart = full_config["oasis3mct"].get("norestart", "F")
            self.coupler = oasis.oasis(
                self.nb_of_couplings,
                self.coupled_execs,
                self.runtime,
                nnorest=self.norestart,
                mct_version=full_config["oasis3mct"].get("mct_version", "2.8"),
                debug_level=full_config["oasis3mct"].get("debug_level", 1),
                lucia=full_config["oasis3mct"].get("use_lucia", False),
            )
        elif name == "yac":
            from . import yac

            self.coupler = yac.yac(
                full_config,
                self.nb_of_couplings,
                self.process_ordering,
                full_config[self.name]["grids"],
                self.runtime,
            )

        else:
            print("Unknown coupler :", name)
            sys.exit(0)

    def prepare(self, full_config, destination_dir):
        self.add_couplings(full_config)
        self.finalize(destination_dir)
        if full_config["general"]["verbose"]:
            self.print_config_files()

        coupler_name = self.name
        if coupler_name == "yac":
            couplingfile = "coupling.xml"
        else:
            couplingfile = "namcouple"
        return couplingfile

    def print_config_files(self):
        self.coupler.print_config_files()

    def add_files(self, full_config):
        self.coupler.next_coupling = 1
        if "coupling_target_fields" in full_config[self.name]:
            for restart_file in list(full_config[self.name]["coupling_target_fields"]):
                for coupling in full_config[self.name]["coupling_target_fields"][
                    restart_file
                ]:
                    coupling = coupling.replace("<--", "%").replace("--", "&")
                    leftside, rest = coupling.split("%")
                    leftside = leftside.strip()
                    interpolation, rightside = rest.split("&")
                    rightside = rightside.strip()
                    interpolation = interpolation.strip()
                    if ":" in leftside:
                        lefts = leftside.split(":")
                    else:
                        lefts = [leftside]

                    if ":" in rightside:
                        rights = rightside.split(":")
                    else:
                        rights = [rightside]

                    if not len(lefts) == len(rights):
                        print("Left and right side of coupling don't match: ", coupling)
                        sys.exit(0)

                    left_grid = lgrid_info = None
                    right_grid = rgrid_info = None

                    for left, right in zip(lefts, rights):
                        found_left = found_right = False
                        for model in list(full_config):
                            if "coupling_fields" in full_config[model]:
                                if left in full_config[model]["coupling_fields"]:
                                    found_left = True
                                    leftmodel = full_config[model]["executable"]
                                    if not left_grid:
                                        left_grid = full_config[model][
                                            "coupling_fields"
                                        ][left]["grid"]
                                        lgrid_info = full_config[model]["grids"][
                                            left_grid
                                        ]
                                    else:
                                        if (
                                            not left_grid
                                            == full_config[model]["coupling_fields"][
                                                left
                                            ]["grid"]
                                        ):
                                            print(
                                                "All fields coupled together need to exist on same grid"
                                            )
                                            sys.exit(0)
                                if right in full_config[model]["coupling_fields"]:
                                    found_right = True
                                    rightmodel = full_config[model]["executable"]
                                    if not right_grid:
                                        right_grid = full_config[model][
                                            "coupling_fields"
                                        ][right]["grid"]
                                        rgrid_info = full_config[model]["grids"][
                                            right_grid
                                        ]
                                    else:
                                        if (
                                            not right_grid
                                            == full_config[model]["coupling_fields"][
                                                right
                                            ]["grid"]
                                        ):
                                            print(
                                                "All fields coupled together need to exist on same grid"
                                            )
                                            sys.exit(0)
                                if found_right and found_left:
                                    break
                        if not found_left:
                            print("Coupling var not found: ", left)
                        if not found_right:
                            print("Coupling var not found: ", right)
                        if not found_left or not found_right:
                            sys.exit(0)

                    direction_info = None
                    if "coupling_directions" in full_config[self.name]:
                        if (
                            right_grid + "->" + left_grid
                            in full_config[self.name]["coupling_directions"]
                        ):
                            direction_info = full_config[self.name][
                                "coupling_directions"
                            ][right_grid + "->" + left_grid]
                    transf_info = None
                    if "coupling_methods" in full_config[self.name]:
                        if interpolation in full_config[self.name]["coupling_methods"]:
                            transf_info = full_config[self.name]["coupling_methods"][
                                interpolation
                            ]

                    self.coupler.add_output_file(
                        lefts, rights, leftmodel, rightmodel, full_config[self.name]
                    )
                self.coupler.add_restart_files(restart_file, full_config)

    def tidy(self, full_config):
        if full_config[self.name]["lresume"] == False:
            self.prepare_restarts(full_config)

    def prepare_restarts(self, full_config):
        if "coupling_target_fields" in full_config[self.name]:
            for restart_file in list(full_config[self.name]["coupling_target_fields"]):
                all_lefts = []
                all_rights = []
                all_leftmodels = []
                all_rightmodels = []
                for coupling in full_config[self.name]["coupling_target_fields"][
                    restart_file
                ]:
                    coupling = coupling.replace("<--", "%").replace("--", "&")
                    leftside, rest = coupling.split("%")
                    leftside = leftside.strip()
                    interpolation, rightside = rest.split("&")
                    rightside = rightside.strip()
                    if ":" in leftside:
                        lefts = leftside.split(":")
                    else:
                        lefts = [leftside]

                    if ":" in rightside:
                        rights = rightside.split(":")
                    else:
                        rights = [rightside]

                    all_lefts += lefts
                    all_rights += rights

                    for left, right in zip(lefts, rights):
                        found_left = found_right = False
                        for model in list(full_config):
                            if "coupling_fields" in full_config[model]:
                                if left in full_config[model]["coupling_fields"]:
                                    found_left = True
                                    leftmodel = model
                                if right in full_config[model]["coupling_fields"]:
                                    found_right = True
                                    rightmodel = model
                                if found_right and found_left:
                                    break

                    # A coupling restart file can contain fields from multiple models,
                    # therefore, we need to concatenate the left and right models
                    # corresponding to each field
                    all_leftmodels += [leftmodel] * len(lefts)
                    all_rightmodels += [rightmodel] * len(rights)
                    # Check that the dimensions are correct
                    dym_issue = False
                    if len(all_lefts) != len(all_leftmodels):
                        print(
                            "Coupling fields and their corresponding models do not"
                            + "have the same dimensions:"
                        )
                        print("all_lefts =", all_lefts)
                        print("all_leftmodels =", all_leftmodels)
                        dym_issue = True
                    if len(all_rights) != len(all_rightmodels):
                        print(
                            "Coupling fields and their corresponding models do not"
                            + "have the same dimensions:"
                        )
                        print("all_rights =", all_rights)
                        print("all_rightmodels =", all_rightmodels)
                        dym_issue = True
                    if dym_issue:
                        sys.exit(0)

                self.coupler.prepare_restarts(
                    restart_file, all_rights, all_rightmodels, full_config
                )
                self.coupler.prepare_restarts(
                    restart_file + "_recv", all_lefts, all_leftmodels, full_config
                )

    def add_couplings(self, full_config):
        self.coupler.next_coupling = 1
        if self.coupler.name == "oasis3mct":
            if "coupling_target_fields" in full_config[self.name]:
                for restart_file in list(
                    full_config[self.name]["coupling_target_fields"]
                ):
                    for coupling in full_config[self.name]["coupling_target_fields"][
                        restart_file
                    ]:
                        coupling = coupling.replace("<--", "%").replace("--", "&")
                        leftside, rest = coupling.split("%")
                        leftside = leftside.strip()
                        interpolation, rightside = rest.split("&")
                        rightside = rightside.strip()
                        interpolation = interpolation.strip()
                        if ":" in leftside:
                            lefts = leftside.split(":")
                        else:
                            lefts = [leftside]

                        if ":" in rightside:
                            rights = rightside.split(":")
                        else:
                            rights = [rightside]

                        if not len(lefts) == len(rights):
                            print(
                                "Left and right side of coupling don't match: ",
                                coupling,
                            )
                            sys.exit(0)

                        left_grid = lgrid_info = None
                        right_grid = rgrid_info = None

                        for left, right in zip(lefts, rights):
                            found_left = found_right = False
                            for model in list(full_config):
                                if "coupling_fields" in full_config[model]:
                                    if left in full_config[model]["coupling_fields"]:
                                        found_left = True
                                        leftmodel = model
                                        if not left_grid:
                                            left_grid = full_config[model][
                                                "coupling_fields"
                                            ][left]["grid"]
                                            lgrid_info = full_config[model]["grids"][
                                                left_grid
                                            ]
                                        else:
                                            if (
                                                not left_grid
                                                == full_config[model][
                                                    "coupling_fields"
                                                ][left]["grid"]
                                            ):
                                                print(
                                                    "All fields coupled together need to exist on same grid"
                                                )
                                                sys.exit(0)
                                    if right in full_config[model]["coupling_fields"]:
                                        found_right = True
                                        rightmodel = model
                                        if not right_grid:
                                            right_grid = full_config[model][
                                                "coupling_fields"
                                            ][right]["grid"]
                                            rgrid_info = full_config[model]["grids"][
                                                right_grid
                                            ]
                                        else:
                                            if (
                                                not right_grid
                                                == full_config[model][
                                                    "coupling_fields"
                                                ][right]["grid"]
                                            ):
                                                print(
                                                    "All fields coupled together need to exist on same grid"
                                                )
                                                sys.exit(0)
                                    if found_right and found_left:
                                        break
                            if not found_left:
                                print("Coupling var not found: ", left)
                            if not found_right:
                                print("Coupling var not found: ", right)
                            if not found_left or not found_right:
                                sys.exit(0)

                        export_mode = full_config[self.name].get("export_mode", "DEFAULT")

                        direction_info = None
                        if "coupling_directions" in full_config[self.name]:
                            if (
                                right_grid + "->" + left_grid
                                in full_config[self.name]["coupling_directions"]
                            ):
                                direction_info = full_config[self.name][
                                    "coupling_directions"
                                ][right_grid + "->" + left_grid]

                                # Use export_mode from coupling_directions if set. Required for NEMO-AGRIF
                                export_mode = direction_info.get("export_mode",export_mode)
                        transf_info = None
                        if "coupling_methods" in full_config[self.name]:
                            if (
                                interpolation
                                in full_config[self.name]["coupling_methods"]
                            ):
                                transf_info = full_config[self.name][
                                    "coupling_methods"
                                ][interpolation]

                        self.coupler.add_coupling(
                            lefts,
                            lgrid_info,
                            rights,
                            rgrid_info,
                            direction_info,
                            transf_info,
                            restart_file,
                            full_config[self.name]["coupling_time_step"],
                            full_config[self.name]["lresume"],
                            export_mode=export_mode,
                        )

            if "coupling_input_fields" in full_config[self.name]:
                for field_name, field_config in full_config[self.name][
                    "coupling_input_fields"
                ].items():
                    self.coupler.add_input_coupling(
                        field_name, field_config["freq"], field_config["field_filepath"]
                    )

        elif self.coupler.name == "yac":
            if "coupling_directions" in full_config[self.name]:
                transient_id = 1
                for direction in list(full_config[self.name]["coupling_directions"]):
                    for coupling_field in full_config[self.name][
                        "coupling_target_fields"
                    ][direction]:
                        self.coupler.add_coupling(
                            coupling_field,
                            transient_id,
                            direction,
                            full_config[self.name],
                        )
                        transient_id += 1

    def finalize(self, destination_dir):
        self.coupler.finalize(destination_dir)
