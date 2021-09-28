class sadfile:
    def __init__(self, config, commands):
        self.name = self.get_sad_filename()
        self.header = self.get_batch_header()
        self.environment = self.get_environment()
        self.commands = commands or self.get_run_commands()

        self.tidy_call = (
            "esm_runscripts "
            + config["general"]["scriptname"]
            + " -e "
            + config["general"]["expid"]
            + " -t tidy_and_resubmit -p ${process} -j "
            + config["general"]["jobtype"]
        )

    @staticmethod
    def write_simple_runscript(self, write_tidy_call=True):
        with open(self.sadfilename, "w") as sadfile:
            for line in self.header:
                sadfile.write(line + "\n")
            sadfile.write("\n")
            for line in self.environment:
                sadfile.write(line + "\n")
            sadfile.write("\n")
            sadfile.write("cd " + self.config["general"]["thisrun_work_dir"] + "\n")
            for line in commands:
                sadfile.write(line + "\n")
            sadfile.write("process=$! \n")
            sadfile.write(
                "cd " + self.config["general"]["experiment_scripts_dir"] + "\n"
            )
            if write_tidy_call:
                sadfile.write(tidy_call + "\n")

        self.submit_command = self.get_submit_command(sadfilename)

        six.print_("\n", 40 * "+ ")
        six.print_("Contents of ", sadfilename, ":")
        with open(sadfilename, "r") as fin:
            print(fin.read())
        if os.path.isfile(self.batch.bs.filename):
            six.print_("\n", 40 * "+ ")
            six.print_("Contents of ", self.batch.bs.filename, ":")
            with open(self.batch.bs.filename, "r") as fin:
                print(fin.read())
