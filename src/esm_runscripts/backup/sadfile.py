class runfile:
    def __init__(self, config, commands):
        self.name = self.get_run_filename()
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
        with open(self.runfilename, "w") as runfile:
            for line in self.header:
                runfile.write(line + "\n")
            runfile.write("\n")
            for line in self.environment:
                runfile.write(line + "\n")
            runfile.write("\n")
            runfile.write("cd " + self.config["general"]["thisrun_work_dir"] + "\n")
            for line in commands:
                runfile.write(line + "\n")
            runfile.write("process=$! \n")
            runfile.write(
                "cd " + self.config["general"]["experiment_scripts_dir"] + "\n"
            )
            if write_tidy_call:
                runfile.write(tidy_call + "\n")

        self.submit_command = self.get_submit_command(runfilename)

        print("\n", 40 * "+ ")
        print("Contents of ", runfilename, ":")
        with open(runfilename, "r") as fin:
            print(fin.read())
        if os.path.isfile(self.batch.bs.filename):
            print("\n", 40 * "+ ")
            print("Contents of ", self.batch.bs.filename, ":")
            with open(self.batch.bs.filename, "r") as fin:
                print(fin.read())
