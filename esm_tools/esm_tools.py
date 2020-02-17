"""Main module."""
import os

def get_paths():
    import os 
    rcfile = os.path.expanduser("~") + "/.esmtoolsrc"

    function_path = namelist_path = runscript_path = ""

    with open(rcfile) as rc:
        while line = rc.readline():
            if line.startswith("FUNCTION"):
                function_path = line.split("=", 1)[-1]
            elif line.startswith("NAMELIST"):
                namelist_path = line.split("=", 1)[-1]
            elif line.startswith("RUNSCRIPT"):
                runscript_path = line.split("=", 1)[-1]
    return function_path, namelist_path, runscript_path
