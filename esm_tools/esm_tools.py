"""Main module."""
import os, sys
    
rcfile = os.path.expanduser("~") + "/.esmtoolsrc"

def get_paths():
    function_path = read_rc_entry("FUNCTION_PATH")
    namelist_path = read_rc_entry("NAMELIST_PATH")
    runscript_path = read_rc_entry("RUNSCRIPT_PATH")
    return function_path, namelist_path, runscript_path

def set_rc_entry(key, value):
    all_lines = [key + "=" + value]

    if os.path.isfile(rcfile):
        with open(rcfile) as rc:
            for line in rc.readlines():
                line = line.strip()
                if not key == line.split("=", 1)[0]:
                    all_lines.append(line)
        os.remove(rcfile)

    with open(rcfile, "w") as rc:
        for line in all_lines:
                rc.write(line + "\n")


def get_rc_entry(key):
    if os.path.isfile(rcfile):
        with open(rcfile) as rc:
            for line in rc.readlines():
                line = line.strip()
                if line.split("=", 1)[0] == key.upper():
                    return line.split("=", 1)[1]
    print (rcfile + " not found, exiting")
    sys.exit(-1)

def import_rc_file():
    if os.path.isfile(rcfile):
        rcdict = {}
        with open(rcfile) as rc:
            for line in rc.readlines():
                line = line.strip()
                rcdict[line.split("=", 1)[0]] = line.split("=", 1)[1]
        return rcdict
    print (rcfile + " not found, exiting")
    sys.exit(-1)
