import os


def set_global_attr(fname, attribute, value):
    os.system("ncatted -a " + attribute + ",global,o,l," + value + " " + fname)
