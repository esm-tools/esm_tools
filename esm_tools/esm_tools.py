def init(thisfolder):
    from esm_rcfile import set_rc_entry
    set_rc_entry("FUNCTION_PATH", thisfolder + "/configs")
    set_rc_entry("NAMELIST_PATH", thisfolder + "/namelists")
    set_rc_entry("RUNSCRIPT_PATH", thisfolder + "/runscripts")
