#!/usr/bin/env python

# TODO: add a documentation

import re
import shutil
import os
import pathlib

# directory and file paths
conf_dir = pathlib.Path.cwd() / 'config'
mh_fname = conf_dir / "mh-linux"
backup_fname = conf_dir / "mh-linux.bak"

# convert them to strings since versions prior to 3.6 os module does not like
# pathlib objects
mh_fname = str(mh_fname)
backup_fname = str(backup_fname)

# create a backup file if it does not exit. Copy the contents of the original
# file to the backup file
if not os.path.isfile(backup_fname):
    shutil.copyfile(mh_fname, backup_fname)

# backup file is read-only and original will be overwritten
backup_file = open(backup_fname, 'r')

# erase the contents of the original file
mh_file = open(mh_fname, 'r+')
mh_file.truncate(0)

# read the backup file line by line and copy the contents to mh file
line = backup_file.readline()
line_num = 1
inside_gcc_block = False         
is_file_changed = False 

# regex search patterns
gcc_block_pattern = r'default\|gcc*.\)'
fc_pattern = r'^\s*FC\s*=\s*[a-zA-Z0-9_]*$'
f77_pattern = r'^\s*F77\s*=\s*[a-zA-Z0-9_]*$'


while line:    
    # check for the start of the GCC case statement
    if re.search(gcc_block_pattern, line) != None:
        inside_gcc_block = True
        #print(f"found the gcc block on line {line_num}")
    
    # inside the GCC section change
    #     FC  = gfortran   -->   FC  = mpif90
    #     F77 = gfortran   -->   F77 = mpif90
    if inside_gcc_block:
        match_FC = re.search(fc_pattern, line)
        match_F77 = re.search(f77_pattern, line)
        
        if match_FC != None:            
            #print(f"FC found on line {line_num}")
            num_spaces = line.find('FC')
            line = ' ' * num_spaces + 'FC = mpif90\n' 
            is_file_changed = True
        
        elif match_F77 != None:
            #print(f"F77 found on line {line_num}")
            num_spaces = line.find('F77')
            line = ' ' * num_spaces + 'F77 = mpif90\n' 
            is_file_changed = True
    
    # end of the GCC case statement
    if ';;' in line and inside_gcc_block:
        #print(f"Block end on line {line_num}")
        inside_gcc_block = False
        
    # write the lines to the mh file
    mh_file.write(line)    
    line = backup_file.readline()
    line_num += 1

# close the files
backup_file.close()            
mh_file.close()            

if is_file_changed:
    print(':: change_mh_file: compiler blocks in the mh-file have been updated')
