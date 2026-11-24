#!/bin/bash
# Usage: ./append_namelist.sh input.txt target.f90

input_file="$1"
target_file="$2"

echo "Appending $input_file to $target_file"
echo "" >> "$target_file"
cat "$input_file" >> "$target_file"
echo "" >> "$target_file"
