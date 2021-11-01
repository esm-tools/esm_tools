#!/bin/bash
# replace full python path in shebang with #!/usr/bin/env python
# required on JUWELS if you switch between Stages
# Sebastian Wahl 2021-04-30
# 
for f in ~/.local/bin/* ; do
	sed -i '1 s@^.*$@#!/usr/bin/env python@' $f
done
