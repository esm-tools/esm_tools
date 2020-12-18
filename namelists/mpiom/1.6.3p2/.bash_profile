module unload netcdf_c/4.3.2-gcc48
module load anaconda3
export PATH=$PATH:~/.local/bin/
export LC_ALL=en_US.utf-8
export LANG=en_US.utf-8

source $HOME/esm_master_tabcomplete.bash

alias sq='squeue -u a270152'

alias status='git status'
alias commit='git commit -m'
alias add='git add'
alias glola='git log --graph --pretty='\''%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset'\'' --all'

alias grep='grep --color'

alias pyd='python3 -m ipdb'

alias esmd='python3 $HOME/esm_diff/esm_diff.py'
alias esmdv='vim $HOME/esm_diff/esm_diff.out'
