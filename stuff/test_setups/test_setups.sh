#!/bin/bash
#
# Test installation of ESM-Tools including 
#
# - full setup of tools into a python venv
# - compile all configuration(s) listed in $configurations
# - run test simulation for all configuration(s) listed in $configurations
#   if a yaml file 'test_${configuration}.yaml' is available in
#   stuff/test_setups/ 
#
# Sebastian Wahl 08/2020
#
###########################################################################
#
# Set basic properties for testing
#
# Which configuration(s) shall be tested?
# Can be overwritten by command line argument
# Use space as separator for multiple configurations
configurations="foci-default focioifs-2.0 oifs-43r3-v1 "

# Which steps to test? Valid options are 'run' and 'compile'
# put steps="compile run" to first compile and the run a test simulation
steps="compile run"
#
# Advanced settings
#
# Test installation of ESM-Tools?
# If set to yes, ESM-Tools will be installed from scratch into
# $workdir/$configuration/env_esm_tools_$configuration
# for each of the configurations listed above
# if set to no, the current installation will be used
test_install='no'

# If test_install=yes: test install a specific version of a tool?
components=(esm_rcfile esm_runscripts esm_parser)
branch=(develop develop develop)

# IF test_install=yes: Which version of ESM-Tools shall be tested
esm_tools_branch='feature/fix_test_script'

# Open run or contained run (i.e. use the virtual env feature?) 
runtype='--open-run'
# Open run and using a test install into a venv won't work as expected 
# as the venv is not used upon resubmission of a job (limitation of ESM-Tools)
# this only works if you use the release branch of each tool
# as the venv is generated based on the release branch of each tool.
[[ "$test_install" == 'yes' ]] && runtype='--contained-run' 
#
# Which plugins shall be installed during testing (not needed anymore as automatic
# plugin installation now works)
# plugins="preprocess postprocess"
#
# setup environment, make sure python3 is available
#
workdir=
account=None
if [[ "$HOSTNAME" =~ blogin ]] ; then
  module load git
  module load anaconda3/2019.10
  workdir=$WORK/tmp
  account=$USER
elif [[ "$HOSTNAME" =~ glogin ]] ; then
  module load git
  module load anaconda3
  workdir=$WORK/tmp
  account=$USER
elif [[ "$HOSTNAME" =~ mlogin ]] ; then
  #module load git
  module unload netcdf_c/4.3.2-gcc48 
  module load anaconda3/bleeding_edge
  workdir=/work/bb0519/tmp
  account=bb0519
elif [[ "$HOSTNAME" =~ juwels ]] ; then
  module --force purge
  module use $OTHERSTAGES 
  # testing the intel2019 stage
  #module load Stages/2019a
  #module load Intel/2019.3.199-GCC-8.3.0
  #module load Python/3.6.8
  #module load git
  # testing the intel2020 stage
  module load Stages/Devel-2020
  module load Intel/2020.4.304-GCC-9.3.0 
  module load IntelMPI/2019.8.254
  module load imkl/2020.2.254
  module load Python/3.8.5 
  module load git
  workdir=$PROJECT/$USER/tmp
  account=$PROJECT
elif [[ "$HOSTNAME" =~ nesh-fe ]] ; then
  module load python/3.8.6
  workdir=$WORK/tmp
  account=None
else
  echo "`date`: ERROR: $0 not yet ported to $HOSTNAME" | tee -a ${logdir}/test_${configuration}.log
  exit 1
fi

###########################################################################
#
# allow command line override of configuration
#
if [[ $# -ge 1 ]] ; then
  configurations=$@
  echo
  echo "`date`: Using configuration(s) $@ from command line input"
  echo
fi

logdir=$(pwd)
scriptdir=$(pwd)

# Loop over configurations to be tested
for configuration in ${configurations} ; do
  # if don't do a test installation into a venv we need to make sure
  # we find the test yamls
  if [[ "$test_install" == 'no' ]] ; then
    if [[ ! -f test_${configuration}.yaml ]] ; then
      echo "`date`: ERROR: test_${configuration}.yaml not available in $(pwd)" | tee -a ${logdir}/test_${configuration}.log
      exit 1
    fi
  fi
  # the following uses the fact that the name of a configuration in
  # ESM-Toos is always configuration=<model or setup>-<version string that may contain '-'>
  # so we need a split on the first '-'
  model=${configuration%%-*} # the characters BEFORE the first hyphen
  version=${configuration#*-} # the characters AFTER the first hyphen

  mkdir -p $workdir
  cd $workdir || exit 1
  
  #
  # setup of esm tools and compilation
  #
  if [[ "$steps" =~ compile ]]; then

    rm -rf $workdir/${configuration}
    mkdir $workdir/$configuration; cd $workdir/$configuration

    if [[ "$test_install" == 'yes' ]] ; then
      # setup virtual environment
      rm -rf env_esm_tools_$configuration
      python -m venv env_esm_tools_$configuration
      source env_esm_tools_$configuration/bin/activate
      # numpy is required by esm_tools and must be available inside the venv
      #pip install --upgrade pip
      pip install numpy
      echo "`date`: OK: Python virtual env setup" | tee -a ${logdir}/test_${configuration}.log

      # install
      git clone -b ${esm_tools_branch} https://github.com/esm-tools/esm_tools.git
      cd esm_tools
      # TODO: on blogin an "pip install ." works on mistral it gives an error even though we are inside a venv
      # pip install -e . as a workaround works.
      pip install -e .
      if [[ $? -gt 0 ]] ; then
        echo "`date`: ERROR: pip install . failed" | tee -a ${logdir}/test_${configuration}.log
        exit 1
      else    
        echo "`date`: OK: pip install ." | tee -a ${logdir}/test_${configuration}.log
      fi

      if ! [[ ${#components[@]} -eq ${#branch[@]} ]] ; then
        echo "`date`: ERROR: components and branch must have the same length" | tee -a ${logdir}/test_${configuration}.log
        exit 1
      fi
      for i in ${!components[@]}; do
        esm_versions upgrade ${components[$i]}=${branch[$i]}
        if [[ $? -gt 0 ]] ; then
          echo "`date`: ERROR: esm_versions upgrade ${components[$i]}=${branch[$i]} failed" | tee -a ${logdir}/test_${configuration}.log
          exit 1
        else
          echo "`date`: OK: esm_versions upgrade ${components[$i]}=${branch[$i]}" | tee -a ${logdir}/test_${configuration}.log
        fi
      done
      esm_versions check | tee -a ${logdir}/test_${configuration}.log

      # install plugins
      mkdir -p $workdir/$configuration/plugins
      for plugin in $plugins ; do
        # TODO: this does not work (at least not in a venv on mistral)
        # pip install git+https://github.com/esm-tools-plugins/${plugin}.git
        cd $workdir/$configuration/plugins/
        git clone https://github.com/esm-tools-plugins/${plugin}.git
        cd $plugin 
        pip install -e .
        if [[ $? -gt 0 ]] ; then
          echo "`date`: ERROR: pip install git+https://github.com/esm-tools-plugins/${plugin}.git failed" | tee -a ${logdir}/test_${configuration}.log
          exit 1
        fi
      done

    fi # test_install

    # compile
    esm_versions check | tee -a ${logdir}/test_${configuration}.log

    mkdir -p $workdir/$configuration/models
    rm -rf $workdir/$configuration/models/${configuration}
    cd $workdir/$configuration/models

    # install does not work for OIFS at the moment, need to file an issue
    # esm_master install-${configuration}
    esm_master get-${configuration}
    if [[ $? -gt 0 ]] ; then
      echo "`date`: ERROR: esm_master get-${configuration} failed" | tee -a ${logdir}/test_${configuration}.log
      exit 1
    fi

    esm_master comp-${configuration}
    if [[ $? -gt 0 ]] ; then
      #cp -pv ~/.esmtoolsrc_ci_backup ~/.esmtoolsrc # restore .esmtoolsrc
      echo "`date`: ERROR: esm_master comp-${configuration} failed" | tee -a ${logdir}/test_${configuration}.log
      exit 1
    fi
    echo "`date`: OK: esm_master install-$configuration" | tee -a ${logdir}/test_${configuration}.log

  fi

  if [[ "$steps" =~ run ]]; then

    cd $workdir/$configuration

    if [[ "$test_install" == 'yes' ]] ; then
      # activate virtual environment with correct esmtoolsrc saved in previous step
      if [[ ! -f env_esm_tools_${configuration}/bin/activate ]]  ; then
        echo "`date`: ERROR: env_esm_tools_${configuration} not available. Did you run the compile step?" | tee -a ${logdir}/test_${configuration}.log
        exit 1
      fi
      source env_esm_tools_$configuration/bin/activate
	 fi

    # modify test_${configuration}.yaml and start the simulation
    mkdir -p $workdir/$configuration/esm-experiments
    rm -rf $workdir/$configuration/esm-experiments/test_${configuration}

    if [[ "$test_install" == 'yes' ]] ; then
       cd $workdir/$configuration/esm_tools/stuff/test_setups
	 else
	    cd $scriptdir 
    fi    

    if [[ -f test_${configuration}.yaml ]]  ; then
      cp test_${configuration}.yaml test_${configuration}_tmp.yaml
      sed -i "s#   base_dir:.*#   base_dir: ${workdir}/${configuration}/esm-experiments#" test_${configuration}_tmp.yaml
      sed -i "s#   model_dir:.*#   model_dir: ${workdir}/${configuration}/models/$configuration#" test_${configuration}_tmp.yaml
      sed -i "s#   account:.*#   account: ${account}#" test_${configuration}_tmp.yaml
      esm_runscripts $runtype -e test_${configuration} test_${configuration}_tmp.yaml

      # TODO: esm_runscripts sometimes returns 0 despite an error 
      if [[ $? -gt 0 ]] ; then
        echo "`date`: ERROR: esm_runscripts -e test_${configuration} test_${configuration}.yaml failed" | tee -a ${logdir}/test_${configuration}.log
        exit 1
      else
        echo "`date`: OK: esm_runscripts -e test_${configuration} test_${configuration}.yaml" | tee -a ${logdir}/test_${configuration}.log
      fi
    else
      echo "`date`: ERROR: test_${configuration}.yaml not available in $(pwd)" | tee -a ${logdir}/test_${configuration}.log
      exit 1
    fi

    # monitor run
    logfile="${workdir}/${configuration}/esm-experiments/test_${configuration}/log/test_${configuration}_${model}.log"
    echo "`date`: Logfile: $logfile" | tee -a ${logdir}/test_${configuration}.log
    seen_in_queue=false

    while [ true ] ; do
      # set seen_in_queue to true once the test simulation has appeared in the queue
      #  -o "%.25j" just lists the experiment id (25 characters to catch long experiment names)
      [[ $(squeue -u $USER -o "%.25j" | grep "test_${configuration}" | wc -l) -gt 0 ]] && seen_in_queue=true

      if [[ -f ${logfile} ]] ; then
        # check on log if the test job has been seen in the queue
        if $seen_in_queue ; then
          # if the run has dissappeared from the queue it either finished
          # sucessfully or crashed
          if [[ $(squeue -u $USER -o "%.25j" | grep "test_${configuration}" | wc -l) -eq 0 ]] ; then
            # wait an extra 30s to make sure the logfile has been written
            # might not be necessary but doesn't hurt :-)
            sleep 30s
            echo "`date`: Content of ${logfile}:" | tee -a ${logdir}/test_${configuration}.log
            cat ${logfile} | tee -a ${logdir}/test_${configuration}.log
            # exp has finished sucessfully
            if grep -q "Experiment over" ${logfile} ; then
              echo "`date`: SUCCESS: Experiment finished successfully" | tee -a ${logdir}/test_${configuration}.log
              break 
            else
              #cp -pv ~/.esmtoolsrc_ci_backup ~/.esmtoolsrc
              echo "`date`: ERROR: Experiment failed" | tee -a ${logdir}/test_${configuration}.log
              exit 1
            fi
          else
            echo "`date`: Experiment queued/running" | tee -a ${logdir}/test_${configuration}.log
            squeue -u $USER -o "%.18i %.9P %.25j %.8u %.2t %.10M %.6D %R %S" | grep test_$configuration
            sleep 5s
          fi
        else # if $seen_in_queue ; then
          echo "`date` Experiment started, but not yet queued" | tee -a ${logdir}/test_${configuration}.log
          tail -1 ${logfile}
          sleep 5s
        fi
      else # if [[ -f ${logfile} ]] ; then
        echo "`date` Experiment not yet started" | tee -a ${logdir}/test_${configuration}.log
        sleep 5s
      fi
    done # while [ true ] ; do

  fi

done # loop over configurations
