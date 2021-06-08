#!/bin/bash

# use sed to feed new parameters/files into the standard analysis batch script
# for the prospective analysis

# peak rate
sed -e 's/01-30/5,10,15,20,25/' -e 's/04_run_superlearner.R/07_run_sl_prospective.R/' RUN_TARGET-pkrate_LAMBDA-lambda-min.sh | sbatch --export=ALL,OBS_SEASON='2016-17'
sed -e 's/01-30/5,10,15,20,25/' -e 's/04_run_superlearner.R/07_run_sl_prospective.R/' RUN_TARGET-pkrate_LAMBDA-lambda-min.sh | sbatch --export=ALL,OBS_SEASON='2017-18'
sed -e 's/01-30/5,10,15,20,25/' -e 's/04_run_superlearner.R/07_run_sl_prospective.R/' RUN_TARGET-pkrate_LAMBDA-lambda-min.sh | sbatch --export=ALL,OBS_SEASON='2018-19'

# peak week
sed -e 's/01-30/5,10,15,20,25/' -e 's/04_run_superlearner.R/07_run_sl_prospective.R/' RUN_TARGET-pkweek_LAMBDA-lambda-min.sh | sbatch --export=ALL,OBS_SEASON='2016-17'
sed -e 's/01-30/5,10,15,20,25/' -e 's/04_run_superlearner.R/07_run_sl_prospective.R/' RUN_TARGET-pkweek_LAMBDA-lambda-min.sh | sbatch --export=ALL,OBS_SEASON='2017-18'
sed -e 's/01-30/5,10,15,20,25/' -e 's/04_run_superlearner.R/07_run_sl_prospective.R/' RUN_TARGET-pkweek_LAMBDA-lambda-min.sh | sbatch --export=ALL,OBS_SEASON='2018-19'

# cumulative hospitalizations
sed -e 's/01-30/5,10,15,20,25/' -e 's/04_run_superlearner.R/07_run_sl_prospective.R/' RUN_TARGET-cumhosp_LAMBDA-lambda-min.sh | sbatch --export=ALL,OBS_SEASON='2016-17'
sed -e 's/01-30/5,10,15,20,25/' -e 's/04_run_superlearner.R/07_run_sl_prospective.R/' RUN_TARGET-cumhosp_LAMBDA-lambda-min.sh | sbatch --export=ALL,OBS_SEASON='2017-18'
sed -e 's/01-30/5,10,15,20,25/' -e 's/04_run_superlearner.R/07_run_sl_prospective.R/' RUN_TARGET-cumhosp_LAMBDA-lambda-min.sh | sbatch --export=ALL,OBS_SEASON='2018-19'
