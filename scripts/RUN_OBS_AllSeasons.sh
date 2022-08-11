#!/bin/bash

# use sed to feed new parameters/files into the standard analysis batch script(s)
sed -e 's/04_run_superlearner.R/14_run_sl_observed_allseasons.R/' -e 's/-n 32/-n 16/' RUN_TARGET-pkrate_LAMBDA-lambda-min.sh | sbatch
sed -e 's/04_run_superlearner.R/14_run_sl_observed_allseasons.R/' -e 's/-n 32/-n 16/' RUN_TARGET-pkweek_LAMBDA-lambda-min.sh | sbatch
sed -e 's/04_run_superlearner.R/14_run_sl_observed_allseasons.R/' -e 's/-n 32/-n 16/' RUN_TARGET-cumhosp_LAMBDA-lambda-min.sh | sbatch
