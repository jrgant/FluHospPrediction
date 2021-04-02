#!/bin/bash

#SBATCH -J SL-FluHosp-cumhosp-lambda-min-Sens-SqErrLoss
#SBATCH --time=1:45:00
#SBATCH --partition=bigmem
#SBATCH --mem=500GB
#SBATCH -n 32

#SBATCH --array=1,5,10,15,20,25,30
#SBATCH -o SL-FluHosp-Target-cumhosp-Lambda-lambda-min-Sens-SqErrLoss-ArrayID-%A-JobID-%J-Week-%a.log

#SBATCH --mail-type=ALL
#SBATCH --mail-user=jrgant@brown.edu

echo ""
echo ""
echo ""

echo "****************************************************************"
echo "Starting job task $SLURM_ARRAY_TASK_ID on $HOSTNAME"
echo "****************************************************************"

echo ""
echo ""
echo ""

echo "================================================================"
echo " INPUT PARAMETERS "
echo "================================================================"

export LAMBDA_SELECT=lambda-min
export LEARNER_SELECT="elast|lasso|ridge|glm|nnet|mars|rf|svm"
export TARGET_SELECT=cumhosp

echo "LAMBDA: $LAMBDA_SELECT"
echo "LEARNER: $LEARNER_SELECT"
echo "PREDICTION TARGET: $TARGET_SELECT"

module load R/3.6.3
cd ~/data/jgantenb/FluHospPrediction

echo ""
echo ""
echo ""

echo "================================================================"
echo " PRINT CODE "
echo "================================================================"

cat inst/05_run_sqerrloss_sensitivity.R

echo "================================================================"
echo " EINDE ROLPAD"
echo "================================================================"

echo ""
echo ""
echo ""

echo "================================================================"
echo " EXECUTE CODE"
echo "================================================================"

Rscript inst/05_run_sqerrloss_sensitivity.R --vanilla
