#!/bin/bash

#SBATCH -J SL-FluHosp-pkrate-lambda-min-Sens-ElastNetRF
#SBATCH --time=30:00
#SBATCH --mem=150GB
#SBATCH -n 8

#SBATCH --array=01-30
#SBATCH -o SL-FluHosp-Target-pkrate-Lambda-lambda-min-Sens-ElastNetRF-ArrayID-%A-JobID-%J-Week-%a.log

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
export LEARNER_SELECT="elastnet|rf"
export TARGET_SELECT=pkrate

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

cat inst/04_run_superlearner.R

echo "================================================================"
echo " EINDE ROLPAD"
echo "================================================================"

echo ""
echo ""
echo ""

echo "================================================================"
echo " EXECUTE CODE"
echo "================================================================"

Rscript inst/04_run_superlearner.R --vanilla
