#!/bin/bash

#SBATCH -J SL-FluHosp-TARGET-TARGET_INPUT_LAMBDA-LAMBDA_INPUT
#SBATCH --time=3:30:00
#SBATCH -p batch
#SBATCH --mem=150GB
#SBATCH -n 32

#SBATCH --array=01-30
#SBATCH -o SL-FluHosp-TARGET-TARGET_INPUT-LAMBDA-LAMBDA_INPUT-ArrayID-%A-JobID-%J-Week-%2a.log

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

export LAMBDA_SELECT=LAMBDA_INPUT
export LEARNER_SELECT=LEARNER_INPUT
export TARGET_SELECT=TARGET_INPUT

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

