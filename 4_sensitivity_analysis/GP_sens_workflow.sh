#!/bin/bash
#
##############################
# Main script for running sensitivity analysis of a GP model. 
# INPUT:
#       GP_DIR: folder with the trained GP models, one job will be submitted per model and EIR level
#       PARAM_RANGES_FILE: full path to the file containing the parameter ranges
#       SENS_DEST_DIR: destiantion folder where the results will be saved
# OUTPUT:
#       The script saves the sensitivity analysis results for each GP model in the SENS_DEST_DIR
#
# SYNTHAX: 
#       bash GP_sens_workflow.sh GP_DIR PARAM_RANGES_FILE SENS_DEST_DIR
# 
#
# created 14.09.2019
# monica.golumbeanu@unibas.ch
#############################

GP_DIR=$1
PARAM_RANGES_FILE=$2
SENS_DEST_DIR=$3

# create destination directory
mkdir -p $SENS_DEST_DIR

# Submit GP sensitivity analysis array job
gp_files=(${GP_DIR}*.RData)
NUM=${#gp_files[@]}

echo $NUM

for EIR in `seq 1 .1 25`
do
    sbatch -W --array=1-$NUM job_sens_GP.sh $GP_DIR $PARAM_RANGES_FILE $EIR $SENS_DEST_DIR
done

    