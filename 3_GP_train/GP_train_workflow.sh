#!/bin/bash
#
##############################
# Main script for training a Gaussian process emulator. 
# INPUT:
#       INPUT_DIR = folder containing postprocessing result for each setting
#       DEST_DIR = folder where the trained models will be saved
#       PREDICTED = name of the output measure to be predicted, must match a
#                   column name in the postprocessing data frame
# OUTPUT:
#       The script submits for each postprocessing result a job
#       which trains a GP emulator.
#
# SYNTHAX: 
#       bash GP_train_workflow.sh INPUT_DIR DEST_DIR PREDICTED
# 
#
# created 14.09.2019
# monica.golumbeanu@unibas.ch
#############################

SIM_DIR=$1
FOLLOW_UP=$2
PREDICTED="prev_red"

INPUT_DIR=$SIM_DIR"postprocessing_"$FOLLOW_UP"/"
DEST_DIR=$SIM_DIR"gp_"$FOLLOW_UP"/"
TRAINING_DIR=$DEST_DIR"/trained/"
RANGES_FILE=$SIM_DIR"param_ranges.RData"

# create destination directories
mkdir -p $DEST_DIR
mkdir -p $TRAINING_DIR

# Submit postprocessing array job
setting_postprocessing_results=(${INPUT_DIR}seeds_*.txt)
NUM=${#setting_postprocessing_results[@]}
sbatch -W --array=1-$NUM job_train_GP.sh $INPUT_DIR $TRAINING_DIR $PREDICTED $RANGES_FILE
    