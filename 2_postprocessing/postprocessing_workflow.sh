#!/bin/bash
#
##############################
# Main script for postprocessing OpenMalaria simulations on the cluster. 
# INPUT:
#       SIM_FOLDER = folder with simulation results
#       PARAM_TAB = file with the simulation parameters
#       OUTPUT_FOLDER = folder with postprocessing results
#       FOLLOW_UP = integer representing the survey index to consider for 
#                   evaluating intervention impact
# OUTPUT:
#       The script creates the following folders in the specified OUTPUT_FOLDER:
#               split/ = folder containing scenario parameters for each setting
#               processed/ = folder containing processing results
#
# SYNTHAX: 
#       bash postprocessing_workflow.sh SIM_FOLDER FOLLOW_UP
# 
#
# created 14.09.2019
# monica.golumbeanu@unibas.ch
#############################

SIM_FOLDER=$1
FOLLOW_UP=$2

PARAM_TAB=$SIM_FOLDER"param_tab.txt"
OM_FOLDER=$SIM_FOLDER"om/"
OUTPUT_FOLDER=$SIM_FOLDER"postprocessing_"$FOLLOW_UP"/"
split_folder=$OUTPUT_FOLDER"split/"

# create the necessary folders
mkdir -p $OUTPUT_FOLDER
mkdir -p $split_folder

# split the parameter table by setting
Rscript split_param_table.R $PARAM_TAB $split_folder

# Submit postprocessing array job
split_files=(${split_folder}*.txt)
NUM=${#split_files[@]}
sbatch -W --array=1-$NUM job_postprocessing.sh $split_folder $OM_FOLDER $OUTPUT_FOLDER $FOLLOW_UP
