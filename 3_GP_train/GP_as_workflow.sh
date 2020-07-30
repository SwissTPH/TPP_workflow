#!/bin/bash
#
##############################
# Main script for refining a Gaussian process emulator with adaptive sampling
# INPUT:
#       SIM_FOLDER: simulation folder containing all the input files as well as
#                   files and folders created by the various steps of the workflow.
#       FOLLOW_UP = integer representing the survey index to consider for 
#                   evaluating intervention impact
#       PREDICTED = name of the output measure to be predicted, must match a
#                   column name in the postprocessing data frame
#
# OUTPUT:
#
# SYNTHAX: 
#       bash GP_as_workflow.sh SIM_FOLDER
#       bash GP_as_workflow.sh ~/MMC/TPP/simulations/MAB_once_3years_avg_prev/scaffold.xml ~/MMC/TPP/simulations/MAB_once_3years_avg_prev/gp_4/trained/ ~/MMC/TPP/simulations/MAB_once_3years_avg_prev/gp_4/as/ ~/MMC/TPP/simulations/MAB_once_3years_avg_prev/param_ranges.RData 4 prev_red
# 
#
# created 02.10.2019
# monica.golumbeanu@unibas.ch
#############################

SIM_FOLDER=$1
FOLLOW_UP=$2

SCAFFOLD_FILE=$SIM_FOLDER"scaffold.xml"
PARAM_RANGES_FILE=$SIM_FOLDER"param_ranges.RData"
GP_FOLDER=$SIM_FOLDER"gp_"$FOLLOW_UP"/"
GP_TRAINING_FOLDER=$GP_FOLDER"trained/"
GP_AS_FOLDER=$GP_FOLDER"as/"
PREDICTED="prev_red"

mkdir -p $GP_AS_FOLDER

# Submit as array job
GP_models=(${GP_TRAINING_FOLDER}*.RData)
NUM=${#GP_models[@]}

for (( ID=0; ID<$NUM; ID++ ))
do  
    GP_model_file=${GP_models[$ID]}
    echo "Adaptive sampling for model $GP_model_file"
    AS_RUN_DIR=$GP_AS_FOLDER"as_model_"$ID"/"
    mkdir -p $AS_RUN_DIR
    scp $SCAFFOLD_FILE $AS_RUN_DIR
    Rscript adaptive_sampling_om.R $AS_RUN_DIR $GP_model_file $PARAM_RANGES_FILE $FOLLOW_UP $PREDICTED
done

