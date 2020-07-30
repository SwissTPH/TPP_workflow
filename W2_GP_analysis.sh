#!/bin/bash
#
################################################
# Main workflow script for training the GP emulator and performing
# all subsequent analyses (sensitivity analysis, optimization)
#
# INPUT: 
#       SIM_FOLDER = simulation folder input files:
#			    param_tab.txt = file where each line is a parameter configuration to simulate
#			    scaffold.xml = xml file containing @parameter@ wildcards for varied values across simulations
#       FOLLOW_UP: index of the year when the follow up is done
#
# OUTPUT:
#	The script creates the following folders in the specified SIM_FOLDER:
#		base/ = folder containing replacement patterns and base xml files 
#               (will be deleted after scenario creation)
#		scenarios/ = folder with scenario files
#		om/ = folder with OpenMalaria simulations
#
# SYNTHAX: 
#	bash OM_base_workflow.sh SIM_FOLDER
#
# created 02.10.2019
# monica.golumbeanu@unibas.ch
###############################################

# Define variables
SIM_FOLDER=$1
FOLLOW_UP=$2

PREDICTED="prev_red"
PARAM_TABLE_FILE=$SIM_FOLDER"param_tab.txt"
PARAM_RANGES_FILE=$SIM_FOLDER"param_ranges.RData"
SCAFFOLD_FILE=$SIM_FOLDER"scaffold.xml"
POSTPROCESSING_FOLDER=$SIM_FOLDER"postprocessing_"$FOLLOW_UP"/"
GP_FOLDER=$SIM_FOLDER"gp_"$FOLLOW_UP"/"
GP_TRAINING_FOLDER=$GP_FOLDER"trained/"
GP_AS_FOLDER=$GP_FOLDER"as/"
GP_SENS_FOLDER=$GP_FOLDER"sensitivity/"

LOG_FILE=$SIM_FOLDER"LOG_ANALYSIS.txt"
JOB_OUTPUTS=~/MMC/TPP/JOB_OUT/

echo "DOWNSTREAM ANALYSIS $FOLLOW_UP" > $LOG_FILE
echo "______________________" >> $LOG_FILE

# Train the GP emulator
echo "Step1: Training the GP model"
echo "Step1: Training the GP model" >> $LOG_FILE
cd 3_GP_train/
bash GP_train_workflow.sh $SIM_FOLDER $FOLLOW_UP
echo "Number of trained models:" >> $LOG_FILE
ls -l $GP_TRAINING_FOLDER | wc -l >> $LOG_FILE

# Adaptive sampling:
echo "Step2: Make 10 runs of adaptive sampling"
echo "Step2: Make 10 runs of adaptive sampling" >> $LOG_FILE
bash GP_as_workflow.sh $SIM_FOLDER $FOLLOW_UP
echo "Number of trained models:" >> $LOG_FILE
ls -l $GP_AS_FOLDER | wc -l >> $LOG_FILE
cd -
    
# Sensitivity analysis
echo "Step3: Sensitivity analysis"
echo "Step3: Sensitivity analysis" >> $LOG_FILE
cd 4_sensitivity_analysis/
bash GP_sens_workflow.sh $GP_AS_FOLDER $PARAM_RANGES_FILE $GP_SENS_FOLDER
echo "Number of sensitivity analyses:" >> $LOG_FILE
ls -l $GP_SENS_FOLDER | wc -l >> $LOG_FILE
cd -

# echo "Step4: Optimization"
# echo "Step4: Optimization" >> $LOG_FILE

