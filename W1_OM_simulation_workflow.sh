#!/bin/bash
#
################################################
# Main script for running OpenMalaria simulations and postprocessing 
#
# INPUT: 
#       SIM_FOLDER = simulation folder, needs to contain the input files:
#			    param_tab.txt = file where each line is a parameter configuration to simulate (see generate_param_table.R in examples/ folder for constructing this file
#			    scaffold.xml = xml scenario file containing @parameter@ wildcards for varied values across simulations, see the examples/ folder for the scenario files used for different interventions
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
OM_FOLDER=$SIM_FOLDER"om/"
SCENARIOS_FOLDER=$SIM_FOLDER"scenarios/"
LOG_FILE=$SIM_FOLDER"LOG.txt"
JOB_OUTPUTS=~/MMC/TPP/JOB_OUT/
    
# Create scenarios and run OpenMalaria simulations
echo "Step1: Creating scenarios and running OpenMalaria" > $LOG_FILE
cd 1_OM_basic_workflow/
bash OM_base_workflow_serial.sh $SIM_FOLDER
echo "Number of simulation files:" >> $LOG_FILE
ls -l $OM_FOLDER | wc -l >> $LOG_FILE
cd -
    
# Postprocess OpenMalaria simulation results immediate follow-up
POSTPROCESSING_FOLDER=$SIM_FOLDER"postprocessing_4/"
echo "Step2: Postprocessing"
echo "Step2: Postprocessing" >> $LOG_FILE
cd 2_postprocessing/
bash postprocessing_workflow.sh $SIM_FOLDER 4
echo "Number of postprocessed files:" >> $LOG_FILE
ls -l $POSTPROCESSING_FOLDER | wc -l >> $LOG_FILE
cd -

# Postprocess OpenMalaria simulation results late follow-up
POSTPROCESSING_FOLDER=$SIM_FOLDER"postprocessing_6/"
echo "Step2: Postprocessing"
echo "Step2: Postprocessing" >> $LOG_FILE
cd 2_postprocessing/
bash postprocessing_workflow.sh $SIM_FOLDER 6
echo "Number of postprocessed files:" >> $LOG_FILE
ls -l $POSTPROCESSING_FOLDER | wc -l >> $LOG_FILE
cd -

# Optional: remove simulation and scenario folders after postprocessing 
# rm -r $OM_FOLDER
# rm -r $SCENARIOS_FOLDER


