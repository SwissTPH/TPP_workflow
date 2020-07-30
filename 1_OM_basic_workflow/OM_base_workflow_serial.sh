#!/bin/bash
#
##############################
# Main script for running OpenMalaria simulations on the cluster. 
# INPUT:
#	SIM_FOLDER = simulation  folder with all necessary input files:
#			param_tab.txt = file where each line is a parameter configuration to simulate
#			scaffold.xml = xml file containing @parameter@ wildcards for varied values across simulations
# OUTPUT:
#	The script creates the following folders in the specified SIM_FOLDER:
#		base/ = folder containing replacement patterns and base xml files
#		scenarios/ = folder with scenario files
#		om/ = folder with OpenMalaria simulations
#
# SYNTHAX: 
#	bash OM_base_workflow.sh SIM_FOLDER
# 
#
# created 14.09.2019
# monica.golumbeanu@unibas.ch
#############################

SIM_FOLDER=$1
PARAM_TABLE_FILE=$SIM_FOLDER"param_tab.txt"
SCAFFOLD_FILE=$SIM_FOLDER"scaffold.xml"
BASE_FOLDER=$SIM_FOLDER"base/"
SCENARIOS_FOLDER=$SIM_FOLDER"scenarios/"
OM_FOLDER=$SIM_FOLDER"om/"

# Create OpenMalaria scenarios
sbatch -W job_create_scenarios.sh $PARAM_TABLE_FILE $SCAFFOLD_FILE $BASE_FOLDER $SCENARIOS_FOLDER
echo "Finished creating scenarios"

# Run OpenMalaria for each scenario
#echo "Running OpenMalaria simulations ..."
#sbatch -W --array=1-$NUM run_OM.sh $SCENARIOS_FOLDER $OM_FOLDER
