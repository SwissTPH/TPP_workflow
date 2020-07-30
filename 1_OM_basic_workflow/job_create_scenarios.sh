#!/bin/bash
#SBATCH --job-name=create_scenarios
#SBATCH --account=penny
#SBATCH -o /scicore/home/penny/golmon00/MMC/TPP/JOB_OUT/create_scenario.out
#SBATCH --mem=100M
#SBATCH --qos=6hours
#SBATCH --cpus-per-task=1
###########################################
# Script that creates scenario base xml files
# 
# INPUT:
#	PARAM_TABLE_FILE: text file containing the parameter values for each scenario (1 scenario per row)
#	SCAFFOLD_FILE: .xml scaffold file with @parameter@ locations
#	BASE_FOLDER: directory where a file containing parameter replacement specifications per scenario
#	SCENARIO_FOLDER: directory where a scenario xml file for each row in the paramter table is created
#
# created on 03.04.2019
# monica.golumbeanu@unibas.ch
###########################################

PARAM_TABLE_FILE=$1
SCAFFOLD_FILE=$2
BASE_FOLDER=$3
SCENARIO_FOLDER=$4

# Extract the number of lines in the parameter table
NUM=$(wc -l < $PARAM_TABLE_FILE)

# Create the scenarios
echo "Creating $NUM-1 scenarios ..."
for (( ID=2; ID<=$NUM; ID++ ))
do  
    # Select the parameter names and the scenario  line from the parameter file 
    column_names=$(sed -n "1p" < $PARAM_TABLE_FILE)
    param_line=$(sed -n "${ID}p" < $PARAM_TABLE_FILE)

    # Construct the replacement patterns
    Rscript create_scenario.R --column_names $column_names --params $param_line --base_folder $BASE_FOLDER --scenario_folder $SCENARIO_FOLDER --scaffold_file $SCAFFOLD_FILE  
    echo "Replacement patterns, base and scenario xml created."
done

# Cleanup
rm -r $BASE_FOLDER


