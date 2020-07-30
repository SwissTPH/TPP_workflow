#!/bin/bash
#SBATCH --job-name=run_OM
#SBATCH --account=penny
#SBATCH -o /scicore/home/penny/golmon00/MMC/TPP/JOB_OUT/OM_output.out
#SBATCH --mem=2G
#SBATCH --qos=30min
#SBATCH --cpus-per-task=1
###########################################
# %A_%a.out  OM_output.out
# Script for submitting OpenMalaria simulations as jobs to the cluster
# Arguments:
#		INPUT_DIR: directory containing the scenario files (.xml)
#		DEST_DIR: directory where two output files will be created per OpenMalaria simulation
# Calling the script:
#	sbatch --array=1-NUM run_OM.sh INPUT_DIR DEST_DIR
#
# created on 01.02.2019
# monica.golumbeanu@unibas.ch
###########################################

INPUT_DIR=$1
DEST_DIR=$2

# Load OpenMalaria module and change to folder with resource files
module purge
ml OpenMalaria/38.0-goolf-1.7.20-Python-2.7.11
cd /scicore/home/penny/golmon00/OM_schema38

# create destination directory
mkdir -p $DEST_DIR

# IMPORTANT: the number of files must equal to the task array length (index starts at 0)
scenario_files=(${INPUT_DIR}*.xml)
#i=0
#echo ${scenario_files[$i]}

# Select scenario file in array
ID=$(expr ${SLURM_ARRAY_TASK_ID} - 1)
scenario_file=${scenario_files[$ID]}
echo "Running simulation for $scenario_file"

# Run OpenMalaria on scenario file
scenario_name=$(basename "$scenario_file" ".xml")
output1=$DEST_DIR$scenario_name"_out.txt"
output2=$DEST_DIR$scenario_name"_cts.txt"
echo "Outputs will be written to $output1 and $output2"
openMalaria --scenario $scenario_file --output $output1 --ctsout $output2
echo "OpenMalaria simulation ended."
parentdir="$(dirname "$INPUT_DIR")"
