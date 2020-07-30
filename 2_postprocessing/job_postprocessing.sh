#!/bin/bash
#SBATCH --job-name=OM_postprocessing
#SBATCH --account=smith
#SBATCH -o /scicore/home/smith/golmon00/MMC/TPP/JOB_OUT/postprocessing_jobs.out
#SBATCH --mem=500MB
#SBATCH --qos=6hours
#SBATCH --cpus-per-task=1
###########################################
# Script for post processing OpenMalaria simulation results
# Arguments:
#               INPUT_DIR: directory containing the parameter table splits
#		        OM_RESULTS_DIR: folder with the OM simulation results corresponding to the scenarios in INPUT_DIR
#               DEST_DIR: directory where the post processing results will be saved
#               FOLLOW_UP = integer representing the survey index to consider for 
#                           evaluating intervention impact
# Calling the script:
#       sbatch --array=1-NUM submit_postprocessing.sh INPUT_DIR OM_RESULTS_DIR DEST_DIR
#	where NUM is the number of splits (settings)
#
# created on 14.05.2019
# monica.golumbeanu@unibas.ch
###########################################

INPUT_DIR=$1
OM_RESULTS_DIR=$2
DEST_DIR=$3
FOLLOW_UP=$4

# IMPORTANT: the number of files must equal to the task array length (index starts at 0)
split_files=(${INPUT_DIR}*.txt)

# Select scenario file in array
ID=$(expr ${SLURM_ARRAY_TASK_ID} - 1)
split_file=${split_files[$ID]}
echo "Postprocessing for $split_file"

Rscript calc_sim_outputs.R $OM_RESULTS_DIR $split_file $DEST_DIR $FOLLOW_UP
