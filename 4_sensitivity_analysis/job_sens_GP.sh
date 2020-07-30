#!/bin/bash
#SBATCH --job-name=sens_GP
#SBATCH --account=smith
#SBATCH -o /scicore/home/smith/golmon00/MMC/TPP/JOB_OUT/sensitivity_analysis.out
#SBATCH --mem=60G
#SBATCH --qos=30min
#SBATCH --cpus-per-task=1

#######################################
# script for sensitivity analysis
#
# created 17.06.2019
# monica.golumbeanu@unibas.ch
######################################

GP_DIR=$1
PARAM_RANGES_FILE=$2
EIR_LVL=$3
SENS_DEST_DIR=$4

# IMPORTANT: the number of files must equal to the task array length (index starts at 0)
gp_files=(${GP_DIR}*.RData)
#i=0
#echo ${split_files[$i]}

# Select scenario file in array
ID=$(expr ${SLURM_ARRAY_TASK_ID} - 1)
gp_file=${gp_files[$ID]}
echo "Postprocessing for $gp_file"

Rscript sens_GP.R $gp_file $PARAM_RANGES_FILE $EIR_LVL $SENS_DEST_DIR
