#!/bin/bash
#SBATCH --job-name=optim_GP
#SBATCH --account=smith
#SBATCH -o /scicore/home/penny/golmon00/MMC/TPP/JOB_OUT/optimisation_analysis.out
#SBATCH --mem=10G
#SBATCH --qos=1day
#SBATCH --cpus-per-task=1

#######################################
# script for optimisation analysis
#
# created 08.01.2020
# monica.golumbeanu@unibas.ch
######################################

GP_DIR=$1
PARAM_RANGES_FILE=$2
OPT_DEST_DIR=$3
OPT_SETUP_FILE=$4
ROW=$5

# IMPORTANT: the number of files must equal to the task array length (index starts at 0)
gp_files=(${GP_DIR}*.RData)

# Select scenario file in array
ID=$(expr ${SLURM_ARRAY_TASK_ID} - 1)
gp_file=${gp_files[$ID]}
echo "Optimisation for $gp_file"

Rscript get_optim_profile.R $gp_file $PARAM_RANGES_FILE $OPT_DEST_DIR $OPT_SETUP_FILE $ROW

