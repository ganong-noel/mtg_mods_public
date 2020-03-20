#!/bin/bash
#SBATCH -n 1 # Number of cores
#SBATCH -N 1 # Ensure that all cores are on one machine
#SBATCH -t 0-48:00 # Runtime in D-HH:MM
#SBATCH -p serial_requeue # Partition to submit to
#SBATCH --mem=250000 # Memory pool for all cores (see also --mem-per-cpu)
	
module load R/3.3.3-fasrc01
module load mpc/1.0.2-fasrc01
module load nlopt/2.4.2-fasrc01
export R_LIBS_USER=$HOME/apps/R:$R_LIBS_USER
R CMD BATCH --quiet --no-restore --no-save master.R output_file.out
