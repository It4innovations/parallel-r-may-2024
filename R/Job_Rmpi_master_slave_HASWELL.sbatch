#!/bin/bash
#SBATCH --export=ALL,LD_PRELOAD=
#SBATCH --job-name Rmpi
#SBATCH --partition=haswell
#SBATCH --nodes=10
#SBATCH --ntasks-per-node 24 ## maximum is 24
#SBATCH --output=logs/%x_%j.out

module load OpenMPI/4.1.4-GCC-11.3.0
module load R/4.2.1-foss-2022a
srun  Rscript Rmpi_master_slave.R
