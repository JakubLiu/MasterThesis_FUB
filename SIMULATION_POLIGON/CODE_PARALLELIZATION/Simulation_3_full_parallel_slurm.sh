#!/usr/bin/bash

#SBATCH --mail-user=jakub-jozef.liu@charite.de
#SBATCH --mail-type=end
#SBATCH --job-name=Sim3_full_parallel
#SBATCH --output=Sim3_full_parallel%j.log
#SBATCH --error=Sim3_full_parallel_error%j.log
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=60
#SBATCH --mem=200G
#SBATCH --time=200:00:00


Rscript Simulation_3_full_parallel.R
