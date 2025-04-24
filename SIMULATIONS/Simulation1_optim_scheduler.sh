#!/usr/bin/bash

#SBATCH --mail-user=jakub-jozef.liu@charite.de
#SBATCH --mail-type=end
#SBATCH --job-name=Sim1_optim
#SBATCH --output=Sim1_optim%j.log
#SBATCH --error=Sim1_optim%j.log
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=60
#SBATCH --mem=200G
#SBATCH --time=200:00:00


Rscript Simulation1_optim.R

