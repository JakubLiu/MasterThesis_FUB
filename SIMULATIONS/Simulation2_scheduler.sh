#!/usr/bin/bash

#SBATCH --mail-user=jakub-jozef.liu@charite.de
#SBATCH --mail-type=end
#SBATCH --job-name=Sim2
#SBATCH --output=Sim2%j.log
#SBATCH --error=Sim2%j.log
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=80
#SBATCH --mem=200G
#SBATCH --time=200:00:00


Rscript Simulation2.R

