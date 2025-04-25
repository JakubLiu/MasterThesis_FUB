#!/usr/bin/bash

#SBATCH --mail-user=jakub-jozef.liu@charite.de
#SBATCH --mail-type=end
#SBATCH --job-name=Sim1
#SBATCH --output=Sim1%j.log
#SBATCH --error=Sim1%j.log
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=80
#SBATCH --mem=200G
#SBATCH --time=40:00:00


Rscript Simulation1.R

