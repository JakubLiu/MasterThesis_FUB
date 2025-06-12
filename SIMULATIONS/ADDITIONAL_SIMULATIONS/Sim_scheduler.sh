#!/usr/bin/bash

#SBATCH --mail-user=jakub-jozef.liu@charite.de
#SBATCH --mail-type=BEGIN,END
#SBATCH --job-name=AdditionalSim1
#SBATCH --output=AdditionalSim1%j.log
#SBATCH --error=AdditionalSim1%j.log
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=70
#SBATCH --mem=200G
#SBATCH --time=40:00:00


Rscript AdditionalSim1.R
