#!/usr/bin/bash

#SBATCH --mail-user=jakub-jozef.liu@charite.de
#SBATCH --mail-type=end
#SBATCH --job-name=FinalSim4
#SBATCH --output=FinalSim4%j.log
#SBATCH --error=FinalSim4%j.log
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=70
#SBATCH --mem=200G
#SBATCH --time=20:00:00


Rscript FinalSim4.R

