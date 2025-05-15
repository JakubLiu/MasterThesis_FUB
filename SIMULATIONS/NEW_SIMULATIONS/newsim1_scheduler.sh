#!/usr/bin/bash

#SBATCH --mail-user=jakub-jozef.liu@charite.de
#SBATCH --mail-type=end
#SBATCH --job-name=newsim1
#SBATCH --output=news1%j.log
#SBATCH --error=news1%j.log
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=50
#SBATCH --mem=200G
#SBATCH --time=30:00:00


Rscript newsim1.R

