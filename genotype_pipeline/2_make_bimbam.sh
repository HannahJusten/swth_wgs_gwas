#!/bin/bash
# author: kira delmore; modified by hj
# date: apr 2021 modified apr 2022
# give it a list of phenotypes, I chose the phenotype we have the most data for.

#SBATCH --job-name=make_bimbam
#SBATCH --ntasks=1
#SBATCH --time=20:00:00
#SBATCH --mem=96GB
#SBATCH --output=job.%J.make_bimbam.out
#SBATCH --mail-type=END
#SBATCH --mail-user=justen@tamu.edu

module load GCC/9.3.0 iccifort/2019.5.281 VCFtools/0.1.16

geno=stitch.pruned_fuller.vcf.gz
path='/scratch/user/justen/stitch'

/scratch/user/justen/tools/qctool_v2.0.8-CentOS\ Linux7.6.1810-x86_64/qctool \
-g $path/$geno \
-filetype vcf \
-ofiletype bimbam_dosage \
-vcf-genotype-field GP \
-og "$geno"_fuller.bimbam

# next 3_make_relatedness_matrix.sh
