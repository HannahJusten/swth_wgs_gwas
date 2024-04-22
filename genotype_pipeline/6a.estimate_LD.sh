#!/bin/bash
# author: hannah justen
# date: feb 2023

#SBATCH --job-name=ld_block_bear_fall
#SBATCH --ntasks=1
#SBATCH --time=1-00:00:00
#SBATCH --mem=250GB
#SBATCH --output=job.%J.ls_block_bear_fall.out

module load GCC/9.3.0 iccifort/2019.5.281 VCFtools/0.1.16

path='/scratch/user/justen/stitch'

#run script for locations on each scaffold separately

vcftools \
--gzvcf /scratch/user/justen/survival_gwas/stitch_survival_0.05.vcf.gz \
--chr super_scaffold_1 \
--geno-r2-positions pos_1 \
--out ./ld_out/stitch_survival_r2_scaf_1_vcftools

