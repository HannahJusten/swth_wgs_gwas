#!/bin/bash
# author: kira delmore; modified by hj
# date: apr 2021 modified apr 2022

#SBATCH --job-name=LD_pruning
#SBATCH --ntasks=1
#SBATCH --time=36:00:00
#SBATCH --mem=96GB
#SBATCH --output=job.%J.LD_pruning.out
#SBATCH --mail-type=END
#SBATCH --mail-user=justen@tamu.edu

geno=stitch
path='/scratch/user/justen/geno_files'

# this is code is from Fuller et al. 2020 (DOI: 10.1126/science.aba4674)- in sup mat; calculatung LD in a window size of 200 kb with step size of 20 and r^2 threshold of 0.2

module load GCC/9.3.0 PLINK/1.9b5

plink --vcf $path/$geno.vcf.gz \
--geno 0.10 \
--allow-extra-chr --indep-pairwise 200 20 0.2 \
--maf 0.05 \
--out $geno.pruned \
--set-missing-var-ids @:#[swth] \
--double-id ##causes both family and wintin-family IDs to be set to sample ID.

#Pruning complete.  282414 of 479297 variants removed.

awk -F ':' '{print$1,$2}' $geno.pruned.prune.in | sed 's/ /	/g' > $geno.pruned.prune.in.vcftools

module purge
module load GCC/9.3.0 VCFtools/0.1.16

vcftools --gzvcf $path/$geno.vcf.gz \
--positions $geno.pruned.prune.in.vcftools \
--recode --recode-INFO-all \
--stdout | gzip -c > $geno.pruned_fuller.vcf.gz

# next 2_make_bimbam.sh
