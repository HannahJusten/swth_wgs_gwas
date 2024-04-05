#!/bin/bash
# author: kira delmore; modified by hj
# date: apr 2021 modified nov 2021
# give it a list of phenotypes; I gave the phenotype file for which we have the most data for

#!/bin/bash
#SBATCH --job-name=relatedness_matrix_2
#SBATCH --ntasks=1
#SBATCH --time=12:00:00
#SBATCH --mem=50G
#SBATCH --output=job.%J.relatedness_matrix2.out
#SBATCH --mail-type=END
#SBATCH --mail-user=justen@tamu.edu

module purge
module load GCC/7.3.0-2.30 OpenMPI/3.1.1 GEMMA/0.98.1

path='/scratch/user/justen'
geno='stitch.pruned_fuller.bimbam'

sed 's/.://' $geno > $geno

mkdir relatedness_matrix
cd relatedness_matrix

## relatedness matrix
gemma \
-g $path/stitch/$geno \
-gk 1 \
-p $path/stitch/pheno/pheno_wing_length.txt \
-o relatedness_matrix_stitch_wing_length_pruned_fuller \
-maf 0.05 -miss 0.10 

# done. the relatedness matrix can now be used from gemma GWAS
