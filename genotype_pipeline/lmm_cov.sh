#!/bin/bash
# author: kira delmore; modified by hj
# date: apr 2021 modified nov 2021
# give it a list of phenotypes
# ./lmm_cov.sh <phenotype list> <cov.txt> <geno>


list="$1"
#geno="$3" # all.fix_missing.maf05.bimbam
#cov="$2"

while read prefix
do
        echo "#!/bin/bash
#SBATCH --job-name=lmm_"$prefix"_cov
#SBATCH --ntasks=1
#SBATCH --time=24:00:00
#SBATCH --mem=96GB
#SBATCH --output=job.%J."$prefix".cov.lmm.out
#SBATCH --mail-type=END
#SBATCH --mail-user=justen@tamu.edu

#module load GCC/9.3.0 iccifort/2019.5.281 VCFtools/0.1.16

module purge
module load GCC/7.3.0-2.30 OpenMPI/3.1.1 GEMMA/0.98.1

path='/scratch/user/justen/stitch'
#geno='wing_combo.bimbam'
geno='$3'
cov='$2'

## prep

cd relatedness_matrix

gemma \
-g \$path/stitch.pruned_fuller.bimbam \
-gk 1 \
-o relatedness_matrix_"$prefix"_fuller \
-p \$path/pheno/pheno_"$prefix".txt \
-maf 0.05 -miss 0.10

## relatedness matrix
#gemma \
#-g \$path/\$geno \
#-gk 1 \
#-o relatedness_matrix_"$prefix"_lmm \
#-p \$path/pheno/pheno_"$prefix".txt \
#-maf 0.05 -miss 0.10 
#mkdir full_stitch

cd ..

cd full_stitch

#mkdir \$geno
#cd \$geno

#mkdir "$prefix"_dir
#cd "$prefix"_dir

mkdir "$prefix"_cov_sex_dir
cd "$prefix"_cov_sex_dir
gemma \
-g \$path/\$geno \
-k \$path/relatedness_matrix/output/relatedness_matrix_"$prefix"_fuller.cXX.txt \
-lmm 4 \
-c \$path/\$cov \
-o "$prefix".lmm \
-p \$path/pheno/pheno_"$prefix".txt \
-maf 0.05 -miss 0.10 

cd output

module purge
module load iccifort/2019.5.281  impi/2018.5.288 R/4.1.0

Rscript --vanilla /scratch/user/justen/stitch/Rscripts/summarize_lmm.R "$prefix"

cd \$path


" > sbatch/"$prefix"_cov.sh

chmod 755 sbatch/"$prefix"_cov.sh

sbatch sbatch/"$prefix"_cov.sh

done < $1
