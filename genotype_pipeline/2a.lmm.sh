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
#SBATCH --job-name=lmm_"$prefix"_full_matrix
#SBATCH --ntasks=1
#SBATCH --time=6:00:00
#SBATCH --mem=96GB
#SBATCH --output=/scratch/user/justen/stitch/job_files/job.%J."$prefix".full_matrix.lmm.out

module purge
module load GCC/7.3.0-2.30 OpenMPI/3.1.1 GEMMA/0.98.1

path='/scratch/user/justen/stitch'

geno='$2'

## prep
cd full_stitch

mkdir "$prefix"_fuller_dir
cd "$prefix"_fuller_dir

## relatedness matrix
gemma \
-g \$path/stitch.pruned_fuller.bimbam \
-gk 1 \
-o relatedness_matrix_"$prefix"_fuller \
-p \$path/pheno/pheno_"$prefix".txt \
-maf 0.05 -miss 0.10 

gemma \
-g \$path/\$geno \
-k ./output/relatedness_matrix_"$prefix"_fuller.cXX.txt \
-lmm 4 \
-o "$prefix".lmm \
-p \$path/pheno/pheno_"$prefix".txt \
-maf 0.05 -miss 0.10 

cd output

module purge
module load iccifort/2019.5.281  impi/2018.5.288 R/4.1.0

Rscript --vanilla /scratch/user/justen/stitch/Rscripts/summarize_lmm.R "$prefix"

cd \$path/


" > sbatch/"$prefix".sh

chmod 755 sbatch/"$prefix".sh

sbatch sbatch/"$prefix".sh

done < $1
