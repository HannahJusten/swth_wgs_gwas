#!/bin/bash

#module load iccifort/2019.5.281  impi/2018.5.288 R/4.1.0

list="$1"

while read prefix
do
	echo "#!/bin/bash
#SBATCH --job-name=gemma_bslmm_"$prefix"
#SBATCH --time=24:00:00
#SBATCH --ntasks=1
#SBATCH --mem=128G
#SBATCH --output=/scratch/user/justen/stitch/job_files/job.%J."$prefix".bslmm.out
#SBATCH --mail-type=END
#SBATCH --mail-user=justen@tamu.edu

module load GCC/9.3.0 iccifort/2019.5.281 VCFtools/0.1.16
module load iccifort/2019.5.281  impi/2018.5.288 R/4.1.0
module load GCC/7.3.0-2.30 OpenMPI/3.1.1 GEMMA/0.98.1

path='/scratch/user/justen/stitch'

geno='stitch.bimbam'

#file='all.fix_missing.maf05.geo.winter.bimbam'
#pfile='geo_winter_loc.txt'

cd full_stitch

cd "$prefix"_fuller_dir

#making relatedness matrix
#gemma \
#-g \$path/\$geno \
#-gk 1 \
#-o relatedness_matrix_"$prefix"_bslmm \
#-p \$path/pheno/pheno_"$prefix".txt \
#-maf 0.05 -miss 0.10 

gemma \
-g \$path/stitch.pruned_fuller.bimbam \
-gk 1 \
-o relatedness_matrix_"$prefix"_bslmm \
-p \$path/pheno/pheno_"$prefix".txt \
-maf 0.05 -miss 0.10 


#running BSLMMs
#remember to change -bslmm flag to 3 if running a categorical/binary phenotype

gemma \
-g \$path/\$geno \
-k output/relatedness_matrix_"$prefix"_bslmm.cXX.txt \
-bslmm 1 \
-w 5000000 -s 20000000 -rpace 100 -wpace 1000 -hmin 0 -hmax 1 -rmin 0 -rmax 1 \
-maf 0.05 -miss 0.10 \
-o "$prefix".bslmm_1 \
-p \$path/pheno/pheno_"$prefix".txt

gemma \
-g \$path/\$geno \
-k output/relatedness_matrix_"$prefix"_bslmm.cXX.txt \
-bslmm 1 \
-w 5000000 -s 20000000 -rpace 100 -wpace 1000 -hmin 0 -hmax 1 -rmin 0 -rmax 1 \
-maf 0.05 -miss 0.10 \
-o "$prefix".bslmm_2 \
-p \$path/pheno/pheno_"$prefix".txt

gemma \
-g \$path/\$geno \
-k output/relatedness_matrix_"$prefix"_bslmm.cXX.txt \
-bslmm 1 \
-w 5000000 -s 20000000 -rpace 100 -wpace 1000 -hmin 0 -hmax 1 -rmin 0 -rmax 1 \
-maf 0.05 -miss 0.10 \
-o "$prefix".bslmm_3 \
-p \$path/pheno/pheno_"$prefix".txt

gemma \
-g \$path/\$geno \
-k output/relatedness_matrix_"$prefix"_bslmm.cXX.txt \
-bslmm 1 \
-w 5000000 -s 20000000 -rpace 100 -wpace 1000 -hmin 0 -hmax 1 -rmin 0 -rmax 1 \
-maf 0.05 -miss 0.10 \
-o "$prefix".bslmm_4 \
-p \$path/pheno/pheno_"$prefix".txt

cd output/
mkdir bslmm_out

Rscript --vanilla /scratch/user/justen/stitch/Rscripts/summarize_bslmm.R "$prefix"

cd ../

" > sbatch/$prefix.bslmm.sh

chmod 755 sbatch/$prefix.bslmm.sh

sbatch sbatch/$prefix.bslmm.sh

done < $1
