#!/bin/bash

#module load iccifort/2019.5.281  impi/2018.5.288 R/4.1.0

list="$1"

while read prefix
do
        echo "#!/bin/bash
#SBATCH --job-name=gemma_bslmm_"$prefix"
#SBATCH --time=24:00:00
#SBATCH --ntasks=1
#SBATCH --mem=360G
#SBATCH --output=/scratch/user/justen/stitch/job_files/job.%J."$prefix".bslmm.out

module load GCC/9.3.0 iccifort/2019.5.281 VCFtools/0.1.16
module load iccifort/2019.5.281  impi/2018.5.288 R/4.1.0
module load GCC/7.3.0-2.30 OpenMPI/3.1.1 GEMMA/0.98.1

path='/scratch/user/justen/stitch'

geno='stitch_geos.recode.vcf.bimbam'

cd full_stitch

#mkdir bslmm_juv

cd bslmm_juv

#making relatedness matrix
gemma \
-g \$path/\$geno \
-gk 1 \
-o relatedness_matrix_"$prefix"_bslmm \
-p \$path/pheno_test_gen_cor/"$prefix".txt \
-maf 0.05 -miss 0.10

#running BSLMM

gemma \
-g \$path/\$geno \
-k output/relatedness_matrix_"$prefix"_bslmm.cXX.txt \
-bslmm 1 \
-w 5000000 -s 20000000 -rpace 100 -wpace 1000 -hmin 0 -hmax 1 -rmin 0 -rmax 1 \
-maf 0.05 -miss 0.10 \
-o "$prefix".bslmm \
-p \$path/pheno_test_gen_cor/"$prefix".txt

" > sbatch/$prefix.bslmm.sh

chmod 755 sbatch/$prefix.bslmm.sh

sbatch sbatch/$prefix.bslmm.sh

done < $1


