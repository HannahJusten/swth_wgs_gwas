#!/bin/bash

#module load iccifort/2019.5.281  impi/2018.5.288 R/4.1.0

list="$1"

while read prefix
do
        echo "#!/bin/bash
#SBATCH --job-name=gemma_predict_"$prefix"
#SBATCH --time=2:00:00
#SBATCH --ntasks=1
#SBATCH --mem=128G
#SBATCH --output=/scratch/user/justen/stitch/job_files/job.%J."$prefix".bslmm.out

module load GCC/9.3.0 iccifort/2019.5.281 VCFtools/0.1.16
module load iccifort/2019.5.281  impi/2018.5.288 R/4.1.0
module load GCC/7.3.0-2.30 OpenMPI/3.1.1 GEMMA/0.98.1

path='/scratch/user/justen/stitch'

geno='stitch_geos.recode.vcf.bimbam'

cd full_stitch

cd bslmm_juv

cd output
gemma \
-g \$path/\$geno \
-o "$prefix"_bslmm_predict \
-p \$path/pheno_test_gen_cor/"$prefix".txt \
-epm "$prefix".bslmm.param.txt \
-emu "$prefix".bslmm.log.txt \
-ebv "$prefix".bslmm.bv.txt \
-k relatedness_matrix_"$prefix"_bslmm.cXX.txt \
-predict 1

" > sbatch/$prefix.bslmm.sh

chmod 755 sbatch/$prefix.bslmm.sh

sbatch sbatch/$prefix.bslmm.sh

done < $1
