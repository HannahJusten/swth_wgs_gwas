#!/bin/bash
# author: hj
# date: Jan 2022
#genome-wide fst

#!/bin/bash
#SBATCH --job-name=fst
#SBATCH --ntasks=1
#SBATCH --time=3:00:00
#SBATCH --mem=50GB
#SBATCH --output=job.%J.fst.out
#SBATCH --mail-type=END
#SBATCH --mail-user=justen@tamu.edu

module load GCC/9.3.0 iccifort/2019.5.281 VCFtools/0.1.16

#vcftools \
#--vcf genotype_all.drew.indel5bp.info.dp.biallelic.missing.recode.vcf \
#--weir-fst-pop coastal_ids.txt \
#--weir-fst-pop inland_ids.txt \
#--chr super_scaffold_1 \
#--out scaf_1_coastal_inland


#vcftools \
#--vcf genotype_all.drew.indel5bp.info.dp.biallelic.missing.recode.vcf \
#--weir-fst-pop coastal_ids.txt \
#--weir-fst-pop inland_ids.txt \
#--chr scaffold_4_arrow_ctg1 \
#--out scaf_4_coastal_inland

vcftools \
--vcf genotype_all.drew.indel5bp.info.dp.biallelic.missing.recode.vcf \
--weir-fst-pop coastal_ids.txt \
--weir-fst-pop inland_ids.txt \
--fst-window-size 10000 \
--out genome-wide_subspecies_fst_10kb

vcftools \
--vcf genotype_all.drew.indel5bp.info.dp.biallelic.missing.recode.vcf \
--weir-fst-pop pacific_spirit.txt \
--weir-fst-pop porpoise.txt \
--fst-window-size 10000 \
--out genome-wide_coastal_fst_10kb

vcftools \
--vcf genotype_all.drew.indel5bp.info.dp.biallelic.missing.recode.vcf \
--weir-fst-pop kamloops.txt \
--weir-fst-pop kelowna.txt \
--fst-window-size 10000 \
--out genome-wide_inland_fst_10kb



