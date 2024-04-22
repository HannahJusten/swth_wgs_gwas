# set wd to where predict output files are stored:

setwd("C:/Users/hcjusten/Dropbox/PhD/Thesis/Chapter_2/Analysis/GWAS/output/stitch/fuller_pruned/genetic_cor_test")

#list of individuals in the order they appear in vcf, important, because the predict files has no column for individuals

order<-read.csv("C:/Users/hcjusten/Dropbox/PhD/Thesis/Chapter_2/Analysis/GWAS/meta_data/geo_order_stitch.csv")

#load list of 
list<-read.csv("C:/Users/hcjusten/Dropbox/PhD/Thesis/Chapter_2/Analysis/GWAS/meta_data/files_gen_cor/list_pheno_for_gen_cor.csv")

for (i in 1:nrow(list$pheno)){
  id=list$pheno[i]
  
  df.i_1<-read.table(paste("./",id,"1_bslmm_predict.prdt.txt",sep=""))
  df.i_2<-read.table(paste("./",id,"2_bslmm_predict.prdt.txt",sep=""))
  df.i_3<-read.table(paste("./",id,"3_bslmm_predict.prdt.txt",sep=""))
  df.i_4<-read.table(paste("./",id,"4_bslmm_predict.prdt.txt",sep=""))
  
  df.i<-cbind(df.i_1,df.i_2,df.i_3,df.i_4)
  names(df.i)=c("V1","V2","V3","V4")
  
  # get 
  df.i=df.i %>% 
    mutate(V5 = coalesce(V1,V2,V3,V4))
  
  #load phenotype file for each trait: to make sure traits to remove predicted phenotypes for individuals that we don't have phenotypes for.
  pheno<-read.csv(paste("C:/Users/hcjusten/Dropbox/PhD/Thesis/Chapter_2/Analysis/GWAS/meta_data/files_gen_cor/",id,".csv",sep=""))
  
  names(pheno)=c("name_in_vcf","trait")
  pheno_2<-join(order,pheno,by="name_in_vcf")
  
  pheno_3=cbind(pheno_2,df.i)
  
  pheno4=pheno_3[!is.na(pheno_3$trait),]
  
  pheno9<-data.frame(pheno4$name_in_vcf,pheno4$V5)
  names(pheno9)=c("name_in_vcf",id)
  
  
  order=join(order,pheno9, by="name_in_vcf")
}

# this file can be used to calculate correlations between these predicted phenotypes or search for a relationship between  predicted and actual phenotype
