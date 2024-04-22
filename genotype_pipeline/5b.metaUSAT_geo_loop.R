#!/usr/bin/env Rscript
#module load iccifort/2019.5.281  impi/2018.5.288 R/4.1.0

args = commandArgs(trailingOnly=TRUE)
list=args[1]

library(CompQuadForm)
library(minqa)
library(psych)
library(survey)
#library(plyr)
library(devtools)
#source("https://github.com/RayDebashree/metaUSAT/blob/master/metaUSAT_v1.17.R?raw=TRUE")
source("/scratch/user/justen/stitch/Rscripts/metaUSAT_v1.17.R")
#devtools::source_url("https://github.com/RayDebashree/metaUSAT/blob/master/metaUSAT_v1.17.R?raw=TRUE")

# load phenotypes

# list of phenotypes

list<-read.table("list")

for (i in 1:nrow(list)){

id<-list$V1[i]

data1<-read.table(paste("/scratch/user/justen/stitch/full_stitch/",id,"_fuller_dir/output/",id,".lmm.assoc.txt",sep=""),h=T)

data1$Z<-data1$beta/data1$se

data1_sub<-data.frame(data1$rs,data1$Z)
names(data1_sub)=c(paste("rs","Z"))

names(data1_sub)=c("rs",id)


results<-data1_sub

}


write.csv(results,"/scratch/user/justen/stitch/metaUSAT/adult_predict/data_final.csv",row.names=F)

