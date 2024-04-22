#!/usr/bin/env Rscript
#module load iccifort/2019.5.281  impi/2018.5.288 R/4.1.0

library(CompQuadForm)
library(minqa)
library(psych)
library(survey)
#library(plyr)
library(devtools)

#source("https://github.com/RayDebashree/metaUSAT/blob/master/metaUSAT_v1.17.R?raw=TRUE")
source("/scratch/user/justen/stitch/Rscripts/metaUSAT_v1.17.R")
#devtools::source_url("https://github.com/RayDebashree/metaUSAT/blob/master/metaUSAT_v1.17.R?raw=TRUE")


#R1<-cor.tetrachor(dat1mio) # the numbers are different depending if we calculate this subset or the whole dataset

#dat_final<-read.csv("/scratch/user/justen/stitch/metaUSAT/fata_final.csv")
dat_final<-read.csv("./fata_final.csv")

R<-cor.tetrachor(dat_final) # I decided to runn the subset rows with the values for the full dataset

############### subsetting data to run things faster ###############


dat1mio<-head(dat_final, 500000)

results<-data.frame()

for (i in 1:nrow(dat1mio)){
test<-dat1mio[i,]
t<-data.matrix(test,rownames.force=NA)

res<-metausat(Z=t, R=R, weights=1)
df<-data.frame(res)
results<-rbind(results,df)
}

write.csv(results, "./results_geo_gwas_1.csv",row.names=F)

###############
