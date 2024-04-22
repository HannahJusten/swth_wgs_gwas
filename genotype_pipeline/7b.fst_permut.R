#!/usr/bin/env Rscript

#module load iccifort/2019.5.281 impi/2018.5.288 R/4.1.0

library(splitstackshape)
library(plyr)
library(dplyr)

#genome-wide_fst_25kb.windowed.weir.fst
#setwd("/scratch/user/justen/fst_between_specs/")

fst<- read.table("genome-wide_fst_25kb.windowed.weir.fst", sep="\t", header=T)

fst_c<- read.table("genome-wide_coastal_fst_25kb.windowed.weir.fst", sep="\t", header=T)

fst_i<- read.table("genome-wide_inland_fst_25kb.windowed.weir.fst", sep="\t", header=T)

df<-data.frame(unique(fst$CHROM))

final<-data.frame()

#####################################################################################################

for (i in 1:nrow(df)){

 fst_sub<-subset(fst,fst$CHROM==df$unique.fst.CHROM.[i])


 fst_c_sub<-subset(fst_c,fst_c$CHROM==df$unique.fst.CHROM.[i])
 fst_c_sub<-data.frame(fst_c_sub$BIN_START,fst_c_sub$N_VARIANTS,fst_c_sub$WEIGHTED_FST,fst_c_sub$MEAN_FST)
 names(fst_c_sub)=c("BIN_START","N_VARIANTS_c","WEIGHTED_FST_c","MEAN_FST_c")


 fst_i_sub<-subset(fst_i,fst_i$CHROM==df$unique.fst.CHROM.[i])
 fst_i_sub<-data.frame(fst_i_sub$BIN_START,fst_i_sub$N_VARIANTS,fst_i_sub$WEIGHTED_FST,fst_i_sub$MEAN_FST)
 names(fst_i_sub)=c("BIN_START","N_VARIANTS_i","WEIGHTED_FST_i","MEAN_FST_i")


 fst_dat<-join(fst_sub,fst_c_sub, by="BIN_START",match="first")

 fst_dat<-join(fst_dat,fst_i_sub, by="BIN_START",match="first")


 final<-rbind(final,fst_dat)

}
#####################################################################################################

order<-read.csv("/scratch/user/justen/stitch/scaffold_order.csv")

#order_sub<-data.frame(order$X..Sequence.Name,order$Assigned.Molecule,order$col,order$col2,order$col3_gray,order$col4_blue,order$col5_red,order$col6_green)
#names(order_sub)=c("CHROM","molecule","col","col2","col3_gray","col4_blue","col5_red","col6_green")

order_sub<-data.frame(order$X..Sequence.Name,order$Assigned.Molecule,order$col3_gray)
names(order_sub)=c("CHROM","molecule","col3_gray")

data<-join(order_sub,final, by="CHROM")

#####################################################################################################
# calculate delta FST

data$max_dif = ifelse(data$MEAN_FST_c > data$MEAN_FST_i, data$MEAN_FST_c,
                      ifelse(data$MEAN_FST_c < data$MEAN_FST_i, data$MEAN_FST_i,data$MEAN_FST_i))


data$final_fst_mean=data$MEAN_FST-data$max_dif

#####################################################################################################
# subset to autosomes
data_auto<- subset(data,data$molecule!="Z")
data_auto2<- subset(data_auto,data_auto$molecule!="W")
#####################################################################################################

# autosomes:

results<-data.frame()

for (i in 1:10000){

data_s1<-sample_n(data_auto2,76)
data_s1_mean<-mean(data_s1$final_fst_mean,na.rm=T)

data.i<-data.frame(i,data_s1_mean)

results<-rbind(results,data.i)
}

#####################################################################################################
