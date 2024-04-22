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


list2<-read.table("list2")

for (i in 1:nrow(list)){

id<-list$V1[i]

data1<-read.table(paste("/scratch/user/justen/stitch/full_stitch/",id,"_fuller_dir/output/",id,".lmm.assoc.txt",sep=""),h=T)

data1$Z<-data1$beta/data1$se

data1_sub<-data.frame(data1$rs,data1$Z)
names(data1_sub)=c(paste("rs","Z_",id,sep=""))


}




data1<-read.table("/scratch/user/justen/stitch/full_stitch/winter_long1_dec22_fuller_dir/output/winter_long1_dec22.lmm.assoc.txt",h=T)
data1$Z<-data1$beta/data1$se

data2<-read.table("/scratch/user/justen/stitch/full_stitch/spring_30_fl_nov_fuller_dir/output/spring_30_fl_nov.lmm.assoc.txt",h=T)
data2$Z<-data2$beta/data2$se


data3<-read.table("/scratch/user/justen/stitch/full_stitch/distance_fall_corrected2_fuller_dir/output/distance_fall_corrected2.lmm.assoc.txt",h=T)
data3$Z<-data3$beta/data3$se

data4<-read.table("/scratch/user/justen/stitch/full_stitch/distance_spring_corrected2_fuller_dir/output/distance_spring_corrected2.lmm.assoc.txt",h=T)
data4$Z<-data4$beta/data4$se

data5<-read.table("/scratch/user/justen/stitch/full_stitch/wing_length_hj_only_new_cov_sex_dir/output/wing_length_hj_only_new.lmm.assoc.txt",h=T)
data5$Z<-data5$beta/data5$se


data6<-read.table("/scratch/user/justen/stitch/full_stitch/kipps_hj_only_new_cov_sex_dir/output/kipps_hj_only_new.lmm.assoc.txt",h=T)
data6$Z<-data6$beta/data6$se



data1_sub<-data.frame(data1$rs,data1$Z)
names(data1_sub)=c("rs","Z_winter_long")

data2_sub<-data.frame(data2$rs,data2$Z)
names(data2_sub)=c("rs","Z_spring_30")

data3_sub<-data.frame(data3$rs,data3$Z)
names(data3_sub)=c("rs","Z_fall_distance")

data4_sub<-data.frame(data4$rs,data4$Z)
names(data4_sub)=c("rs","Z_spring_distance")

data5_sub<-data.frame(data5$rs,data5$Z)
names(data5_sub)=c("rs","Z_wing_length")

data6_sub<-data.frame(data6$rs,data6$Z)
names(data6_sub)=c("rs","Z_kipps")

dat<-merge(data1_sub, data2_sub, by="rs", all=F)
dat1<-merge(dat, data3_sub, by="rs", all=F)
dat2<-merge(dat1, data4_sub, by="rs", all=F)

dat3<-merge(dat2, data5_sub, by="rs", all=F)
dat4<-merge(dat3, data6_sub, by="rs", all=F)



write.csv(dat4,"/scratch/user/justen/stitch/metaUSAT/adult_predict/data_final.csv",row.names=F)

dat<-dat4[,-1]

write.csv(dat4,"/scratch/user/justen/stitch/metaUSAT/adult_predict/data_ready_metaUSAT.csv",row.names=F)


###########################################################################################################################################


data1<-read.table("/scratch/user/justen/stitch/full_stitch/fall_bear_radio1_fuller_dir/output/fall_bear_radio1.lmm.assoc.txt",h=T)
data1$Z<-data1$beta/data1$se

data2<-read.table("/scratch/user/justen/stitch/full_stitch/fall_radio_long_35_fuller_dir/output/fall_radio_long_35.lmm.assoc.txt",h=T)
data2$Z<-data2$beta/data2$se


data3<-read.table("/scratch/user/justen/stitch/full_stitch/wing_length_hj_only_new_cov_sex_dir/output/wing_length_hj_only_new.lmm.assoc.txt",h=T)
data3$Z<-data3$beta/data3$se

data4<-read.table("/scratch/user/justen/stitch/full_stitch/kipps_hj_only_new_cov_sex_dir/output/kipps_hj_only_new.lmm.assoc.txt",h=T)
data4$Z<-data4$beta/data4$se


data1_sub<-data.frame(data1$rs,data1$Z)
names(data1_sub)=c("rs","Z_fall_bear")

data2_sub<-data.frame(data2$rs,data2$Z)
names(data2_sub)=c("rs","Z_fall_35")

data3_sub<-data.frame(data3$rs,data3$Z)
names(data3_sub)=c("rs","Z_wing_length")

data4_sub<-data.frame(data4$rs,data4$Z)
names(data4_sub)=c("rs","Z_kipps")

dat<-merge(data1_sub, data2_sub, by="rs", all=F)
dat1<-merge(dat, data3_sub, by="rs", all=F)
dat2<-merge(dat1, data4_sub, by="rs", all=F)

write.csv(dat2,"/scratch/user/justen/stitch/metaUSAT/juv_predict/data_final.csv",row.names=F)

dat<-dat4[,-1]

write.csv(dat,"/scratch/user/justen/stitch/metaUSAT/juv_predict/data_ready_metaUSAT.csv",row.names=F)

write.csv(dat,"/scratch/user/justen/stitch/metaUSAT/juv_predict/fata_final.csv",row.names=F)


#data5<-read.table("fall_bear.lmm.assoc.txt",h=T)
#data6<-read.table("fall_30.lmm.assoc.txt",h=T)
#data7<-read.table("wing_length.lmm.assoc.txt",h=T)
#data8<-read.table("kipps.lmm.assoc.txt",h=T)

#for (i in seq(along=data){
#data.i<-data.frame(data[i])

#data7$Z_wing<-data7$beta/data7$se
#data8$Z_kipps<-data8$beta/data8$se

#data1_sub<-data.frame(data1$rs,data1$Z_bear_fall)
#names(data1_sub)=c("rs","Z_bear_fall")
#data2_sub<-data.frame(data2$rs,data2$Z_winter)
#names(data2_sub)=c("rs","Z_winter")
#data3_sub<-data.frame(data3$rs,data3$Z_sp_30)
#names(data3_sub)=c("rs","Z_sp_30")
#data4_sub<-data.frame(data4$rs,data4$Z_bear_spring)
#names(data4_sub)=c("rs","Z_bear_spring")

#data5_sub<-data.frame(data5$rs,data5$Z_fa_bear)
#names(data5_sub)=c("rs","Z_fa_bear")
#data6_sub<-data.frame(data6$rs,data6$Z_fa_30)
#names(data6_sub)=c("rs","Z_fa_30")
#data7_sub<-data.frame(data7$rs,data7$Z_wing)
#names(data7_sub)=c("rs","Z_wing")
#data8_sub<-data.frame(data8$rs,data8$Z_kipps)
#names(data8_sub)=c("rs","Z_kipps")

#dat<-merge(data1_sub, data2_sub, by="rs", all=T)
#dat1<-merge(dat, data3_sub, by="rs", all=T)

#dat<-merge(data1_sub, data2_sub, by="rs", all=F)
#dat1<-merge(dat, data3_sub, by="rs", all=F)
#dat2<-merge(dat1, data4_sub, by="rs", all=F)

#dat3<-merge(dat2, data5_sub, by="rs", all=F)
#dat4<-merge(dat3, data6_sub, by="rs", all=F)
#dat5<-merge(dat4, data7_sub, by="rs", all=F)
#dat6<-merge(dat5, data8_sub, by="rs", all=F)

#write.csv(dat2,"/scratch/user/justen/stitch/metaUSAT/data_final.csv",row.names=F)

#dat_final<-data.frame(dat6$Z_sp_arr,dat6$Z_fa_arr,dat6$Z_winter,dat6$Z_fa_dep,dat6$Z_fa_bear,dat6$Z_fa_30,dat6$Z_wing,dat6$Z_kipps)
#dat_final<-data.frame(dat2$Z_bear_fall,dat2$Z_winter,dat2$Z_sp_30,dat2$Z_bear_spring)


#write.csv(dat_final,"fata_final.csv", row.names=F)
