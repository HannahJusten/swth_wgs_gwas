# This script plots the filtered data and exports individual dataframes for each tag

library(remotes)
library(motus)
library(motusData)

# other packages

library(maps)
library(tidyverse)
library(rworldmap)

library(lubridate)

library(curl)

library(DBI)
library(RSQLite)
library(dbplyr)
library(splitstackshape)
library(plyr)
library(dplyr)



proj.num <- 280

Sys.setenv(TZ = "UTC")

setwd("C:/Users/hcjusten/Dropbox/PhD/Thesis/Chapter_2/Analysis/Motus/data/july2_2021")

#df.alltags.sub <- read.csv("./df.alltags_nov_2021.csv")

load("C:/Users/hcjusten/Dropbox/PhD/Thesis/Chapter_2/Analysis/Motus/data/july2_2021/df_alltags_relaxed#280_Jan2022.rds.RData")

df.alltags.sub<-df.alltags.sub_year


#########################################################################################################################################
#########################################################################################################################################


tagSummary <- df.alltags.sub %>%
  group_by(motusTagID) %>% 
  summarize(nDet = n(),
            nRecv = length(unique(recvDeployName)),
            totDay = length(unique(doy)))


# rounding the detection time to hour (saves processing time)

df.alltags.sub2 <- df.alltags.sub %>%
  mutate(hour = as.POSIXct(round(ts, "hour"))) %>% 
  select(motusTagID, port, tagDeployStart, tagDepLat, tagDepLon, 
         recvDeployLat, recvDeployLon, recvDeployName, antBearing, speciesEN,recvProjID, year, doy, hour) %>% 
  distinct()


df.alltags.sub.relaxed <- df.alltags.sub %>%
  mutate(ts = as_datetime(ts, tz = "UTC"), 
         year = year(ts), # extract year from ts
         time=format(ts,"%H:%M:%S"),
         doy = yday(ts)) %>% 
  filter(!is.na(recvDeployLat))

df.alltags.sub.relaxed <- df.alltags.sub.relaxed %>%
  mutate(ts = as_datetime(tagDeployStart, tz = "UTC"), 
         yeardeploy = year(ts), # extract year from ts
         doydeploy = yday(ts)) 



dat19<-read.csv("C:/Users/hcjusten/Documents/delmore_lab/SWTH/banding_sheets/banding_2019_motus.csv")

dat20<-read.csv("C:/Users/hcjusten/Documents/delmore_lab/SWTH/banding_sheets/banding_2020_motus.csv")

dat21<-read.csv("C:/Users/hcjusten/Documents/delmore_lab/SWTH/banding_sheets/banding_2021_motus.csv")


dat19<-data.frame(dat19$motusTagID,dat19$..Reference..,dat19$Site)
names(dat19)<-c("motusTagID","reference","site")
dat19<-dat19[!is.na(dat19$motusTagID),]


dat20<-data.frame(dat20$motusTagID,dat20$Reference..,dat20$Site)
names(dat20)<-c("motusTagID","reference","site")
dat20<-dat20[!is.na(dat20$motusTagID),]

dat21<-data.frame(dat21$motusTagID,dat21$Reference..,dat21$Site)
names(dat21)<-c("motusTagID","reference","site")
dat21<-dat21[!is.na(dat21$motusTagID),]


dat<-rbind(dat19,dat20,dat21)

df.alltag.final<-merge(dat,df.alltags.sub.relaxed, by="motusTagID", all.y=T)

df.alltag.final<-df.alltag.final[!is.na(df.alltag.final$reference),]

df.alltag.final<-subset(df.alltag.final,df.alltag.final$tagProjID==280)

df.alltag.final <- filter(df.alltag.final, probability == 1)


############################################################################################################

ir <- df.alltag.final %>% group_by(motusTagID)

df<-data.frame(group_keys(ir))

setwd("C:/Users/hcjusten/Dropbox/PhD/Thesis/Chapter_2/Analysis/Motus/data/july2_2021/before_manual/")

for (i in 1:nrow(df)){
  
  data_sub<-subset(df.alltag.final,df.alltag.final$motusTagID==df[i,])
  
  
  id<-df[i,]
  
  
  write.csv(data_sub, paste("full_df_before/",id,"_all_track_before_manual.csv",sep=""),row.names=F)
  
  rm(data_sub,id)
}

############################################################################################################
