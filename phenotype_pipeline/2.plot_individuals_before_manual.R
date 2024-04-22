# This script plots the filtered data and exports individual dataframes for each tag


# library(motus)
# library(tidyverse)
# library(ggmap)
# library(lubridate)
# library(dplyr)
# library(plyr)
# library(ggplot2)


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

#156

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


library(plyr)

df.alltag.final<-merge(dat,df.alltags.sub.relaxed, by="motusTagID", all.y=T)

df.alltag.final<-df.alltag.final[!is.na(df.alltag.final$reference),]

df.alltag.final<-subset(df.alltag.final,df.alltag.final$tagProjID==280)

df.alltag.final <- filter(df.alltag.final, probability == 1)


library(splitstackshape)
library(plyr)
library(dplyr)

ir <- df.alltag.final %>% group_by(motusTagID)


df<-data.frame(group_keys(ir))

setwd("C:/Users/hcjusten/Dropbox/PhD/Thesis/Chapter_2/Analysis/Motus/data/july2_2021/before_manual/")

for (i in 1:nrow(df)){
  
  data_sub<-subset(df.alltag.final,df.alltag.final$motusTagID==df[i,])
  
  
  id<-df[i,]
  
  
  write.csv(data_sub, paste("full_df_before/",id,"_all_track_before_manual.csv",sep=""),row.names=F)
  
  rm(data_sub,id)
}


#ex<-unique(df.alltag.final$motusTagID)
#length(ex)

#df.alltag.final<-df.alltag.final[!is.na(df.alltag.final$tagDeployStart),]
#############################################################################
# Mapping data

# Simplify the data by summarizing by the runID
# If you want to summarize at a finer (or coarser) scale, you can also create
# other groups. The simplest alternative is a rounded timestamp variable; for
# example by using mutate(ts.h = plyr::round_any(ts, 3600) function call. Other
# options are to just use date (e.g date = as_date(ts))

df.alltags.sub <- filter(df.alltags.sub, probability == 1)

#remove falsely east detections

df.alltags.sub1 <- subset(df.alltags.sub, df.alltags.sub$recvDeployName != "Tadoussac - Andr?")
df.alltags.sub1 <- subset(df.alltags.sub1, df.alltags.sub1$recvDeployName != "Lockoff")
df.alltags.sub1 <- subset(df.alltags.sub1, df.alltags.sub1$recvDeployName != "Triton2")


df.alltags.sub3<- subset(df.alltags.sub1, df.alltags.sub1$recvDeployLon < -70)

df.alltags.sub4<- subset(df.alltags.sub3, df.alltags.sub3$runLen > 3)



fun.getpath <- function(df) {
  df %>%
    filter(tagProjID == proj.num, # keep only tags registered to the sample project
           !is.na(recvDeployLat) | !(recvDeployLat == 0)) %>% 
    group_by(motusTagID, runID, recvDeployName, ambigID, 
             tagDepLon, tagDepLat, recvDeployLat, recvDeployLon,tagDeployStart) %>%
    summarize(max.runLen = max(runLen), ts.h = mean(ts)) %>%
    arrange(motusTagID, ts.h) %>%
    data.frame()
} # end of function call

df.alltags.path <- fun.getpath(df.alltags.sub)


#df.alltags.sub.path <- df.alltags.sub %>%
#  filter(tagProjID == proj.num, year(tagDeployStart) == 2020) %>% # only tags registered to project
#  arrange(motusTagID, ts) %>%       # order by time stamp for each tag
#  mutate(date = as_date(ts)) %>%    # create date variable
#  group_by(motusTagID, date, recvDeployName, ambigID, 
#           tagDepLon, tagDepLat, recvDeployLat, recvDeployLon,tagDeployStart)


#########################################################################################################################################

library(ggmap)

register_google('AIzaSyAJgCcOqjLwCx5zuBAqP5OYaeYJsyDofhE')

gmap <-  get_stamenmap(bbox = c(left = -140, right = -60, bottom = -10, top = 60),
                       maptype = "terrain-background", # select maptype
                       zoom = 3) # zoom, must be a whole number


library(splitstackshape)
library(plyr)
library(dplyr)

ir <- df.alltags.sub_high %>% group_by(motusTagID)


df<-data.frame(group_keys(ir))

setwd("C:/Users/hcjusten/Dropbox/PhD/Thesis/Chapter_2/Analysis/Motus/data/july2_2021/before_manual/")

for (i in 2:nrow(df)){
  
  data_sub<-subset(df.alltags.sub,df.alltags.sub$motusTagID==df[i,])

  df.alltags.path <- fun.getpath(data_sub)
  id<-df[i,]

  df.tmp <- df.alltags.path %>%
    arrange(ts.h)  %>% # arange by hour
    as.data.frame()


  testrunID<-rep("NA",nrow(df.tmp))
  max.runLen<-rep("NA",nrow(df.tmp))
  deploymentloc<-rep("deployment_loc",nrow(df.tmp))
  ambigID<-rep("NA",nrow(df.tmp))
  max.runLen<-rep("NA",nrow(df.tmp))

  deploloc<-data.frame(df.tmp$motusTagID, testrunID,deploymentloc,ambigID,df.tmp$tagDepLon, df.tmp$tagDepLat, df.tmp$tagDepLat,df.tmp$tagDepLon,max.runLen, df.tmp$tagDeployStart, df.tmp$tagDeployStart)
  names(deploloc)<-c("motusTagID","runID","recvDeployName","ambigID","tagDepLon","tagDepLat","recvDeployLat","recvDeployLon","max.runLen","ts.h","tagDeployStart")

  deployadd<-rbind(deploloc,df.tmp)

  #pdf(paste("plots/",id,"_track_before_manual_filter.pdf",sep=""), useDingbats=F)
  # pdf(paste("plots/",id,"_track_before_manual_filter.pdf",sep=""))
  #   print(ggmap(gmap) +
  #     theme_bw() + 
  #     geom_point(data = deployadd, aes(x = tagDepLon, y = tagDepLat), 
  #            shape = 21, colour = "black", fill = "black") +
  #     geom_point(data = deployadd, aes(x = recvDeployLon, y = recvDeployLat), 
  #            shape = 21, colour = "black", fill = "yellow") +
  #     geom_path(data = deployadd, 
  #           aes(x = recvDeployLon, y = recvDeployLat, group = motusTagID, col = "red")))
  # dev.off()



#head(data_sub)
#test<-data_sub[1,]

#test$recvDeployLat2<-test$tagDepLat
#test$recvDeployLon2<-test$


#names(test)<-c("hitID","runID","batchID","ts","tsCorrected","sig","sigsd","noise","freq","freqsd","slop","burstSlop","done","motusTagID","ambigID","port","nodeNum","runLen","motusFilter","bootnum","tagProjID","mfgID","tagType","codeSet","mfg","tagModel","tagLifespan","nomFreq","tagBI","pulseLen","tagDeployID","speciesID","markerNumber","markerType","tagDeployStart","tagDeployEnd","tagDepLat","tagDepLon","tagDepAlt","tagDepComments","tagDeployTest","fullID","deviceID","recvDeployID","recvDeployLat recvDeployLon recvDeployAlt  recv recvDeployName recvSiteName isRecvMobile recvProjID recvUtcOffset  antType antBearing antHeight speciesEN speciesFR speciesSci speciesGroup tagProjName recvProjName  gpsLat    gpsLon gpsAlt probability year doy")

for (i in 2:nrow(df)){
  
  data_sub<-subset(df.alltags.sub,df.alltags.sub$motusTagID==df[i,])
  

  id<-df[i,]
  
  # testrunID<-rep("NA",1)
  # max.runLen<-rep("NA",1)
  # deploymentloc<-rep("deployment_loc",1)
  # ambigID<-rep("NA",1)
  # max.runLen<-rep("NA",1)
  # 
  # 
  # deploy_sub<-data.frame(data_sub$motusTagID,data_sub$tagDeployStart,data_sub$tagDeployEnd,data_sub$tagDepLat,data_sub$tagDepLon)
  # names(deploy_sub)<-c("motusTagID","tagDeployStart","tagDeployEnd","tagDepLat","tagDepLon")
  # 
  # 
  # new_df<-merge(data_sub, deploy_sub, by="motusTagID", all.x=T,all.y=F)
  # 
  
  
  # deployadd2 <- deployadd %>%
  #   mutate(ts = as_datetime(ts.h, tz = "UTC"), # convert ts to POSIXct format
  #        year = year(ts.h),
  #        month = month(ts.h),# extract year from ts
  #        doy = yday(ts.h))#%>% # extract numeric day of year from ts

  #write.csv(deployadd2, paste(id,"_track_before_manual.csv",sep=""))
  
  write.csv(data_sub, paste("full_df_before/",id,"_all_track_before_manual.csv",sep=""))
  
  rm(deployadd,deployadd2,deploloc, data_sub,df.tmp, id, df.alltags.path,testrunID, max.runLen)
}

#########################################################################################################################################


tbl.recvDeps <- tbl(sql.motus, "recvDeps")
# df.recvDeps <- tbl.recvDeps %>% 
#   collect() %>% 
#   as.data.frame() %>% 
#   mutate(tsStart = as_datetime(tsStart, tz = "UTC", origin = "1970-01-01"),
#          tsEnd = as_datetime(tsEnd, tz = "UTC", origin = "1970-01-01"),
#          # for deployments with no end dates, make an end date a year from now
#          tsEnd = if_else(is.na(tsEnd), 
#                          as.POSIXct(format(Sys.time(), "%Y-%m-%d %H:%M:%S")) +
#                            dyears(1), 
#                          tsEnd), 
#          tsEnd = as.POSIXct(tsEnd, tz = "UTC", origin = "1970-01-01"))
# 
# # get running intervals for all receiver deployments
siteOp <- with(df.recvDeps, interval(tsStart, tsEnd))

# set the date range you're interested in
dateRange <- interval(as.POSIXct("2019-07-01"), as.POSIXct("2020-05-20"))

# create new variable "active" which will be set to TRUE if the receiver was
# active at some point during your specified date range, and FALSE if not
df.recvDeps$active <- int_overlaps(siteOp, dateRange) 

# create map with receivers active during specified date range as red, and
# receivers with detections as yellow
ggmap(gmap) +
  theme_bw() + 
  geom_point(data = subset(df.recvDeps, active == TRUE), 
             ggplot2::aes(x = longitude, y = latitude), 
             shape = 21, colour = "black", fill = "red") +
  geom_point(data = df.tmp, aes(x = recvLon, y = recvLat), 
             shape = 21, colour = "black", fill = "yellow") +
  geom_path(data = df.tmp, 
            aes(x = recvLon, y = recvLat, group = motusTagID, 
                col = as.factor(motusTagID))) +
  scale_color_discrete(name = "MotusTagID")





df.alltags %>% 
  select(ts, motusTagID, runLen, speciesEN, tagDepLat, tagDepLon, 
         recvLat, recvLon) %>% 
  summary()
#########################################################################################################################################


setwd("C:/Users/hcjusten/Dropbox/PhD/Thesis/Chapter_2/Analysis/Motus/data/october_2020/new/")

# set system environment - should be part of every working session

Sys.setenv(TZ = "UTC")

# working with sample data project

proj.num <- 280

# when downloading data for the first time: you will be asked for your motus name and password

sql.motus <- tagme(projRecv = proj.num, new = T, update = TRUE)



tbl.recvDeps <- tbl(sql.motus, "recvDeps") 


# df.projRecvs <- tbl.recvDeps %>%
#   filter(projectID == proj.num) %>%
#   collect() %>%
#   as.data.frame() %>%
#   mutate(tsStart = as_datetime(tsStart, tz = "UTC", origin = "1970-01-01"),
#          tsEnd = as_datetime(tsEnd, tz = "UTC", origin = "1970-01-01"))
# 
# summary(df.projRecvs)




df.recvDeps <- tbl.recvDeps %>%
  collect() %>%
  as.data.frame() %>%
  mutate(tsStart = as_datetime(tsStart, tz = "UTC", origin = "1970-01-01"),
         tsEnd = as_datetime(tsEnd, tz = "UTC", origin = "1970-01-01"))




library(rgdal)     # R wrapper around GDAL/OGR
library(ggplot2)   # for general plotting
library(ggmaps) 
library(raster)
projection<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
e<-extent(c(-140,-40,-40,70))
world2<-rgdal::readOGR("C:/Users/hcjusten/Documents/delmore_lab/general_resources/admin_layers/world_adm0.shp")

#projection<-"+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs" 


world2_proj<-spTransform(world2, CRS=CRS(projection))

world2_e<-crop(world2_proj,e)

plot(world2_e)

#pdf("all_tracks_all_receivers_simple_back.pdf")

ggplot() +
  geom_path(data=world2_e, aes(x=long,y=lat, group=group),color = 'black', fill = 'white', size = .2)+
  
  
  
  
  
  geom_point(data = df.recvDeps, 
             aes(longitude, latitude, colour = "grey"), 
             shape = 21)
 
#dev.off()

# df.recvDeps <- tbl.recvDeps %>% 
#   collect() %>% 
#   as.data.frame() %>% 
#   mutate(tsStart = as_datetime(tsStart, tz = "UTC", origin = "1970-01-01"),
#          tsEnd = as_datetime(tsEnd, tz = "UTC", origin = "1970-01-01"),
#          # for deployments with no end dates, make an end date a year from now
#          tsEnd = if_else(is.na(tsEnd), 
#                          as.POSIXct(format(Sys.time(), "%Y-%m-%d %H:%M:%S")) +
#                            dyears(1), 
#                          tsEnd), 
#          tsEnd = as.POSIXct(tsEnd, tz = "UTC", origin = "1970-01-01"))
receivers<-read.csv("C:/Users/hanna/Downloads/receiver-deployments.csv")


df.recvDeps <- receivers %>% 
  collect() %>% 
  #as.data.frame() %>% 
  mutate(tsStart = as_datetime(tsStart, tz = "UTC", origin = "1970-01-01"),
         tsEnd = as_datetime(tsEnd, tz = "UTC", origin = "1970-01-01"),
         # for deployments with no end dates, make an end date a year from now
         tsEnd = if_else(is.na(tsEnd), 
                         as.POSIXct(format(Sys.time(), "%Y-%m-%d %H:%M:%S")) +
                           dyears(1), 
                         tsEnd), 
         tsEnd = as.POSIXct(tsEnd, tz = "UTC", origin = "1970-01-01"))
# get running intervals for all receiver deployments
siteOp <- with(df.recvDeps, interval(tsStart, tsEnd))

# set the date range you're interested in
dateRange <- interval(as.POSIXct("2019-07-01"), as.POSIXct("2020-05-20"))

# create new variable "active" which will be set to TRUE if the receiver was
# active at some point during your specified date range, and FALSE if not
df.recvDeps$active <- int_overlaps(siteOp, dateRange) 

test<-subset(df.recvDeps, df.recvDeps$longitude<= -25)
df.recvDeps<-test 

  ggplot() +
    geom_path(data=world2_e, aes(x=long,y=lat, group=group),color = 'black', fill = 'white', size = .2)+
    geom_point(data = subset(df.recvDeps, active == TRUE), 
             ggplot2::aes(x = longitude, y = latitude), 
             shape = 21, colour = "black",fill="red")
  
#########################################################################
 
  pdf("all_receivers_detected_May_kira.pdf", useDingbats=FALSE)
  
  ggplot() +
    #aes(long, lat)+
    #geom_polygon()+
    geom_path(data=world2_e, aes(x=long,y=lat, group=group),color = 'black', fill = 'white', size = .2)+
    #theme_bw() +
    #coord_equal()+
     geom_point(data = df.recvDeps,
                aes(longitude, latitude, colour = "grey"),
                shape = 21)+
    #  geom_path(data = deployadd, 
    #            aes(x = recvLon, y = recvLat, group = motusTagID, col = "red"))+
    #  geom_point(data = deployadd, aes(x = tagDepLon, y = tagDepLat), 
    #             shape = 21, colour = "black", fill = "black") +
      geom_point(data = deployadd, aes(x = recvLon, y = recvLat), 
                 shape = 21, colour = "black", fill = "yellow") 
    # # 
  
  dev.off()
  
  

ggplot() +
  #aes(long, lat)+
  #geom_polygon()+
  geom_path(data=world2_e, aes(x=long,y=lat, group=group),color = 'black', fill = 'white', size = .2)+
  #theme_bw() +
  #coord_equal()+
  # geom_path(data = deployadd, 
  #           aes(x = recvLon, y = recvLat, group = motusTagID, col = "red"))+
  # geom_point(data = deployadd, aes(x = tagDepLon, y = tagDepLat), 
  #            shape = 21, colour = "black", fill = "black") +
  # geom_point(data = deployadd, aes(x = recvLon, y = recvLat), 
  #            shape = 21, colour = "black", fill = "yellow") +
  geom_point(data = df.recvDeps, 
             aes(longitude, latitude, colour = "grey"), 
             shape = 21)

