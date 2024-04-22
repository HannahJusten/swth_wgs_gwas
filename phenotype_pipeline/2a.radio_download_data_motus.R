##################################################################################################
#
# 1. Download updated data from the Motus website
#
##################################################################################################
#
#1) Install libraries
#2) Load data
#3) Convert to data frame
#

##################################################################################################

# 1) Install libraries

#install.packages("remotes")

#remotes::install_github("motusWTS/motus@master")
#remotes::install_github("MotusWTS/motusData")
#remotes::install_github("MotusWTS/motus", force=T)
#remotes::install_github("r-lib/httr")

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

# Complete list of functions within the Motus server R package can be found on Github:
#https://github.com/jbrzusto/motusServer

##################################################################################################
##################################################################################################

# 2) Load data

# set working directory, data will be stored here

setwd("C:/Users/hcjusten/Dropbox/PhD/Thesis/Chapter_2/Analysis/Motus/data/july2_2021")

# set system environment - should be part of every working session

Sys.setenv(TZ = "UTC")

# working with sample data project

proj.num <- 280

# when downloading data for the first time: you will be asked for your motus name and password

sql.motus <- tagme(projRecv = proj.num, new = F, update = TRUE)

## you can also define a working directory here and download the data new
#sql.motus <- tagme(projRecv = proj.num, new = TRUE, update = TRUE, 
#                                     dir = "./")

#username: hjusten
#password: Extraordinary2020

##################################################################################################
##################################################################################################


# 3) Convert to data frame

# retrieve alltags table from the motus file, it contains the detection data + metadata
tbl.alltags <- tbl(sql.motus, "alltags")

# might be better to include the right time stamp while converting
df.alltags <- tbl.alltags %>% 
  collect() %>% 
  as.data.frame() %>%     # for all fields in the df (data frame)
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))

# Get overview of the unfiltered detections

# to summarize the detection data before converting into a df - find number of different detections for each tag at each receiver

df.detectSum <- tbl.alltags %>% 
  count(motusTagID, recv) %>%
  collect() %>%
  as.data.frame() 


#############################################################################################

df.alltags <- df.alltags %>%
  mutate(ts = as_datetime(ts, tz = "UTC"), # convert ts to POSIXct format
         year = year(ts),
         month = month(ts),# extract year from ts
         doy = yday(ts))#%>% # extract numeric day of year from ts

#############################################################################################
#############################################################################################

# load meta data for the motus file
metadata(sql.motus, projectIDs=proj.num)

# check version of the motus R package that was used to download the data
#checkVersion(sql.motus)

# check if data is complete: should be 236 tags in Nov 2020:
tbl.tags <- tbl(sql.motus, "tags") 
df.tags <- tbl.tags %>%
  filter(projectID == proj.num) %>%
  collect() %>%
  as.data.frame()

nrow(df.tags)
# number of registered tags that were deployed

#unique(df.tags$tagID)

#############################################################################################
#############################################################################################


tbl.tagDeps <- tbl(sql.motus, "tagDeps") 

df.tagDeps <- tbl.tagDeps %>%
  filter(projectID == proj.num) %>%
  collect() %>%
  as.data.frame() %>% # once in df format, can format dates with lubridate
  mutate(tsStart = as_datetime(tsStart, tz = "UTC", origin = "1970-01-01"),
         tsEnd = as_datetime(tsEnd, tz = "UTC", origin = "1970-01-01")) 

anti_join(df.tags, df.tagDeps, by = "tagID") 

# number of deployments per tag

df.alltags %>%
  select(motusTagID, tagDeployID) %>%
  filter(!(is.na(tagDeployID))) %>% # remove NA tagDeployIDs
  distinct() %>%
  group_by(motusTagID) %>%
  mutate(n = n()) %>%
  filter(n > 1)

#############################################################################################
# range of metadata
df.tagDeps %>%
  select(tagID, projectID, tsStart, tsEnd, speciesID, latitude, longitude) %>%
  summary()

# generate list of species IDs in project 280 metadata
sp.list <- unique(df.tagDeps$speciesID)  

# Species metadata
tbl.species <- tbl(sql.motus, "species") 
tbl.species %>%
  filter(id %in% sp.list) %>%
  collect() %>%
  as.data.frame()

###############################################################################################

tbl.recvDeps <- tbl(sql.motus, "recvDeps") 

df.projRecvs <- tbl.recvDeps %>%
  filter(projectID == proj.num) %>%
  collect() %>%
  as.data.frame() %>%
  mutate(tsStart = as_datetime(tsStart, tz = "UTC", origin = "1970-01-01"),
         tsEnd = as_datetime(tsEnd, tz = "UTC", origin = "1970-01-01"))

summary(df.projRecvs)

df.projRecvs %>%
  mutate(dateStart = date(tsStart)) %>% 
  select(-serno,-fixtureType, -macAddress, -tsStart, -tsEnd, -elevation, 
         -projectID, -status, -receiverType, -siteName) %>%
  arrange(deviceID, latitude, dateStart)

df.projRecvs.long <- df.projRecvs %>%
  select(deviceID, deployID, tsStart, tsEnd) %>% 
  tidyr::gather(when, ts, c(tsStart, tsEnd)) %>%
  # fake end date:
  mutate(ts = if_else(is.na(ts), max(ts, na.rm = TRUE) + duration(1, "month"), ts)) 

###############################################################################################

# Load receiver and antenna metadata

tbl.antDeps <- tbl(sql.motus, "antDeps")
# 
df.antDeps <- tbl.antDeps %>%
  select(deployID, port, antennaType, bearing, heightMeters) %>%
  collect() %>%
  as.data.frame()

tbl(sql.motus, "alltags") %>%
  filter(tagProjID == proj.num) %>% # subset to include only tags registered to project
  count(motusTagID) %>%
  as.data.frame()
# 
# 
tbl(sql.motus, "alltags") %>%
  filter(tagProjID == proj.num) %>% # subset to include only tags registered to project
  mutate(rl.gt.3 = if_else(runLen == 3, "run 3", "run > 3")) %>%
  count(motusTagID, rl.gt.3) %>%
  collect() %>%
  spread(key = rl.gt.3, value = n)
# 
# 
 filter(tbl(sql.motus, "alltags"), runLen <= 3) %>% collect() %>% nrow()
# 
# # Identify runs to remove
to_remove <- tbl(sql.motus, "runs") %>%
  select(runID, motusFilter) %>%
  filter(motusFilter == 0)
# 
tbl_filtered <- anti_join(tbl(sql.motus, "alltags"), to_remove, by = "runID")
# 
filter(tbl_filtered, runLen <= 3) %>% collect() %>% nrow()


# 
 tbl_motusFilter <- filterByActivity(sql.motus, return = "all")
# 
 df.alltags.sub <- tbl_motusFilter %>% 
   # filter(probability == 1) %>%
   collect() %>%
   as.data.frame() %>%
   mutate(ts = as_datetime(ts),  # work with dates AFTER transforming to flat file
          tagDeployStart = as_datetime(tagDeployStart),
          tagDeployEnd = as_datetime(tagDeployEnd))
 
 df.alltags.sub$probability[is.na(df.alltags.sub$probability)]<-1
 
 df.alltags.sub_year <- df.alltags.sub %>%
   mutate(ts = as_datetime(ts, tz = "UTC"), # convert ts to POSIXct format
          year = year(ts), # extract year from ts
          doy = yday(ts)) %>% # extract numeric day of year from ts
   filter(!is.na(recvDeployLat))

###############################################################################################
###############################################################################################


# Filtering relaxed


tbl_relaxed <- filterByActivity(sql.motus, minLen = 2, maxLen = 4,
                                maxRuns = 500, ratio = 0.95, return = "all", view = "alltagsGPS")

tbl.alltags.gps <- filterByActivity(sql.motus, return = "all", view = "alltagsGPS") 
 
df.alltags.sub <- tbl_relaxed %>% 
 # filter(probability == 1) %>%
  collect() %>%
  as.data.frame() %>%
  mutate(ts = as_datetime(ts),  # work with dates AFTER transforming to flat file
         tagDeployStart = as_datetime(tagDeployStart),
         tagDeployEnd = as_datetime(tagDeployEnd))

df.alltags.sub$probability[is.na(df.alltags.sub$probability)]<-1

write.csv(df.alltags.sub, "./df.alltags_nov_2021.csv")

df.alltags.sub_year <- df.alltags.sub %>%
  mutate(ts = as_datetime(ts, tz = "UTC"), # convert ts to POSIXct format
         year = year(ts), # extract year from ts
         doy = yday(ts)) %>% # extract numeric day of year from ts
  filter(!is.na(recvDeployLat))

df.alltags <- tbl_relaxed %>%
  mutate(recvLat = if_else((is.na(gpsLat)|gpsLat == 0|gpsLat == 999),
                           recvDeployLat, gpsLat),
         recvLon = if_else((is.na(gpsLon)|gpsLon == 0|gpsLon == 999),
                           recvDeployLon, gpsLon),
         recvAlt = if_else(is.na(gpsAlt), recvDeployAlt, gpsAlt)) %>%
  select(-noise, -slop, -burstSlop, -done, -bootnum, -mfgID,
         -codeSet, -mfg, -nomFreq, -markerNumber, -markerType,
         -fullID, -deviceID, -recvDeployLat,
         -recvDeployLon, -recvDeployAlt, -speciesGroup, -gpsLat,
         -gpsLon, -recvAlt, -recvSiteName) %>%
  collect() %>%
  as.data.frame() %>%
  mutate(ts = as_datetime(ts),  # work with dates AFTER transforming to flat file
         tagDeployStart = as_datetime(tagDeployStart),
         tagDeployEnd = as_datetime(tagDeployEnd),
         recvLat = plyr::round_any(recvLat, 0.05),
         recvLon = plyr::round_any(recvLon, 0.05),
         recvDeployName = if_else(is.na(recvDeployName),
                                  paste(recvLat, recvLon, sep=":"),
                                  recvDeployName))

df.alltags$probability[is.na(df.alltags$probability)]<-1
df.alltags.sub_recv <- filter(df.alltags, probability == 1)


df.alltags.sub$probability[is.na(df.alltags.sub$probability)]<-1
# save data as RDS file

# full dataset, with relaxed probability values column

saveRDS(df.alltags, "./df_alltags_#280_nov2021.rds") 

###############################################################################################
###############################################################################################
