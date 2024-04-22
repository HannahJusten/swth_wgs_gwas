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



# location of tag deployments
na.lakes <- map_data(map = "lakes")
na.lakes <- mutate(na.lakes, long = long - 360)

# Include all of the Americas to begin
na.map <- map_data(map = "world2")
na.map <- filter(na.map, region %in% c("Canada", "USA", "Mexico"))

na.map <- mutate(na.map, long = long- 360)

################################################################################################

# map the location of tag deployments for the sample data

xmin <- min(df.tagDeps$longitude, na.rm = TRUE) - 5
xmax <- max(df.tagDeps$longitude, na.rm = TRUE) + 5
ymin <- min(df.tagDeps$latitude, na.rm = TRUE) - 5
ymax <- max(df.tagDeps$latitude, na.rm = TRUE) + 5

###############################################################################################
# Map 
library(mapproj)

ggplot(data = na.lakes, aes(x = long, y = lat)) + 
  geom_polygon(data = na.map, aes(long, lat, group = group), 
               colour = "grey", fill="grey98") + 
  geom_polygon(aes(group = group), colour = "grey", fill = "white") +
  coord_map(projection = "mercator", 
            xlim = c(xmin, xmax), 
            ylim = c(ymin, ymax)) +
  labs(x = "", y = "") + 
  theme_bw() + 
  geom_point(data = filter(df.tagDeps, projectID == 280), 
             aes(longitude, latitude), size = 2, shape = 1, colour = "red")


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
#parts missing

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

ggplot(data = df.projRecvs.long, 
       aes(x = ts, y = as.factor(deviceID), colour = as.factor(deployID))) +
  theme(legend.position = "none") +
  geom_line(lwd = 3) + 
  # instead, centre to the right
  geom_text(data = filter(df.projRecvs.long, when == "tsStart"), 
            aes(label = deployID), hjust = "left", nudge_y = 0.2, size = 3, angle = 45) +
  theme_bw() +
  labs(x = "Year", y = "Receiver ID")

ggplot(data = df.projRecvs.long, 
       aes(x = yday(ts), y = as.factor(deviceID), colour = as.factor(deployID))) +
  theme_bw() +
  theme(legend.position = "none") + 
  geom_line(lwd = 3) + 
  # centre labels to the left
  geom_text(data = filter(df.projRecvs.long, when == "tsStart"), 
            aes(label = deployID), hjust = "left", nudge_y = 0.4, size = 3) +
  labs(x = "Day of year", y = "Receiver ID") +
  facet_grid(year(ts) ~ ., scales = "free")


# Location of receiver deployments

df.recvDeps <- tbl.recvDeps %>%
  collect() %>%
  as.data.frame() %>%
  mutate(tsStart = as_datetime(tsStart, tz = "UTC", origin = "1970-01-01"),
         tsEnd = as_datetime(tsEnd, tz = "UTC", origin = "1970-01-01"))

ggplot(data = na.lakes, aes(x = long, y = lat)) + 
  theme_bw() + 
  geom_polygon(data = na.map, aes(long, lat, group = group), 
               colour = "grey", fill = "grey98") +
  geom_polygon(aes(group = group), colour = "grey", fill = "white") +
  coord_map(projection = "mercator", xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  labs(x = "", y = "") + 
  geom_point(data = df.recvDeps, 
             aes(longitude, latitude, colour = as.logical(projectID == 280)), 
             size = 0.8, shape = 4) +
  scale_colour_manual(values = c("grey30", "red"), name = "Project 280 Deployment")

#######

xmin <- min(df.projRecvs$longitude, na.rm = TRUE) - 20
xmax <- max(df.projRecvs$longitude, na.rm = TRUE) + 20
ymin <- min(df.projRecvs$latitude, na.rm = TRUE) - 10
ymax <- max(df.projRecvs$latitude, na.rm = TRUE) + 10

# xmin <- min(df.projRecvs$longitude, na.rm = TRUE) - 5
# xmax <- max(df.projRecvs$longitude, na.rm = TRUE) + 5
# ymin <- min(df.projRecvs$latitude, na.rm = TRUE) - 5
# ymax <- max(df.projRecvs$latitude, na.rm = TRUE) + 5

#######

# map
ggplot(data = na.lakes, aes(x = long, y = lat))+ 
  theme_bw() + 
  geom_polygon(data = na.map, aes(long, lat, group = group), colour = "grey", fill = "grey98") +
  geom_polygon(aes(group = group), colour = "grey", fill = "white") +
  coord_map(projection = "mercator", xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  labs(x = "", y = "") +
  geom_point(data = filter(df.projRecvs, !is.na(latitude), !is.na(deviceID)), aes(longitude, latitude, colour = as.factor(name)), size = 2, shape = 1)+
  scale_colour_discrete(name = "Receiver") 

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
 
 
 # just wanted to test if that worked
 #test_df<-subset(test,test$probability ==1)
 #test_df0<-subset(test,test$probability ==0)
 write.csv(df.alltags.sub_year, "./df.alltags_filter_run3_2023.csv", row.names = F)
# 
# tbl_strict <- filterByActivity(sql.motus, minLen = 4, maxLen = 10,
#                                maxRuns = 50, ratio = 0.75, return = "all")

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

# just wanted to test if that worked
#test_df<-subset(test,test$probability ==1)
#test_df0<-subset(test,test$probability ==0)
write.csv(df.alltags.sub, "./df.alltags_nov_2021.csv")

df.alltags.sub_year <- df.alltags.sub %>%
  mutate(ts = as_datetime(ts, tz = "UTC"), # convert ts to POSIXct format
         year = year(ts), # extract year from ts
         doy = yday(ts)) %>% # extract numeric day of year from ts
  filter(!is.na(recvDeployLat))

# does the dataset not have GPS data?
# because I am running into issues

# Problem with `mutate()` input `gpsID`.
# x can only subtract from "POSIXt" objects
# i Input `gpsID` is `purrr::map_int(...)`.
# Backtrace:
#   1. motus::getGPS(sql.motus, data = df.alltags.sub, by = "closest")
# 14. base::.handleSimpleError(...)
# 15. dplyr:::h(simpleError(msg, call))
# Run `rlang::last_trace()` to see the full context.

# Retrieve GPS data for each hitID
gps_index <- getGPS(sql.motus, data = df.alltags.sub, by = "closest")

# Merge GPS points in with our data
df.alltags.sub <- left_join(df.alltags.sub, gps_index, by = "hitID")



# the next code did not work:
# Error: Can't subset columns that don't exist.
# x Column `gpsLat` doesn't exist.
# Run `rlang::last_error()` to see where the error occurred.

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

saveRDS(df.alltags, "./df_alltags_#280_2Apr2021.rds")  

saveRDS(df.alltags, "./df_alltags_#280_nov2021.rds") 

# and subsetted dataset with relaxed filtering

saveRDS(df.alltags.sub, "./df_alltags_relaxed#280_13Jul2020.rds")

saveRDS(df.alltags.sub, "./df_alltags_relaxed#280_Jan2022.rds")


write.csv(df.alltags.sub, "./df_alltags_sub_Jan_2022.csv")

write.csv(df.alltags, "./df_alltags_second_output_nov_2021.csv")

df.alltags<-read.csv("./october_2020/new/df_alltags#280_nov2020.csv")

# reads in your file "df.alltags.rds" saved in the data folder
#df.alltags.saved <- readRDS("./df_alltags#280_02Dec2019.rds") 

###############################################################################################
###############################################################################################
