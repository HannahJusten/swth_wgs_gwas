########################################################################################################################################################################
########################################################################################################################################################################
#
# Script 1 for Lig files - geolight
# Based on Kira Delmore's geolight script
# This script provides a quick first look at the data. 
#
########################################################################################################################################################################
########################################################################################################################################################################

# Load packages

#install.packages("devtools")

library(devtools)
library(FLightR)
library(plyr)
#devtools::install_github("SWotherspoon/SGAT")
#devtools::install_github("SWotherspoon/BAStag")
#devtools::install_github("SLisovski/TwGeos")
library(TwGeos)
library(maps)
library(labeling)
library(grid)
#install.packages("readxl")
library(readxl)
library(dplyr)
library(tidyr)
#install.packages("lubridate")
library(lubridate)
#devtools::install_github("SLisovski/GeoLocTools")
library(GeoLocTools)
#devtools::install_github("SLisovski/GeoLight")
library(GeoLight)

setupGeolocation()

########################################################################################################################################################################

# directories

setwd("C:/Users/hcjusten/Dropbox/PhD/Thesis/Chapter_2/Analysis/Geolocator/")

out.dir <- "C:/Users/hcjusten/Dropbox/PhD/Thesis/Chapter_2/Analysis/Geolocator/output/"

#dir.create(file.path(out.dir,"Step1.2_geolight_cL_output_new_2022"),showWarnings = FALSE)

out.dir2<-"C:/Users/hcjusten/Dropbox/PhD/Thesis/Chapter_2/Analysis/Geolocator/output/Step1.2_geolight_cL_output_new_2022"
########################################################################################################################################################################

#functions

########################################################################################################################################################################

readLig <- function(file,skip=0) {
  ## Read csv file and add column names
  d <- read.csv(file,header=FALSE,skip=skip,
                col.names=c("Valid","Date","Julian","Light"),
                colClasses=c("character","character","numeric","integer"))
  ## Parse date
  d$Date <- as.POSIXct(strptime(d$Date,"%d/%m/%y %H:%M:%S",tz="GMT"))
  d
}

par(mar=c(1,1,1,1))

########################################################################################################################################################################

# Load metadata 

### template for which columns are needed is available on the delmore_lab github /SWTH/geolocators_Hannah 

#meta_data<- read.csv("C:/Users/hcjusten/Dropbox/PhD/Thesis/Chapter_2/Analysis/Geolocator/meta_data/metadata_recapture_2020.csv")
#meta_data<- read.csv("C:/Users/hcjusten/Dropbox/PhD/Thesis/Chapter_2/Analysis/Geolocator/meta_data/metadata_recapture_cL.csv")
meta_data<- read.csv("C:/Users/hcjusten/Dropbox/PhD/Thesis/Chapter_2/Analysis/Geolocator/meta_data/metadata_recapture_all_2.csv")
# determine light data directory

### Path to directory with light (.lux/.lig) files

#lux_path <- "C:/Users/hcjusten/Dropbox/PhD/Thesis/Chapter_2/Analysis/Geolocator/light_data_2020"

lux_path <- "C:/Users/hcjusten/Dropbox/PhD/Thesis/Chapter_2/Analysis/Geolocator/light_data"

########################################################################################################################################################################

# Loop through all birds in the metadata spreadsheet

for (i in 1:nrow(meta_data)) {

  id <- meta_data$Geo[i]
  print(id)
  
  lux_file <- meta_data$lux_file[i]
  if (is.na(lux_file) || (!is.na(meta_data$skip[i]) && meta_data$skip[i]=="Y")) {
    print("skipping")
    next
  }
  
  fn <- file.path(lux_path,
                      lux_file)
  
  row_id <- i

# create a subdirectory per geo with the output files

    #dir.create(file.path(out.dir,"Step1_geolight_output/",id),showWarnings = FALSE)
  dir.create(file.path(out.dir2,"/",id),showWarnings = FALSE)
  
# Load light data into R depending on the file format
    
  if (meta_data$lux_format[i] == "lig"){
    dat <- readLig(fn)
    dat$datetime <- as.POSIXct(strptime(dat$Date, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"))
    dat$light<-dat$Light
    } else {
        dat<-luxTrans(fn)
    }
  
  # if (meta_data$lux_format[i] == "lig"){
  #   d.lux <- readLig(fn)
  #   d.lux$Date <- as.POSIXct(strptime(d.lux$Date, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"))
  #   d.lux<-data.frame(d.lux$Date,d.lux$Light)
  #   names(d.lux)[names(d.lux)=="d.lux.Date"] <- "Date"
  #   names(d.lux)[names(d.lux)=="d.lux.Light"] <- "Light"
  #   
  # } else {
  #   d.lux<-luxTrans(fn)
  #   names(d.lux)[names(d.lux)=="datetime"] <- "Date"
  #   names(d.lux)[names(d.lux)=="light"] <- "Light"
  # }
  
# Load deployment dates
    
  start_date = as.Date(meta_data$date_apply[row_id])
  end_date = as.Date(meta_data$date_remove[row_id])
  dat = subset(dat, dat$datetime > as.POSIXct(start_date) & dat$datetime < as.POSIXct(end_date))

  night_filter = lightFilter(dat$light,baseline=0,iter=4)
  dat$light = night_filter

########################################################################################################################################################################
  
# Calculate twilights
  # treshold = 1.5 is good for SWTH
  
dat_transitions<-twilightCalc(dat$datetime,
                              dat$light,
                              LightThreshold=1.5,    # Here is where you set the threshold level
                              ask=FALSE) 

twl<-dat_transitions

## filter outlier twilight events

pdf(paste(out.dir2,"/",id,"/loess_filter_",id,".pdf", sep=""))
loess_filter=loessFilter(twl$tFirst,twl$tSecond,twl$type,k=4,plot=T)
dev.off()

twl = cbind (loess_filter,twl)
twl = subset(twl,twl$loess_filter!="FALSE")
twl = twl[2:4]

########################################################################################################################################################################

# Calibrate data

pdf(paste(out.dir2,"/",id,"/",id,"_calib.pdf",sep=""))
par(mar=c(1,1,1,1))
plot.new()
start_calib <- as.Date (start_date + 2)
end_calib <- as.Date(start_calib + 14)

#start_calib <- as.Date (start_date + 2)
#end_calib <- as.Date(meta_data$calib_date_apply_1[i])

if (meta_data$calib_date_remove_1[i] == "NA"){
#if (meta_data$calib_date_remove_1[i] == NA){  
  calib_dat = subset (dat, dat$datetime > as.POSIXct(start_calib) & dat$datetime < as.POSIXct(end_calib))

  } else{

  start_calib2 <- as.Date (meta_data$calib_date_remove_1[i])
  end_calib2 <- as.Date(end_date -1)

  calib_dat = subset(dat, (dat$datetime > as.POSIXct(start_calib) & dat$datetime < as.POSIXct(end_calib))| (dat$datetime > as.POSIXct(start_calib2) & dat$datetime < as.POSIXct(end_calib2)))

  }

calib_twl <- twilightCalc(calib_dat$datetime, calib_dat$light, LightThreshold = 1.5, ask = F)
calib_twl$tFirst <- as.POSIXct(calib_twl$tFirst, tz = "GMT")
calib_twl$tSecond <- as.POSIXct(calib_twl$tSecond, tz = "GMT")
lon.calib <- meta_data$long_remove[row_id]
lat.calib <- meta_data$lat_remove[row_id]
angle <- getElevation(calib_twl, known.coord = c(lon.calib, lat.calib),plot=T)[1]

#angle <- getElevation(calib_twl,known.coord=c(lon.calib,lat.calib), plot=F)
#problem with lnorm.pars
dev.off()

########################################################################################################################################################################

## first location estimates
# tol is new to this version of GeoLight

crds0 <- coord(twl, degElevation = 90-angle, tol = 0)

#crds0 <- coord(twl, degElevation = angle, tol = 0)

crds1 <- coord(twl, degElevation = 90-angle, tol = 0.13)

#crds1 <- coord(twl, degElevation = angle, tol = 0.13)

#crds1 <- coord(twl, degElevation = 90-angle, tol = 0.8) # higher values had the same result

pdf(paste(out.dir2,"/",id,"/",id,"_twl_tol.pdf",sep=""))
plot(twl[,1], crds0[,2], type = "o", pch = 16, col = "firebrick", xlab = "Time", ylab = "Latitude")
points(twl[,1], crds1[,2], type = "o", pch = 16, col = "cornflowerblue")
abline(v = as.POSIXct("2016-09-21"), lty = 2)
legend("bottomleft", c("equinox", "tol = 0", "tol = 0.13"), pch = c(NA, 16, 16), lty = c(2,1,1),
       col = c("black", "firebrick", "cornflowerblue"))
dev.off()

########################################################################################################################################################################

# changeLight

pdf(paste(out.dir2,"/",id,"/",id,"_cL.pdf",sep=""))
cL <- changeLight(twl$tFirst, twl$tSecond, type = twl$type, quantile=0.95, summary = F, days = 2)
dev.off()

write.csv(cL$migTable, file=paste(out.dir2,"/",id,"/",id,"_schedule_cL.csv",sep=""))

########################################################################################################################################################################

# siteMap

pdf(paste(out.dir2,"/",id,"/",id,"_siteMap.pdf",sep=""))
siteMap(crds = crds1, site = cL$site, xlim = c(-140, -30), ylim = c(-40, 70))
dev.off()

#siteMap(crds = crds1, site = cL$site, xlim = c(-130, -10), ylim = c(-40, 60)) # makes it worse

save.image(paste(out.dir2,"/",id,"/",id,"_workspace_geolight.RData",sep=""))

rm(mS,twl,dat,dat_transitions,calib_dat,calib_twl,cL,crds0,crds1,end_calib2,start_calib2, x,xy, angle, end_calib, end_date, fn,i,id, night_filter,lon.calib, loess_filter, lat.calib,lux_file,row_id, start_calib, start_date)

}

################################################################################
################################################################################
################################################################################

# rest if parts of old script; i am keeping it for now.


########################################################################################################################################################################

if (meta_data$mergeSites[i] == "y"){

# mS

  pdf(paste(out.dir2,"/",id,"/",id,"_sitemerge.pdf",sep=""))
  mS <- mergeSites(twl, site = cL$site, degElevation = angle, distThreshold = 300) ## how does mS calculate mean lat when there are no values

#mS <- mergeSites(twl, site = cL$site, degElevation = angle, distThreshold = 300) ## how does mS calculate mean lat when there are no values

#mS <- mergeSites(twl, site = cL$site, degElevation = 90-angle, distThreshold = 300) ## how does mS calculate mean lat when there are no values
  
  #devtools::install_github("SLisovski/GeoLight",  ref = "Update_2.01")
  #devtools::install_github("SLisovski/GeoLight")
  
  # mS <- mergeSites2(twl = twl, site = cL$site, 
  #                   distThreshold = 500, 
  #                   degElevation = angle,         # the HE corrected zero sun elevation angle
  #                   alpha = angle[3:4], method = "gamma", # parameters and model of the twilight errro
  #                   mask = "land")
  
  dev.off()

########################################################################################################################################################################

# siteMap with connecting points

  pdf(paste(out.dir2,"/",id,"/",id,"_sitemap_points_only.pdf",sep=""))
  siteMap(crds1, mS$site, type = "cross", hull = F, lwd = 4, cex = 2, xlim=c(-140,-30),ylim=c(-40,70))
  #points(mS$summary[,2:3], pch = 21, bg = "white", type = "b", lwd = 2, cex = 1.5)
  dev.off()

  #pdf(paste(out.dir2,"/",id,"/",id,"_sitemap_points.pdf",sep=""))
  #siteMap(crds1, mS$site, type = "cross", hull = F, lwd = 4, cex = 2, xlim=c(-140,-30),ylim=c(-40,70))
  ##points(mS$summary[,2:3], pch = 21, bg = "white", type = "b", lwd = 2, cex = 1.5)
  ##points(mS$summary[,2:3], pch = 21, bg = "white", type = "b", lwd = 2, cex = 1.5)
  #dev.off()


########################################################################################################################################################################

# movement schedule

  x <- schedule(twl$tFirst, twl$tSecond, site = mS$site)

  write.csv(x, file=paste(out.dir2,"/",id,"/",id,"_schedule.csv",sep=""))
  
  # test; extracting lat longs for merged sites
  
  xy<-data.frame(mS$summary)
  
  write.csv(xy, file=paste(out.dir2,"/",id,"/",id,"_lat_long_merged_sites.csv",sep=""))
  

  } else {

########################################################################################################################################################################

# raw data points

  pdf(paste(out.dir2,"/",id,"/",id,"_raw_data_points.pdf",sep=""))
  par(oma=c(5,0,0,0))
  map(xlim=c(-140,-30),ylim=c(-40,70),interior=F,col="darkgrey")
  map(xlim=c(-140,-30),ylim=c(-40,70),boundary=F,lty=2,col="darkgrey",add=T)
  mtext(c("Longitude (degrees)","Latitude (degrees)"),side=c(1,2),line=c(2.2,2.5),font=3)
  map.axes()
  points(crds1,cex=0.5,pch=20)
  dev.off()

}
########################################################################################################################################################################

pdf(paste(out.dir2,"/",id,"/",id,"_raw_data_points.pdf",sep=""))
par(oma=c(5,0,0,0))
map(xlim=c(-140,-30),ylim=c(-40,70),interior=F,col="darkgrey")
map(xlim=c(-140,-30),ylim=c(-40,70),boundary=F,lty=2,col="darkgrey",add=T)
mtext(c("Longitude (degrees)","Latitude (degrees)"),side=c(1,2),line=c(2.2,2.5),font=3)
map.axes()
points(crds1,cex=0.5,pch=20)
dev.off()

# save workspace

save.image(paste(out.dir2,"/",id,"/",id,"_workspace_geolight.RData",sep=""))

rm(mS,twl,dat,dat_transitions,calib_dat,calib_twl,cL,crds0,crds1,end_calib2,start_calib2, x,xy, angle, end_calib, end_date, fn,i,id, night_filter,lon.calib, loess_filter, lat.calib,lux_file,row_id, start_calib, start_date)

}
