# step 1 of Blackcap geolocation analysis -
# Initial processing of light data in preparation for geolocation analysis.
# Modified by Benjamin Van Doren (bmvandoren@gmail.com) from script by Kira Delmore
# Last updated 17 August 2019


# Load packages
#install.packages("devtools")
library(devtools)
library(FLightR)
library(plyr)
#devtools::install_github("SWotherspoon/SGAT")
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
library(GeoLight)
setupGeolocation()

########################################################################################################################################################################

# directories

setwd("C:/Users/hcjusten/Dropbox/PhD/Thesis/Chapter_2/Analysis/Geolocator/")

out.dir <- "C:/Users/hcjusten/Dropbox/PhD/Thesis/Chapter_2/Analysis/Geolocator/output/"


#dir.create(file.path(out.dir,"Step2_processing_twilights_output_new_2022"),showWarnings = FALSE)


out.dir2<-"C:/Users/hcjusten/Dropbox/PhD/Thesis/Chapter_2/Analysis/Geolocator/output/Step2_processing_twilights_output_new_2022"

########################################################################################################################################################################

### Load metadata ### did not modify this, will add data manually

### template for which columns are needed is available on the delmore_lab github /SWTH/geolocators_Hannah 

meta_data<- read.csv("C:/Users/hcjusten/Dropbox/PhD/Thesis/Chapter_2/Analysis/Geolocator/meta_data/metadata_recapture_all_2.csv")

### Path to directory with light (.lux/.lig) files

lux_path <- "C:/Users/hcjusten/Dropbox/PhD/Thesis/Chapter_2/Analysis/Geolocator/light_data"

do.manual = TRUE # preprocess manually with TwGeos::preprocessLight
edit.current = TRUE # If a twilight file exists, edit it? (as opposed to starting from scratch)
#do.manual=F
########################################################################################################################################################################

# Loop through all birds in the metadata spreadsheet
for (i in 1:nrow(meta_data)) {
#for (i in 40:nrow(meta_data)) {
 id <- meta_data$Geo[i]
  print(id)
  # not necessary
 lux_file <- meta_data$lux_file[i]
  # if (is.na(lux_file) || (!is.na(meta_data$skip[i]) && meta_data$skip[i]=="Y")) {
  #   print("skipping")
  #   next
  # }
  
 fn <- file.path(lux_path, lux_file)
  
row_id <- i

# create a subdirectory per geo with the output files

dir.create(file.path(out.dir,"Step2_processing_twilights_output_new_2022/",id),showWarnings = FALSE)

dir.create(file.path(out.dir,"Step2_processing_twilights_output_new_2022/",id,"/twl"),showWarnings = FALSE)

dir.create(file.path(out.dir,"Step2_processing_twilights_output_new_2022/",id,"/tags"),showWarnings = FALSE)

####################################################################################
  #setwd(paste("C:/Users/hanna/Dropbox/PhD/Field/2019/SWTH/Geolocators_2018/Recapture_2019/",id,"/Results/",sep=""))
 
 ## load raw data
  #d.lux <- readMTlux(fn)  
  #d.lux <- readMTlux("C:/Users/hanna/Dropbox/PhD/Field/2019/SWTH/Geolocators_2018/Recapture_2019/BP479/BP479_16Jul19_050023driftadj.lux")
  
  # Load light data into R depending on the file format
  
  if (meta_data$lux_format[i] == "lig"){
    d.lux <- readLig(fn)
    d.lux$Date <- as.POSIXct(strptime(d.lux$Date, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"))
    d.lux<-data.frame(d.lux$Date,d.lux$Light)
    names(d.lux)[names(d.lux)=="d.lux.Date"] <- "Date"
    names(d.lux)[names(d.lux)=="d.lux.Light"] <- "Light"
    
  } else {
    d.lux<-luxTrans(fn)
    names(d.lux)[names(d.lux)=="datetime"] <- "Date"
    names(d.lux)[names(d.lux)=="light"] <- "Light"
  }
  
  # remove pre and post deployment data
 
  start_date = as_date(meta_data$date_apply[row_id])+1 # add one day because date of application will include some false readings
  end_date = as_date(meta_data$date_remove[row_id])
  
  #start_date = as.Date("2018-07-22")+1 # add one day because date of application will include some false readings

  #end_date = as.Date("2019-06-06")
  
 # write.csv(d.lux, file="C:/Users/hanna/Dropbox/PhD/Field/2019/SWTH/Geolocators_2018/Recapture_2019/d.lux_BP460.csv")
 # in excel I filtered for light values higher than 60000 and found out when these values occured first, because i think the battery got low at that point and that might be why we see so high values
  # the day is may 21 2019 so I am subsetting the data set to everything before that

#end_date = as.Date ("2019-05-21")

  #how do I know that the device stopped earlier
   # manual trim (e.g. if logger stopped working early)
  start_date_trim = as_date(meta_data$start_date_trim[row_id])
  end_date_trim = as_date(meta_data$end_date_trim[row_id])
  start_date = max(start_date,start_date_trim,na.rm=T)
  end_date = min(end_date,end_date_trim,na.rm=T)
  
  nrow(d.lux)
  d.lux <- subset(d.lux, date(d.lux$Date) >= start_date
                  & d.lux$Date <= as_datetime(paste(end_date,"12:00:00")) # can use sunrise on morning of recapture
                  )
  nrow(d.lux)
  
  lon.calib <- meta_data$lon_apply[row_id]
  lat.calib <- meta_data$lat_apply[row_id]
 

  # Raw light plot with calibration site lines
  pdf(paste(out.dir,"Step2_processing_twilights_output_new_2022/",id,"/twl/",id,"_initial_lightImage.pdf", sep=""),width=10,height=6)
  offset<-8 # choose 8 to have light more in the middle, 20 would have the dark in the middle
  threshold <- 1.5
  TwGeos::lightImage(tagdata = d.lux,
             offset = offset,
             # zlim = c(0, 8),
             zlim = c(0,20),
             dt = 300)
  tsimageDeploymentLines(d.lux$Date, lon = lon.calib, lat = lat.calib,
                         offset = offset, lwd = 2, col = adjustcolor("orange", alpha.f = 0.7))
  TwGeos::lightImage(tagdata = d.lux,
                     offset = offset,
                     # zlim = c(0, 8),
                     zlim = c(0,150),
                     dt = 300)
  tsimageDeploymentLines(d.lux$Date, lon = lon.calib, lat = lat.calib,
                         offset = offset, lwd = 2, col = adjustcolor("orange", alpha.f = 0.7))
  dev.off()
  
  # plot of all light levels by day
  
  col = colorRampPalette(c('black',"purple",'orange'))(50)[as.numeric(cut(d.lux[2000:5000,2],breaks = 50))]
  
  plot_byday <- ggplot(d.lux,aes(x=Date,y=Light)) +
    geom_point() +
    geom_line() +
    scale_y_log10() +
    #scale_y_log10(oob = scales::squish_infinite) +  # that did not help
    scale_x_datetime(date_breaks = "1 day") +
    theme_bw()
  
  pdf(paste(out.dir,"Step2_processing_twilights_output_new_2022/",id,"/twl/",id,"_light_byday.pdf",sep=""),width=nrow(d.lux)/100,height=4)
    print(plot_byday)
  dev.off()
  
  # png(paste(out.dir,"Step2_processing_twilights_output/",id,"/twl/",id,"_light_byday.png",sep=""),width=nrow(d.lux)/100,height=4,unit="in",res=100)
  # print(plot_byday)
  # dev.off()
  
  if (do.manual) {
    # preprocess to define twilights
    old.twl <- if(edit.current) {
      old.fn <- paste(out.dir,"Step2_processing_twilights_output_new/",id,"/twl/",id,"_preproc_twl.rds", sep="")
      if (file.exists(old.fn)) { readRDS(old.fn) } else { NULL }
    } else { NULL }
    
    twl_manual <- TwGeos::preprocessLight(d.lux,
                           threshold,
                           offset,
                           lmax = 8,
                           twilights=old.twl, # edit previous run if it exists
                           gr.Device = "x11") # mac
                           # gr.Device = "default")
  }

  # automatically define twilights
  seed1 <- as.POSIXct(paste(as.character(start_date+5),"00:00:00"), tz = "GMT")
  seed2 <- as.POSIXct(paste(as.character(end_date-5),"00:00:00"), tz = "GMT")
  seed3 <- as.POSIXct(paste(as.character(as_date(mean(c(seed1,seed2)))),"00:00:00"), tz = "GMT")
  twl_orig  <- findTwilights(tagdata = d.lux,
                             threshold = threshold,
                             include = c(seed1,seed3,seed2))
  head(twl_orig)
 # twl_orig<-twl_manual
  # identity outliers automatically
  twl_auto <- try(twilightEdit(twilights = twl_orig,
                           offset = 7,
                      window = 4,           # two days before and two days after
                      outlier.mins = 30,    # difference in mins
                      stationary.mins = 15, # are the other surrounding twilights within 25 mins of one another
                      plot = TRUE))
  if (class(twl_auto)=="try-error") { 
    twl_auto=twl_orig
    twl_auto$Twilight0=twl_auto$Twilight
    twl_auto$Deleted=FALSE
    print("Note: twl_auto set to twl_orig because twilightEdit failed") 
  }
  
  if (do.manual) {
    # identify outliers FROM manually processed data
    # this will overwrite the Deleted column from the manual step, so need to account
    twl_manual_and_auto <- try(twilightEdit(twilights = twl_manual,
                                        offset = 7,
                             window = 4,           # two days before and two days after
                             outlier.mins = 30,    # difference in mins
                             stationary.mins = 15, # are the other surrounding twilights within 25 mins of one another
                             plot = TRUE))
    if (class(twl_manual_and_auto)=="try-error") { 
      twl_manual_and_auto=twl_manual
      twl_manual_and_auto$Twilight0=twl_manual_and_auto$Twilight3
      print("Note: twl_manual_and_auto set to twl_manual because twilightEdit failed") 
    } else {
      twl_manual_and_auto$Deleted = ifelse(twl_manual$Deleted & !twl_manual_and_auto$Edited,TRUE,twl_manual_and_auto$Deleted)
    }
  }
###################
    # I could not get the previous loop to run, 
  #Error in data.frame(Twilight = as.POSIXct(c(sunrT[, 1], sunsT[, 1]), origin = "1970-01-01",  : 
  #arguments imply differing number of rows: 71, 72
  # I am not sure where this data frame is formed to check the input files
  
  
  # plot the versions of edited data as a lightImage
    
    pdf(paste(out.dir,"Step2_processing_twilights_output_new_2022/",id,"/twl/",id,"_auto_twl_lightImage.pdf",sep=""),width=10,height=6)
    lightImage(tagdata = d.lux,
               offset = offset,
               zlim = c(0, 8),
               dt = 300)
    tsimagePoints(twl_auto$Twilight,
                  offset = offset,
                  pch = 16, 
                  cex = .6,
                  col = with(twl_auto,as.numeric(factor(
                    ifelse(Twilight==Twilight0,ifelse(!Deleted,Rise,"Deleted"),"Modified"),
                    levels=c("Deleted","TRUE","FALSE","Modified")))+1))
    dev.off()
    

    if (do.manual) {
      pdf(paste(out.dir,"Step2_processing_twilights_output_new_2022/",id,"/twl/",id,"_preproc_twl_lightImage.pdf",sep=""),width=12,height=6)
      lightImage(tagdata = d.lux,
                 offset = offset,
                 zlim = c(0, 8),
                 dt = 300)
      tsimagePoints(twl_manual$Twilight,
                    offset = offset,
                    pch = 16, 
                    cex = .6,
                    col = with(twl_manual,as.numeric(factor(
                        ifelse(Twilight==Twilight3,ifelse(!Deleted,Rise,"Deleted"),"Modified"),
                                                          levels=c("Deleted","TRUE","FALSE","Modified")))+1))
      dev.off()
      
      pdf(paste(out.dir,"Step2_processing_twilights_output_new/",id,"/twl/",id,"_preproc_and_auto_twl_lightImage.pdf",sep=""),width=12,height=6)
      lightImage(tagdata = d.lux,
                 offset = offset,
                 zlim = c(0, 8),
                 dt = 300)
      tsimagePoints(twl_manual_and_auto$Twilight,
                    offset = offset,
                    pch = 16, 
                    cex = .6,
                    col = with(twl_manual_and_auto,as.numeric(factor(
                      ifelse(Twilight==Twilight0,ifelse(!Deleted,Rise,"Deleted"),"Modified"),
                      levels=c("Deleted","TRUE","FALSE","Modified")))+1))
      dev.off()
    }
    
  # check for double twilights - if any exist, retain earlier dawn/later dusk 
# something is not right here, does not work
    
  remove.multiple.twilights <- function(twl) {
    add.deleted <- FALSE
    
  if (!"Deleted" %in% names(twl)) {
      twl$Deleted <- FALSE
      add.deleted <- TRUE
    } 
    dup.twilights <- twl %>% 
      filter(!Deleted) %>% 
      # filter({if("Deleted" %in% names(.)) Deleted else NULL} == FALSE) %>% 
      mutate(date=date(Twilight)) %>% 
      group_by(date,Rise) %>% 
      dplyr::summarise(n = n()) %>% 
      arrange(desc(n)) %>% 
      filter(n>1)
    if (nrow(dup.twilights)>0) {
      cat("multiple twilights on",
          paste(dup.twilights$date,collapse = ", "),
          "\n -",
          "retaining earlier dawn/later dusk\n")
      twl.dedup <- twl %>% mutate(date=date(Twilight)) %>% 
        ddply(c('date','Rise'),function(ddf) {
          ddf.del <- ddf %>% filter(Deleted)
          ddf.rest <- ddf %>% filter(!Deleted)
          if (nrow(ddf.rest)>1) {
            # browser()
            # if a sunrise, take the earlier one
            if (unique(ddf$Rise)) { 
              ddf.rest %>% arrange(Twilight) %>% dplyr::slice(1) %>% bind_rows(ddf.del)
            } else { # sunset, take later
              ddf.rest %>% arrange(desc(Twilight)) %>% dplyr::slice(1) %>% bind_rows(ddf.del)
            }
            
          } else {
            return(ddf)
          }
          
        }) %>% dplyr::select(-date)
      if (add.deleted) return(twl.dedup %>% dplyr::select(-Deleted))
      return(twl.dedup)
    } else {
      if (add.deleted) return(twl %>% dplyr::select(-Deleted))
      return(twl)
    }
    
  }
  
  # twl_auto <- readRDS(file=paste(out.dir,"Step2_processing_twilights_output/",id,"/twl/",id,"_auto_twl.rds",sep=""))
  # # twl_orig <- readRDS(file=paste(out.dir,"Step2_processing_twilights_output/",id,"/twl/",id,"_orig_twl.rds",sep=""))
  # twl_manual_and_auto <- readRDS(file=paste(out.dir,"Step2_processing_twilights_output/",id,"/twl/",id,"_preproc_and_auto_twl.rds",sep=""))
  # twl_manual <- readRDS(file=paste(out.dir,"Step2_processing_twilights_output/",id,"/twl/",id,"_preproc_twl.rds",sep=""))
  

  # remove any multiple twilights
  twl_auto <- remove.multiple.twilights(twl_auto)
  # twl_orig <- remove.multiple.twilights(twl_orig) # leave as original
  if (do.manual) {
  twl_manual_and_auto <- remove.multiple.twilights(twl_manual_and_auto)
  twl_manual <- remove.multiple.twilights(twl_manual)
  }
  
  # save twl (twilight) files
  
  saveRDS(twl_auto,file=paste(out.dir,"Step2_processing_twilights_output_new_2022/",id,"/twl/",id,"_auto_twl.rds", sep=""))
  saveRDS(twl_orig,file=paste(out.dir,"Step2_processing_twilights_output_new_2022/",id,"/twl/",id,"_orig_twl.rds", sep=""))
  if (do.manual) {
    saveRDS(twl_manual_and_auto,file=paste(out.dir,"Step2_processing_twilights_output_new_2022/",id,"/twl/",id,"_preproc_and_auto_twl.rds",sep=""))
    saveRDS(twl_manual,file=paste(out.dir,"Step2_processing_twilights_output_new_2022/",id,"/twl/",id,"_preproc_twl.rds",sep=""))
  }
  
  # saveRDS(twl_auto,file=paste(out.dir,"Step2_processing_twilights_output/",id,"/twl/",id,"_auto_twl.rds",sep=""))
  # saveRDS(twl_orig,file=pasteout.dir,"Step2_processing_twilights_output/",id,"/twl/",id,"_orig_twl.rds",sep=""))
  # if (do.manual) {
  #   saveRDS(twl_manual_and_auto,file=paste(out.dir,"Step2_processing_twilights_output/",id,"/twl/",id,"_preproc_and_auto_twl.rds",sep=""))
  #   saveRDS(twl_manual,file=paste(out.dir,"Step2_processing_twilights_output/",id,"/twl/",id,"_preproc_twl.rds",sep=""))
  # }
  
  # convert to TAGS format for FLR and save
  
  TAGS.twilights.auto.raw<-twGeos2TAGS(d.lux, twl_auto, threshold=threshold)
  write.csv(TAGS.twilights.auto.raw,file=paste(out.dir,"Step2_processing_twilights_output_new_2022/",id,"/tags/",id,"_tags_auto.csv",sep=""),row.names = F)
  
  TAGS.twilights.orig.raw<-twGeos2TAGS(d.lux, twl_orig %>% mutate(Deleted=FALSE,Edited=FALSE), threshold=threshold)
  write.csv(TAGS.twilights.orig.raw,file=paste(out.dir,"Step2_processing_twilights_output_new_2022/",id,"/tags/",id,"_tags_orig.csv",sep=""),row.names = F)
  
  if (do.manual) {
    TAGS.twilights.manual.raw<-twGeos2TAGS(d.lux, twl_manual, threshold=threshold)
    TAGS.twilights.manual.and.auto.raw<-twGeos2TAGS(d.lux, twl_manual_and_auto, threshold=threshold)
    write.csv(TAGS.twilights.manual.raw,file=paste(out.dir,"Step2_processing_twilights_output_new_2022/",id,"/tags/",id,"_tags_preproc.csv",sep=""),row.names = F)  
    write.csv(TAGS.twilights.manual.and.auto.raw,file=paste(out.dir,"Step2_processing_twilights_output_new_2022/",id,"/tags/",id,"_tags_preproc_and_auto.csv",sep=""),row.names = F)
  }
  rm(twl_auto, twl_manual, twl_manual_and_auto, twl_orig, lat.calib, lon.calib, d.lux, old.twl, TAGS.twilights.auto.raw, TAGS.twilights.manual.and.auto.raw, TAGS.twilights.manual.raw, TAGS.twilights.orig.raw, end_date, end_date_trim, id, start_date, start_date_trim, row_id, fn, plot_byday)
}
 
