
library(motus)
library(tidyverse)
library(ggmap)
library(lubridate)
library(geosphere)
library("rgeos")
library(plyr)
library(sp)
library(rgdal)     # R wrapper around GDAL/OGR
library(ggplot2)   # for general plotting
library(ggmaps) 
library(raster)

proj.num <- 280

Sys.setenv(TZ = "UTC")

library(ggmap)

register_google(>insert API code>)

gmap <-  get_stamenmap(bbox = c(left = -140, right = -60, bottom = -10, top = 60),
                       maptype = "terrain-background", # select maptype
                       zoom = 3) # zoom, must be a whole number

quantNorm =function(x){qnorm(rank(x,ties.method = "average")/(length(x)+1))}

setwd("C:/Users/hcjusten/Dropbox/PhD/Thesis/Chapter_2/Analysis/Motus/data/july2_2021/before_manual")


files=list.files(pattern= ".csv$") # list all files that are shapefiles

#files=list.files(pattern= "5[:print:].csv") # list all files that are shapefiles

#files=list.files(str_extract("5.\\.csv"))
df_new<-data.frame()

  
for(i in 1:length(files)){ 

  df.i <- read.csv(files[i])
  
  df.sub<-df.i[!is.na(df.i$migratory_leg),]
  
  id=gsub("_.*$","",files[i])
    
  df_new<-rbind(df_new,df.sub)
  
}


  ggmap(gmap) +
    theme_bw() +
    geom_point(data = df_new, aes(x = tagDepLon, y = tagDepLat),
           shape = 21, colour = "black", fill = "black") +
    geom_point(data = df_new, aes(x = recvDeployLon, y = recvDeployLat),
           shape = 21, colour = "black", fill = "yellow") +
    geom_path(data = df_new,
          aes(x = recvDeployLon, y = recvDeployLat, group = motusTagID, col = "red"))

  # release site

  x<--122.885346
  y<-50.2199868

  spdf<-data.frame(x,y)
  
  coords<-spdf[,c("x","y")]
  
  #crs<-CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs") 
  
  crs<-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") 
  
  spdf2 <- SpatialPointsDataFrame(coords      = coords,
                                 data        = spdf, 
                                 proj4string = crs)
  
 # proj4string(spdf2)<-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  projection<-"+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"
  
  spdf2 = spTransform(spdf2,CRS(projection))
  
  df.buff1<-rgeos::gBuffer(spdf2,width=300000)
  
 
  #projection<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  #e<-extent(c(-140,-40,-40,70))
  e<-extent(c(-2100000,2800000,-3500000,1000000))
  
  #e<-extent(c(-3100000,3800000,-4500000,2000000))
  
  world2<-rgdal::readOGR("C:/Users/hcjusten/Documents/delmore_lab/general_resources/admin_layers/world_adm0.shp")
  
  #projection<-"+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs" 
  
  world2_proj<-spTransform(world2, CRS=CRS(projection))
  
  world2_e<-crop(world2_proj,e)
  
  plot(world2_e)
 # plot(world2_proj)
  #plot(spdf2,add=T)
  plot(df.buff1, add=T)
 
  df_new_detect<-subset(df_new,df_new$recvDeployName!="deployment_loc")
  
  original<-df_new_detect

  #spring:
  #spring1<-subset(df_new_detect,df_new_detect$migratory_leg=="spring1")
  #fall<-spring1

  #fall:
  
  fall<-subset(df_new_detect,df_new_detect$migratory_leg!="spring1")
  
  fall$migratory_leg<-gsub("fall1 ","fall1",fall$migratory_leg)
  
  
  coords2<-fall[,c("recvDeployLon","recvDeployLat")]
  
  crs<-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") 
  
  spdf_detect <- SpatialPointsDataFrame(coords      = coords2,
                                  data        = fall, 
                                  proj4string = crs)
  
  # proj4string(spdf2)<-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  spdf2_detect = spTransform(spdf_detect,CRS(projection))
  
  
  plot(spdf2_detect,add=T)
  
 df1<-spdf2_detect[!is.na(over(spdf2_detect,df.buff1)),]

 df1_ready<-df1[!duplicated(df1$motusTagID),]
 

 #now do bearings from these dataframes    
 plot(df1, add=T,col="red")
 plot(df1_ready, add=T,col="red")
 
 # convert bact to normal data frame
 
 df1_df<-as.data.frame(df1_ready)
 
 ################################################################################################
 
 # pulling out timing of these individuals
 
 test1<-df1_df[!is.na(df1_df$doy),]
 
 qnorm_doy<-quantNorm(test1$doy)
 
 test1$qnorm_doy_fall_r1 <-qnorm_doy

 tes1_sub<-data.frame(test1$motusTagID, test1$qnorm_doy_fall_r1,test1$doy)
 names(tes1_sub)=c("motusTagID","qnorm_doy_fall_r1","doy_fall_r1")
 
 df1_time<-join(df1_df,tes1_sub, by="motusTagID")
 
 rm(test1)

 order<-read.csv("C:/Users/hcjusten/Dropbox/PhD/Thesis/Chapter_2/Analysis/GWAS/meta_data/final/combo_order.csv")
 
 #get bearings for each location for each ring separately
 
 df1_df<-df1_df[!is.na(df1_df$recvDeployLat),]
 
 results<-data.frame()
 
 for (i in 1:nrow(df1_df)) {
   #for (i in 1:23) {
   
   #for (i in 1:2) { 
   id <- df1_df$motusTagID[i]
   print(id)
   
   bearing_fall_1 = bearingRhumb(
     p1 = cbind(df1_df$tagDepLon[i], df1_df$tagDepLat[i]),
     p2 = cbind(df1_df$recvDeployLon[i], df1_df$recvDeployLat[i])
   ) %% 360
   
   result_i<-data.frame(id,bearing_fall_1)
   results<-rbind(results,result_i)
   
   rm(bearing_fall_1,id,result_i)
 }
 
 fall_bear1<-results

 fall_bear$dist<-rep(1,nrow(fall_bear))
 
 ggplot(data=fall_bear,
        aes(bearing_fall_1,dist))+
   geom_segment(aes(xend = bearing_fall_1, yend = 0.1)) +
   geom_point() +
   scale_x_continuous(limits = c(-180, 180),
                      breaks =  seq(-180, 180, 90)) +
   # scale_y_log10() +
   coord_polar(start = pi) +
   ggtitle(label="fall - secition 0-300km")+
   theme_bw()

 ####################################################################################################
 
 fa_bear1<-fall_bear[!is.na(fall_bear$bearing_fall_1),]
 
 qnorm_bear<-quantNorm(fa_bear1$bearing_fall_1)
 
 fa_bear1$qnorm_fall_bear_1 <- qnorm_bear
 
 fa_bear1<-data.frame(fa_bear1$id, fa_bear1$qnorm_fall_bear_1)
 names(fa_bear1)=c("id","qnorm_fall_bear_1")
 
 write.csv(fa_bear1,"C:/Users/hcjusten/Dropbox/PhD/Thesis/Chapter_2/Analysis/GWAS/meta_data/radio_fall_bearing1_qnorm.csv",row.names=F)
 
 
