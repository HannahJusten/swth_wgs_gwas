
library(motus)
library(tidyverse)
library(ggmap)
library(lubridate)
library(geosphere)
library("rgeos")
library(plyr)

proj.num <- 280

Sys.setenv(TZ = "UTC")

library(ggmap)

register_google('AIzaSyAJgCcOqjLwCx5zuBAqP5OYaeYJsyDofhE')

gmap <-  get_stamenmap(bbox = c(left = -140, right = -60, bottom = -10, top = 60),
                       maptype = "terrain-background", # select maptype
                       zoom = 3) # zoom, must be a whole number

quantNorm =function(x){qnorm(rank(x,ties.method = "average")/(length(x)+1))}

setwd("C:/Users/hcjusten/Dropbox/PhD/Thesis/Chapter_2/Analysis/Motus/data/july2_2021/before_manual")


files=list.files(pattern= ".csv$") # list all files that are shapefiles

#files=list.files(pattern= "5[:print:].csv") # list all files that are shapefiles

#files=list.files(str_extract("5.\\.csv"))
df_new<-data.frame()

#for(i in seq_along(files)){
  
  
  #for(i in 274:length(files)){
  
for(i in 1:length(files)){ 

  df.i <- read.csv(files[i])
  
  df.sub<-df.i[!is.na(df.i$migratory_leg),]
  
  id=gsub("_.*$","",files[i])
# 
#   pdf(paste("C:/Users/hcjusten/Dropbox/PhD/Thesis/Chapter_2/Analysis/Motus/data/july2_2021/after_manual_plots/",id,"_track_after_manual_filter.pdf",sep=""))
#   print(ggmap(gmap) +
#     theme_bw() +
#     geom_point(data = df.sub, aes(x = tagDepLon, y = tagDepLat),
#            shape = 21, colour = "black", fill = "black") +
#     geom_point(data = df.sub, aes(x = recvDeployLon, y = recvDeployLat),
#            shape = 21, colour = "black", fill = "yellow") +
#     geom_path(data = df.sub,
#           aes(x = recvDeployLon, y = recvDeployLat, group = motusTagID, col = "red")))
#   dev.off()
    
  df_new<-rbind(df_new,df.sub)
  
}
  

#test<-subset(df_new, df_new$recvDeployLon>-75)
#test<-subset(test,test$recvDeployName!="deployment_loc")
#unique(test$motusTagID)

  #pdf(paste("C:/Users/hcjusten/Dropbox/PhD/Thesis/Chapter_2/Analysis/Motus/data/july2_2021/after_manual_plots/all_track_after_manual_filter.pdf",sep=""))  
  ggmap(gmap) +
    theme_bw() +
    geom_point(data = df_new, aes(x = tagDepLon, y = tagDepLat),
           shape = 21, colour = "black", fill = "black") +
    geom_point(data = df_new, aes(x = recvDeployLon, y = recvDeployLat),
           shape = 21, colour = "black", fill = "yellow") +
    geom_path(data = df_new,
          aes(x = recvDeployLon, y = recvDeployLat, group = motusTagID, col = "red"))
  #dev.off()
  
  x<--122.885346

  y<-50.2199868

  library(sp)
  
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
  
 # df.buff1<-rgeos::gBuffer(spdf2,width=500000)
  df.buff1<-rgeos::gBuffer(spdf2,width=300000)
  
  df.buff15<-rgeos::gBuffer(spdf2,width=1500000)
  df.buff25<-rgeos::gBuffer(spdf2,width=2500000)
  df.buff35<-rgeos::gBuffer(spdf2,width=3500000)
  df.buff55<-rgeos::gBuffer(spdf2,width=5000000)
  df.buff80<-rgeos::gBuffer(spdf2,width=8000000)

  #plot(df.buff)
  
  library(rgdal)     # R wrapper around GDAL/OGR
  library(ggplot2)   # for general plotting
  library(ggmaps) 
  library(raster)
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
  plot(df.buff15, add=T)
  plot(df.buff25, add=T)
  plot(df.buff35, add=T)
  plot(df.buff55, add=T)
  plot(df.buff80, add=T)
  #pdf("all_tracks_all_receivers_simple_back.pdf")
  
  
  df_new_detect<-subset(df_new,df_new$recvDeployName!="deployment_loc")
  
  original<-df_new_detect
  
  
  spring1<-subset(df_new_detect,df_new_detect$migratory_leg=="spring1")
  
  fall<-spring1
  
  #> unique(df_new_detect$migratory_leg)
  #[1] "fall1"   "spring1" "fall2"   "fall3"   "fall1 " 
  
  
  #Fall:
  
  fall<-subset(df_new_detect,df_new_detect$migratory_leg!="spring1")
  
  fall$migratory_leg<-gsub("fall1 ","fall1",fall$migratory_leg)
  
  
  coords2<-fall[,c("recvDeployLon","recvDeployLat")]
  
  #crs<-CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs") 
  
  crs<-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") 
  
  spdf_detect <- SpatialPointsDataFrame(coords      = coords2,
                                  data        = fall, 
                                  proj4string = crs)
  
  # proj4string(spdf2)<-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  spdf2_detect = spTransform(spdf_detect,CRS(projection))
  
  
  plot(spdf2_detect,add=T)
  
  

  res2 <- gDifference( df.buff15,df.buff1)
  
  res3 <- gDifference( df.buff25,df.buff15)
  
  res4 <- gDifference( df.buff35,df.buff25)
  
  res5<-gDifference( df.buff55,df.buff35)
  
  res6 <- gDifference( df.buff80,df.buff55)
  
  
 df1<-spdf2_detect[!is.na(over(spdf2_detect,df.buff1)),]
 
 df2<-spdf2_detect[!is.na(over(spdf2_detect,res2)),]
 
 df3<-spdf2_detect[!is.na(over(spdf2_detect,res3)),]
 
 df4<-spdf2_detect[!is.na(over(spdf2_detect,res4)),]
 
 df5<-spdf2_detect[!is.na(over(spdf2_detect,res5)),]
 
 df6<-spdf2_detect[!is.na(over(spdf2_detect,res6)),]
 
 
 # test visually if we got the right thing
 
 plot(world2_e)
 plot(res2, add=T, col="blue")
 plot(res3, add=T, col="red")
 plot(res4, add=T, col="yellow")
 plot(res5, add=T, col="green")
 plot(res6, add=T,col="red") 
 
 df1_ready<-df1[!duplicated(df1$motusTagID),]
 
 df2_ready<-df2[!duplicated(df2$motusTagID),]
 
 df3_ready<-df3[!duplicated(df3$motusTagID),]
 
 df4_ready<-df4[!duplicated(df4$motusTagID),]
 
 df5_ready<-df5[!duplicated(df5$motusTagID),]
 
 df6_ready<-df6[!duplicated(df6$motusTagID),]
 
 #now do bearings from these dataframes    
 plot(df1, add=T,col="red")
 plot(df2, add=T,col="blue")
 plot(df3, add=T,col="orange")
 plot(df4, add=T)
 plot(df5, add=T,col="green")
 
 
 plot(df1_ready, add=T,col="red")
 plot(df2_ready, add=T,col="blue")
 plot(df3_ready, add=T,col="orange")
 plot(df4_ready, add=T)
 plot(df5_ready, add=T,col="green")
 plot(df6_ready, add=T)
 
 
 # convert bact to normal data frame
 
 df1_df<-as.data.frame(df1_ready)
 
 df2_df<-as.data.frame(df2_ready)
 df3_df<-as.data.frame(df3_ready)
 df4_df<-as.data.frame(df4_ready)
 df5_df<-as.data.frame(df5_ready)
 df6_df<-as.data.frame(df6_ready)
 
 #df1_df<-as.data.frame(df1_ready)
 
 ################################################################################################
 
 # pulling out timing of these individuals
 
 test1<-df1_df[!is.na(df1_df$doy),]
 
 qnorm_doy<-quantNorm(test1$doy)
 
 test1$qnorm_doy_fall_r1 <-qnorm_doy

 
 tes1_sub<-data.frame(test1$motusTagID, test1$qnorm_doy_fall_r1,test1$doy)
 names(tes1_sub)=c("motusTagID","qnorm_doy_fall_r1","doy_fall_r1")
 
 df1_time<-join(df1_df,tes1_sub, by="motusTagID")
 
 rm(test1)
 
 
 test1<-df2_df[!is.na(df2_df$doy),]
 
 qnorm_doy<-quantNorm(test1$doy)
 
 test1$qnorm_doy_fall_r2 <-qnorm_doy
 
 
 tes2_sub<-data.frame(test1$motusTagID, test1$qnorm_doy_fall_r2,test1$doy)
 names(tes2_sub)=c("motusTagID","qnorm_doy_fall_r2","doy_fall_r2")
 
 df2_time<-join(df2_df,tes2_sub, by="motusTagID")
 
 rm(test1)
 
 test1<-df3_df[!is.na(df3_df$doy),]
 
 qnorm_doy<-quantNorm(test1$doy)
 
 test1$qnorm_doy_fall_r3 <-qnorm_doy
 
 
 tes3_sub<-data.frame(test1$motusTagID, test1$qnorm_doy_fall_r3,test1$doy)
 names(tes3_sub)=c("motusTagID","qnorm_doy_fall_r3","doy_fall_r3")
 
 df3_time<-join(df3_df,tes3_sub, by="motusTagID")
 
 
 rm(test1)
 
 test1<-df4_df[!is.na(df4_df$doy),]
 
 qnorm_doy<-quantNorm(test1$doy)
 
 test1$qnorm_doy_fall_r4 <-qnorm_doy
 
 
 tes4_sub<-data.frame(test1$motusTagID, test1$qnorm_doy_fall_r4,test1$doy)
 names(tes4_sub)=c("motusTagID","qnorm_doy_fall_r4","doy_fall_r4")
 
 df4_time<-join(df4_df,tes4_sub, by="motusTagID")
 
 
 rm(test1)
 
 test1<-df5_df[!is.na(df5_df$doy),]
 
 qnorm_doy<-quantNorm(test1$doy)
 
 test1$qnorm_doy_fall_r5 <-qnorm_doy
 
 
 tes5_sub<-data.frame(test1$motusTagID, test1$qnorm_doy_fall_r5,test1$doy)
 names(tes5_sub)=c("motusTagID","qnorm_doy_fall_r5","doy_fall_r5")
 
 df5_time<-join(df5_df,tes5_sub, by="motusTagID")
 
 rm(test1)
 
 test1<-df6_df[!is.na(df6_df$doy),]
 
 qnorm_doy<-quantNorm(test1$doy)
 
 test1$qnorm_doy_fall_r6 <-qnorm_doy
 
 
 tes6_sub<-data.frame(test1$motusTagID, test1$qnorm_doy_fall_r6,test1$doy)
 names(tes6_sub)=c("motusTagID","qnorm_doy_fall_r6","doy_fall_r6")
 
 df6_time<-join(df6_df,tes6_sub, by="motusTagID")
 
 
 #df_all_time<-merge(df1_time,df2_time,by="motusTagID")
 
 
 order<-read.csv("C:/Users/hcjusten/Dropbox/PhD/Thesis/Chapter_2/Analysis/GWAS/meta_data/final/combo_order.csv")
 
 
time_all<-merge(tes1_sub,tes2_sub,by="motusTagID",all.x=T,all.y=T)
 
time_all<-merge(time_all,tes3_sub,by="motusTagID",all.x=T,all.y=T)
 
time_all<-merge(time_all,tes4_sub,by="motusTagID",all.x=T,all.y=T)

time_all<-merge(time_all,tes5_sub,by="motusTagID",all.x=T,all.y=T)
 
time_all<-merge(time_all,tes6_sub,by="motusTagID",all.x=T,all.y=T)

write.csv(time_all, "C:/Users/hcjusten/Dropbox/PhD/Thesis/Chapter_2/Analysis/GWAS/meta_data/radio_timing_raw.csv", row.names=F)

write.csv(time_all, "C:/Users/hcjusten/Dropbox/PhD/Thesis/Chapter_2/Analysis/GWAS/meta_data/spring_ids_radio.csv", row.names=F)





 #get bearings for each location for each ring separately
 


 
 
 df1_df<-df1_df[!is.na(df1_df$recvDeployLat),]
 df1_df<- df1_df[!is.na( df1_df$tagDepLat),]
 
 df2_df<- df2_df[!is.na( df2_df$tagDepLat),]
 
 df3_df<- df3_df[!is.na( df3_df$tagDepLat),]
 
 df4_df<- df4_df[!is.na( df4_df$tagDepLat),]
 
 df5_df<- df5_df[!is.na( df5_df$tagDepLat),]
 
 df6_df<- df6_df[!is.na( df6_df$tagDepLat),]
 
 
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
  
 results<-data.frame()
   
 for (i in 1:nrow(df2_df)) {
   #for (i in 1:23) {
   
   #for (i in 1:2) { 
   id <- df2_df$motusTagID[i]
   print(id)
   
   bearing_fall_2 = bearingRhumb(
     p1 = cbind(df2_df$tagDepLon[i], df2_df$tagDepLat[i]),
     p2 = cbind(df2_df$recvDeployLon[i], df2_df$recvDeployLat[i])
   ) %% 360
   
   result_i<-data.frame(id,bearing_fall_2)
   results<-rbind(results,result_i)
   
   rm(bearing_fall_2,id,result_i)
 }
 
 fall_bear2<-results
 
 results<-data.frame()
 
 for (i in 1:nrow(df3_df)) {
   #for (i in 1:23) {
   
   #for (i in 1:2) { 
   id <- df3_df$motusTagID[i]
   print(id)
   
   bearing_fall_3 = bearingRhumb(
     p1 = cbind(df3_df$tagDepLon[i], df3_df$tagDepLat[i]),
     p2 = cbind(df3_df$recvDeployLon[i], df3_df$recvDeployLat[i])
   ) %% 360
   
   result_i<-data.frame(id,bearing_fall_3)
   results<-rbind(results,result_i)
   
   rm(bearing_fall_3,id,result_i)
 }
 
 fall_bear3<-results 
 
 results<-data.frame()
 
 for (i in 1:nrow(df4_df)) {
   #for (i in 1:23) {
   
   #for (i in 1:2) { 
   id <- df4_df$motusTagID[i]
   print(id)
   
   bearing_fall_4 = bearingRhumb(
     p1 = cbind(df4_df$tagDepLon[i], df4_df$tagDepLat[i]),
     p2 = cbind(df4_df$recvDeployLon[i], df4_df$recvDeployLat[i])
   ) %% 360
   
   result_i<-data.frame(id,bearing_fall_4)
   results<-rbind(results,result_i)
   
   rm(bearing_fall_4,id,result_i)
 }
 
 fall_bear4<-results
 
 results<-data.frame()
 
 for (i in 1:nrow(df5_df)) {
   #for (i in 1:23) {
   
   #for (i in 1:2) { 
   id <- df5_df$motusTagID[i]
   print(id)
   
   bearing_fall_5 = bearingRhumb(
     p1 = cbind(df5_df$tagDepLon[i], df5_df$tagDepLat[i]),
     p2 = cbind(df5_df$recvDeployLon[i], df5_df$recvDeployLat[i])
   ) %% 360
   
   result_i<-data.frame(id,bearing_fall_5)
   results<-rbind(results,result_i)
   
   rm(bearing_fall_5,id,result_i)
 }
 
 fall_bear5<-results
 
 results<-data.frame()
 
 for (i in 1:nrow(df6_df)) {
   #for (i in 1:23) {
   
   #for (i in 1:2) { 
   id <- df6_df$motusTagID[i]
   print(id)
   
   bearing_fall_6 = bearingRhumb(
     p1 = cbind(df6_df$tagDepLon[i], df6_df$tagDepLat[i]),
     p2 = cbind(df6_df$recvDeployLon[i], df6_df$recvDeployLat[i])
   ) %% 360
   
   result_i<-data.frame(id,bearing_fall_6)
   results<-rbind(results,result_i)
   
   rm(bearing_fall_6,id,result_i)
 }
 
 fall_bear6<-results
 
 fall_bear<-data.frame()
 
 fall_bear<-merge(fall_bear1,fall_bear2, by="id", all.x=T,all.y=T)
 fall_bear<-merge(fall_bear,fall_bear3,by="id", all.x=T,all.y=T) 
 fall_bear<-merge(fall_bear,fall_bear4,by="id", all.x=T,all.y=T)
 fall_bear<-merge(fall_bear,fall_bear5,by="id", all.x=T,all.y=T)
 fall_bear<-merge(fall_bear,fall_bear6,by="id", all.x=T,all.y=T)
 
 write.csv(fall_bear,"C:/Users/hcjusten/Dropbox/PhD/Thesis/Chapter_2/Analysis/GWAS/meta_data/radio_fall_bearing.csv",row.names=F)
 
 hist(fall_bear$bearing_fall_1)
 hist(fall_bear$bearing_fall_2)
 hist(fall_bear$bearing_fall_3)
 hist(fall_bear$bearing_fall_4)
 hist(fall_bear$bearing_fall_5)
 hist(fall_bear$bearing_fall_6)
 
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

 ggplot(data=fall_bear,
        aes(bearing_fall_2,dist))+
   geom_segment(aes(xend = bearing_fall_2, yend = 0.1)) +
   geom_point() +
   scale_x_continuous(limits = c(-180, 180),
                      breaks =  seq(-180, 180, 90)) +
   # scale_y_log10() +
   coord_polar(start = pi) +
   ggtitle(label="fall - secition 300-1500km")+
   theme_bw()
 
 ggplot(data=fall_bear,
        aes(bearing_fall_3,dist))+
   geom_segment(aes(xend = bearing_fall_3, yend = 0.1)) +
   geom_point() +
   scale_x_continuous(limits = c(-180, 180),
                      breaks =  seq(-180, 180, 90)) +
   # scale_y_log10() +
   coord_polar(start = pi) +
   ggtitle(label="fall - secition 1500-2500km")+
   theme_bw()
 
 ggplot(data=fall_bear,
        aes(bearing_fall_4,dist))+
   geom_segment(aes(xend = bearing_fall_4, yend = 0.1)) +
   geom_point() +
   scale_x_continuous(limits = c(-180, 180),
                      breaks =  seq(-180, 180, 90)) +
   # scale_y_log10() +
   coord_polar(start = pi) +
   ggtitle(label="fall - secition 2500-3500km")+
   theme_bw()
 
 ggplot(data=fall_bear,
        aes(bearing_fall_5,dist))+
   geom_segment(aes(xend = bearing_fall_5, yend = 0.1)) +
   geom_point() +
   scale_x_continuous(limits = c(-180, 180),
                      breaks =  seq(-180, 180, 90)) +
   # scale_y_log10() +
   coord_polar(start = pi) +
   ggtitle(label="fall - secition 3500-5000km")+
   theme_bw()
 
 ggplot(data=fall_bear,
        aes(bearing_fall_6,dist))+
   geom_segment(aes(xend = bearing_fall_6, yend = 0.1)) +
   geom_point() +
   scale_x_continuous(limits = c(-180, 180),
                      breaks =  seq(-180, 180, 90)) +
   # scale_y_log10() +
   coord_polar(start = pi) +
   ggtitle(label="fall - secition 5000-8000km")+
   theme_bw()

 ####################################################################################################
 
 
 
 
 fa_bear1<-fall_bear[!is.na(fall_bear$bearing_fall_1),]
 
 qnorm_bear<-quantNorm(fa_bear1$bearing_fall_1)
 
 fa_bear1$qnorm_fall_bear_1 <- qnorm_bear
 
 fa_bear1<-data.frame(fa_bear1$id, fa_bear1$qnorm_fall_bear_1)
 names(fa_bear1)=c("id","qnorm_fall_bear_1")
 
 
 
 fa_bear2<-fall_bear[!is.na(fall_bear$bearing_fall_2),]
 
 qnorm_bear<-quantNorm(fa_bear2$bearing_fall_2)
 
 fa_bear2$qnorm_fall_bear_2 <- qnorm_bear
 
 fa_bear2<-data.frame(fa_bear2$id, fa_bear2$qnorm_fall_bear_2)
 names(fa_bear2)=c("id","qnorm_fall_bear_2")
 
 
 fa_bear3<-fall_bear[!is.na(fall_bear$bearing_fall_3),]
 
 qnorm_bear<-quantNorm(fa_bear3$bearing_fall_3)
 
 fa_bear3$qnorm_fall_bear_3 <- qnorm_bear
 
 fa_bear3<-data.frame(fa_bear3$id, fa_bear3$qnorm_fall_bear_3)
 names(fa_bear3)=c("id","qnorm_fall_bear_3")
 
 
 
 fa_bear4<-fall_bear[!is.na(fall_bear$bearing_fall_4),]
 
 qnorm_bear<-quantNorm(fa_bear4$bearing_fall_4)
 
 fa_bear4$qnorm_fall_bear_4 <- qnorm_bear
 
 fa_bear4<-data.frame(fa_bear4$id, fa_bear4$qnorm_fall_bear_4)
 names(fa_bear4)=c("id","qnorm_fall_bear_4")

 
 
 fa_bear5<-fall_bear[!is.na(fall_bear$bearing_fall_5),]
 
 qnorm_bear<-quantNorm(fa_bear5$bearing_fall_5)
 
 fa_bear5$qnorm_fall_bear_5 <- qnorm_bear
 
 fa_bear5<-data.frame(fa_bear5$id, fa_bear5$qnorm_fall_bear_5)
 names(fa_bear5)=c("id","qnorm_fall_bear_5")
 
 
 fa_bear6<-fall_bear[!is.na(fall_bear$bearing_fall_6),]
 
 qnorm_bear<-quantNorm(fa_bear6$bearing_fall_6)
 
 fa_bear6$qnorm_fall_bear_6 <- qnorm_bear
 
 fa_bear6<-data.frame(fa_bear6$id, fa_bear6$qnorm_fall_bear_6)
 names(fa_bear6)=c("id","qnorm_fall_bear_6")
 
 fall_final<-data.frame()
 
#fall_final<- merge(fall_final,fa_bear1,by="id",all.x=T,all.y=T)
 
fall_final<-merge(fa_bear1,fa_bear2,by="id",all.x=T,all.y=T)
 
fall_final<-merge(fall_final,fa_bear3,by="id",all.x=T,all.y=T)

fall_final<-merge(fall_final,fa_bear4,by="id",all.x=T,all.y=T)

fall_final<-merge(fall_final,fa_bear5,by="id",all.x=T,all.y=T)

fall_final<-merge(fall_final,fa_bear6,by="id",all.x=T,all.y=T)

 #the correlation doesn't seem to work, probably because there is 


#library(corrgram)
 
 #test<-cor(fall_final)
 
 
 #cor(fall_final$qnorm_fall_bear_1,fall_final$qnorm_fall_bear_2)
 
 write.csv(fall_final,"C:/Users/hcjusten/Dropbox/PhD/Thesis/Chapter_2/Analysis/GWAS/meta_data/radio_fall_bearing_qnorm.csv",row.names=F)
 
 