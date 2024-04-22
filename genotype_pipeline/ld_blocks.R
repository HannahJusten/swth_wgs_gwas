#!/usr/bin/env Rscript

args = commandArgs(trailingOnly=TRUE)
prefix=args[1]

## quick check of lmm 
#module load iccifort/2019.5.281  impi/2018.5.288 R/4.1.0

library(ggplot2)
library(splitstackshape)
library(plyr)
library(dplyr)

#prefix="distance_fall_corrected2"

#path="/scratch/user/justen/stitch/full_stitch/"

#setwd(paste(path,prefix,"_fuller_dir/output/ld_out/",sep=","))

files_pattern <- list.files()
files<-grep(".geno.ld",files_pattern,value=TRUE)

#############

for (i in seq_along(files)) {
  
  data = read.table(files[i], header = T)
  
  data=subset(data,select=-c(CHR2))
  
  cutoff <- data[,5] > 0.5 ## might change this
  data$center2 <- data$center <- ifelse(cutoff,1,0)
  
  data=data[!is.na(data$R.2),]
  
  #unique(data$POS1)
  
  ir<-data %>% group_by(POS1)
  
  df<-data.frame(group_keys(ir))
  
  for (s in 1:nrow(df)){
    
    test<-group_split(ir)[[s]]
    
    df_sub<-data.frame(test)
    ids<- df$POS1[s]
    
    #assign(paste("df",id, sep="_"),df_sub)
    
  ##data_test<-subset(data,data$R.2>0.5)
  
  #if (max(data[,5])>0.5){
  
  ## find SNPs in LD
  ## start by finding strings of 1s with at most two rows between
  
  jj <- 1
  while(jj < length(df_sub[,1])-3) {
    if (df_sub[jj,"center"] == 1){
      if (df_sub[jj+1,"center"] == 0 & df_sub[jj+2,"center"] == 0 & df_sub[jj+3,"center"] == 1){
        df_sub[jj+1,"center2"] <-  df_sub[jj+2,"center2"] <- 1
        jj <- jj + 3
        next
      } else if (df_sub[jj+1,"center"] == 0 & df_sub[jj+2,"center"] == 1){
       df_sub[jj+1,"center2"] <- 1
        jj <- jj + 2
        next
      }
    }
    jj <- jj + 1
    
  }

  #jj <- 1
  #while(jj < length(df_sub[,1])-3) {
  #  if (df_sub[jj,"center"] == 1){
  #    if (df_sub[jj+1,"center"] == 0 & df_sub[jj+2,"center"] == 0 & df_sub[jj+3,"center"] == 0 & df_sub[jj+4,"center"] == 0 & df_sub[jj+5,"center"] == 0 & df_sub[jj+6,"center"] == 1){
  #      df_sub[jj+1,"center2"] <-  df_sub[jj+2,"center2"] <- df_sub[jj+3,"center2"] <- df_sub[jj+4,"center2"] <- df_sub[jj+5,"center2"] <- 1
  #      jj <- jj + 6
  #      next
  #    } else if (df_sub[jj+1,"center"] == 0 & df_sub[jj+2,"center"] == 0 & df_sub[jj+3,"center"] == 0 & df_sub[jj+4,"center"] == 0 & df_sub[jj+5,"center"] == 1){
  #     df_sub[jj+1,"center2"] <-  df_sub[jj+2,"center2"] <- df_sub[jj+3,"center2"] <- df_sub[jj+4,"center2"] <- 1
  #      jj <- jj + 5
  #      next
  #    } else if (df_sub[jj+1,"center"] == 0 & df_sub[jj+2,"center"] == 0 & df_sub[jj+3,"center"] == 0 & df_sub[jj+4,"center"] == 1){
  #      df_sub[jj+1,"center2"] <-  df_sub[jj+2,"center2"] <-  df_sub[jj+3,"center2"] <- 1
  #      jj <- jj + 4
  #      next
  #    } else if (df_sub[jj+1,"center"] == 0 & df_sub[jj+2,"center"] == 0 & df_sub[jj+3,"center"] == 1){
  #      df_sub[jj+1,"center2"] <-  df_sub[jj+2,"center2"] <- 1
  #     jj <- jj + 3
  #      next
  #    } else if (df_sub[jj+1,"center"] == 0 & df_sub[jj+2,"center"] == 1){
  #      df_sub[jj+1,"center2"] <- 1
  #      jj <- jj + 2
  #      next
  #    }
  #  }
  #  jj <- jj + 1

  #}

  ## print string at the snp of interest
  win <- min.diff <- which.min(abs(df_sub[,2]-df_sub[,3]))
  rws <- win
  up <- down <- 1
  tt <- 2
  while(up || down){
    if(df_sub[win-up,7] == 1 && up) {
      rws[tt] <- win-up
      tt <- tt + 1
      up <- up + 1
    }  else {
      up <- 0
    }
    if(df_sub[win+down,7] == 1 && down) {
      rws[tt] <- win+down
      tt <- tt + 1
      down <- down + 1
    }  else {
      down <- 0
    }
  }
  
  extract <- df_sub[sort(rws),] ## print
  names(extract) = c("chr","pos1","pos2", "n_ind", "r2","center","center2")
  
 # write.table(extract,paste(path,prefix,"/output/",files[i],"_",ids,"_ld.csv",sep=""),quote=FALSE,sep=",",row.names=F,col.names=TRUE) ##could code this
#write.table(extract,paste(ids,"_5_ld.csv",sep=""),quote=FALSE,sep=",",row.names=F,col.names=TRUE) ##could code this

write.table(extract,paste("./results_plog6/",files[i],"_",ids,"_3_ld.csv",sep=""),quote=FALSE,sep=",",row.names=F,col.names=TRUE) ##could code this

}

}


