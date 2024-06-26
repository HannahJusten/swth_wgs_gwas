#!/usr/bin/env Rscript

args = commandArgs(trailingOnly=TRUE)
prefix=args[1]

#module load iccifort/2019.5.281  impi/2018.5.288 R/4.1.0

path="/scratch/user/justen/stitch/full_stitch/"

setwd(paste(path,prefix,"_fuller_dir/output/ld_out/",sep=","))

files_pattern <- list.files()
files<-grep("_5_ld",files_pattern,value=TRUE)

results1<-data.frame()

for (i in seq_along(files)) {
  
  #data = read.table(files[i], header = T)
  data = read.csv(files[i], header = T)  
  
  dat1=head(data,1)
  dat2=tail(data,1)
  dat.i<-data.frame(dat1$chr,dat1$pos1,dat1$pos2,dat2$pos2)
  results1<-rbind(results1,dat.i)
}

write.csv(results1,"results_5gaps.csv", row.names=F)
