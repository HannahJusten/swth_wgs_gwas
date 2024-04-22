library(tidyverse)
installlibrary(data.table)
library(ggplot2)
library(genomation) # I was not able to load all these libraries, so i found other programs to load gff files online

library(GenomicRanges)  # I was not able to load all these libraries, so i found other programs to load gff files online

library(rtracklayer)
library(stringr)
library(goseq)

################################################################################
# functions from this website:
##https://stat.ethz.ch/pipermail/bioconductor/2008-October/024669.html

getAttributeField <- function (x, field, attrsep = ";") {
  s = strsplit(x, split = attrsep, fixed = TRUE)
  sapply(s, function(atts) {
    a = strsplit(atts, split = "=", fixed = TRUE)
    m = match(field, sapply(a, "[", 1))
    if (!is.na(m)) {
      rv = a[[m]][2]
    }
    else {
      rv = as.character(NA)
    }
    return(rv)
  })
}

gffRead <- function(gffFile, nrows = -1) {
  cat("Reading ", gffFile, ": ", sep="")
  gff = read.table(gffFile, sep="\t", as.is=TRUE, quote="",
                   header=FALSE, comment.char="#", nrows = nrows,
                   colClasses=c("character", "character", "character", "integer",  
                                "integer",
                                "character", "character", "character", "character"))
  colnames(gff) = c("seqname", "source", "feature", "start", "end",
                    "score", "strand", "frame", "attributes")
  cat("found", nrow(gff), "rows with classes:",
      paste(sapply(gff, class), collapse=", "), "\n")
  stopifnot(!any(is.na(gff$start)), !any(is.na(gff$end)))
  return(gff)
}

################################################################################

## prep for pulling genes
gff <- gffToGRanges("/scratch/user/delmore/annotation/agat/GCF_009819885.2_bCatUst1.pri.v2_genomic_wfxnl.gff")

#read.delim("C:/Users/hcjusten/Downloads/fall_bear_archival_5_ldblock.gff", header=F, comment.char="#") -> gff

#gff.genes <- gff[gff[,2]=="gene",]

gff_rnas <- subset(gff,type %in% c("mRNA","guide_RNA","lnc_RNA","snoRNA","tRNA"))

files = list.files (pattern="para.csv")

for (i in seq_along(files)) {

tryCatch({

id = gsub("_para.csv","",files[i])

## load data and prep
dat <- read.csv(files[i],h=T)
dat$gamma_mod <- 1-dat$gamma
vars <- c("scaf","bp")
dat <- separate(dat,RS,into=vars,sep=":")
dat$bp <- as.numeric(dat$bp)

## summarize
nsnps <- 500
dat$mark=1+((1:nrow(dat))-1) %/% nsnps

# get product
dat_prod <- dat %>% 
  group_by(mark,scaf) %>% 
  summarise_at("gamma_mod", prod, na.rm = TRUE)

# get window information
dat_min <- dat %>% 
  group_by(mark,scaf) %>% 
  summarise_at("bp", min, na.rm = TRUE)

dat_max <- dat %>% 
  group_by(mark,scaf) %>% 
  summarise_at("bp", max, na.rm = TRUE)

dat_mean <- dat %>% 
  group_by(mark,scaf) %>% 
  mutate(meanpos = mean(bp)) %>%
  ungroup()

## combine
dat_mean <- dat_mean[,c(2,11,12)]
names(dat_mean) <- c("scaf","mark","meanpos")
setDT(dat_prod); setDT(dat_mean); setDT(dat_min); setDT(dat_max)
temp <- merge(dat_prod,dat_min)
temp2 <- merge(temp,dat_max)
names(temp2)<-c("mark","scaf","gamma_mod","start","end")
temp2$size <- temp2$end-temp2$start
combo <- dat_mean[temp2,mult = "first", on = "mark", nomatch=0L]
combo$pip_region <- 1-combo$gamma_mod

## write things out
write.table(combo,file=paste(id,"_",nsnps,"_snps.csv",sep=""),quote=FALSE,sep=",",row.names=TRUE,col.names=TRUE)
high <- subset(combo,combo$pip_region>0.01)
high$scaf2 <- high$scaf
high$scaf <- gsub("super_scaffold_1","NC_046221.1",high$scaf)
high$scaf <- gsub("scaffold_2_arrow_ctg1","NC_046222.1",high$scaf)
high$scaf <- gsub("super_scaffold_3","NC_046223.1",high$scaf)
high$scaf <- gsub("super_scaffold_5","NC_046224.1",high$scaf)
high$scaf <- gsub("scaffold_4_arrow_ctg1","NC_046225.1",high$scaf)
high$scaf <- gsub("scaffold_6_arrow_ctg1","NC_046226.1",high$scaf)
high$scaf <- gsub("scaffold_8_arrow_ctg1","NC_046227.1",high$scaf)
high$scaf <- gsub("scaffold_9_arrow_ctg1","NC_046228.1",high$scaf)
high$scaf <- gsub("scaffold_10_arrow_ctg1","NC_046229.1",high$scaf)
high$scaf <- gsub("scaffold_11_arrow_ctg1","NC_046230.1",high$scaf)
high$scaf <- gsub("scaffold_12_arrow_ctg1","NC_046231.1",high$scaf)
high$scaf <- gsub("scaffold_13_arrow_ctg1","NC_046232.1",high$scaf)
high$scaf <- gsub("super_scaffold_17","NC_046233.1",high$scaf)
high$scaf <- gsub("scaffold_14_arrow_ctg1","NC_046234.1",high$scaf)
high$scaf <- gsub("scaffold_16_arrow_ctg1","NC_046235.1",high$scaf)
high$scaf <- gsub("scaffold_18_arrow_ctg1","NC_046236.1",high$scaf)
high$scaf <- gsub("scaffold_19_arrow_ctg1","NC_046237.1",high$scaf)
high$scaf <- gsub("scaffold_20_arrow_ctg1","NC_046238.1",high$scaf)
high$scaf <- gsub("scaffold_22_arrow_ctg1","NC_046240.1",high$scaf)
high$scaf <- gsub("scaffold_23_arrow_ctg1","NC_046241.1",high$scaf)
high$scaf <- gsub("scaffold_24_arrow_ctg1","NC_046242.1",high$scaf)
high$scaf <- gsub("super_scaffold_35","NC_046243.1",high$scaf)
high$scaf <- gsub("scaffold_28_arrow_ctg1","NC_046244.1",high$scaf)
high$scaf <- gsub("scaffold_27_arrow_ctg1","NC_046245.1",high$scaf)
high$scaf <- gsub("scaffold_30_arrow_ctg1","NC_046246.1",high$scaf)
high$scaf <- gsub("scaffold_31_arrow_ctg1","NC_046247.1",high$scaf)
high$scaf <- gsub("super_scaffold_33","NC_046248.1",high$scaf)
high$scaf <- gsub("scaffold_32_arrow_ctg1","NC_046249.1",high$scaf)
high$scaf <- gsub("super_scaffold_38","NC_046250.1",high$scaf)
high$scaf <- gsub("super_scaffold_39","NC_046251.1",high$scaf)
high$scaf <- gsub("super_scaffold_46","NC_046252.1",high$scaf)
high$scaf <- gsub("super_scaffold_59","NC_046253.1",high$scaf)
high$scaf <- gsub("super_scaffold_45","NC_046254.1",high$scaf)
high$scaf <- gsub("super_scaffold_53","NC_046255.1",high$scaf)
high$scaf <- gsub("super_scaffold_55","NC_046256.1",high$scaf)
high$scaf <- gsub("super_scaffold_56","NC_046257.1",high$scaf)
high$scaf <- gsub("scaffold_50_arrow_ctg1","NC_046258.1",high$scaf)
high$scaf <- gsub("super_scaffold_58","NC_046259.1",high$scaf)
high$scaf <- gsub("scaffold_51_arrow_ctg1","NC_046260.1",high$scaf)
high$scaf <- gsub("super_scaffold_25_w","NC_046261.2",high$scaf)
high$scaf <- gsub("super_scaffold_7","NC_046262.2",high$scaf)
high$scaf <- gsub("scaffold_26_arrow_ctg1","NW_024879445.1",high$scaf)
high$scaf <- gsub("scaffold_29_arrow_ctg1","NW_024879446.1",high$scaf)
write.table(high[,c(1,6,7)],file=paste(id,"_",nsnps,"_snps_pip01.bed",sep=""),row.names = F,col.names = F, quote = F, sep="\t")

## plot
combo$scaf<-factor(combo$scaf,levels=c("super_scaffold_1","scaffold_2_arrow_ctg1","super_scaffold_3","super_scaffold_5","scaffold_4_arrow_ctg1","scaffold_6_arrow_ctg1","scaffold_8_arrow_ctg1","scaffold_9_arrow_ctg1","scaffold_10_arrow_ctg1","scaffold_11_arrow_ctg1","scaffold_12_arrow_ctg1","scaffold_13_arrow_ctg1","super_scaffold_17","scaffold_14_arrow_ctg1","scaffold_16_arrow_ctg1","scaffold_18_arrow_ctg1","scaffold_19_arrow_ctg1","scaffold_20_arrow_ctg1","scaffold_22_arrow_ctg1","scaffold_23_arrow_ctg1","scaffold_24_arrow_ctg1","super_scaffold_35","scaffold_28_arrow_ctg1","scaffold_27_arrow_ctg1","scaffold_30_arrow_ctg1","scaffold_31_arrow_ctg1","super_scaffold_33","scaffold_32_arrow_ctg1","super_scaffold_38","super_scaffold_39","super_scaffold_46","super_scaffold_59","super_scaffold_45","super_scaffold_53","super_scaffold_55","super_scaffold_56","scaffold_50_arrow_ctg1","super_scaffold_58","scaffold_51_arrow_ctg1","super_scaffold_25_w","super_scaffold_7","scaffold_26_arrow_ctg1","scaffold_29_arrow_ctg1"))
combo <- combo[order(combo$scaf,combo$meanpos),]
combo$id <- 1:nrow(combo)

chrom=unique(combo$scaf)
bpMidVec <- vector()

for (j in 1:length(chrom)){
  chrom.j <- chrom[j]
  ndx <- subset(combo, combo$scaf==chrom.j)
  posSub <- nrow(ndx)
  bpMidVec[j] <- posSub/2 + min(ndx$id)
}

g=ggplot(combo,aes(id,pip_region,colour=as.factor(scaf)))

pdf(paste(id,"_",nsnps,"_snps.pdf",sep=""),width=12,height=8)
print(ggplot(combo,aes(id,pip_region,colour=as.factor(scaf)))+
  geom_point()+
  labs(x="chromosome")+
  theme_bw(base_size=20)+
  theme(
    #axis.ticks.x = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    axis.title.x = element_text(vjust=-0.75),
    axis.title.y = element_text(vjust=2.5),
    legend.position="none",
    plot.margin = unit(c(2, 2, 2, 2), "cm"),
    panel.grid.major = element_blank(),
    panel.margin = unit(1, "lines"))+
  scale_x_continuous(labels=as.character(chrom), breaks=bpMidVec)+
  scale_color_manual(values=rep(c('#666666','#660099'),length(chrom))) +
  geom_hline(yintercept=0.01))
  #facet_grid(leg ~ ., scales="free")
dev.off()

## pull out genes

candidates <- readBed(paste(id,"_",nsnps,"_snps_pip01.bed",sep=""))
overlap_rnas <- subsetByOverlaps(gff_rnas,candidates)
overlap_rnas_df <- data.frame(overlap_rnas)
overlap_rnas_df_unique <- overlap_rnas_df[!duplicated(overlap_rnas_df$gene), ]
overlap_rnas_df_unique_subset <- subset(overlap_rnas_df_unique, select=c("seqnames","start","end","gene","Ontology_term"))
export(overlap_rnas_df_unique,paste(id,"_",nsnps,"_snps_pip01_genes.gff",sep=""),format='gff3')

}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
