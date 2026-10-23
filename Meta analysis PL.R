library("data.table")
library("tidyverse")
library("ggplot2")
library("ggpubr")
library("lubridate")
library("svMisc")
library("nlme")
library("ggpp")
library("ggpmisc")
library("tidyr")
library("car")
library("zoo")
library("stringr")
library("ggh4x")
library("meta")
library("estmeansd")

WD<-getwd() 
WD<-paste(WD,"/Assesing Lake Ice Strength Github/Data/",sep="")

Meta_data<-read.csv(paste(WD,"Table References/MetaDataAnalysisList.csv",sep=""))
Meta_data<-Meta_data%>%filter(Ice_Type=="Black Ice")
Meta_comp<-Meta_data[Meta_data$Test=="Compression",]
Meta_flex<-Meta_data[Meta_data$Test=="Flexural",]
Meta_frank<-read.csv(paste(WD,"Table References/Frankenstein data.csv",sep=""))


Compression_Meta<-metamean(n,
                          Average,
                          SD,
                          data = Meta_comp,
                          sm = "MRAW",
                          studlab = Study)


forest(Compression_Meta,
       layout="Revman5",
       sortvar=studlab,
       xlab="Compression Strength \n (MPa)",
       ff.xlab = "bold", fs.xlab = 12,
       leftcols=c("Study", "n", "mean", "sd","ci"),
       rightcols=FALSE,
       pooled.totals = TRUE,
       digits.mean = 2, digits.sd = 2,
       random = TRUE, fixed = FALSE,
       fs.heading = 16, fs.study = 16, fs.hetstat = 10,
       colgap = "5mm", colgap.forest = "5mm",
       col.square="#00bfc4", col.square.lines="black",
       col.diamond="maroon", col.diamond.lines="black",
       print.Q = TRUE, print.pval.Q = TRUE, print.tau.ci = TRUE, overall.hetstat=TRUE)

Flexural_Meta<-metamean(n,
                           Average,
                           SD,
                           data = Meta_flex,
                           sm = "MRAW",
                           studlab = Study)
forest(Flexural_Meta,
       layout="Revman5",
       sortvar=studlab,
       xlab="Flexural Strength \n (MPa)",
       ff.xlab = "bold", fs.xlab = 12,
       leftcols=c("Study", "n", "mean", "sd","ci"),
       rightcols=FALSE,
       pooled.totals = TRUE,
       digits.mean = 2, digits.sd = 2,
       random = TRUE, common = FALSE,
       fs.heading = 16, fs.study = 16, fs.hetstat = 10,
       colgap = "5mm", colgap.forest = "5mm",
       col.square="#00bfc4", col.square.lines="black",
       col.diamond="maroon", col.diamond.lines="black",
       print.Q = TRUE, print.pval.Q = TRUE, print.tau.ci = TRUE, overall.hetstat=TRUE)

Franken_Meta<-metamean(n,
                        Average,
                        std,
                        data = Meta_frank[Meta_frank$Lit=="Franken vol1",],
                        sm = "MRAW",
                        studlab = Date)

forest(Franken_Meta,
       layout="Revman5",
       sortvar=studlab,
       xlab="Flexural Strength \n (MPa)",
       ff.xlab = "bold", fs.xlab = 12,
       leftcols=c("Date", "n", "mean", "sd","ci"),
       rightcols=FALSE,
       pooled.totals = TRUE,
       digits.mean = 2, digits.sd = 2,
       random = TRUE, common = FALSE,
       fs.heading = 12, fs.study = 12, fs.hetstat = 10,
       colgap = "5mm", colgap.forest = "5mm",
       col.square="#00bfc4", col.square.lines="black",
       col.diamond="maroon", col.diamond.lines="black",
       print.Q = TRUE, print.pval.Q = TRUE, print.tau.ci = TRUE, overall.hetstat=TRUE)
summary(Franken_Meta)


Franken_Meta<-metamean(n,
                       Average,
                       std,
                       data = Meta_frank[Meta_frank$Lit=="Franken vol2",],
                       sm = "MRAW",
                       studlab = Date)

forest(Franken_Meta,
       layout="Revman5",
       sortvar=studlab,
       xlab="Flexural Strength \n (MPa)",
       ff.xlab = "bold", fs.xlab = 12,
       leftcols=c("Date", "n", "mean", "sd","ci"),
       rightcols=FALSE,
       pooled.totals = TRUE,
       digits.mean = 2, digits.sd = 2,
       random = TRUE, common = FALSE,
       fs.heading = 12, fs.study = 12, fs.hetstat = 10,
       colgap = "5mm", colgap.forest = "5mm",
       col.square="#00bfc4", col.square.lines="black",
       col.diamond="maroon", col.diamond.lines="black",
       print.Q = TRUE, print.pval.Q = TRUE, print.tau.ci = TRUE, overall.hetstat=TRUE)
summary(Franken_Meta)


Compression_Meta_nat<-metamean(n,
                           Average,
                           SD,
                           data = Meta_comp[Meta_comp$Ice_source=="Natural",],
                           sm = "MRAW",
                           studlab = Study)


forest(Compression_Meta_nat,
       layout="Revman5",
       sortvar=studlab,
       xlab="Compression Strength \n (MPa)",
       ff.xlab = "bold", fs.xlab = 12,
       leftcols=c("Study", "n", "mean", "sd","ci"),
       rightcols=FALSE,
       pooled.totals = TRUE,
       digits.mean = 2, digits.sd = 2,
       random = TRUE, fixed = FALSE,
       fs.heading = 12, fs.study = 12, fs.hetstat = 10,
       colgap = "5mm", colgap.forest = "5mm",
       col.square="#00bfc4", col.square.lines="black",
       col.diamond="maroon", col.diamond.lines="black",
       print.Q = TRUE, print.pval.Q = TRUE, print.tau.ci = TRUE, overall.hetstat=TRUE)
