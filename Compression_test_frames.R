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

WD<-getwd() 
WD<-paste(WD,"/Assesing Lake Ice Strength Github/Data/",sep="")

# FInding folders and names ####
Folders<-list.files(path = paste(WD,"Collection_Initial_Drop",sep=""), full.names = TRUE)
Names<-list.files(path = paste(WD,"Collection_Initial_Drop",sep=""), full.names = FALSE)

# Extract the first word (first name) ####
Legend <- word(Names, 1)
print(Legend)
Location<-word(Names,2)
Dates<-word(Names, 3)
Dates <- str_remove_all(Dates, ".txt")
print(Dates)


# Conbining all the dates to a frame ####
Compression_tests<-data.table()
for(i in 1:length(Folders)){
  
  Compression_frame<-data.table()
  file<-list.files()
  dat<-read.delim(file[i], header = TRUE, stringsAsFactors = TRUE,skip = 7)
  Compression_frame<-rbind(Compression_frame,dat)
  #Compression_frame$file<-Compression_frame[Names[i]]
  Compression_frame$Sample<-Compression_frame[,Legend[i]]
  Compression_frame$Location<-Compression_frame[,Location[i]]
  Compression_frame$Dates<-Compression_frame[,Dates[i]]
  Compression_tests<-rbind(Compression_tests,Compression_frame)
}


#### Plotting General overview of the data ####
Compression_tests$Sample_Dates<-paste(Compression_tests$Sample,Compression_tests$Location,Compression_tests$Dates)

uni_Location=unique(Compression_tests$Location)
for(i in 1:length(uni_Location)){
  Section<-Compression_tests[Location==uni_Location[i]]
  ggplot(Section)+geom_line(aes(x=sec,y=kN,color=Dates),size=1,alpha=0.5)+theme_bw()

  ggplot(Section)+geom_line(aes(x=mm,y=kN,color=Dates),size=1,alpha=0.5)+theme_bw()

  ggplot(Section)+geom_line(aes(x=sec,y=kN,color=Dates),size=1,alpha=0.5)+theme_bw()

  ggplot(Section)+geom_line(aes(x=mm,y=kN,color=Dates),size=1,alpha=0.5)+theme_bw()

}

uni_Sample<-unique(Compression_tests$Sample)
for(i in 1:length(uni_Sample)){
  Sample_section<-Compression_tests[Sample==uni_Sample[i]]
  uni_date<-unique(Sample_section$Dates)
  for(j in 1:length(uni_date)){
  Sample_Section_date<-Sample_section[Dates==uni_date[j]]
  uni_location<-unique(Sample_Section_date$Location)
    for (k in 1:length(uni_location)){
      Sample_Section_location<-Sample_Section_date[Location==uni_location[k]]
      Graph_name<-paste(unique(Sample_Section_location$Sample),unique(Sample_Section_location$Location),unique(Sample_Section_location$Date))
      ggplot(Sample_Section_location)+geom_line(aes(x=mm,y=kN,color=Dates),size=1,alpha=0.5)+theme_bw()+ggtitle(Graph_name)+
        geom_point(aes(x=Sample_Section_location[which.max(kN),mm],y=max(kN)),size=3)+theme(legend.position = "none")

        ggplot(Sample_Section_location)+geom_line(aes(x=sec,y=kN,color=Dates),size=1,alpha=0.5)+theme_bw()+ggtitle(Graph_name)+
        geom_point(aes(x=Sample_Section_location[which.max(kN),mm],y=max(kN)),size=3)+theme(legend.position = "none")
      }
   }
}

#### Extracting slopes and peaks for each sampling occassion ####

uni<-unique(Compression_tests$Sample)
Peaks_frame<-data.frame()
Peak_vector<-vector()
Sample_vector<-vector()
Slope_vector<-vector()
Accentuator<-vector()
Sec<-vector()
for(i in 1:length(uni)){
  Input<-Compression_tests[Sample==uni[i],]
  Date_uni<-unique(Input$Sample_Dates)
  for(j in 1:length(Date_uni)){
    Separator<-Input[Sample_Dates==Date_uni[j],]
      Near<-Separator[1:which.max(Separator$kN*0.7),] 
      Near_peak<-Near$kN[which.max(Near$kN*0.7)]
      Near_sec<-Near$sec[which.max(Near$kN*0.7)]
      Near <- na.omit(Near)
      Near_func<-lm(Near$kN ~ poly(Near$mm,3))
      Accent<-Near[length(Near$mm),mm]
      # Near_func2<-lm(log(Near$kN) ~ Near$mm)
      Start=length(Near$mm)/2
      End=which.min(abs(Near[Start,mm]+1-Near[,mm]))
      b<-predict(Near_func)
      lin<-lm(b[Start:End]~Near[Start:End,mm])
      slope<-coefficients(lin)[[2]]
      Graph_name<-unique(Near$Sample_Dates)
      
      ggplot()+geom_line(aes(x=Near$mm,y=Near$kN),size=1.5)+ggtitle(paste(unique(Near$Sample),unique(Near$Location),unique(Near$Dates),round(Near_peak,1),"kN"))+
      ylab("Pressure [kN]")+xlab("Accentuator [mm]")+theme_bw()+theme(element_text(size=14))+
        geom_line(aes(x=Near$mm,y=predict(Near_func),color="3rd polynomial"),size=1.5)+
        geom_line(aes(x=Near[Start:End,mm],y=b[Start:End],color="Fitted"),size=1.5)

      
        Peak_vector<-c(Peak_vector, Near_peak)
        Sample_vector<-c(Sample_vector, Date_uni[j])
        Slope_vector<-c(Slope_vector,slope)
        Accentuator<-c(Accentuator,Accent)
        Sec<-c(Sec,Near_sec)
        }
  
}


Peaks_frame<-cbind(Peak_vector,Slope_vector,Sample_vector,Accentuator)
Peaks_frame<-as.data.frame(Peaks_frame)
Bay<-word(Peaks_frame$Sample_vector,1)
Location<-word(Peaks_frame$Sample_vector,2)
Dates<-word(Peaks_frame$Sample_vector,3)
Peaks_frame<-cbind(Peaks_frame,Bay)
Peaks_frame<-cbind(Peaks_frame,Location)
Peaks_frame<-cbind(Peaks_frame,Dates)

#Extract data for manual addition and separating replicates from the rest
#write.csv(Peaks_frame,paste(WD,"Peaks_Extracted_R_Frame.csv",sep="))


#### Data converted to MPa for comparison between size classes of cubes ####

Peaks<-read.csv(paste(WD,"Peaks_Extracted.csv",sep=""))
Peaks<-as.data.frame(Peaks)
Peaks$date <- ymd(paste(Peaks$Year, Peaks$Month, Peaks$Day, sep = "-"))

Min_Area<-vector()
for (i in 1:length(Peaks$Peaks)){
  if(Peaks[i,"Top.Area"]>Peaks[i,"Bot.Area"]){Min_Area<-c(Min_Area,Peaks[i,"Bot.Area"])}else{
    Min_Area<-c(Min_Area,Peaks[i,"Top.Area"])
  }
}

Peaks<-cbind(Peaks,Min_Area)
Peaks<-as.data.table(Peaks)
Peaks$MPa<-Peaks$Peaks*1000/(Peaks$Min_Area*100)



uni<-unique(Compression_tests$Sample_Dates)
Sample_vector<-vector()
Slope_vector<-vector()

for(i in 1:length(uni)){
  Input<-Compression_tests[Sample_Dates==uni[i],]
  Area<-Peaks[Sample_vector==uni[i],]
  Input$MPa<-Input$kN*1000/(Area$Min_Area*100)
  
  # Fitting and extracting linear slope 
  Near<-Input[1:which.max(Input$MPa*0.7),]
  Near_func<-lm(Near$MPa ~ poly(Near$mm,3))
  Accent<-Near[length(Near$mm),mm]
  # Near_func2<-lm(log(Near$kN) ~ Near$mm)
  Start=length(Near$mm)/2
  End=which.min(abs(Near[Start,mm]+1-Near[,mm]))
  b<-predict(Near_func)
  lin<-lm(b[Start:End]~Near[Start:End,mm])
  slope<-coefficients(lin)[[2]]
  Graph_name<-unique(Near$Sample_Dates)
  
  ggplot()+geom_line(aes(x=Near$mm,y=Near$MPa),size=1.5)+ggtitle(paste(unique(Near$Sample),unique(Near$Location),unique(Near$Dates),round(Near_peak,1),"kN"))+
    ylab("Pressure [kN]")+xlab("Accentuator [mm]")+theme_bw()+theme(element_text(size=14))+
    geom_line(aes(x=Near$mm,y=predict(Near_func),color="3rd polynomial"),size=1.5)+
    geom_line(aes(x=Near[Start:End,mm],y=b[Start:End],color="Fitted"),size=1.5)

  Sample_vector<-c(Sample_vector, uni[i])
  Slope_vector<-c(Slope_vector,slope)
  
}

MPA_Slopes<-cbind(Slope_vector,Sample_vector)
#write.csv(MPA_Slopes,paste(WD,"MPA_Slopes.csv",sep="))
#rm(Peaks)

#### Analysis of MPA and Kn data after slope and peak detection and MPa conversion ####
Peaks<-read.csv(paste(WD,"Peaks_Extracted.csv",sep=""))
Peaks<-as.data.frame(Peaks)
Peaks$date <- ymd(paste(Peaks$Year, Peaks$Month, Peaks$Day, sep = "-"))

Peaks<-cbind(Peaks,Min_Area)
Peaks<-as.data.table(Peaks)
Peaks$MPa<-Peaks$Peaks*1000/(Peaks$Min_Area*100)
Peaks<-as.data.table(Peaks)

# A jumble of separating datasets
Peaks_BI<-Peaks[Type=="Black",]
Peaks_WI<-Peaks[Type=="White",]
Peaks_BI<-as.data.frame(Peaks_BI)
Peaks_WI<-as.data.frame(Peaks_WI)
Cooks<-Peaks_BI[Peaks_BI$Bay=="COOKS",]
KB<-Peaks_BI[Peaks_BI$Bay=="KB",]
PL<-Peaks_BI[Peaks_BI$Bay=="PL",]
Peaks_Near<-Peaks_BI[Peaks_BI$Location=="NEAR",]
Peaks_Off<-Peaks_BI[Peaks_BI$Location=="OFF",]
Cooks_Near<-Cooks[Cooks$Location=="NEAR",]
Cooks_Off<-Cooks[Cooks$Location=="OFF",]
KB_Near<-KB[KB$Location=="NEAR",]
KB_Off<-KB[KB$Location=="OFF",]

#General overview ####
# Tests between the two Bays
wilcox.test(Cooks$Peak,KB$Peak)
wilcox.test(Cooks$Slopes,KB$Slopes)
wilcox.test(Cooks$MPa,KB$MPa)
wilcox.test(Cooks$Mpa_Slopes,KB$Mpa_Slopes)

kruskal.test(Peaks_BI$MPa~Peaks_BI$Bay)
kruskal.test(Peaks_BI$Mpa_Slopes~Peaks_BI$Bay)

kruskal.test(Peaks_Near$MPa~Peaks_Near$Bay)
kruskal.test(Peaks_Off$MPa~Peaks_Off$Bay)

# Tests Within Cooks Bay
kruskal.test(Cooks$Peak~Cooks$Location)
kruskal.test(Cooks$Slopes~Cooks$Location)
kruskal.test(Cooks$Accentuator~Cooks$Location)
kruskal.test(Cooks$MPa~Cooks$Location)
kruskal.test(Cooks$Mpa_Slopes~Cooks$Location)

kruskal.test(Cooks$Peak~Cooks$date)
kruskal.test(Cooks$Slopes~Cooks$date)
kruskal.test(Cooks$Accentuator~Cooks$date)
kruskal.test(Cooks$MPa~Cooks$date)
kruskal.test(Cooks$Mpa_Slopes~Cooks$date)

#Tests within Kempenfelt Bay
kruskal.test(KB$Peak~KB$Location)
kruskal.test(KB$Slopes~KB$Location)
kruskal.test(KB$Accentuator~KB$Location)
kruskal.test(KB$MPa~KB$Location)
kruskal.test(KB$Mpa_Slopes~KB$Location)

kruskal.test(KB$Peak~KB$date)
kruskal.test(KB$Slopes~KB$date)
kruskal.test(KB$Accentuator~KB$date)
kruskal.test(KB$MPa~KB$date)
kruskal.test(KB$Mpa_Slopes~KB$date)


# Comparison of ice quality
ggplot(Peaks)+geom_boxplot(aes(y=MPa,x=factor(Type),fill=factor(Type)))+theme_bw()+
  xlab("")+ylab("Compression strength (MPa)")+theme(axis.text = element_text(size=11),
                                                            axis.title = element_text(size=11),
                                                            legend.text = element_text(size=11),
                                                            legend.position = "bottom",
                                                            legend.title = element_blank())




# General overview with all three locations
ggplot(Peaks_BI)+geom_boxplot(aes(y=MPa,x=factor(Bay),fill=factor(Location)))+theme_bw()+
  xlab("Location")+ylab("Compression strength (MPa)")+theme(axis.text = element_text(size=16),
                         axis.title = element_text(size=16),
                         legend.text = element_text(size=16),
                         legend.position = "bottom",
                         legend.title = element_blank())


BI<-Peaks[Type=="Black"]
WI<-Peaks[Type=="White"]

print(paste("black ice mean",round(mean(BI$MPa),3),"white ice mean",round(mean(WI$MPa),3)))
print(paste("black ice mean",round(sd(BI$MPa),3),"white ice mean",round(sd(WI$MPa),3)))
print(paste("black ice mean",round(max(BI$MPa),3),"white ice mean",round(max(WI$MPa),3)))
print(paste("black ice mean",round(min(BI$MPa),3),"white ice mean",round(min(WI$MPa),3)))
print(paste("black ice mean",round(length(BI$MPa),3),"white ice mean",round(length(WI$MPa),3)))


ggplot(Peaks_BI)+geom_boxplot(aes(y=Mpa_Slopes,x=factor(Bay),fill=factor(Location)))+theme_bw()


# Analysed across dates ####

uni_dat<-unique(Peaks_BI_Simc$date)
uni_dat<-sort(uni_dat)
for(i in 1:length(uni_dat)){
  Step<-Peaks_BI_Simc[Peaks_BI_Simc$date==uni_dat[i],]
  x<-Step[Step$Location=="NEAR",]
  y<-Step[Step$Location=="OFF",]

kw3<-kruskal.test(Step$MPa~Step$Location)
print(paste("Kruskall-Wallis",unique(Step$Bay), uni_dat[i]))
print(kw3)

}

# PEAKS
ggplot(Peaks_BI_Simc)+geom_boxplot(aes(y=MPa,x=factor(date),fill=factor(Location)))+
  theme_bw()+ theme(legend.title = element_blank())+ggtitle("Peaks in MPa")+
  xlab("Sampling dates")+ylab("Peaks (MPa)")

ggplot(Peaks_BI_Simc)+geom_boxplot(aes(y=MPa,x=factor(date),fill=factor(Location)))+
  theme_bw()+ theme(legend.title = element_blank())+ggtitle("Peaks in MPa")+
  xlab("Sampling dates")+ylab("Peaks (MPa)")+facet_wrap(~Bay,ncol=1)

# SLOPES
ggplot(Peaks_BI_Simc)+geom_boxplot(aes(y=Slopes,x=factor(date),fill=factor(Location)))+
  theme_bw()+
  theme(legend.title = element_blank())+ggtitle("Slopes mm s-1")+
  xlab("Sampling dates")+ylab("mm s-1")


ggplot(Peaks_BI_Simc)+geom_boxplot(aes(y=Mpa_Slopes,x=factor(date),fill=factor(Location)))+
  theme_bw()+
  theme(legend.title = element_blank())+ggtitle("Slopes mm s-1")+
  xlab("Sampling dates")+ylab("mm s-1")


# EXPLORING ELASTICITY
ggplot(Peaks_BI_Simc)+geom_boxplot(aes(y=MPa/Mpa_Slopes,x=factor(date),fill=factor(Location)))+
  theme_bw()+
  theme(legend.title = element_blank())+ggtitle("MPa/Slopes")+
  xlab("Sampling dates")+ylab("MPa mm s-1")


Near_Simc<-Peaks_BI_Simc[Peaks_BI_Simc$Location=="NEAR",]
Off_Simc<-Peaks_BI_Simc[Peaks_BI_Simc$Location=="OFF",]

x<-Near_Simc$MPa/Near_Simc$Mpa_Slopes
y<-Off_Simc$MPa/Off_Simc$Mpa_Slopes

wilcox.test(x,y)

x<-Peaks_BI_Simc[Peaks_BI_Simc$date < "2026-02-19",]
y<-Peaks_BI_Simc[Peaks_BI_Simc$date > "2026-02-19",] 

wilcox.test(x$MPa/x$Mpa_Slopes,y$MPa/y$Mpa_Slopes)
ks.test(x$MPa/x$Mpa_Slopes,y$MPa/y$Mpa_Slopes)

# Analysig Density interactions ####

Peaks_BI$density<-Peaks_BI$Weight/Peaks_BI$Volume
W<-lm(Peaks_BI$MPa~Peaks_BI$density)
Peaks_dens<-Peaks_BI[!is.na(Peaks_BI$density), ]

ggplot(Peaks_dens)+geom_point(aes(y=MPa,x=density))+ggtitle("p = 0.002, R2 = 0.395")+
  geom_line(aes(y=predict(W),x=density))+
  theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "none")

cor.test(Peaks_BI$MPa,Peaks_BI$density,method="kendall")

ggplot(Peaks_dens)+geom_point(aes(y=MPa,x=density),size=3)+ggtitle("p = 0.002, R2 = 0.395")+
  geom_line(aes(y=predict(W),x=density))+
  theme_bw()+theme(axis.text = element_text(size=12),axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "none")

# including white ice
Peaks$density<-Peaks$Weight/Peaks$Volume
Peaks<-as.data.frame(Peaks)
Peaks_dens_whit<-Peaks[!is.na(Peaks$density), ]

Wb<-lm(Peaks_dens_whit$MPa~Peaks_dens_whit$Accentuator)
ggplot(Peaks_dens_whit)+geom_point(aes(y=MPa,x=Accentuator,color=factor(date)),size=3)+ggtitle("p < 0.01, R2 = 0.188")+
  geom_line(aes(y=predict(Wb),x=Accentuator))+
  theme_bw()+theme(axis.text = element_text(size=12),axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "bottom")


Wb2<-lm(Peaks_dens_whit$MPa~Peaks_dens_whit$density)
ggplot(Peaks_dens_whit)+geom_point(aes(y=MPa,x=density,color=factor(Type)),size=3)+ggtitle("p < 0.01, R2 = 0.279")+
  geom_line(aes(y=predict(Wb2),x=density))+
  theme_bw()+theme(axis.text = element_text(size=12),axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "none")


Wb3<-lm(Peaks_dens_whit$Peaks~Peaks_dens_whit$density)
ggplot(Peaks_dens_whit)+geom_point(aes(y=Peaks,x=density,color=factor(Type)),size=3)+ggtitle("p < 0.01, R2 = 0.500")+
  geom_line(aes(y=predict(Wb3),x=density))+ylab("Peaks (kN)")+xlab(bquote('Ice density '(kg~ m^-3)))+
  theme_bw()+theme(axis.text = element_text(size=16),
                   axis.title = element_text(size=16),
                   legend.text = element_text(size=16),
                   legend.position = "bottom",
                   legend.title = element_blank())


# Analysing variability in the data ####
Peaks_BI_Simc$Spec<-cbind(Peaks_BI_Simc$Spec,c(paste(Peaks_BI_Simc$Bay,Peaks_BI_Simc$Location,Peaks_BI_Simc$date)))
unique_spec<-unique(Peaks_BI_Simc$Spec)
SD_Vec<-vector()
Var_vec<-vector()
Date_Vec<-vector()
Bay_Vec<-vector()
Location_Vec<-vector()


for(i in 1:length(unique_spec)){
  Var_sec<-Peaks_BI_Simc[Peaks_BI_Simc$Spec==unique_spec[i],]
  SD<-sd(Var_sec$MPa/Var_sec$Mpa_Slopes)
  Var<-var(Var_sec$MPa/Var_sec$Mpa_Slopes)
  
  print(SD)
  print(Var)
  SD_Vec<-c(SD_Vec,as.numeric(SD))
  Var_vec<-c(Var_vec,as.numeric(Var))
  Date_Vec<-c(Date_Vec,unique(Var_sec$date))
  Bay_Vec<-c(Bay_Vec,unique(Var_sec$Bay))
  Location_Vec<-c(Location_Vec,unique(Var_sec$Location))
  
}

df_Var<-cbind(SD_Vec,Var_vec,Date_Vec,Bay_Vec,Location_Vec)
df_Var<-unlist(df_Var)
df_Var<-as.data.frame(df_Var)

df_Var$SD_Vec<-as.numeric(df_Var$SD_Vec)
df_Var$Var_vec<-as.numeric(df_Var$Var_vec)

df_Var$Date_Vec<-as.Date(as.numeric(df_Var$Date_Vec))
df_Var$SD_Vec<-as.numeric(df_Var$SD_Vec)

ggplot(df_Var)+geom_density(aes(x=SD_Vec,fill=factor(Location_Vec)),alpha=0.5)+theme_bw()+theme(legend.title = element_blank())

ggplot(df_Var[df_Var$Date_Vec<"2026-02-19",])+geom_density(aes(x=Var_vec,fill=factor(Location_Vec)),alpha=0.5)+theme_bw()+theme(legend.title = element_blank())

pre_var<-df_Var[df_Var$Date_Vec<"2026-02-19",]

a<-pre_var[pre_var$Location_Vec=="NEAR",]
b<-pre_var[pre_var$Location_Vec=="OFF",]

print("Variance test Komogorov-Smirnov")
ks.test(a$Var_vec,b$Var_vec)

print("SD test Komogorov-Smirnov")
ks.test(a$SD_Vec,b$SD_Vec)
wilcox.test(a$Var_vec,b$Var_vec)

# rm(df_Var)
