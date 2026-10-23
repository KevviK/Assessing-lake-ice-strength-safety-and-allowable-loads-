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
library("patchwork")

WD<-getwd() 
WD<-paste(WD,"/Assesing Lake Ice Strength Github/Data/",sep="")
#### DATA IMPORT AND HANDLING
Peaks<-read.csv(paste(WD,"Peaks_Extracted.csv",sep=""))
Literature_Frame2<-read.csv(paste(WD,"Table References/MetaDataAnalysisList.csv",sep=""))

Ice_Data_PL<-read.csv(paste(WD,"Ice_quality_bar_PL FOCUS.csv",sep=""))
Ice_Data_KB<-read.csv(paste(WD,"Ice_quality_bar_KB_FOCUS.csv",sep=""))
Ice_data<-rbind(Ice_Data_KB,Ice_Data_PL)

#### COMPARISONS TO OUR DATA FROM LITERATURE ####
XLabels<-unique(Literature_Frame2$Study)
CompFrame<-Literature_Frame2[Literature_Frame2$Test=="Compression",]
FlexFrame<-Literature_Frame2[Literature_Frame2$Test=="Flexural",]
Mean_Diff<-mean(FlexFrame$Average)/mean(CompFrame$Average)
Median_Diff<-median(FlexFrame$Average)/median(CompFrame$Average)

ggplot(Literature_Frame2)+geom_bar(aes(x=Study,y=Average,fill=Test),position = "dodge", stat = "identity")+
  theme_bw()+theme(legend.position = "None",axis.text.x = element_text(angle = 45, hjust = 1),axis.title = element_blank(),axis.text = element_text(size=14))+ylab("    ")+
  facet_wrap(~Test,scales = "free",ncol=1)


Literature_Frame_BI<-Literature_Frame2[Literature_Frame2$Ice_Type=="Black Ice",]
Comp_Lit<-Literature_Frame_BI[Literature_Frame_BI$Test=="Compression",]
Flex_Lit<-Literature_Frame_BI[Literature_Frame_BI$Test=="Flexural",]

custom_order<-c("This study","Zhang et al. (2024)","Zhang et al. (2022)","Zhang et al. (2022)*","John et al. (2018)",
                "Renliang et al. (2017)", "Renliang et al. (2017)*","Qi et al. (2017)",
                "Bragov (2015)","Petrovic (2003)","Nixon (1996)","Kuehn et al. (1993)",
                "Schulson (1990)","Lee & Schulson (1988)","Cole (1987)","Jones Chew (1983)",
                "Timco & Frederking (1982)","Haynes (1978)","Weeks & Assur (1969)","Weeks & Assur (1969)*",
                "Weeks & Assur (1969)**","Butkovich (1955)")

Comp_Lit$Study <- factor(Comp_Lit$Study, levels = custom_order)
x<-ggplot(Comp_Lit)+geom_bar(aes(x=factor(Study),y=Average,fill=Ice_source),position = "dodge", stat = "identity")+
  theme_bw()+  ylab("MPa")+theme(legend.position = "None",
                                 axis.text.x = element_text(angle = 55, hjust = 1),
                                 axis.title.x = element_blank(),
                                 axis.text = element_text(size=14),
                                 axis.title.y = element_text(size=14))
  
  
custom_order2<-c("Aly et al (2019)","Aly et al (2019)*","Aly et al (2019)**","Karulina et al. (2019)*",
                "Karulina et al. (2019)**","John et al. (2018)","Han et al. (2016)","Han et al. (2016)*",
                "Han et al. (2016)**","Barette (2011)","Barette (2011)*","Barette (2011)**",
                "Gow & Ueda (1989)","Gow & Ueda (1989)*","Gow & Ueda (1989)**","Timco & Frederking (1982)",
                "Gow et al. (1978)","Gold (1971)","Hitch (1959)")
Flex_Lit$Study <- factor(Flex_Lit$Study, levels = custom_order2)
y<-ggplot(Flex_Lit)+geom_bar(aes(x=factor(Study),y=Average,fill=Ice_source),position = "dodge", stat = "identity")+
  theme_bw()+theme(legend.position = "None",axis.text.x = element_text(angle = 55, hjust = 1),axis.title.x = element_blank(),axis.text = element_text(size=14),axis.title.y = element_text(size=14))+
  ylab("MPa")

ggarrange(x,y, ncol = 1)


ggplot(Ice_data%>% filter(Station=="simcoe.deep"  | Station=="paint.deep"))+geom_bar(aes(x=factor(Format_Date),y=Height,fill=factor(Type,levels=c('snow_avg_cm','white_ice_cm','slush_cm','black_ice_cm'))),position = "stack", stat = "identity",colour="black")+
  theme_bw()+theme(axis.title.y = element_text(vjust=2,size=14),legend.position = "bottom",legend.title = element_blank(),axis.text.x = element_text(hjust=1,angle=45),axis.text = element_text(size=12))+ylab("")+xlab("")+
  scale_fill_manual(values = c("white", "grey","#ADD8E6","#3b3b3b"))+facet_wrap(~Station,ncol=1,scales = "free_x")


#### ANALYSING USING ONLY OUR DATASET ON COMPRESSION STRENGTH ONLY ####
Literature_Frame2$KGCM<-Literature_Frame2$Average*10.197 #Converting MPa to Kg cm-2
Study<-Literature_Frame2[Literature_Frame2$Study=="This study",]
Ice_frame<-Ice_data%>% filter(Type=="black_ice_cm"  | Type=="white_ice_cm")

Allowable<-vector()
for(i in 1:length(Ice_frame$Height)){
  if(Ice_frame[i,"Type"]=="white_ice_cm"){
    Selected_KGCM<-Study[2,"KGCM"]
    Allowable<-c(Allowable,Ice_frame[i,"Height"]^2*Selected_KGCM)}else{
      Selected_KGCM<-Study[1,"KGCM"]
      Allowable<-c(Allowable,Ice_frame[i,"Height"]^2*Selected_KGCM)
    }
}

Ice_frame_Study<-cbind(Ice_frame,Allowable)

# 
# print(paste("Mean",mean(Compression_Frame[Compression_Frame$Ice_source=="Laboratory","Average"]),"SE",sd(Compression_Frame[Compression_Frame$Ice_source=="Laboratory","Average"])/sqrt(length(Compression_Frame[Compression_Frame$Ice_source=="Laboratory","Average"]))))
# print(paste("Mean",mean(Compression_Frame[Compression_Frame$Ice_source=="Natural","Average"]),"SE",sd(Compression_Frame[Compression_Frame$Ice_source=="Natural","Average"])/sqrt(length(Compression_Frame[Compression_Frame$Ice_source=="Natural","Average"]))))

Ice_frame_KB<-Ice_frame[Ice_frame$Station=="simcoe.deep",]

#### FLEXURAL STRENGTH BY MODIFYING OUR DATASET ####

Allowable_flex<-vector()
for(i in 1:length(Ice_frame$Height)){
  if(Ice_frame[i,"Type"]=="white_ice_cm"){
    Selected_KGCM<-Study[2,"KGCM"]*(Mean_Diff)
    Allowable_flex<-c(Allowable_flex,Ice_frame[i,"Height"]^2*Selected_KGCM)}else{
      Selected_KGCM<-Study[1,"KGCM"]*(Mean_Diff)
      Allowable_flex<-c(Allowable_flex,Ice_frame[i,"Height"]^2*Selected_KGCM)
    }
}

Ice_frame_Study<-cbind(Ice_frame_Study,Allowable_flex)
ggplot(Ice_frame_Study%>% filter(Station=="simcoe.deep"  | Station=="paint.deep"))+
  geom_bar(aes(x=Format_Date,y=Allowable_flex,fill=factor(Type,levels=c('white_ice_cm','black_ice_cm'))),position = "stack", stat = "identity",colour="black")+
  scale_fill_manual(values = c("grey","#3b3b3b"))+ylab("Allowable loads [kg]")+
  theme_bw()+xlab("")+theme(axis.title.y = element_text(vjust=2,size=14),legend.position = "bottom",legend.title = element_blank(),axis.text.x = element_text(hjust=1,angle=45),axis.text = element_text(size=12))+facet_wrap(~Station,ncol=1,scales="free_x")

ggplot(Ice_frame_Study%>% filter(Station=="simcoe.deep"  | Station=="paint.deep"))+
  geom_bar(aes(x=Format_Date,y=c(Allowable),fill=factor(Type,levels=c('white_ice_cm','black_ice_cm'))),position = "stack", stat = "identity",colour="black")+
  scale_fill_manual(values = c("grey","#3b3b3b"))+ylab("Allowable loads [kg]")+
  theme_bw()+theme(axis.title.y = element_text(vjust=2,size=14),legend.position = "bottom",legend.title = element_blank(),axis.text.x = element_text(hjust=1,angle=45),axis.text = element_text(size=12))+facet_wrap(~Station,ncol=1,scales="free_x")

Ice_test<-Ice_data%>% filter(Station=="simcoe.deep"  | Station=="paint.deep")
Ice_test<-Ice_test%>% filter(Type=="black_ice_cm"  | Type=="white_ice_cm")
ICE_simc<-Ice_test%>% filter(Station=="simcoe.deep")
ICE_paint<-Ice_test%>% filter(Station=="paint.deep")

Ice_Simc_sum<-ICE_simc%>%  
  group_by(Format_Date,Station,Season) %>%
  dplyr::summarise(Height=sum(Height),na.rm=TRUE)

Ice_PL_sum<-ICE_paint%>%  
  group_by(Format_Date,Station,Season) %>%
  dplyr::summarise(Height=sum(Height),na.rm=TRUE)

uni<-unique(Ice_test$Season)
for(i in 1:length(uni)){
PL<-Ice_PL_sum[Ice_PL_sum$Season==uni[i],]
SI<-Ice_Simc_sum[Ice_Simc_sum$Season==uni[i],]  
 w<- wilcox.test(PL$Height,SI$Height,exact=FALSE)
 print(uni[i])
 print(w)
}


#### Analysing direct data for statistical differences

Ice_frame_Study<-Ice_frame_Study%>% filter(Station=="simcoe.deep"  | Station=="paint.deep")

uni_lake<-unique(Ice_frame_Study$Station)
Total_vec<-vector()
Date_vec<-vector()
Lake_vec<-vector()

for(i in 1:length(uni_lake)){
  Ice_frame_lake<-Ice_frame_Study[Ice_frame_Study$Station==uni_lake[i],]
  uni=unique(Ice_frame_lake$Format_Date)
  for(j in 1:length(uni)){
  Date_Ice_frame<-Ice_frame_lake[Ice_frame_lake$Format_Date==uni[j],]
  Total_vec<-c(Total_vec,sum(Date_Ice_frame$Allowable_flex,na.rm=TRUE))
  Date_vec<-c(Date_vec,uni[j])
  Lake_vec<-c(Lake_vec,uni_lake[i])
}}
test<-cbind(Lake_vec,Date_vec,Total_vec)
test<-as.data.frame(test)
test$Total_vec<-as.numeric(test$Total_vec)
test$Date_vec<-ymd(test$Date_vec)
colnames(test)<-c("Station","Format_Date","Total_strength")
a<-test[test$Station=="simcoe.deep",]
b<-test[test$Station=="paint.deep",]
uni<-unique((year(a$Format_Date)))
for(i in 1:length(uni)){
  sep1<-a[year(a$Format_Date)==uni[i],]
  sep2<-b[year(b$Format_Date)==uni[i],]
  k<-ks.test(sep1$Total_strength,sep2$Total_strength)
  t<-wilcox.test(sep1$Total_strength,sep2$Total_strength)
  print(paste("season",uni[i],"Wilcox"))
  print(t)
  print(paste("season",uni[i],"Kolmogorov"))
  print(k)
  print(paste("season",uni[i]))
  print(paste("simcoe median",median(sep1$Total_strength,na.rm = TRUE),"Paint lake median",median(sep2$Total_strength,na.rm = TRUE)))
}


# CALCULATING EXPECTED ALLOWABLE LAODS USIN GLITERATURE DATA FOR COMPARISON OF OUR STUDY ####


Comp_WI_Mod<-Literature_Frame2%>%filter(Study %in% c("Butkovich (1955)","This study"))
WI_Mod_Comp<-mean(Comp_WI_Mod[Comp_WI_Mod$Ice_Type=="White Ice","Average"]/Comp_WI_Mod[Comp_WI_Mod$Ice_Type=="Black Ice","Average"])
Flex_WI_Mod<-Literature_Frame2%>%filter(Study %in% c("Barette (2011)"))
WI_Mod_Flex<-mean(Flex_WI_Mod[Flex_WI_Mod$Ice_Type=="White Ice","Average"]/Flex_WI_Mod[Flex_WI_Mod$Ice_Type=="Black Ice","Average"])
Compression_Frame<-Literature_Frame2%>%filter(Test=="Compression" & Ice_Type=="Black Ice")
Flexural_Frame<-Literature_Frame2%>%filter(Test=="Flexural" & Ice_Type=="Black Ice")

#COMPRESSION
uni<-unique(Compression_Frame$Study)
dt_list <- vector("list", length(uni))
for (i in 1:length(uni)){
  Study_input<-Compression_Frame[Compression_Frame==uni[i],"Average"]*10.1972
  Ice_frame_input<-Ice_frame
  for(j in 1:length(Ice_frame_input$Height)){
    if(Ice_frame[j,"Type"]=="white_ice_cm"){
      Ice_frame_input[j,"Allowable"]<-Ice_frame_input[j,"Height"]^2*(Study_input*WI_Mod_Comp)
      Ice_frame_input[j,"Study"]<-uni[i]
      Ice_frame_input[j,"Bearing"]<-Study_input*WI_Mod_Comp  
      Ice_frame_input[j,"Method"]<-"Compression"
      }else{
      Ice_frame_input[j,"Allowable"]<-Ice_frame_input[j,"Height"]^2*(Study_input)
      Ice_frame_input[j,"Study"]<-uni[i]
      Ice_frame_input[j,"Bearing"]<-Study_input
      Ice_frame_input[j,"Method"]<-"Compression"
      }
    dt_list[[i]] <- Ice_frame_input
    
  }
}
Ice_frame_input_complete_Compression<-rbindlist(dt_list)


# FLEXURAL DATA
uni<-unique(Flexural_Frame$Study)
dt_list_flex <- vector("list", length(uni))
for (i in 1:length(uni)){
  Study_input<-Flexural_Frame[Flexural_Frame==uni[i],"Average"]*10.1972
  Ice_frame_input<-Ice_frame
  for(j in 1:length(Ice_frame_input$Height)){
    if(Ice_frame[j,"Type"]=="white_ice_cm"){
      Ice_frame_input[j,"Allowable"]<-Ice_frame_input[j,"Height"]^2*(Study_input*WI_Mod_Flex)
      Ice_frame_input[j,"Study"]<-uni[i]
      Ice_frame_input[j,"Bearing"]<-Study_input*WI_Mod_Comp
      Ice_frame_input[j,"Method"]<-"Flexible"
      }
    else{
        Ice_frame_input[j,"Allowable"]<-Ice_frame_input[j,"Height"]^2*(Study_input)
        Ice_frame_input[j,"Study"]<-uni[i]
        Ice_frame_input[j,"Bearing"]<-Study_input
        Ice_frame_input[j,"Method"]<-"Flexible"
      }
    dt_list_flex[[i]] <- Ice_frame_input
    
  }
}
Ice_frame_input_complete_flex<-rbindlist(dt_list_flex)
 
Ice_frame_input_complete<-rbind(Ice_frame_input_complete_Compression,Ice_frame_input_complete_flex)

Ice_frame_input_complete_Compression<-Ice_frame_input_complete_Compression%>%filter(Station=="simcoe.deep"  | Station=="paint.deep")
Ice_frame_input_complete_Compression<-Ice_frame_input_complete_Compression%>%filter(Type=="black_ice_cm")
This_Comp<-Ice_frame_input_complete_Compression[Ice_frame_input_complete_Compression$Study=="This study",]


a<-ggplot()+geom_point(aes(x=Ice_frame_input_complete_Compression$Height, y=Ice_frame_input_complete_Compression$Allowable))+
  geom_point(aes(x=This_Comp$Height,y=This_Comp$Allowable,color="red"))+theme_bw()+theme(legend.position = "none")+
  ylab("Allowable loads (kg)")+xlab("black ice height (cm)")+ggtitle("Compresssion loads")

Ice_frame_input_complete_flex<-Ice_frame_input_complete_flex%>%filter(Station=="simcoe.deep"  | Station=="paint.deep")
Ice_frame_input_complete_flex<-Ice_frame_input_complete_flex%>%filter(Type=="black_ice_cm")

b<-ggplot()+geom_point(aes(x=Ice_frame_input_complete_flex$Height, y=Ice_frame_input_complete_flex$Allowable))+
  geom_point(aes(x=This_Comp$Height,y=This_Comp$Allowable*0.26,color="red"))+geom_point(aes(x=This_Comp$Height,y=This_Comp$Allowable*0.46,color="blue"))+theme_bw()+
  theme(legend.position = "none",axis.text = element_text(size=12),legend.text = element_text(size=12))+
  ylab("Allowable loads (kg)")+xlab("black ice height (cm)")

ggarrange(a,b)
 
This_Comp%>%group_by(Station,Season)%>%summarise(mean(Allowable*Mean_Diff),
                                                 max(Allowable*Mean_Diff),
                                                 min(Allowable*Mean_Diff),)

#### Comparison of Gold, Weyhenmeyer and our data ####

#GOLD's formula
Gold_vec<-vector()
Weyhen_vec<-vector()
Total_vec<-vector()
Total_height<-vector()
WI_Perc<-vector()
for(i in 1:length(Ice_frame_Study$Height)){
  if (i %% 2 == 0) {
    Gold_vec<-c(Gold_vec,NA)
  } else {
    Golds<-sum(Ice_frame_Study[i,"Height"],Ice_frame_Study[i+1,"Height"])^2*3.5
    Gold_vec<-c(Gold_vec,Golds)
  }
  
  if (i %% 2 == 0) {
    Weyhen_vec<-c(Weyhen_vec,NA)
  } else {
    WI<-Ice_frame_Study[i+1,"Height"]/sum(Ice_frame_Study[i,"Height"],Ice_frame_Study[i+1,"Height"])
    print(WI)
    print(Ice_frame_Study[i,"Height"])
    print(Ice_frame_Study[i+1,"Height"])
    Weyhen<-((sum(Ice_frame_Study[i,"Height"],Ice_frame_Study[i+1,"Height"])^2*3.5)/2)*(1+((100-WI*100)/100))
    Weyhen_vec<-c(Weyhen_vec,Weyhen)
  }
  
  if (i %% 2 == 0) {
    Total_vec<-c(Total_vec,NA)
  } else {
    
    Total<-sum(Ice_frame_Study[i,"Allowable_flex"],Ice_frame_Study[i+1,"Allowable_flex"])
    Total_vec<-c(Total_vec,Total)
  }
  
  if (i %% 2 == 0) {
    Total_height<-c(Total_height,NA)
  } else {
    
    Height<-sum(Ice_frame_Study[i,"Height"],Ice_frame_Study[i+1,"Height"])
    Total_height<-c(Total_height,Height)
  }
  
  if (i %% 2 == 0) {
    WI_Perc<-c(WI_Perc,NA)
  } else {
    
    WI<-Ice_frame_Study[i+1,"Height"]/sum(Ice_frame_Study[i,"Height"],Ice_frame_Study[i+1,"Height"])
    WI_Perc<-c(WI_Perc,WI)
  }
  
}

Ice_frame_Study<-cbind(Ice_frame_Study,Gold_vec)
Ice_frame_Study<-cbind(Ice_frame_Study,Weyhen_vec)
Ice_frame_Study<-cbind(Ice_frame_Study,Total_vec)
Ice_frame_Study<-cbind(Ice_frame_Study,Total_height)
Ice_frame_Study<-cbind(Ice_frame_Study,WI_Perc)


ggplot(Ice_frame_Study)+geom_point(aes(x=Total_height,y=Total_vec,color="Our"),size=2)+
  geom_point(aes(x=Total_height,y=Weyhen_vec,color="Weyhen"),shape=17,size=2)+
geom_point(aes(x=Total_height,y=Gold_vec,color="Gold"),size=2)+
  theme_bw()+ylab("Allowable loads")+xlab("Ice Thickness")+
  theme(axis.text = element_text(size=12),axis.title = element_text(size=12),legend.title = element_blank())

z<-ggplot(Ice_frame_Study)+geom_density(aes(x=Total_vec,fill="Our"),alpha=0.3)+
  geom_density(aes(x=Weyhen_vec,fill="Weyhenmeyer"),alpha=0.3)+
  geom_density(aes(x=Gold_vec,fill="Gold"),alpha=0.3)+
  theme_bw()+
  theme(axis.text = element_text(size=16),legend.text = element_text(size=16),axis.title = element_text(size=16),legend.title = element_blank(),legend.position = "bottom")

y<-ggplot(Ice_frame_Study)+geom_point(aes(x=Total_vec,y=Gold_vec,color="Gold"),size=2)+
  geom_point(aes(x=Total_vec,y=Weyhen_vec,color="Weyhenmeyer"),size=2)+theme_bw()+
  geom_line(aes(x=Total_vec,y=Total_vec))+xlab("Allowable Loads (This Study)")+ylab("Allowable Loads \nWeyhenmeyer (2022) or Gold (1971)")+
  theme(axis.text = element_text(size=12),legend.text = element_text(size=12),axis.title = element_text(size=12),legend.title = element_blank())

ggarrange(b,y)

Ice_frame_Study%>% filter(Station=="simcoe.deep"  | Station=="paint.deep")%>%group_by(Station,Season)%>%
  summarise(mean(Total_vec,na.rm=TRUE),
            max(Total_vec,na.rm=TRUE),
            min(Total_vec,na.rm=TRUE),
            sd(Total_vec,na.rm=TRUE))

Ice_frame_Study%>% filter(Station=="simcoe.deep"  | Station=="paint.deep")%>%group_by(Station,Season)%>%
  summarise(mean(Gold_vec,na.rm=TRUE),
            max(Gold_vec,na.rm=TRUE),
            min(Gold_vec,na.rm=TRUE),
            sd(Gold_vec,na.rm=TRUE))


Ice_frame_Study%>% filter(Station=="simcoe.deep"  | Station=="paint.deep")%>%group_by(Station,Season)%>%
  summarise(mean(Weyhen_vec,na.rm=TRUE),
            max(Weyhen_vec,na.rm=TRUE),
            min(Weyhen_vec,na.rm=TRUE),
            sd(Weyhen_vec,na.rm=TRUE))

