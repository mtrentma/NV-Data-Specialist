library(chron) #this is how we used to do time.  Unwilling to change for this exercise
library(ggplot2)
library(patchwork)
library(reshape2); library(dplyr); library(tidyr)
library(tidyverse)
library(ggbreak)
setwd("C:/Users/Matt Trentman/IDrive-Sync/EMMA/Nevada Data Specialist")

Aim_indicators <- read_csv("BLM_Natl_AIM_Lotic_Indicators_Hub.csv")

my_labels<-c((expression(atop("Small", paste("Stream")))),(expression(atop("Large", paste("Stream")))),"River")

#Figure
  Aim_indicators %>% 
  filter(BLM_AdminState == "NV",Project!="National_Lotic_AIM_Rivers" ) %>% 
  group_by(OBJECTID) %>%
  mutate(StreamSize=ifelse(StreamOrder <= 2, "Small Stream",ifelse( 2 < StreamOrder & StreamOrder<= 4, "Large Stream", "River"))) %>% 
  mutate(StreamSize = as.factor(StreamSize))%>%
  mutate(StreamSize = factor(StreamSize, levels = c("Small Stream", "Large Stream", "River")))%>%
  
  ggplot(aes(x=StreamSize, y=PctBankStable))+
  geom_boxplot(width=0.4)+
  geom_jitter(shape=16, position=position_jitter(0.1), size=1.6)+
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=0, ymax=65, fill = "red", alpha=0.2)+
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=65, ymax=75, fill = "yellow", alpha=0.2)+ 
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=75, ymax=100, fill = "green", alpha=0.2)+
  theme_classic()+
  ylab("Bank Stability (%)")+
  xlab("")+
  theme(axis.title.x=element_text(size=20,colour = "black"))+
  theme(axis.title.y=element_text(size=20,colour = "black"))+
  theme(axis.text.y=element_text(size=20,colour = "black"))+
  theme(axis.text.x=element_text(size=20,colour = "black"))+
  scale_y_continuous(limits=c(0,102), breaks=seq(from=0, to=100, by=20),expand = c(0,0))+
  scale_x_discrete(labels=my_labels)

  
  
  Aim_indicators %>% 
    filter(BLM_AdminState == "NV",Project!="National_Lotic_AIM_Rivers" ) %>% 
    group_by(OBJECTID) %>%
    mutate(StreamSize=ifelse(StreamOrder <= 2, "Small Stream",ifelse( 2 < StreamOrder & StreamOrder<= 4, "Large Stream", "River"))) %>% 
    mutate(StreamSize = as.factor(StreamSize))%>%
    mutate(StreamSize = factor(StreamSize, levels = c("Small Stream", "Large Stream", "River")))%>%
    
    ggplot(aes(x=StreamSize, y=FloodplainConnectivity))+
    geom_boxplot(width=0.4)+
    geom_jitter(shape=16, position=position_jitter(0.1), size=1.6)+
    annotate("rect", xmin=-Inf, xmax=Inf, ymin=0, ymax=65, fill = "red", alpha=0.2)+
    annotate("rect", xmin=-Inf, xmax=Inf, ymin=65, ymax=75, fill = "yellow", alpha=0.2)+ 
    annotate("rect", xmin=-Inf, xmax=Inf, ymin=75, ymax=100, fill = "green", alpha=0.2)+
    theme_classic()+
    ylab("Floodplain Connectivity")+
    xlab("")+
    theme(axis.title.x=element_text(size=20,colour = "black"))+
    theme(axis.title.y=element_text(size=20,colour = "black"))+
    theme(axis.text.y=element_text(size=20,colour = "black"))+
    theme(axis.text.x=element_text(size=20,colour = "black"))+
    scale_y_continuous(limits=c(0,5), breaks=seq(from=0, to=5, by=0.5),expand = c(0,0)) 
  
  
  
x<-c(2014, 2015, 2017,  2019, 2020, 2021, 2022)
y<-c(10, 28,0,  87, 54, 78, 84)
NV_byyear<-as.data.frame(cbind(x,y))

labs<-c(2014, 2015, 2015,  2019, 2020, 2021, 2022)

ggplot(data= NV_byyear,aes(x=factor(x), y=y))+
  geom_bar(stat="identity")+
  theme_classic()+
  ylab("Points Sampled")+
  xlab("")+
  theme(axis.title.x=element_text(size=20,colour = "black"))+
  theme(axis.title.y=element_text(size=20,colour = "black"))+
  theme(axis.text.y=element_text(size=18,colour = "black"))+
  theme(axis.text.x=element_text(size=18,colour = "black"))+
  scale_y_continuous(limits=c(0,90), breaks=seq(from=0, to=90, by=20),expand = c(0,0))+
  scale_x_discrete(labels=labs, limits=labs)+
  annotate(geom="text", x=3, y=10, col="black",label="italic(Break)", 
           parse = TRUE, size = 7)

  
#Table
  Aim_indicators %>% 
    filter(BLM_AdminState == "NV",Project!="National_Lotic_AIM_Rivers" ) %>% 
    group_by(OBJECTID) %>%
    mutate(StreamSize=ifelse(StreamOrder <= 2, "Small Stream",ifelse( 2 < StreamOrder & StreamOrder<= 4, "Large Stream", "River"))) %>% 
    mutate(StreamSize = as.factor(StreamSize))%>%
    mutate(StreamSize = factor(StreamSize, levels = c("Small Stream", "Large Stream", "River")))%>%
    mutate(StabCat = ifelse(PctBankStable <= 65, "Poor",ifelse( 65 < PctBankStable & PctBankStable<= 75, "OK", "Good"))) %>% 
    group_by(StreamSize, StabCat) %>%
    summarise(n = n()) %>%
    filter(!is.na(StabCat))%>%
    mutate(percent = n / sum(n)*100)
  
  Aim_indicators %>% 
    filter(BLM_AdminState == "NV",Project!="National_Lotic_AIM_Rivers" ) %>% 
    group_by(OBJECTID) %>%
    mutate(StreamSize=ifelse(StreamOrder <= 2, "Small Stream",ifelse( 2 < StreamOrder & StreamOrder<= 4, "Large Stream", "River"))) %>% 
    mutate(StreamSize = as.factor(StreamSize))%>%
    mutate(StreamSize = factor(StreamSize, levels = c("Small Stream", "Large Stream", "River")))%>%
    mutate(StabCat = ifelse(PctBankStable <= 65, "Poor",ifelse( 65 < PctBankStable & PctBankStable<= 75, "OK", "Good"))) %>% 
    group_by(StreamSize, BeaverSigns) %>%
    summarise(n = n()) %>%
    filter(!is.na(BeaverSigns
                  ))%>%
    mutate(percent = n / sum(n)*100)
    