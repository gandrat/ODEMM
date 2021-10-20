# Library
library(dplyr)
library(ggplot2)
library(stringr)
library(reshape2)
library(RColorBrewer)
require(tidyverse)

rm(list=ls()) #removing previous objects

#Set the theme for plots----------
theme_set(
  theme_bw(base_size = 10)+
    theme(text=element_text(family="Times"),
          plot.title = element_text(hjust = 0.5, face='bold',size=12))
)

# Load df -----------
# Saved after 00_preprocess.R script
load("Data/SBS_v3.Rda")
data.m<-melt(data%>%select(Sector, Pressure, Ecological.Characteristic,ImpactRisk),id=c('ImpactRisk'))

#Grid for all relationships-----------
ggplot(data,aes(x=Ecological.Characteristic,y=Pressure,fill=Overlap))+geom_tile()+
  facet_wrap(~Sector)+
  scale_fill_brewer(palette = 'Spectral',direction=-1)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = 'top')+
  xlab('Ecological Component')
ggsave('figures/overlap_eco_press.pdf', width=30,height = 30,units = 'cm',dpi=150)

ggplot(data,aes(x=Ecological.Characteristic,y=Pressure,fill=DoI))+geom_tile()+
  facet_wrap(~Sector)+
  scale_fill_brewer(palette = 'Spectral',direction=-1)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = 'top')+
  xlab('Ecological Component')
ggsave('figures/doi_eco_press.pdf', width=30,height = 30,units = 'cm',dpi=150)

ggplot(data,aes(x=Ecological.Characteristic,y=Pressure,fill=Frequency))+geom_tile()+
  facet_wrap(~Sector)+
  scale_fill_brewer(palette = 'Spectral',direction=-1)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = 'top')+
  xlab('Ecological Component')
ggsave('figures/frequency_eco_press.pdf', width=30,height = 30,units = 'cm',dpi=150)

ggplot(data,aes(x=Ecological.Characteristic,y=Pressure,fill=ImpactRisk))+geom_tile()+
  facet_wrap(~Sector)+
  scale_fill_distiller(palette = 'Spectral',direction=-1)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = 'top')+
  xlab('Ecological Component')
ggsave('figures/ImpactRisk_eco_press.pdf', width=30,height = 30,units = 'cm',dpi=150)



#Grid - Separated Plots--------------
s<-'Test'
data<-data%>%mutate(Sector=str_replace(Sector,'/',' '))
for(s in unique(data$Sector)){
  ggplot(data%>%filter(Sector==sprintf(s,'%s')),aes(x=Ecological.Characteristic,y=Pressure,fill=Overlap))+geom_tile()+
    facet_wrap(~Sector)+
    theme_bw()+
    scale_fill_brewer(palette = 'Spectral',direction=-1)+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          legend.position = 'top')
  ggsave(paste0('figures/overlap_eco_press_',sprintf(s,'%s'),'.jpg'), width=15,height = 15,units = 'cm',dpi=300)
  
}

data<-data%>%mutate(Pressure=str_replace(Pressure,'/',' '))
for(s in unique(data$Pressure)){
ggplot(data%>%filter(Pressure==sprintf(s,'%s')),aes(x=Ecological.Characteristic,y=Sector,fill=DoI))+geom_tile()+
  facet_wrap(~Pressure)+
  theme_bw()+
  scale_fill_brewer(palette = 'Spectral',direction=-1)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = 'top')
ggsave(paste0('figures/doi_eco_press_',sprintf(s,'%s'),'.jpg'), width=15,height = 15,units = 'cm',dpi=300)
}

for(s in unique(data$Sector)){
ggplot(data%>%filter(Sector==sprintf(s,'%s')),aes(x=Ecological.Characteristic,y=Pressure,fill=Frequency))+geom_tile()+
  facet_wrap(~Sector)+
  theme_bw()+
  scale_fill_brewer(palette = 'Spectral',direction=-1)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = 'top')
ggsave(paste0('figures/freq_eco_press_',sprintf(s,'%s'),'.jpg'), width=15,height = 15,units = 'cm',dpi=300)
}

