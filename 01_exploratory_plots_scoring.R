# Library

rm(list=ls()) #removing previous objects
library(dplyr)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(stringr)



# Make a connection data frame
raw = read.csv("Data/Pressure_Assessment_SBS_V2.csv")

data = raw[!raw$Overlap == "NO", ]

#Remove empty spaces at ecochar string
data$Ecological.Characteristic<-str_trim(data$Ecological.Characteristic, side = c("both"))
unique(data$Ecological.Characteristic)
# Only need the sectors, pressures, eco char and their scores
# data = data[ , c(1:8)]
# add columns with the values asscociated with the clasifactions
# score each rating according to Knight et al 2015
data$Overlap.Score = ifelse(data$Overlap == "W", 1,
                            ifelse(data$Overlap == "L", 0.37, 
                                   ifelse(data$Overlap == "S", 0.03, NA)))

data$Frequency.Score = ifelse(data$Frequency == "P", 1,
                              ifelse(data$Frequency == "C", 0.67, 
                                     ifelse(data$Frequency == "O", 0.33,
                                            ifelse(data$Frequency == "R", 0.08, NA ))))

data$DoI.Score = ifelse(data$DoI == "A" , 1,
                        ifelse(data$DoI == "C", 0.13,
                               ifelse(data$DoI == "L", 0.01, NA)))

# data$Resilience.Score = ifelse(data$Resilience == "L", 0.55,
#                                ifelse(data$Resilience == "M", 0.06, 
#                                       ifelse(data$Resilience == "H", 0.01, NA)))
# 
# data$Persistence.Score = ifelse(data$Persistence == "C", 1,
#                                 ifelse(data$Persistence == "H", 0.55,
#                                        ifelse(data$Persistence == "M", 0.06, 
#                                               ifelse(data$Persistence == "L", 0.01, NA))))



### Calculate Impact Risk, Recovery Lag and Total Risk
data$ImpactRisk = data$Overlap.Score*data$Frequency.Score*data$DoI.Score
data<-data%>%select(Sector, Pressure, Ecological.Characteristic, Overlap, Frequency, DoI, ImpactRisk)

data<-data[complete.cases(data),]

#Set factor levels
data$Overlap<-factor(data$Overlap,levels = c('S','L','W'))
data$Frequency<-factor(data$Frequency,levels = c('R','O','C','P'))
data$DoI<-factor(data$DoI,levels = c('L','C','A'))

data$Sector[data$Sector == "Non-renewable (oil & gas)"] <- "Non-renewable"

#Boxplots ImpactRisk-----------
data.m<-melt(data%>%select(Sector, Pressure, Ecological.Characteristic,ImpactRisk),id=c('ImpactRisk'))

ggplot(data.m,aes(x=value,y=ImpactRisk))+geom_boxplot()+
  facet_wrap(~variable,scales='free_y',nrow=3)+
  coord_flip()+
  theme_bw()

ggsave('figures/risk_boxplots.jpg', width=20,height =30,units = 'cm',dpi=300)

#Grid

ggplot(data,aes(x=Ecological.Characteristic,y=Pressure,fill=Overlap))+geom_tile()+
  facet_wrap(~Sector)+
  theme_bw()+
  scale_fill_brewer(palette = 'Spectral',direction=-1)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = 'top')
ggsave('figures/overlap_eco_press.jpg', width=25,height = 27,units = 'cm',dpi=300)

ggplot(data,aes(x=Ecological.Characteristic,y=Sector,fill=DoI))+geom_tile()+
  facet_wrap(~Pressure)+
  theme_bw()+
  scale_fill_brewer(palette = 'Spectral',direction=-1)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = 'top')
ggsave('figures/doi_eco_press.jpg', width=25,height = 27,units = 'cm',dpi=300)
ggsave('figures/doi_eco_sec.jpg', width=25,height = 27,units = 'cm',dpi=300)

ggplot(data,aes(x=Ecological.Characteristic,y=Pressure,fill=Frequency))+geom_tile()+
  facet_wrap(~Sector)+
  theme_bw()+
  scale_fill_brewer(palette = 'Spectral',direction=-1)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = 'top')
ggsave('figures/frequency_eco_press.jpg', width=25,height = 27,units = 'cm',dpi=300)


ggplot(data,aes(x=Ecological.Characteristic,y=Pressure,fill=ImpactRisk))+geom_tile()+
  facet_wrap(~Sector)+
  theme_bw()+
  scale_fill_distiller(palette = 'Spectral',direction=-1)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = 'top')
ggsave('figures/ImpactRisk_eco_press.jpg', width=25,height = 27,units = 'cm',dpi=300)


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
# ggsave('figures/doi_eco_press.jpg', width=25,height = 27,units = 'cm',dpi=300)
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

