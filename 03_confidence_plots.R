require(dplyr)
require(ggplot2)
require(stringr)
require(scales)
require(tidyverse)

rm(list=ls()) #removing previous objects


#Set the theme for plots----------
theme_set(
  theme_bw(base_size = 18)+
    theme(text=element_text(family="Times"),
          plot.title = element_text(hjust = 0.5, face='bold',size=14))
)


# Load df -----------
# Saved after 00_preprocess.R script
load("Data/SBS_v3.Rda")

#Confidence plots by Sector------------
#Only Fishing
ggplot(data%>%filter(idsec==6),aes(x=Pressure,y=Ecological.Characteristic,color=Confidence,size=ImpactRisk))+
  geom_point(shape=15)+
  facet_wrap(~Sector, nrow = 3)+
  theme(axis.text.x = element_text(angle=90, hjust=0))+
  scale_color_manual(values=c('#9e0142','#f46d43','#abdda4','#66c2a5','#3288bd'),
                     limits=fct_unique(data$Confidence))+
  xlab(NULL)+ylab(NULL)+
  scale_x_discrete(position='top')+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(size='Impact Risk', color='Confidence Level')
ggsave('figures/confidence_fishing_sbs.pdf', width = 30, height = 20, units = 'cm', dpi=100)

#Only Tourism
ggplot(data%>%filter(idsec==17),aes(x=Pressure,y=Ecological.Characteristic,color=Confidence,size=ImpactRisk))+
  geom_point(shape=15)+
  facet_wrap(~Sector, nrow = 3)+
  theme(axis.text.x = element_text(angle=90, hjust=0))+
  scale_color_manual(values=c('#9e0142','#f46d43','#abdda4','#66c2a5','#3288bd'),
                     limits=fct_unique(data$Confidence))+
  xlab(NULL)+ylab(NULL)+
  scale_x_discrete(position='top')+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(size='Impact Risk', color='Confidence Level')
ggsave('figures/confidence_tourism_sbs.pdf', width = 30, height = 20, units = 'cm', dpi=100)

#Only Infrastructure
ggplot(data%>%filter(idsec%in%c(18,4,8)),aes(x=Pressure,y=Ecological.Characteristic,color=Confidence,size=ImpactRisk))+
  geom_point(shape=15)+
  facet_wrap(~Sector, nrow = 1)+
  theme(axis.text.x = element_text(angle=90, hjust=0))+
  scale_color_manual(values=c('#9e0142','#f46d43','#abdda4','#66c2a5','#3288bd'),
                     limits=fct_unique(data$Confidence))+
  xlab(NULL)+ylab(NULL)+
  scale_x_discrete(position='top')+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x=element_text(vjust=1))+
  labs(size='Impact Risk', color='Confidence Level')
ggsave('figures/confidence_infra_sbs.pdf', width = 40, height = 20, units = 'cm', dpi=100)
