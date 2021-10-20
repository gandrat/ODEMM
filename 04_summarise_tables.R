require(dplyr)
require(readODS)
require(stringr)
require(tidyr)
require(tidyverse)

#Set the theme for plots----------
theme_set(
  theme_bw(base_size = 9)+
    theme(text=element_text(family="Times"),
          plot.title = element_text(hjust = 0.5, face='bold',size=9))
)

# Load df -----------
# Saved after 00_preprocess.R script
load("Data/SBS_v3.Rda")

data$link<-1

press_summary<-data%>%group_by(Pressure)%>%
  summarise(sector=length(unique(Sector)),
            ecochar=length(unique(Ecological.Characteristic)),
            links=n(),
            percent=sum(link)*100/nrow(data),
            ir_average=mean(ImpactRisk),
            ir_sum=sum(ImpactRisk))
press_summary$ir_average_rank<-rank(-press_summary$ir_average)
press_summary$ir_sum_rank<-rank(-press_summary$ir_sum, )
press_summary<-press_summary%>%arrange(ir_sum_rank)
write_ods(press_summary,'Data/pressure_summary.ods')


sector_summary<-data%>%group_by(Sector)%>%
  summarise(pressure=length(unique(Pressure)),
            ecochar=length(unique(Ecological.Characteristic)),
            links=n(),
            percent=sum(link)*100/nrow(data),
            ir_average=mean(ImpactRisk),
            ir_sum=sum(ImpactRisk))
sector_summary$ir_average_rank<-rank(-sector_summary$ir_average)
sector_summary$ir_sum_rank<-rank(-sector_summary$ir_sum, )
sector_summary<-sector_summary%>%arrange(ir_sum_rank)
write_ods(sector_summary,'Data/sector_summary.ods')

ecochar_summary<-data%>%group_by(Ecological.Characteristic)%>%
  summarise(sector=length(unique(Pressure)),
            sector=length(unique(Sector)),
            links=n(),
            percent=sum(link)*100/nrow(data),
            ir_average=mean(ImpactRisk),
            ir_sum=sum(ImpactRisk))
ecochar_summary$ir_average_rank<-rank(-ecochar_summary$ir_average)
ecochar_summary$ir_sum_rank<-rank(-ecochar_summary$ir_sum)
ecochar_summary<-ecochar_summary%>%arrange(ir_sum_rank)
write_ods(ecochar_summary,'Data/ecochar_summary.ods')

#Risk of Impact plots---------

load('Data/SBS_v3.Rda')

data<-merge(data,ecochar_summary%>%transmute(Ecological.Characteristic=Ecological.Characteristic,
                                             ecochar_rank=ir_sum_rank),
            by='Ecological.Characteristic')

data<-merge(data,press_summary%>%transmute(Pressure=Pressure,
                                           pressure_rank=ir_sum_rank),
            by='Pressure')

data<-merge(data,sector_summary%>%transmute(Sector=Sector,
                                            sector_rank=ir_sum_rank),
            by='Sector')

data$Pressure<-fct_reorder(data$Pressure, data$pressure_rank)
data$Ecological.Characteristic<-fct_reorder(data$Ecological.Characteristic, data$ecochar_rank)
data$Sector<-fct_reorder(data$Sector, data$sector_rank)


ggplot(data,aes(x=Pressure,y=ImpactRisk,fill=Confidence))+
  geom_bar(stat='identity')+
  scale_fill_manual(values=c('#9e0142','#f46d43','#abdda4','#66c2a5','#3288bd'))+
  xlab(NULL)+ylab("Sum Risk of Impact")+
  theme(axis.text.x=element_text(angle=45, hjust=1))
ggsave('figures/sum_impact_bar_pressure.png',width=16, height = 10, units='cm',dpi=150 )

ggplot(data,aes(x=Sector,y=ImpactRisk,fill=Confidence))+
  geom_bar(stat='identity')+
  scale_fill_manual(values=c('#9e0142','#f46d43','#abdda4','#66c2a5','#3288bd'))+
  xlab(NULL)+ylab("Sum Risk of Impact")+
  theme(axis.text.x=element_text(angle=45, hjust=1))
ggsave('figures/sum_impact_bar_sector.png',width=16, height = 10, units='cm',dpi=150)

ggplot(data,aes(x=Ecological.Characteristic,y=ImpactRisk,fill=Confidence))+
  geom_bar(stat='identity')+
  scale_fill_manual(values=c('#9e0142','#f46d43','#abdda4','#66c2a5','#3288bd'))+
  xlab(NULL)+ylab("Sum Risk of Impact")+
  theme(axis.text.x=element_text(angle=45, hjust=1))
ggsave('figures/sum_impact_bar_ecochar.png',width=16, height = 10, units='cm',dpi=150)
