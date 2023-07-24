#This script was originally developed by Mette Skern-Mauritzen

#Load packages
require(RColorBrewer)
require(plyr)
require(ggplot2)
require(gridExtra)
require(tidyverse)
require(reshape2)
require(ggpubr)
require(grid)

rm(list=ls()) #removing previous objects

theme_set(
  theme_bw(base_size = 10)+
    theme(text=element_text(family="Helvetica"))
)


# read in data
load('Data/trindade_v1.Rda')

#Preprocess----------

## Calculate Impact Risk, Recovery Lag and log IR
data$LN.IR = log(data$ImpactRisk)

## set up data frame for some of the initial plots
BPIS = data
names(BPIS)[names(BPIS) == 'Ecological.Characteristic'] <- 'EcoChar'


# Calculate conectance

d = ddply (BPIS, c("Sector", "Pressure", "EcoChar"), summarise,
           Count = length(ImpactRisk))

## Sectors----------
SP = ddply (d, c("Sector", "Pressure"), summarise,
            Sum = sum(Count))

SE = ddply (d, c("Sector", "EcoChar"), summarise,
            Sum = sum(Count))

SecPress = ddply (SP, c("Sector"), summarise,
                  TotLinks = sum(Sum),
                  Pressures = length(Sum))

SecEco = ddply (SE, c("Sector"), summarise,
                Eco = length(Sum))

Sectors = merge (SecPress, SecEco)

Sectors$Connect = (Sectors$TotLinks/ (length (BPIS$ImpactRisk))) *100

Sectors = Sectors[order(Sectors$Connect, decreasing = TRUE), ]

## Pressures-----------

PE = ddply (d, c("Pressure", "EcoChar"), summarise,
            Sum = sum(Count))

PressSec = ddply (SP, c("Pressure"), summarise,
                  TotLinks = sum(Sum),
                  Sectors = length(Sum))

PressEco = ddply (PE, c("Pressure"), summarise,
                  Eco = length(Sum))

Pressures = merge (PressSec, PressEco)

Pressures$Connect = (Pressures$TotLinks/ (length (BPIS$ImpactRisk))) *100

Pressures = Pressures[order(Pressures$Connect, decreasing = TRUE), ]

## Eco Char--------------

EcoSec = ddply (SE, c("EcoChar"), summarise,
                TotLinks = sum(Sum),
                Sectors = length(Sum))

EcoPress = ddply (PE, c("EcoChar"), summarise,
                  Pressure = length(Sum))

Eco = merge (EcoSec, EcoPress)


Eco$Connect = (Eco$TotLinks/ (length (BPIS$ImpactRisk))) *100

Eco = Eco[order(Eco$Connect, decreasing = TRUE), ]

#Conectance calculation-------

## Sectors-----------

SecRank = ddply (data, "Sector", summarise,
                 SumIR = sum(ImpactRisk))

SecRank = SecRank[order(-SecRank$SumIR),]

BoxPlotSector = merge(BPIS, SecRank)

Sectors = merge (Sectors, SecRank)


ns<-length(unique(Sectors$Sector))

##Pressure---------




Pressures$Pressure<- str_replace(Pressures$Pressure, '/', ' - ')


##Eco-------






#Merge--------------
Eco$type<-'Ecological Components'
eco2<-Eco%>%transmute(name=EcoChar, Connect=Connect, type=type)
Sectors$type<-'Sectors'
sec2<-Sectors%>%transmute(name=Sector, Connect=Connect, type=type)
Pressures$type<-'Pressures'
pres2<-Pressures%>%transmute(name=Pressure, Connect=Connect, type=type)


all<-rbind(eco2,sec2, pres2)

ggplot(all,aes(x= reorder(name, Connect),y=Connect))+
  geom_bar(stat = "identity",fill='grey70')+
  # facet_wrap(~type, ncol=1, scales='free_y')+
  facet_grid(type~.,scales='free_y',space='free_y')+
  scale_y_continuous(limits=c(0,32),expand = c(0, 0))+
  xlab(NULL)+ylab('Connectance')+
  coord_flip()
ggsave('figures/connectance_v2.jpg', width=90, height=210, units = 'mm', dpi=600)
ggsave('figures/connectance_v2.eps', width=90, height=210, units = 'mm', dpi=600)

# Impact Risk
sum_ir <- BPIS%>%select(Sector, Pressure, EcoChar, ImpactRisk)%>%
  group_by(Sector) %>%mutate(sumIR_sec=sum(ImpactRisk))
sum_ir <- sum_ir%>%select(Sector, Pressure, EcoChar, ImpactRisk,sumIR_sec)%>%
  group_by(Pressure) %>%mutate(sumIR_pres=sum(ImpactRisk))
sum_ir <- sum_ir%>%select(Sector, Pressure, EcoChar, ImpactRisk,sumIR_sec,sumIR_pres)%>%
  group_by(EcoChar) %>%mutate(sumIR_eco=sum(ImpactRisk))

ir.sec<-sum_ir%>%transmute(name=Sector, ImpactRisk=ImpactRisk, sumIR=sumIR_sec, type='Sectors')
ir.pres<-sum_ir%>%transmute(name=Pressure, ImpactRisk=ImpactRisk, sumIR=sumIR_pres, type='Pressures')
ir.eco<-sum_ir%>%transmute(name=EcoChar, ImpactRisk=ImpactRisk, sumIR=sumIR_eco, type='Ecological Components')

ir<-rbind(ir.sec,ir.pres, ir.eco)

ggplot (ir , aes(x= reorder(name, sumIR), y = ImpactRisk))+
  facet_grid(type~.,scales='free_y',space='free_y')+
  # geom_violin(fill='gray70')+
  geom_boxplot(fill='gray70',outlier.size = .2, lwd=.2)+
  stat_boxplot(geom ='errorbar', width =.5, lwd=.2)+
  coord_flip()+
  xlab(NULL)+ylab('Impact Risk')
ggsave('figures/impactrisk_v2.jpg', width=90, height=210, units = 'mm', dpi=600)
ggsave('figures/impactrisk_v2.eps', width=90, height=210, units = 'mm', dpi=600)
