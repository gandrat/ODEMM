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


# read in data
load('Data/SBS_v3.Rda')

### Calculate Impact Risk, Recovery Lag and log IR
data$LN.IR = log(data$ImpactRisk)

### set up data frame for some of the initial plots
BPIS = data
names(BPIS)[names(BPIS) == 'Ecological.Characteristic'] <- 'EcoChar'


# Calculate conectance

d = ddply (BPIS, c("Sector", "Pressure", "EcoChar"), summarise,
           Count = length(ImpactRisk))

#### Sectors----------
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

### Pressures-----------

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

### Eco Char--------------

EcoSec = ddply (SE, c("EcoChar"), summarise,
                TotLinks = sum(Sum),
                Sectors = length(Sum))

EcoPress = ddply (PE, c("EcoChar"), summarise,
                  Pressure = length(Sum))

Eco = merge (EcoSec, EcoPress)


Eco$Connect = (Eco$TotLinks/ (length (BPIS$ImpactRisk))) *100

Eco = Eco[order(Eco$Connect, decreasing = TRUE), ]

## BarPlots------------

#Sectors
Sectors$Sector <- factor(Sectors$Sector, levels = Sectors$Sector[order(Sectors$Connect)])

ggplot (Sectors, aes (x = Sector, y = Connect))+
  geom_bar(stat = 'identity', colour = "red", fill = "red") +
  ylab ("Proportional Connectance")+
  theme_bw() +
  coord_flip()
ggsave("figures/Connectance_Sectors.pdf", width = 6, height = 8)

#Pressures
Pressures$Pressure <- factor(Pressures$Pressure, levels = Pressures$Pressure[order(Pressures$Connect)])

ggplot (Pressures, aes (x = Pressure, y = Connect))+
  geom_bar(stat = 'identity', colour = "red", fill = "red") +
  ylab ("Proportional Connectance")+
  theme_bw() +
  coord_flip()
ggsave("figures/Connectance_Pressures.pdf", width = 6, height = 8)


#Ecochar
Eco$EcoChar <- factor(Eco$EcoChar, levels = Eco$EcoChar[order(Eco$Connect)])


ggplot (Eco, aes (x = EcoChar, y = Connect))+
  geom_bar(stat = 'identity', colour = "red", fill = "red") +
  ylab ("Proportional Connectance")+
  xlab ("Ecological Components")+
  theme_bw() +
  coord_flip()
ggsave("figures/Connectance_Ecochar.pdf", width = 6, height = 8)


# Box Plots and proportional connectance plot (used for D1.1)-----------
#### Sectors

SecRank = ddply (data, "Sector", summarise,
                 SumIR = sum(ImpactRisk))

SecRank = SecRank[order(-SecRank$SumIR),]

BoxPlotSector = merge(BPIS, SecRank)

Sectors = merge (Sectors, SecRank)


n<-length(unique(Sectors$Sector))
Seca = ggplot (Sectors, aes (x= reorder(Sector, SumIR), y = Connect))+
  geom_bar(stat = "identity", 
           color = "black", 
           fill = colorRampPalette(brewer.pal(9,"Pastel1"))(n)) +
  ylab ("Proportional Connectance")+
  theme_bw() +
  coord_flip()+
  theme(axis.title.y=element_blank(),
        axis.text.y = element_text(size=12),
        axis.title.x=element_blank(),
        axis.text.x =element_blank(),
        axis.ticks.x=element_blank(),
        plot.margin = unit(c(5,1,0,6), "mm"))


Secb = ggplot (BoxPlotSector , aes(x= reorder(Sector, SumIR), y = ImpactRisk))+
  geom_boxplot(fill = colorRampPalette(brewer.pal(9,"Pastel1"))(n))+
  stat_boxplot(geom ='errorbar', width =0.5)+ coord_flip()+
  theme_bw()+
  ylab("Impact Risk")+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x =element_blank(),
        axis.ticks.x=element_blank(),
        plot.margin = unit(c(5,1,0,0), "mm"))

Secc= ggplot (BoxPlotSector, aes(x= reorder(Sector, SumIR), y = LN.IR))+
  geom_boxplot(fill = colorRampPalette(brewer.pal(9,"Pastel1"))(n))+
  stat_boxplot(geom ='errorbar', width =0.5)+ 
  coord_flip()+
  theme_bw()+
  ylab("Impact Rank")+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x =element_blank(),
        axis.ticks.x=element_blank(),
        plot.margin = unit(c(5,5,0,0), "mm"))


BoxSector = grid.arrange(Seca, Secb , Secc, ncol = 3, widths = c(6, 4, 4))

pdf ("figures/BoxPlotSectors_noXlabel.pdf", width = 12, height = 8)
grid.arrange(Seca, Secb , Secc, ncol = 3, widths = c(6, 4, 4))
dev.off()

#### Pressures-----------------

PressRank = ddply (data, "Pressure", summarise,
                   SumIR = sum(ImpactRisk))

PressRank = PressRank[order(-PressRank$SumIR),]


BoxPlotPressure = merge(BPIS, PressRank)

Pressures = merge (Pressures, PressRank)

n<-length(unique(Pressures$Pressure))

Pressures$Pressure<- str_replace(Pressures$Pressure, '/', ' -')
BoxPlotPressure$Pressure<- str_replace(BoxPlotPressure$Pressure, '/', ' -')

Pressa = ggplot (Pressures, aes (x= reorder(Pressure, SumIR), y = Connect))+
  geom_bar(stat = "identity", 
           color = "black", 
           fill = colorRampPalette(brewer.pal(9,"Pastel1"))(n)) +
  ylab ("Proportional Connectance")+
  theme_bw() +
  coord_flip()+
  theme(axis.title.y=element_blank(),
        axis.text.y = element_text(size=12),
        axis.title.x=element_blank(),
        axis.text.x =element_blank(),
        axis.ticks.x=element_blank(),
        plot.margin = unit(c(5,1,5,6), "mm"))


Pressb = ggplot (BoxPlotPressure, aes(x= reorder(Pressure, SumIR), y = ImpactRisk))+
  geom_boxplot(fill = colorRampPalette(brewer.pal(9,"Pastel1"))(n))+
  stat_boxplot(geom ='errorbar', width =0.5)+ coord_flip()+
  theme_bw()+
  ylab("Impact Risk")+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x =element_blank(),
        axis.ticks.x=element_blank(),
        plot.margin = unit(c(5,1,5,0), "mm"))

Pressc= ggplot (BoxPlotPressure, aes(x= reorder(Pressure, SumIR), y = LN.IR))+
  geom_boxplot(fill = colorRampPalette(brewer.pal(9,"Pastel1"))(n))+
  stat_boxplot(geom ='errorbar', width =0.5)+ 
  coord_flip()+
  theme_bw()+
  ylab("Impact Rank")+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x =element_blank(),
        axis.ticks.x=element_blank(),
        plot.margin = unit(c(5,5,5,0), "mm"))

BoxPressure = grid.arrange(Pressa, Pressb , Pressc, ncol = 3, widths = c(6, 4, 4))

pdf ("figures/BoxPlotPressures_noXlabel.pdf", width = 12, height = 8)
grid.arrange(Pressa, Pressb , Pressc, ncol = 3, widths = c(6, 4, 4))
dev.off()

#### Eco Char----------------
EcoRank = ddply (BPIS, "EcoChar", summarise,
                 SumIR = sum(ImpactRisk))

EcoRank = EcoRank[order(-EcoRank$SumIR),]


BoxPlotEco = merge(BPIS, EcoRank)

Eco = merge (Eco, EcoRank)

#### Plots
n<-length(unique(Eco$EcoChar))
Ecoa = ggplot (Eco, aes (x= reorder(EcoChar, SumIR), y = Connect))+
  geom_bar(stat = "identity", 
           color = "black", 
           fill = colorRampPalette(brewer.pal(9,"Pastel1"))(n)) +
  ylab ("Connectance")+
  theme_bw() +
  coord_flip()+
  theme(axis.title.y=element_blank(),
        axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=12),
        plot.margin = unit(c(0,1,5,31), "mm"))

Ecob = ggplot (BoxPlotEco , aes(x= reorder(EcoChar, SumIR), y = ImpactRisk))+
  geom_boxplot(fill = colorRampPalette(brewer.pal(9,"Pastel1"))(n))+
  stat_boxplot(geom ='errorbar', width =0.5)+ coord_flip()+
  theme_bw()+
  ylab("Impact Risk")+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_text(size=12),
        plot.margin = unit(c(0,1,5,0), "mm"))

Ecoc= ggplot (BoxPlotEco, aes(x= reorder(EcoChar, SumIR), y = LN.IR))+
  geom_boxplot(fill = colorRampPalette(brewer.pal(9,"Pastel1"))(n))+
  stat_boxplot(geom ='errorbar', width =0.5)+ 
  coord_flip()+
  theme_bw()+
  ylab("Impact Rank")+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_text(size=12),
        plot.margin = unit(c(0,5,5,0), "mm"))

BoxEco = grid.arrange(Ecoa, Ecob, Ecoc, ncol = 3, widths = c(6, 4, 4))

pdf ("figures/BoxPlotEco.pdf", width = 12, height = 8)
grid.arrange(Ecoa, Ecob , Ecoc, ncol = 3, widths = c(6, 4, 4))
dev.off()


### Combine all together---------------
pdf ("figures/BoxPlot_arrange.pdf", width = 12, height = 18)
grid.arrange(BoxSector, BoxPressure, BoxEco, ncol = 1, heights =  c(6,7,8))
grid.text("a) Sectors", x = unit(0.02, "npc"), y = unit(0.989, "npc"), just = "left", gp=gpar(fontsize=16))
grid.text("b) Pressures", x = unit(0.02, "npc"), y = unit(0.71, "npc"), just = "left", gp=gpar(fontsize=16))
grid.text("c) Ecological Components", x = unit(0.035, "npc"), y = unit(0.383, "npc"), just = "left", gp=gpar(fontsize=16))
dev.off()



