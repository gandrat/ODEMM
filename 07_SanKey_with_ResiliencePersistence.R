#Sankey to link activities, pressures and ecological components


#Load packages----------
require(d3Network)
require(networkD3)
require(tidyr)
require(dplyr)
require(readxl)
require(htmlwidgets)

rm(list=ls())


#Load data-------
# read the Pressure table that include persistence and resilience scoring
ODEMMfile <- "Data/MA_SBS_Pressures_and_Vulnerabilities.xlsx" 
ODEMM <- readxl::read_xlsx(ODEMMfile)
unique(ODEMM$Sector)
unique(ODEMM$EcoChar)



# Sort Sectors, Pressures and Ecological components by the sum of their impact risk.


#  (this is optional, but it allows to have all the components sorted by impact Risk is the Sankey)
SECTOR<-ODEMM%>% group_by(Sector) %>% summarise(sum = sum(ImpactRisk,na.rm=T))%>% arrange(by_group = desc(sum))
PRESSURE<-group_by(ODEMM, Pressure) %>% summarise(sum = sum(ImpactRisk,na.rm=T))%>% arrange(by_group = desc(sum))
ECOCHAR<-group_by(ODEMM, EcoChar) %>% summarise(sum = sum(ImpactRisk,na.rm=T))%>% arrange(by_group = desc(sum))

# Create Nodes and Links tables needed for the Sankey:------

#Mynodes is a table with the name of all nodes and an ID for each of the nodes
#The group colum is optional, but it allows to control the color of the box (here all the boc have the same color)
Mynodes<-tibble(Name=c(SECTOR$Sector,PRESSURE$Pressure,ECOCHAR$EcoChar),
                 ID=c(1:length(unique(Name)))-1,
                 Group="unique")
#ODEMM_ID is a the ODEMM table with 3 new columns that correspond to the IDs of Sectors, Pressures and Ecological components (based on the IDs in the table Mynodes)
ODEMM_ID <- ODEMM %>% left_join(Mynodes,by=c('Sector'='Name')) %>% rename(ID.Sector=ID) %>% 
  left_join(Mynodes,by=c('Pressure'='Name')) %>% rename(ID.Pressure=ID) %>%
  left_join(Mynodes,by=c('EcoChar'='Name')) %>% rename(ID.EcoChar=ID)

#Mylinks is a table that specify all the links sector-pressure and pressure-ecological component and the associated Impact risk, Recovery year and Resilience/Persistence

#The folowing line create two tables (tibble), one for the first set of links (sector-pressure) and one for the second set of links (pressure-eco. component)
#the RP column correspond to persistance in the first table and resilience in the second table
# then the two table are put together 
Mylinks<-bind_rows( tibble(source=ODEMM_ID$ID.Sector,target=ODEMM_ID$ID.Pressure,ImpactRisk=ODEMM_ID$ImpactRisk,Ryr=as.character(ODEMM_ID$Ryr),PR=as.character(ODEMM_ID$Persistence.Score)),
                tibble(source=ODEMM_ID$ID.Pressure,target=ODEMM_ID$ID.EcoChar,ImpactRisk=ODEMM_ID$ImpactRisk,Ryr=as.character(ODEMM_ID$Ryr),PR=as.character(ODEMM_ID$Resilience.Score)))

#This is optional:
# In Mylinks there is many repeated links. They have the same source and target and same Ryr or PR but then in the Sankey they cross
# This will group the link that are similar and sum the impact risk:
Mylinks2<-Mylinks %>% 
  group_by(source,target,Ryr) %>% 
  summarise(sumIR = sum(ImpactRisk)) # this one keeps the information on revevery year (Ryr)

Mylinks3<-Mylinks %>% 
  group_by(source,target,PR) %>%     # this one keeps the information about persistence/resilience (PR)
  summarise(sumIR = sum(ImpactRisk))

# Just to check that the total impact risk is the same
sum(Mylinks$ImpactRisk)==sum(Mylinks3$sumIR) # if FALSE there is something wrong!


# SANKEY ---------

# 1) Sankey with links colored in function of Recovery year:
#That define the colors palette
#domain() needs to contain all the possible values of Ryr. Here we have 9 diferent possibilities but THIS MAY NEED TO BE ADAPTED TO EACH CASE STUDY 
# range() contain the colors. To generate a gradient of color for a diferent number of possibilities use : c(colorRampPalette(c("#1b98e0", "red"))(length(unique(Mylinks$Ryr))),"#C4C4C4")

my_color_Ryr <-'d3.scaleOrdinal() .domain(["2","7","12","56","61","101","106","110","155","unique"]) .range(["#1B98E0","#3785C3","#5471A8","#705F8C","#8D4C70","#A93853","#C62638","#E2121C","#FF0000","#C4C4C4"])'
RYR <-sankeyNetwork(Links = Mylinks2, Nodes = Mynodes, Source = "source",
                     Target = "target", Value = "sumIR", NodeID = "Name",units = "TWh", fontSize = 18, nodeWidth = 30,
                     colourScale = my_color_Ryr, LinkGroup="Ryr", NodeGroup="Group",iterations = 0)

RYR

# Save Sankey in html
saveWidget(RYR, file='sankey_plots/Sankey_with_RecoveryYears_SBS.html')

# quick legend in a different window (haven't manage to add the legend on the Sankey)
my_categories <- c(sort(unique(as.numeric(Mylinks$Ryr))),"unique")
my_colours <-c(colorRampPalette(c("#1b98e0", "red"))(length(unique(Mylinks$Ryr))),"#C4C4C4")
X11();plot(1,1,type="n",axes=FALSE,xlab="",ylab="")
legend(x=0.6,y=1.4,legend=paste(my_categories[1:9],"years"),fill=my_colours[1:9],border=FALSE,cex=2.5,bty="n")



# 2) Sankey with links colored in function of Persistence (between sectors and pressures) and Resilience (between pressures and eco. components)

my_color_PR <-'d3.scaleOrdinal() .domain(["0.01","0.06","0.55","1","unique"]) .range(["#3785C3","#796E97","#BC576B","#FF4040","#C4C4C4"])'
# Sankey with Persistense and resilience scoring
PR <- sankeyNetwork(Links = Mylinks3, Nodes = Mynodes, Source = "source",
                    Target = "target", Value = "sumIR", NodeID = "Name",units = "TWh", fontSize = 20, nodeWidth = 30,
                    colourScale = my_color_PR, LinkGroup="PR", NodeGroup="Group",iterations = 0)

PR
# 
# # legend
# my_categories <- c("0.01","0.06","0.55","1")
# my_colours <-c("#3785C3","#796E97","#BC576B","#FF4040")
# X11();plot(1,1,type="n",axes=FALSE,xlab="",ylab="")
# legend(x=0.6,y=1.4,legend=my_categories,fill=my_colours,border=FALSE,cex=2.5,bty="n")
# 
