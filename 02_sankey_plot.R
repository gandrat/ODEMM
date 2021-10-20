# Library
library(networkD3)
library(dplyr)
library(ggalluvial)
library(RColorBrewer)

rm(list=ls()) #removing previous objects

#Load df saved after 00_preprocess.R script
load("Data/SBS_v3.Rda")
unique(data$Sector)

#Filter for some sectors-----------
# data<-data%>%filter(Sector=='Tourism/Recreation')
# data<-data%>%filter(Sector=='Fishing')
# data<-data%>%filter(Sector%in%c('Coastal Infrastructure','Land-based Industry','Waste Water Treatment'))
# data$group<-'Group'

#Reorganize df to source>target
data$id<-as.character(row.names(data))

links<-data%>%transmute(source=Sector,target=Pressure, value=ImpactRisk, linkgroup=Pressure)

links<-rbind(links,data%>%transmute(source=Pressure,target=Ecological.Characteristic, value=ImpactRisk, linkgroup=Pressure))


# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(links$source), as.character(links$target)) %>% 
    unique()
)
nodes$group<-'nodes'

# With networkD3, connection must be provided using id, not using real name like in the data dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1


# prepare color scale: I give one specific color for each node.
n <- length(unique(links$linkgroup))
pal <- unlist(mapply(brewer.pal,n,'Set3'))
pal2<-unlist(mapply(brewer.pal,n-12,'Set2'))
pal<-rbind(pal,pal2)
pal<-paste(shQuote(pal), collapse=", ")
dom<-unique(links$linkgroup)
dom<-paste(shQuote(dom), collapse=", ")

my_color <-paste0("d3.scaleOrdinal().domain([",dom,",'nodes']).range([",pal,",'grey'])")

# Make the Network. I call my colour scale with the colourScale argument
p <- sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget", 
                   Value = "value", NodeID = "name",
                   fontSize = 12, fontFamily = 'Ubuntu',
                   colourScale=my_color, LinkGroup="linkgroup",NodeGroup = "group")
p

# save the widget
library(htmlwidgets)
saveWidget(p, file='figures/sankeySBS.html')
