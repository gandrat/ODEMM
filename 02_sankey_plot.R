# Library
library(networkD3)
library(dplyr)
library(ggalluvial)
library(RColorBrewer)
library(stringr)

rm(list=ls()) #removing previous objects

#Load df saved after 00_preprocess.R script
load("Data/SBS_v4.Rda")
unique(data$Sector)
unique(data$Pressure)

data$Pressure<-str_replace(data$Pressure,'Introduction of Contaminating compounds','Contaminating compounds')
unique(data$Pressure)

#Reorganize df to source>target
data$id<-as.character(row.names(data))

#Filter low values for sankey
data<-data%>%filter(ImpactRisk>0.01)

links<-data%>%transmute(source=Sector,target=Pressure, value=ImpactRisk, linkgroup=Pressure)

links<-rbind(links,data%>%transmute(source=Pressure,target=Ecological.Characteristic, value=ImpactRisk, linkgroup=Pressure))

#Grouping links---------
links<-links%>%group_by(source,target, linkgroup)%>%summarise(value=sum(value)*10)

links<-links%>%arrange(value)

summary(links$value)

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- links%>%group_by(source)%>%summarise(value=sum(value))
names(nodes)<-c('name','value')
nodes1<- links%>%group_by(target)%>%summarise(value=sum(value))
names(nodes1)<-c('name','value')
nodes<-rbind(nodes,nodes1)
nodes <- nodes%>%group_by(name)%>%summarise(value=sum(value))%>%arrange(-value)
nodes$group<-'nodes'
nodes<-nodes%>%select(-value)


# With networkD3, connection must be provided using id, not using real name like in the data dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1
links$IDtarget <- match(links$target, nodes$name)-1
nodes<-data.frame(nodes)
nodes$name<-as.character(nodes$name)
nodes[6,1]<-'Input of Organic Matter'

links<-data.frame(links)
links<-links%>%arrange(value,IDsource,IDtarget)



# prepare color scale: I give one specific color for each node.
n <- length(unique(links$linkgroup))
pal <- unlist(mapply(brewer.pal,n,'Set2'))
pal2<-unlist(mapply(brewer.pal,n-nrow(pal),'Dark2'))
pal<-rbind(pal,pal2)

pal<-paste(shQuote(pal), collapse=", ")
dom<-unique(links$linkgroup)
dom<-paste(shQuote(dom), collapse=", ")

my_color <-paste0("d3.scaleOrdinal().domain([",dom,",'nodes']).range([",pal,",'grey'])")



# Make the Network. I call my colour scale with the colourScale argument
p <- sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget", 
                   Value = "value", NodeID = "name",
                   fontSize = 20, fontFamily = 'Times',
                   colourScale=my_color, LinkGroup="linkgroup",NodeGroup = "group",iterations=0)
p

# save the widget
library(htmlwidgets)
saveWidget(p, file='figures/sankeySBS.html')

# save the widget
library(webshot)
webshot("figures/sankeySBS.html","figures/sankey_SBS.jpg", vwidth = 900, vheight = 1000)
