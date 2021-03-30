# Library
library(networkD3)
library(dplyr)
library(reshape2)
library(ggalluvial)
library(ggplot2)

# Make a connection data frame
raw = read.csv("Data/SBS_Pressure_Assessment.csv")

data = raw[!raw$Overlap == "NO", ]

#Remove empty spaces at ecochar string
data$Ecological.Characteristic<-str_trim(data$Ecological.Characteristic, side = c("both"))
unique(data$Ecological.Characteristic)
# Only need the sectors, pressures, eco char and their scores
data = data[ , c(1:8)]
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

data$Resilience.Score = ifelse(data$Resilience == "L", 0.55,
                               ifelse(data$Resilience == "M", 0.06, 
                                      ifelse(data$Resilience == "H", 0.01, NA)))

data$Persistence.Score = ifelse(data$Persistence == "C", 1,
                                ifelse(data$Persistence == "H", 0.55,
                                       ifelse(data$Persistence == "M", 0.06, 
                                              ifelse(data$Persistence == "L", 0.01, NA))))



### Calculate Impact Risk, Recovery Lag and Total Risk
data$ImpactRisk = data$Overlap.Score*data$Frequency.Score*data$DoI.Score



data$Sector[data$Sector == "Non-renewable (oil & gas)"] <- "Non-renewable"



 
data$value<-data$ImpactRisk

data<-data%>%select(Sector, Pressure, Ecological.Characteristic, value)

data<-data[complete.cases(data),]
data$group<-'Group'


ggplot(data,
       aes(y = value, axis1 = Sector, axis2 = Pressure, axis3=Ecological.Characteristic))+
  geom_stratum(width = 1/12, fill = "black", color = "grey")+
  geom_alluvium(aes(fill = value),
                width = 0, knot.pos = 0, reverse = FALSE) +
  guides(fill = FALSE) +
  geom_stratum(width = 1/8, reverse = FALSE) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),
            reverse = FALSE) +
  scale_x_continuous(breaks = 1:3, labels = c("Sector", "Pressure", "Ecological.Characteristics"))


#Trying to reorganize df to source>target
data$id<-as.character(row.names(data))

links<-data%>%transmute(source=Sector,target=Pressure, value=value, linkgroup=id)

links<-rbind(links,data%>%transmute(source=Pressure,target=Ecological.Characteristic, value=value, linkgroup=id))


# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(links$source), as.character(links$target)) %>% 
    unique()
)

# With networkD3, connection must be provided using id, not using real name like in the data dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

# prepare color scale: I give one specific color for each node.
my_color <- 'd3.scaleOrdinal() .domain(data$source) 
                                .range([data$color])'

# Make the Network. I call my colour scale with the colourScale argument
p <- sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget", 
                   Value = "value", NodeID = "name",
                   fontSize = 14, fontFamily = 'Ubuntu')
p

# save the widget
# library(htmlwidgets)
# saveWidget(p, file=paste0( getwd(), "/HtmlWidget/sankeyColor1.html"))