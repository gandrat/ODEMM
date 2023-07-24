require(readODS)
require(dplyr)
require(tidyverse)

#Load data (Libreoffice spreadsheet format)-----------
raw = read_ods("Data/trindade_v1.ods")

# Remove non-existent links
data = raw[!raw$Overlap == "NO", ]

# Score each rating according to Knights et al. 2015-------
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

### Calculate Impact Risk, Recovery Lag and Total Risk
data$ImpactRisk = data$Overlap.Score*data$Frequency.Score*data$DoI.Score

data<-data[complete.cases(data),]

#Set factor levels and renaming categories--------------
data$Overlap<-factor(data$Overlap,levels = c('S','L','W'))
data$Frequency<-factor(data$Frequency,levels = c('R','O','C','P'))
data$DoI<-factor(data$DoI,levels = c('L','C','A'))

data$Sector[data$Sector == "Non-renewable (oil & gas)"] <- "Non-renewable"

data$Pressure<-fct_reorder(data$Pressure, data$idpress)

data$Ecological.Characteristic<-fct_reorder(data$Ecological.Characteristic, data$ideco)

data$Sector<-fct_reorder(data$Sector, data$idsec)

data<-data%>%mutate(Confidence=str_replace(Confidence,'1a','No Specific Expertise'))
data<-data%>%mutate(Confidence=str_replace(Confidence,'1b','Specific Expertise'))
data<-data%>%mutate(Confidence=str_replace(Confidence,'2a','Global Literature'))
data<-data%>%mutate(Confidence=str_replace(Confidence,'2b','Regional Literature'))
data<-data%>%mutate(Confidence=str_replace(Confidence,'3','Data Regional - Monitoring'))
data$Confidence<-factor(data$Confidence,levels=c('No Specific Expertise','Specific Expertise',
                                                 'Global Literature','Regional Literature',
                                                 'Data Regional - Monitoring'))

save(data,file='Data/trindade_v1.Rda')
