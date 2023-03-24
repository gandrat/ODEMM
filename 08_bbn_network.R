# Script for Bayesian Belief Network (BBN) conceptual model

require(dplyr)
require(ggplot2)

rm(list=ls()) #removing previous objects


#Set the theme for plots----------
theme_set(
  theme_bw(base_size = 10)+
    theme(text=element_text(family="Times"),
          plot.title = element_text(hjust = 0.5, face='bold',size=12))
)

#Load data -----------
load('Data/SBS_v4.Rda')

# Get only main sector and pressure
unique(data$Sector)

df<-data%>%filter(Sector %in% c('Fishing', 'Shipping', 'Land-based Industry','Waste Water Treatment'))
unique(df$Sector)

unique(data$Pressure)
df<-df%>%filter(Pressure %in% c('Litter', 'Species Extraction', 'Incidental Loss of spp.','Bycatch',
                                'Introduction of Contaminating compounds','Input of Organic Matter (including N&P)'))
unique(paste0(df$Pressure,df$idpressure))

df<-df%>%mutate(pressure_class=ifelse(Pressure %in% c('Species Extraction', 'Incidental Loss of spp.','Bycatch'),'Loss of species',
                                  ifelse(Pressure %in% c('Introduction of Contaminating compounds','Input of Organic Matter (including N&P)'),'Contaminating compounds',
                                         'Litter')))

df<-df%>%mutate(sector_class=ifelse(Sector %in% c('Land-based Industry','Waste Water Treatment'),'Inland activities',
                                      ifelse(Sector == 'Shipping','Shipping',
                                             'Fishing')))
unique(df$Ecological.Characteristic)
df<-df%>%mutate(ecochar_class=ifelse(Ecological.Characteristic %in% c('Littoral sediment','Littoral rock & reef', 'Mangroves', 'Saltmarshes'),'Inland and littoral',
                                    ifelse(Ecological.Characteristic %in% c('Shallow rock & reef','Shallow sediment'),'Shallow benthic',
                                           ifelse(Ecological.Characteristic %in% c('Shelf rock & reef','Shelf sediment'),'Shelf benthic',
                                                  ifelse(Ecological.Characteristic %in% c('Coastal Pelagic'),'Coastal pelagic',
                                                         ifelse(Ecological.Characteristic %in% c('Shelf Pelagic'),'Shelf pelagic',
                                                                'High mobile species'))))))

view(df%>%select(Ecological.Characteristic, ecochar_class))



dfs<-df%>%select(sector_class, pressure_class, ecochar_class) 

dfs<-unique(dfs)

write_ods(dfs,'output_data/odemm_main_sbs.ods')

# Load ecosystem services-------------
es<-read_ods("Data/bbn_relationship_list.ods")
cices_div<-es$`Ecosystem Service (CICES - Division simplified)`
cices_div<-cices_div[!is.na(cices_div)]
es_odemm<-es$`Ecosystem Service (Debby)
es_odemm<-es_odemm[!is.na(es_odemm)]

#Getting all combination - Draft
bbn1<-as.data.frame(expand.grid(dfs,es_odemm))
dfs%>%expand(es_odemm)
