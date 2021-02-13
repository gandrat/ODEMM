#Script made to transform raw spreadsheet into DF used to create ODEMM horrendograms
#Made by Tiago Gandra <prof.tiago.gandra@gmail.com>

require(dplyr)
require(readODS)

rm(list=ls()) #removing previous objects

#Loading tables from ODS spreadsheet
pressures<-read_ods("Data/stage1.ods",sheet='pressures')
ecochar<-read_ods("Data/stage1.ods",sheet='ecochars')
sectors<-read_ods("Data/stage1.ods",sheet='sectors')


secpress<-read_ods("Data/stage1.ods",sheet='secPress_clean')
rownames(secpress)<-secpress$pressure
secpress<-secpress%>%select(-pressure)


ecopress<-read_ods("Data/stage1.ods",sheet='ecopress_clean')
rownames(ecopress)<-ecopress$ecochar
ecopress<-ecopress%>%select(-ecochar)

sececo<-read_ods("Data/stage1.ods",sheet='SecEco_clean')
rownames(sececo)<-sececo$sector
sececo<-sececo%>%select(-sector)

#Transforming DF in long tables with relationships
lsecpress <- data.frame(rows = rownames(secpress), stack(secpress))
names(lsecpress)<-c('idpress','secpress','idsec')
# lsecpress<-lsecpress%>%filter(secpress==1)%>%select(-secpress)

lecopress <- data.frame(rows = rownames(ecopress), stack(ecopress))
names(lecopress)<-c('ideco','ecopress','idpress')
# lecopress<-lecopress%>%filter(ecopress==1)%>%select(-ecopress)

lsececo <- data.frame(rows = rownames(sececo), stack(sececo))
names(lsececo)<-c('idsec','sececo','ideco')
# lsececo<-lsececo%>%filter(sececo==1)%>%select(-sececo)


#Merging long tables into one table
all<-merge(lecopress,lsecpress,by='idpress')
all<-merge(all,lsececo, by=c('ideco','idsec'))

all<-all%>%arrange(idsec,idpress,ideco)

#Adding names to table (instead of pressures, ecochar and sectors codes)
all<-merge(all,sectors,by='idsec')
all<-merge(all,pressures,by='idpress')
all<-merge(all,ecochar,by='ideco')

stage2<-all%>%transmute(idsec=as.numeric(substring(idsec,2,3)), sector=sector, idpress=as.numeric(substring(idpress,2,3)), pressure=pressure, 
                        ideco=as.numeric(substring(ideco,2,3)), ecochar=ecochar, 
                        overlap= case_when(secpress==1 & secpress==1 & sececo==1 ~ 'S'))

stage2[is.na(stage2)]<-'NO'


stage2<-stage2%>%arrange(idsec,idpress,ideco)

nrow(stage2%>%filter(overlap=='S'))
#Saving table stage2
write.csv(stage2, "output_data/stage2.csv")

#Creating fake values for scoring
raw<-stage2%>%filter(overlap=='S')%>%
  transmute(Sector=sector, Pressure=pressure, Ecological.Characteristic=ecochar,
                             Overlap=overlap, Overlap.Score=.1,Frequency.Score=.1,DoI.Score=.1,Resilience.Score=.1,Persistence.Score=.1)

#Saving data to use in shiny app
save(raw,file='Data/raw.Rda')

