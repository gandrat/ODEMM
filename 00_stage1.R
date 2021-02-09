require(dplyr)
require(readODS)

rm(list=ls()) #removing previous objects

pressures<-read_ods("Data/stage1.ods",sheet='pressures')


ecochar<-read_ods("Data/stage1.ods",sheet='ecochars')


sectors<-read_ods("Data/stage1.ods",sheet='sectors')


secpress<-read_ods("Data/stage1.ods",sheet='secPress_clean')
rownames(secpress)<-secpress$pressure
secpress<-secpress%>%select(-pressure)
lsecpress <- data.frame(rows = rownames(secpress), stack(secpress))
names(lsecpress)<-c('idpress','secpress','idsec')
lsecpress<-lsecpress%>%filter(secpress==1)%>%select(-secpress)

ecopress<-read_ods("Data/stage1.ods",sheet='ecopress_clean')
rownames(ecopress)<-ecopress$ecochar
ecopress<-ecopress%>%select(-ecochar)
lecopress <- data.frame(rows = rownames(ecopress), stack(ecopress))
names(lecopress)<-c('ideco','ecopress','idpress')
lecopress<-lecopress%>%filter(ecopress==1)%>%select(-ecopress)



sececo<-read_ods("Data/stage1.ods",sheet='SecEco_clean')
rownames(sececo)<-sececo$sector
sececo<-sececo%>%select(-sector)

lsececo <- data.frame(rows = rownames(sececo), stack(sececo))
names(lsececo)<-c('idsec','sececo','ideco')
lsececo<-lsececo%>%filter(sececo==1)%>%select(-sececo)

all<-merge(lecopress,lsecpress,by='idpress')
all<-merge(all,lsececo, by=c('ideco','idsec'))


table(all$ideco)
table(all$idsec)
table(all$idpress)


all<-all%>%arrange(idsec,ideco,idpress)

all<-merge(all,sectors,by='idsec')
all<-merge(all,pressures,by='idpress')
all<-merge(all,ecochar,by='ideco')

raw<-all%>%transmute(Sector=sector, Pressure=pressure, Ecological.Characteristic=ecochar,
                             Overlap="S", Overlap.Score=.1,Frequency.Score=.1,DoI.Score=.1,Resilience.Score=.1,Persistence.Score=.1)
save(raw,pressures,sectors,ecochar,file='Data/raw.Rda')

