
library(reshape2)
zM<-melt(z)
f_IPV_Ntarget<-na.omit(zM$`f-IPV Ntarget`)
f_IPV_Ntarget<-as.numeric(f_IPV_Ntarget)
f_IPV_Ntarget<-data.frame(f_IPV_Ntarget)
f_IPV_Ntarget<-filter(f_IPV_Ntarget,f_IPV_Ntarget>0)
f_IPV_Ntarget[1:5,1]
a<-replicate(5,'f-IPV')

IPV_Ntarget<-na.omit(zM$`IPV Ntarget`)
IPV_Ntarget<-as.numeric(IPV_Ntarget)
IPV_Ntarget<-data.frame(IPV_Ntarget)
library(dplyr)
IPV_Ntarget<-filter(IPV_Ntarget,IPV_Ntarget>0)
IPV_Ntarget[1:6,1]
b<-replicate(6,'IPV')

IPV_bOPV<-na.omit(zM$`IPV + bOPV Ntarget`)
IPV_bOPV<-as.numeric(IPV_bOPV)
IPV_bOPV<-data.frame(IPV_bOPV)
IPV_bOPV<-filter(IPV_bOPV,IPV_bOPV>0)
IPV_bOPV[1:5,1]
d<-replicate(5,'IPV+bOPV')

mOPV2_Ntarget<-na.omit(zM$`mOPV2 Ntarget`)
mOPV2_Ntarget<-as.numeric(mOPV2_Ntarget)
mOPV2_Ntarget<-data.frame(mOPV2_Ntarget)
mOPV2_Ntarget<-filter(mOPV2_Ntarget,mOPV2_Ntarget>0)
mOPV2_Ntarget[1:81,1]
e<-replicate(81,'mOPV2')

nOPV2_Ntarget<-na.omit(zM$`nOPV2 Ntarget`)
nOPV2_Ntarget<-as.numeric(nOPV2_Ntarget)
nOPV2_Ntarget<-data.frame(nOPV2_Ntarget)
nOPV2_Ntarget<-filter(nOPV2_Ntarget,nOPV2_Ntarget>0)
nOPV2_Ntarget[1:26,1]
f<-replicate(26,'nOPV2')

tOPV_Ntarget<-na.omit(zM$`tOPV Ntarget`)
tOPV_Ntarget<-as.numeric(tOPV_Ntarget)
tOPV_Ntarget<-data.frame(tOPV_Ntarget)
tOPV_Ntarget<-filter(tOPV_Ntarget,tOPV_Ntarget>0)
tOPV_Ntarget[1:11,1]
m<-replicate(11,'tOPV')

Vaccine_type<-c(a,b,d,e,f,m)
Targeted_Population<-c(f_IPV_Ntarget[1:5,1],IPV_Ntarget[1:6,1],IPV_bOPV[1:5,1],
                       mOPV2_Ntarget[1:81,1],nOPV2_Ntarget[1:26,1],tOPV_Ntarget[1:11,1])
Targeted_Population
TP<-data.frame(Vaccine_type,Targeted_Population)

library(ggplot2)
p1<-ggplot(TP, aes(x=Targeted_Population, fill=Vaccine_type, color=Vaccine_type)) +
  geom_histogram(position="identity", alpha=0.5)+
  scale_y_log10()+facet_wrap(~Vaccine_type)


  
  
p1

f_IPV_Ncampaigns<-na.omit(zM$`f-IPV Ncampaigns`)
f_IPV_Ncampaigns<-as.numeric(f_IPV_Ncampaigns)
f_IPV_Ncampaigns<-data.frame(f_IPV_Ncampaigns)
f_IPV_Ncampaigns<-filter(f_IPV_Ncampaigns,f_IPV_Ncampaigns>0)
f_IPV_Ncampaigns[1:7,1]
g<-replicate(7,'f-IPV')

IPV_Ncampaigns<-na.omit(zM$`IPV Ncampaigns`)
IPV_Ncampaigns<-as.numeric(IPV_Ncampaigns)
IPV_Ncampaigns<-data.frame(IPV_Ncampaigns)
IPV_Ncampaigns<-filter(IPV_Ncampaigns,IPV_Ncampaigns>0)
IPV_Ncampaigns[1:10,1]
h<-replicate(10,'IPV')

IPV_bOPV_Ncampaigns<-na.omit(zM$`IPV + bOPV Ncampaigns`)
IPV_bOPV_Ncampaigns<-as.numeric(IPV_bOPV_Ncampaigns)
IPV_bOPV_Ncampaigns<-data.frame(IPV_bOPV_Ncampaigns)
IPV_bOPV_Ncampaigns<-filter(IPV_bOPV_Ncampaigns,IPV_bOPV_Ncampaigns>0)
IPV_bOPV_Ncampaigns[1:5,1]
i<-replicate(5,'IPV+bOPV')

mOPV2_Ncampaigns<-na.omit(zM$`mOPV2 Ncampaigns`)
mOPV2_Ncampaigns<-as.numeric(mOPV2_Ncampaigns)
mOPV2_Ncampaigns<-data.frame(mOPV2_Ncampaigns)
mOPV2_Ncampaigns<-filter(mOPV2_Ncampaigns,mOPV2_Ncampaigns>0)
mOPV2_Ncampaigns[1:81,1]
j<-replicate(81,'mOPV2')

nOPV2_Ncampaigns<-na.omit(zM$`nOPV2 Ncampaigns`)
nOPV2_Ncampaigns<-as.numeric(nOPV2_Ncampaigns)
nOPV2_Ncampaigns<-data.frame(nOPV2_Ncampaigns)
nOPV2_Ncampaigns<-filter(nOPV2_Ncampaigns,nOPV2_Ncampaigns>0)
nOPV2_Ncampaigns[1:26,1]
k<-replicate(26,'nOPV2')

tOPV_Ncampaigns<-na.omit(zM$`tOPV Ncampaigns`)
tOPV_Ncampaigns<-as.numeric(tOPV_Ncampaigns)
tOPV_Ncampaigns<-data.frame(tOPV_Ncampaigns)
tOPV_Ncampaigns<-filter(tOPV_Ncampaigns,tOPV_Ncampaigns>0)
tOPV_Ncampaigns[1:11,1]
l<-replicate(11,'tOPV')

Vaccine_type<-c(g,h,i,j,k,l)
Number_of_Campaigns<-c(f_IPV_Ncampaigns[1:7,1],
                       IPV_Ncampaigns[1:10,1],
                       IPV_bOPV_Ncampaigns[1:5,1],
                       mOPV2_Ncampaigns[1:81,1],
                       nOPV2_Ncampaigns[1:26,1],
                       tOPV_Ncampaigns[1:11,1])
NC<-data.frame(Vaccine_type,Number_of_Campaigns)  
library(ggplot2)
p2<-ggplot(NC, aes(x=Number_of_Campaigns, fill=Vaccine_type, color=Vaccine_type)) +
  geom_histogram(position="identity", alpha=0.5)
p2
