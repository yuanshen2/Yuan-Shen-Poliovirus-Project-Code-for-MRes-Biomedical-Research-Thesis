# Clean the workspace and console
closeAllConnections(); rm(list=ls())
cat("\014"); graphics.off()

setwd("C:/Users/25673/Documents/EpiFliter/R files")
# Folder path for results
folres = paste0("./results/test/")

# Main packages
library("EpiEstim")
library("caTools")
# Main functions to run EpiFilter
files.sources = list.files(path = "./main")
for (i in 1:length(files.sources)) {
  source(paste0(c("./main/", files.sources[i]), collapse = ''))
}

#read data
pol_dat<-read.csv("~/EpiFliter/DEMOCRATIC REPUBLIC OF THE CONGO-RDC-MAN-3.csv",stringsAsFactors = F)
#format dates
pol_dat$Date.Onset<-as.Date(pol_dat$Date.Onset)
pol_dat$Date.Onset<-as.Date(pol_dat$Date.Onset,format = "%d/%m/%Y")


#compute daily incidence
library(incidence)
Ipol_full <- incidence(pol_dat$Date.Onset,1)
Ipol <-Ipol_full$counts
dates  <- Ipol_full$dates


# add on 0s for last 3 months if last case was more than 6 months ago
last_case <-max(pol_dat$Date.Onset)
today <- as.Date("2022-12-13")
if (last_case<today-6*30){
  Ipol <-c(Ipol,rep(0,90))
  dates <-c(dates,seq(last_case+1, by=1, length.out = 90))
}


# Time series lengths
nday = length(dates); tday = 1:nday

# Approxumate serial interval (10 days) / or generation time
wdist = dgamma(tday, shape = 10, scale = 1) # serial interval mean is 30 days
#var=shape*(scale^2)

# Total infectiousness
Lday = rep(0, nday) 
for(i in 2:nday){
  # Total infectiousness
  Lday[i] = sum(Ipol[seq(i-1, 1, -1)]*wdist[1:(i-1)])    
}

######################################################################
## EpiFilter: provides formally smoothed and exact estimates
# Method based on Bayesian recursive filtering and smoothing
######################################################################

# Setup grid and noise parameters
Rmin = 0.01; Rmax = 10; eta = 0.2  ## eta is noise for which R(t) takes the random walk over time.The larger it is the more this method is like EpiEstim

# Uniform prior over grid of size m
m = 200; pR0 = (1/m)*rep(1, m)
# Delimited grid defining space of R
Rgrid = seq(Rmin, Rmax, length.out = m)

# Filtered (causal) estimates as list [Rmed, Rhatci, Rmean, pR, pRup, pstate]
Rfilt = epiFilter(Rgrid, m, eta, pR0, nday, Lday[tday], Ipol[tday], 0.025)
# Causal predictions from filtered estimates [pred predci]
Ifilt = recursPredict(Rgrid, Rfilt[[4]], Lday[tday], Rfilt[[3]], 0.025)

# Smoothed estimates as list of [Rmed, Rhatci, Rmean, qR]
Rsmooth = epiSmoother(Rgrid, m, Rfilt[[4]], Rfilt[[5]], nday, Rfilt[[6]], 0.025)
# Smoothed predictions from filtered estimates [pred predci]
Ismooth = recursPredict(Rgrid, Rsmooth[[4]], Lday[tday], Rsmooth[[3]], 0.025)

# Plot estimates and predictions from filtering
plotEpiFilter(Rfilt[[3]][2:nday], Rfilt[[2]][, 2:nday], Ifilt[[1]], Ifilt[[2]],
              'EpiFilterpolio', Ipol[2:nday], folres, eta)

# Plot estimates and predictions from smoothing
plotEpiFilter(Rsmooth[[3]][2:nday], Rsmooth[[2]][, 2:nday], Ismooth[[1]], Ismooth[[2]],
              'EpiSmoothpolio', Ipol[2:nday], folres, eta)


#once you have run all the code in the polio epi filter

#smoothed median
Rsmooth[[3]][2:nday]

#dates
dates[2:nday]

#50% and 95% quantiles of estimates (Rhat)
Rsmooth[[2]][, 2:nday]#row1 and 2 95%Credible CI; #row2 50% CrI

Dates<-dates[2:nday]
Median<-Rsmooth[[3]][2:nday]
credible_interval<-Rsmooth[[2]][, 2:nday]
lci<-credible_interval[1,]
uci<-credible_interval[2,]
mydata<-data.frame(
  Dates,Median,lci,uci
)
mydata
library(dplyr)
library(ggplot2)
pl2 <- ggplot(data=mydata,aes(x=Dates,y=Median))+geom_line(size=1)

pl2 <-pl2+geom_ribbon(aes(ymin=lci, ymax=uci), fill="deepskyblue",alpha=0.3)

pl2 <- pl2 + theme_classic()
pl2 <- pl2 + labs(x= "Dates", y = "Reproduction Number")


pl2<-pl2+scale_x_date(date_breaks = "3 months",
                      limits =as.Date(c('2021-10-09','2022-09-24')) )

pl2<-pl2+geom_hline(yintercept = 1,linetype="dashed")
pl2<-pl2+scale_y_log10()+coord_cartesian(ylim=c(0.5,10))
pl2

#changing shape and scale
setwd("C:/Users/25673/Documents/EpiFliter/R files")
# Folder path for results
folres = paste0("./results/test/")

# Main packages
library("EpiEstim")
library("caTools")
# Main functions to run EpiFilter
files.sources = list.files(path = "./main")
for (i in 1:length(files.sources)) {
  source(paste0(c("./main/", files.sources[i]), collapse = ''))
}

#read data
pol_dat<-read.csv("~/EpiFliter/DEMOCRATIC REPUBLIC OF THE CONGO-RDC-MAN-3.csv",stringsAsFactors = F)
#format dates
pol_dat$Date.Onset<-as.Date(pol_dat$Date.Onset)
pol_dat$Date.Onset<-as.Date(pol_dat$Date.Onset,format = "%d/%m/%Y")


#compute daily incidence
library(incidence)
Ipol_full <- incidence(pol_dat$Date.Onset,1)
Ipol <-Ipol_full$counts
dates  <- Ipol_full$dates


# add on 0s for last 3 months if last case was more than 6 months ago
last_case <-max(pol_dat$Date.Onset)
today <- as.Date("2022-12-13")
if (last_case<today-6*30){
  Ipol <-c(Ipol,rep(0,90))
  dates <-c(dates,seq(last_case+1, by=1, length.out = 90))
}


# Time series lengths
nday = length(dates); tday = 1:nday

# Approxumate serial interval (10 days) / or generation time
wdist = dgamma(tday, shape = 28, scale = 1.5) # serial interval mean is 30 days
#var=shape*(scale^2)

# Total infectiousness
Lday = rep(0, nday) 
for(i in 2:nday){
  # Total infectiousness
  Lday[i] = sum(Ipol[seq(i-1, 1, -1)]*wdist[1:(i-1)])    
}

######################################################################
## EpiFilter: provides formally smoothed and exact estimates
# Method based on Bayesian recursive filtering and smoothing
######################################################################

# Setup grid and noise parameters
Rmin = 0.01; Rmax = 10; eta = 0.2  ## eta is noise for which R(t) takes the random walk over time.The larger it is the more this method is like EpiEstim

# Uniform prior over grid of size m
m = 200; pR0 = (1/m)*rep(1, m)
# Delimited grid defining space of R
Rgrid = seq(Rmin, Rmax, length.out = m)

# Filtered (causal) estimates as list [Rmed, Rhatci, Rmean, pR, pRup, pstate]
Rfilt = epiFilter(Rgrid, m, eta, pR0, nday, Lday[tday], Ipol[tday], 0.025)
# Causal predictions from filtered estimates [pred predci]
Ifilt = recursPredict(Rgrid, Rfilt[[4]], Lday[tday], Rfilt[[3]], 0.025)

# Smoothed estimates as list of [Rmed, Rhatci, Rmean, qR]
Rsmooth = epiSmoother(Rgrid, m, Rfilt[[4]], Rfilt[[5]], nday, Rfilt[[6]], 0.025)
# Smoothed predictions from filtered estimates [pred predci]
Ismooth = recursPredict(Rgrid, Rsmooth[[4]], Lday[tday], Rsmooth[[3]], 0.025)

# Plot estimates and predictions from filtering
plotEpiFilter(Rfilt[[3]][2:nday], Rfilt[[2]][, 2:nday], Ifilt[[1]], Ifilt[[2]],
              'EpiFilterpolio', Ipol[2:nday], folres, eta)

# Plot estimates and predictions from smoothing
plotEpiFilter(Rsmooth[[3]][2:nday], Rsmooth[[2]][, 2:nday], Ismooth[[1]], Ismooth[[2]],
              'EpiSmoothpolio', Ipol[2:nday], folres, eta)


#once you have run all the code in the polio epi filter

#smoothed median
Rsmooth[[3]][2:nday]

#dates
dates[2:nday]

#50% and 95% quantiles of estimates (Rhat)
Rsmooth[[2]][, 2:nday]#row1 and 2 95%Credible CI; #row2 50% CrI

Dates<-dates[2:nday]
Median<-Rsmooth[[3]][2:nday]
credible_interval<-Rsmooth[[2]][, 2:nday]
lci<-credible_interval[1,]
uci<-credible_interval[2,]
mydata<-data.frame(
  Dates,Median,lci,uci
)
mydata
library(dplyr)
library(ggplot2)
pl4 <- ggplot(data=mydata,aes(x=Dates,y=Median))+geom_line(size=1)

pl4 <-pl4+geom_ribbon(aes(ymin=lci, ymax=uci), fill="deepskyblue",alpha=0.3)


pl4 <- pl4 + theme_classic()
pl4 <- pl4 + labs(x= "Dates", y = "Reproduction Number")

pl4<-pl4+scale_x_date(date_breaks = "3 months",
                      limits =as.Date(c('2021-10-09','2022-09-24')) )

pl4<-pl4+geom_hline(yintercept = 1,linetype="dashed")
pl4<-pl4+scale_y_log10()+coord_cartesian(ylim=c(0.5,10))
pl4

#Incidence Plot
DEMOCRATIC_REPUBLIC_OF_THE_CONGO_RDC_MAN_3$`Date Onset`<-as.Date(DEMOCRATIC_REPUBLIC_OF_THE_CONGO_RDC_MAN_3$`Date Onset`)
library(ggplot2)
library(incidence)
dat<-DEMOCRATIC_REPUBLIC_OF_THE_CONGO_RDC_MAN_3$`Date Onset`
i<-incidence(dat)
plot(i)
i.7<-incidence(dat,interval="1 week")
plot(i.7)

i.7n <- data.frame(Dates = i.7$dates, Incidence = i.7$counts)

p <-ggplot(i.7n,aes(x=Dates,y=Incidence))+geom_col()+
  
  scale_x_date(date_breaks = "3 months",
               limits =as.Date(c('2021-10-09','2022-09-24')) )
plot(p)
Sys.setlocale("LC_TIME","English")

#Campaign Plot
Congo_Campaign_Data$`Campaign Dates`<-as.Date(Congo_Campaign_Data$`Campaign Dates`)

library(ggplot2)
paklabs <- ggplot(Congo_Campaign_Data,aes(x=`Campaign Dates`,y=`Targeted Population`))+
  geom_col()+
  scale_x_date(date_breaks = "3 months",
               limits =as.Date(c('2021-10-09','2022-09-24')) )
paklabs


#EpiEstim Plot
# add on 0s for last 3 months if last case was more than 6 months ago
CongoEpiEstimdata<-read.csv("~/EpiFliter/CongoEpiEstimdata.csv")
CongoEpiEstimdata$dates<-as.Date(CongoEpiEstimdata$dates)
last_case <-max(CongoEpiEstimdata$dates)
today <- as.Date("2022-12-13")
if (last_case<today-6*30){
  
  Ipol <-c(rep(0,90))
  dates <-seq(last_case+1, by=1, length.out = 90)
  temp <-data.frame(dates = dates,I=Ipol)
  CongoEpiEstimdata <- rbind(CongoEpiEstimdata,temp)
}
library(EpiEstim)
library(ggplot2)
#Pakistan_EpiEstim_Data$dates<-as.Date(Pakistan_EpiEstim_Data$dates)
T<-nrow(CongoEpiEstimdata)
t_start<-seq(2,T-29)
t_end<-t_start+29
res_weekly<-estimate_R(CongoEpiEstimdata,
                       method="parametric_si",
                       config=make_config(list(
                         t_start=t_start,
                         t_end=t_end,
                         mean_si=10,
                         std_si=sqrt(10)))
)
plot(res_weekly,"R")

Median<-res_weekly$R$`Median(R)`
lci<-res_weekly$R$`Quantile.0.025(R)`
uci<-res_weekly$R$`Quantile.0.975(R)`
Dates<-res_weekly$dates [res_weekly$R$t_end]
mydata<-data.frame(
  Dates,Median,lci,uci
)
mydata
library(dplyr)
library(ggplot2)
pl3 <- ggplot(data=mydata,aes(x=Dates,y=Median))

pl3 <-pl3+geom_ribbon(aes(ymin=lci, ymax=uci), fill="deepskyblue",alpha=0.3)

pl3<-pl3+geom_line(aes(x=Dates, y=Median),size=1)

pl3 <- pl3 + theme_classic()

pl3 <- pl3 + labs(x= "Dates", y = "Reproduction Number")

pl3<-pl3+scale_x_date(date_breaks = "3 months",
                      limits =as.Date(c('2021-10-09','2022-09-24')) )

pl3<-pl3+geom_hline(yintercept = 1,linetype="dashed")
pl3<-pl3+scale_y_log10()+coord_cartesian(ylim=c(0.5,10))#Missing argument to function call
pl3

#changing mean and sd
res_weekly<-estimate_R(CongoEpiEstimdata,
                       method="parametric_si",
                       config=make_config(list(
                         t_start=t_start,
                         t_end=t_end,
                         mean_si=42,
                         std_si=sqrt(63)))
)
#plot(res_weekly,"R")

Median<-res_weekly$R$`Median(R)`
lci<-res_weekly$R$`Quantile.0.025(R)`
uci<-res_weekly$R$`Quantile.0.975(R)`
Dates<-res_weekly$dates [res_weekly$R$t_end]
mydata<-data.frame(
  Dates,Median,lci,uci
)
#mydata
# library(dplyr)
# library(ggplot2)
pl5 <- ggplot(data=mydata,aes(x=Dates,y=Median))

pl5 <-pl5+geom_ribbon(aes(ymin=lci, ymax=uci), fill="deepskyblue",alpha=0.3)

pl5<-pl5+geom_line(aes(x=Dates, y=Median),size=1)

pl5 <- pl5 + theme_classic()

pl5 <- pl5 + labs(x= "Dates", y = "Reproduction Number")

pl5<-pl5+scale_x_date(date_breaks = "3 months",
                      limits =as.Date(c('2021-10-09','2022-09-24')) )

pl5<-pl5+geom_hline(yintercept = 1,linetype="dashed")
pl5<-pl5+scale_y_log10()+coord_cartesian(ylim=c(0.5,10))
pl5

#Combined plot
library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())
figure<-ggarrange(p,paklabs,pl2,pl3,pl4,pl5,
                  labels=c("Congo Incidence Plot",
                           "Congo Campaign Plot",
                           "Congo Poliomyelitis Reproduction Number Estimation Plot Made with EpiFliter(shape=10,scale=1)",
                           "Congo Poliomyelitis Reproduction Number Estimation Plot Made with EpiEstim(mean=10,SD=sqrt(10))",
                           "Congo Poliomyelitis Reproduction Number Estimation Plot Made with EpiFliter(shape=28,scale=1.5)",
                           "Congo Poliomyelitis Reproduction Number Estimation Plot Made with EpiEstim(mean=42,SD=3*sqrt(7))"
                  ),
                  
                  ncol=1,nrow=6,
                  align="v")

figure


