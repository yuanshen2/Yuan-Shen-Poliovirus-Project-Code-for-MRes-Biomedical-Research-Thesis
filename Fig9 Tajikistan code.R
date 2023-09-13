# Clean the workspace and console
closeAllConnections(); rm(list=ls())
cat("\014"); graphics.off()

setwd("C:/Users/25673/Documents/EpiFliter/R files")
# Folder path for results
folres = paste0("./results/test/")

#A Tajikistan Incidence Plot
Tajikistan_Incidence_Data <- read_csv("~/EpiFliter/Tajikistan Incidence Data.csv")
Tajikistan_Incidence_Data$`Date Onset`<-as.Date(Tajikistan_Incidence_Data$`Date Onset`)
library(ggplot2)
library(incidence)
dat<-Tajikistan_Incidence_Data$`Date Onset`
i<-incidence(dat)
plot(i)
i.7<-incidence(dat,interval="1 week")
plot(i.7)

i.7n <- data.frame(Dates = i.7$dates, Incidence = i.7$counts)

p <-ggplot(i.7n,aes(x=Dates,y=Incidence))+
  geom_col(color = "deepskyblue4", fill = "deepskyblue")+
  
  scale_x_date(date_breaks = "3 months",
               limits =as.Date(c('2020-11-08','2021-12-02')) )
plot(p)
Sys.setlocale("LC_TIME","English")

#Tajikistan Campaign Plot
Tijikistan_Campaign_Continuous_Data <- read_csv("~/EpiFliter/Tijikistan Campaign Continuous Data.csv")
Tijikistan_Campaign_Continuous_Data$`Activity Dates`<-as.Date(Tijikistan_Campaign_Continuous_Data$`Activity Dates`)
library(ggplot2)
paklabs <- ggplot(Tijikistan_Campaign_Continuous_Data,aes(x=`Activity Dates`,y=`Targeted Population`))+
  geom_col()+
  scale_x_date(date_breaks = "3 months",
               limits =as.Date(c('2020-11-08','2021-12-02')) )
paklabs

#Tajikistan EpiFilter Plot
# Main packages
library("EpiEstim")
library("caTools")
# Main functions to run EpiFilter
files.sources = list.files(path = "./main")
for (i in 1:length(files.sources)) {
  source(paste0(c("./main/", files.sources[i]), collapse = ''))
}

#read data
pol_dat<-read.csv("~/EpiFliter/Tajikistan.csv",stringsAsFactors = F)
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
today <- as.Date("2022-11-25")
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
pl2 <- pl2 + labs(x = "Dates", y = expression(paste("R"[t])))


pl2<-pl2+scale_x_date(date_breaks = "3 months",
                      limits =as.Date(c('2020-11-08','2021-12-02')) )

pl2<-pl2+geom_hline(yintercept = 1,linetype="dashed")
pl2<-pl2+scale_y_log10()+coord_cartesian(ylim=c(0.5,10))
pl2

#Tajikistan EpiEstim Plot
#EpiEstim Plot
# add on 0s for last 3 months if last case was more than 6 months ago
Tajikistan_EpiEstim_Data <- read_csv("~/EpiFliter/Tajikistan EpiEstim Data.csv")
Tajikistan_EpiEstim_Data$dates<-as.Date(Tajikistan_EpiEstim_Data$dates)
last_case <-max(Tajikistan_EpiEstim_Data$dates)
today <- as.Date("2022-11-25")
if (last_case<today-6*30){
  
  Ipol <-c(rep(0,90))
  dates <-seq(last_case+1, by=1, length.out = 90)
  temp <-data.frame(dates = dates,I=Ipol)
  Tajikistan_EpiEstim_Data <- rbind(Tajikistan_EpiEstim_Data,temp)
}
library(EpiEstim)
library(ggplot2)
T<-nrow(Tajikistan_EpiEstim_Data)
t_start<-seq(2,T-29)
t_end<-t_start+29
res_weekly<-estimate_R(Tajikistan_EpiEstim_Data,
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

pl3 <- pl3 + labs(x = "Dates", y = expression(paste("R"[t])))

pl3<-pl3+scale_x_date(date_breaks = "3 months",
                      limits =as.Date(c('2020-11-08','2021-12-02')) )

pl3<-pl3+geom_hline(yintercept = 1,linetype="dashed")
pl3<-pl3+scale_y_log10()+coord_cartesian(ylim=c(0.5,10))
pl3

#Combine Plots
library(ggplot2)
library(ggpubr)
figure<-ggarrange(p,paklabs,pl2,pl3,
                  labels=c("A","B","C","D"),
                  ncol=1,nrow=4,
                  align="v")


figure