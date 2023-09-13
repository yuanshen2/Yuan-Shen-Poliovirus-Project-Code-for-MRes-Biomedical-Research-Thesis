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
pol_dat<-read.csv("~/EpiFliter/Afghanistan.csv",stringsAsFactors = F)
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


#insert fit
tset = 1:length(dates[2:nday]) # use dates instead
Ismooth[[1]] # predicted incidence
Ismooth[[2]][1:2,] # 95%CrI prediction and check Ismooth[[2]][3:4,] are 50% CrI
Ipol[2:nday] # observed incidence
#plot
Dates<-tset
Median<-Ismooth[[1]]
Observed<-Ipol[2:nday]
credible_interval<-Ismooth[[2]][1:2,]
lci<-credible_interval[1,]
uci<-credible_interval[2,]
mydata<-data.frame(
  Dates,Median,Observed,lci,uci
)
mydata
library(dplyr)
library(ggplot2)

mf<-ggplot(mydata, aes(x = Dates)) +
  geom_line(aes(y = Median), colour="deeppink2",lwd=1) +
  geom_point(mydata,mapping=aes(x = Dates, y = Observed))+
 # scale_color_manual('Number of Cases', values=c('red', 'steelblue')) +
  theme_minimal()

mf <-mf+geom_ribbon(aes(ymin=lci, ymax=uci), fill="deepskyblue",alpha=0.3)

mf <- mf + theme_classic()
mf <- mf + labs(x= "Dates", y = "Daily Case Incidence")
mf

