#eta=0.05
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
pol_dat<-read.csv("~/EpiFliter/Pakistan.csv",stringsAsFactors = F)
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
Rmin = 0.01; Rmax = 10; eta = 0.05  ## eta is noise for which R(t) takes the random walk over time.The larger it is the more this method is like EpiEstim

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
pl2a <- ggplot(data=mydata,aes(x=Dates,y=Median))+geom_line(size=1)

pl2a <-pl2a+geom_ribbon(aes(ymin=lci, ymax=uci), fill="deepskyblue",alpha=0.3)

pl2a <- pl2a + theme_classic()
pl2a <- pl2a + labs(x= "Dates", y = "Reproduction Number")


pl2a<-pl2a+scale_x_date(date_breaks = "3 months",
                        limits =as.Date(c('2019-06-23','2021-11-20')) )

pl2a<-pl2a+geom_hline(yintercept = 1,linetype="dashed")
pl2a<-pl2a+scale_y_log10()+coord_cartesian(ylim=c(0.5,10))
pl2a


#eta=0.1
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
pol_dat<-read.csv("~/EpiFliter/Pakistan.csv",stringsAsFactors = F)
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
Rmin = 0.01; Rmax = 10; eta = 0.1  ## eta is noise for which R(t) takes the random walk over time.The larger it is the more this method is like EpiEstim

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
pl2b <- ggplot(data=mydata,aes(x=Dates,y=Median))+geom_line(size=1)

pl2b <-pl2b+geom_ribbon(aes(ymin=lci, ymax=uci), fill="deepskyblue",alpha=0.3)

pl2b <- pl2b + theme_classic()
pl2b <- pl2b + labs(x= "Dates", y = "Reproduction Number")


pl2b<-pl2b+scale_x_date(date_breaks = "3 months",
                      limits =as.Date(c('2019-06-23','2021-11-20')) )

pl2b<-pl2b+geom_hline(yintercept = 1,linetype="dashed")
pl2b<-pl2b+scale_y_log10()+coord_cartesian(ylim=c(0.5,10))
pl2b



#eta=0.2
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
pol_dat<-read.csv("~/EpiFliter/Pakistan.csv",stringsAsFactors = F)
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
pl2c <- ggplot(data=mydata,aes(x=Dates,y=Median))+geom_line(size=1)

pl2c <-pl2c+geom_ribbon(aes(ymin=lci, ymax=uci), fill="deepskyblue",alpha=0.3)

pl2c <- pl2c + theme_classic()
pl2c <- pl2c + labs(x= "Dates", y = "Reproduction Number")


pl2c<-pl2c+scale_x_date(date_breaks = "3 months",
                        limits =as.Date(c('2019-06-23','2021-11-20')) )

pl2c<-pl2c+geom_hline(yintercept = 1,linetype="dashed")
pl2c<-pl2c+scale_y_log10()+coord_cartesian(ylim=c(0.5,10))
pl2c

#eta=0.5
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
pol_dat<-read.csv("~/EpiFliter/Pakistan.csv",stringsAsFactors = F)
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
Rmin = 0.01; Rmax = 10; eta = 0.5  ## eta is noise for which R(t) takes the random walk over time.The larger it is the more this method is like EpiEstim

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
pl2d <- ggplot(data=mydata,aes(x=Dates,y=Median))+geom_line(size=1)

pl2d <-pl2d+geom_ribbon(aes(ymin=lci, ymax=uci), fill="deepskyblue",alpha=0.3)

pl2d <- pl2d + theme_classic()
pl2d <- pl2d + labs(x= "Dates", y = "Reproduction Number")


pl2d<-pl2d+scale_x_date(date_breaks = "3 months",
                        limits =as.Date(c('2019-06-23','2021-11-20')) )

pl2d<-pl2d+geom_hline(yintercept = 1,linetype="dashed")
pl2d<-pl2d+scale_y_log10()+coord_cartesian(ylim=c(0.5,10))
pl2d

#eta=1
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
pol_dat<-read.csv("~/EpiFliter/Pakistan.csv",stringsAsFactors = F)
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
Rmin = 0.01; Rmax = 10; eta = 1  ## eta is noise for which R(t) takes the random walk over time.The larger it is the more this method is like EpiEstim

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
pl2e <- ggplot(data=mydata,aes(x=Dates,y=Median))+geom_line(size=1)

pl2e <-pl2e+geom_ribbon(aes(ymin=lci, ymax=uci), fill="deepskyblue",alpha=0.3)

pl2e <- pl2e + theme_classic()
pl2e <- pl2e + labs(x= "Dates", y = "Reproduction Number")


pl2e<-pl2e+scale_x_date(date_breaks = "3 months",
                        limits =as.Date(c('2019-06-23','2021-11-20')) )

pl2e<-pl2e+geom_hline(yintercept = 1,linetype="dashed")
pl2e<-pl2e+scale_y_log10()+coord_cartesian(ylim=c(0.5,10))
pl2e

#Pakistan EpiEstim Data
Pakistan_EpiEstim_Data<-read.csv("~/EpiFliter/Pakistan EpiEstim Data.csv")
Pakistan_EpiEstim_Data$dates<-as.Date(Pakistan_EpiEstim_Data$dates)
last_case <-max(Pakistan_EpiEstim_Data$dates)
today <- as.Date("2022-11-25")
if (last_case<today-6*30){
  
  Ipol <-c(rep(0,90))
  dates <-seq(last_case+1, by=1, length.out = 90)
  temp <-data.frame(dates = dates,I=Ipol)
  Pakistan_EpiEstim_Data <- rbind(Pakistan_EpiEstim_Data,temp)
}
library(EpiEstim)
library(ggplot2)
#Pakistan_EpiEstim_Data$dates<-as.Date(Pakistan_EpiEstim_Data$dates)
T<-nrow(Pakistan_EpiEstim_Data)
t_start<-seq(2,T-29)
t_end<-t_start+29
res_weekly<-estimate_R(Pakistan_EpiEstim_Data,
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
                      limits =as.Date(c('2019-06-23','2021-11-20')) )

pl3<-pl3+geom_hline(yintercept = 1,linetype="dashed")
pl3<-pl3+scale_y_log10()+coord_cartesian(ylim=c(0.5,10))#Missing argument to function call
pl3

#Combined Plot
library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())
figure<-ggarrange(pl2a,pl2b,pl2c,pl2d,pl2e,pl3,
                  labels=c("eta=0.05",
                           "eta=0.1",
                           "eta=0.2",
                           "eta=0.5",
                           "eta=1",
                           "EpiEstim Plot"
                  ),
                  
                  ncol=1,nrow=6,
                  align="v")

figure
