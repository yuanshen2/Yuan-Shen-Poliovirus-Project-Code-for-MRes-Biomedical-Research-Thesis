library(readr)
Table <- read_csv("Table.csv")
library(dplyr)
T_with_infections<-mutate(Table,number_of_infections=Table$Number_of_Cases)
View(T_with_infections)

library(ggplot2)
Size<-data.frame(
  outbreaks=T_with_infections$Outbreaks,
  size=T_with_infections$number_of_infections
)
psize<-ggplot(Size,aes(x=size))+
  geom_histogram(color="black", fill="grey")+
  scale_x_log10()+
  labs(x="Infection Size",y="Number of Outbreaks")+
  theme_classic()
psize
library("fitdistrplus")

fw<-fitdist(Size$size,"nbinom")
summary(fw)
fg<-fitdist(Size$size,"gamma")
par(mfrow=c(1,2))
plot.legend<-c("negative binomial","gamma")
denscomp(list(fw,fg),legendtext = plot.legend, main = "", xlab = "Number of Cases")
mtext("A", side = 3, line = 0.5, adj = 0, font = 2, cex = 1.5)
cdfcomp(list(fw,fg),legendtext = plot.legend, main = "", xlab = "Number of Cases")
mtext("B", side = 3, line = 0.5, adj = 0, font = 2, cex = 1.5)
