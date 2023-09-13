library(ggplot2)
duration<-as.numeric(Table$Duration_days)
Duration<-data.frame(outbreaks=Table$Outbreaks,
                     duration)

pduration<-ggplot(Duration,aes(x=duration))+
  geom_histogram(color="black", fill="grey",bins=30)+
  labs(x="Duration(days)",y="Number of Outbreaks")+
  theme_classic()

