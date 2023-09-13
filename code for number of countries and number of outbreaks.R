library(readr)
Polio_Outbreaks_Data <- read_csv("Polio Outbreaks Data.csv")
unique_countries <- unique(Polio_Outbreaks_Data$`Place Admin 0`)
num_unique_countries <- length(unique_countries)
num_unique_countries


library(readr)
Table <- read_csv("~/Polio Outbreaks/Table.csv")
unique_outbreaks<-unique(Table$Outbreaks)
num_unique_outbreaks<-length(unique_outbreaks)
num_unique_outbreaks
