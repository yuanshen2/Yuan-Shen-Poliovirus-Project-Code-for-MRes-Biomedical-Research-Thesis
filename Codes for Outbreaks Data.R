
Polio_Outbreaks_Data$`Date Onset`<-as.Date(Polio_Outbreaks_Data$`Date Onset`)
library(dplyr)
x<-arrange(Polio_Outbreaks_Data,Polio_Outbreaks_Data$`Place Admin 0`)
Outbreaks<-paste(x$`Place Admin 0`,
                 x$`Emergence Group(s)`,
                 sep ="-" )
y<-mutate(x,Outbreaks)
y1<-arrange(y,y$Outbreaks)
mydata<-data.frame(Outbreaks=y1$Outbreaks,Dates=y1$`Date Onset`)
y2<-group_by(mydata,Outbreaks)%>%
  summarise(first_day=min(Dates),
            last_day=max(Dates),
           Duration=last_day-first_day)
        

z<-group_by(y1,y1$Outbreaks)%>%
  count(Outbreaks)

mydata2<-data.frame(Outbreaks=y1$Outbreaks,Ages=y1$`Calculated Age (months)`)
  y3<-group_by(mydata2,Outbreaks)
  #MA<-median(y3$Ages,na.rm=TRUE)
  y3<-y3 %>% tidyr::drop_na(Ages)
  y4<-summarise(y3,'Median Age(months)'=median(Ages))

  y5<-group_by(mydata,Outbreaks)%>%
    summarise(first_day=min(Dates))
  
Outbreaks<-z$Outbreaks
Number_of_Cases<-z$n
Duration_days<-y2$Duration
Median_Age_months<-y4$`Median Age(months)`
Date_of_the_First_Case<-y5$first_day
Table<-data.frame(Outbreaks,Number_of_Cases,Duration_days,#Median_Age_months,
                  Date_of_the_First_Case)
Table <- merge(Table,y4,by="Outbreaks",all.x=TRUE)
View(Table)
write.csv(Table,"C:\\Users\\25673\\Documents\\Polio Outbreaks\\Table.csv",fileEncoding="UTF-8",row.names=FALSE)
allcases<-arrange(Polio_Outbreaks_Data,Polio_Outbreaks_Data$`Date Onset`)
