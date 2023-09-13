library(tidyverse)
library(dplyr)
Pakistan_group_1<-filter(Pakistan, Pakistan$`Place Admin 1`%in%c("PUNJAB","SINDH",
                                                                 "BALOCHISTAN"))
write.csv(Pakistan_group_1,
          "C:\\Users\\25673\\Documents\\EpiFliter\\R files\\Pakistan_PSB.csv",
                        fileEncoding="UTF-8",row.names=FALSE)
Date_Onset<-Pakistan_PSB$`Date Onset`
Calculated_Age_months<-Pakistan_PSB$`Calculated Age (months)`
mydata<-data.frame(Date_Onset,Calculated_Age_months)
mydata$Date_Onset<-as.Date(mydata$Date_Onset)
Pakistan_PSB_Incidence<-arrange(mydata,Date_Onset)
write.csv(Pakistan_PSB_Incidence,"C:\\Users\\25673\\Documents\\EpiFliter\\R files\\Pakistan_PSB_Incidence.csv",
                                 fileEncoding="UTF-8",row.names=FALSE)

Pakistan_group_2<-filter(Pakistan,Pakistan$`Place Admin 1`%in%c("GBALTISTAN","ISLAMABAD",
                                                                "KPAKHTUNKHWA"))
write.csv(Pakistan_group_2,
          "C:\\Users\\25673\\Documents\\EpiFliter\\R files\\Pakistan_GIK.csv",
          fileEncoding="UTF-8",row.names=FALSE)
Date_Onset<-Pakistan_GIK$`Date Onset`
Calculated_Age_months<-Pakistan_GIK$`Calculated Age (months)`
mydata<-data.frame(Date_Onset,Calculated_Age_months)
mydata$Date_Onset<-as.Date(mydata$Date_Onset)
Pakistan_GIK_Incidence<-arrange(mydata,Date_Onset)
write.csv(Pakistan_GIK_Incidence,"C:\\Users\\25673\\Documents\\EpiFliter\\R files\\Pakistan_GIK_Incidence.csv",
          fileEncoding="UTF-8",row.names=FALSE)


