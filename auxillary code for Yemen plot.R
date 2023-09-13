library(dplyr)
campaigns_Yemen<-filter(SIAs,Country=="YEMEN")
View(campaigns_Yemen)
campaigns_Yemen$mid<-campaigns_Yemen$`Activity Start Date`+
  (campaigns_Yemen$`Activity End Date`-campaigns_Yemen$`Activity Start Date`)/2
campaigns_Yemen$mid<-as.Date(format(campaigns_Yemen$mid,"%Y-%m-%d"))
campaigns<-filter(campaigns_Yemen,between(campaigns_Yemen$mid,as.Date('2021-07-06'),as.Date('2022-10-12')))
View(campaigns)

Dates<-YEMEN_YEM_TAI_1$`Date Onset`
YemenEpiEstim<-group_by(YEMEN_YEM_TAI_1,YEMEN_YEM_TAI_1$`Date Onset`)%>%
  count(`Date Onset`)
View(YemenEpiEstim)
YemenEpiEstim$`Date Onset`<-as.Date(YemenEpiEstim$`Date Onset`)
x<-arrange(YemenEpiEstim,YemenEpiEstim$`Date Onset`)

View(x)
write.csv(x,
          "C:\\Users\\25673\\Documents\\EpiFliter\\YemenEpiEstimData.csv",
          fileEncoding="UTF-8",row.names=FALSE)
