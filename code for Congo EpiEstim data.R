library(dplyr)
campaigns_Congo<-filter(SIAs,Country=="DEMOCRATIC REPUBLIC OF THE CONGO")
View(campaigns_Congo)
campaigns_Congo$mid<-campaigns_Congo$`Activity Start Date`+
  (campaigns_Congo$`Activity End Date`-campaigns_Congo$`Activity Start Date`)/2
campaigns_Congo$mid<-as.Date(format(campaigns_Congo$mid,"%Y-%m-%d"))
campaigns<-filter(campaigns_Congo,between(campaigns_Congo$mid,as.Date('2021-10-09'),as.Date('2022-09-24')))
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