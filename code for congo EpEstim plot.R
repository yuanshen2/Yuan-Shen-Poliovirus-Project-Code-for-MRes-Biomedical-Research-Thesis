x<-arrange(DEMOCRATIC_REPUBLIC_OF_THE_CONGO_RDC_MAN_3,
           DEMOCRATIC_REPUBLIC_OF_THE_CONGO_RDC_MAN_3$`Date Onset`)
y<-group_by(x,x$`Date Onset`)%>%
  count('Date Onset')
View(y)
write.csv(y, "C:\\Users\\25673\\Documents\\EpiFliter\\CongoEpiEstimdata.csv",
          fileEncoding="UTF-8",row.names=FALSE)
