library(tidyr)
library(dplyr)
#Nigeria_NC
dates<-Nigeria_NC_Incidence$Date_Onset
Nigeria_NC_Estim<-group_by(Nigeria_NC_Incidence,Nigeria_NC_Incidence$Date_Onset)%>%
  count(Nigeria_NC_Incidence$Date_Onset)
dates<-Nigeria_NC_Estim$`Nigeria_NC_Incidence$Date_Onset`
I<-Nigeria_NC_Estim$n
df<-data.frame(dates,I)
df$dates<-as.Date(df$dates)
df_complete <- df %>%
  complete(dates = seq(from = min(df$dates), to = max(df$dates), by = "day")) %>%
  replace_na(list(I = 0))
write.csv(df_complete,"C:\\Users\\25673\\Documents\\EpiFliter\\R files\\Nigeria_NC_EpiEstim.csv",
          fileEncoding="UTF-8",row.names=FALSE)

#Nigeria_S
dates<-Nigeria_S_Incidence$Date_Onset
Nigeria_S_Estim<-group_by(Nigeria_S_Incidence,Nigeria_S_Incidence$Date_Onset)%>%
  count(Nigeria_S_Incidence$Date_Onset)
dates<-Nigeria_S_Estim$`Nigeria_S_Incidence$Date_Onset`
I<-Nigeria_S_Estim$n
df<-data.frame(dates,I)
df$dates<-as.Date(df$dates)
df_complete <- df %>%
  complete(dates = seq(from = min(df$dates), to = max(df$dates), by = "day")) %>%
  replace_na(list(I = 0))
write.csv(df_complete,"C:\\Users\\25673\\Documents\\EpiFliter\\R files\\Nigeria_S_EpiEstim.csv",
          fileEncoding="UTF-8",row.names=FALSE)
#Nigeria_NW
dates<-Nigeria_NW_Incidence$Date_Onset
Nigeria_NW_Estim<-group_by(Nigeria_NW_Incidence,Nigeria_NW_Incidence$Date_Onset)%>%
  count(Nigeria_NW_Incidence$Date_Onset)
dates<-Nigeria_NW_Estim$`Nigeria_NW_Incidence$Date_Onset`
I<-Nigeria_NW_Estim$n
df<-data.frame(dates,I)
df$dates<-as.Date(df$dates)
df_complete <- df %>%
  complete(dates = seq(from = min(df$dates), to = max(df$dates), by = "day")) %>%
  replace_na(list(I = 0))
write.csv(df_complete,"C:\\Users\\25673\\Documents\\EpiFliter\\R files\\Nigeria_NW_EpiEstim.csv",
          fileEncoding="UTF-8",row.names=FALSE)

#Nigeria_NE
dates<-Nigeria_NE_Incidence$Date_Onset
Nigeria_NE_Estim<-group_by(Nigeria_NE_Incidence,Nigeria_NE_Incidence$Date_Onset)%>%
  count(Nigeria_NE_Incidence$Date_Onset)
dates<-Nigeria_NE_Estim$`Nigeria_NE_Incidence$Date_Onset`
I<-Nigeria_NE_Estim$n
df<-data.frame(dates,I)
df$dates<-as.Date(df$dates)
df_complete <- df %>%
  complete(dates = seq(from = min(df$dates), to = max(df$dates), by = "day")) %>%
  replace_na(list(I = 0))
write.csv(df_complete,"C:\\Users\\25673\\Documents\\EpiFliter\\R files\\Nigeria_NE_EpiEstim.csv",
          fileEncoding="UTF-8",row.names=FALSE)
