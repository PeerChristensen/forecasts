

library(tidyverse)
library(RODBC)
library(lubridate)

library(forecast)
library(tsibble)
library(fable)
library(feasts)
library(fpp3)

credentials <- read_rds("credentials.rds")

channel <- odbcConnect(credentials[1],credentials[2],credentials[3])


query <- "SELECT 
	SubscriptionMembersDate_Key,
	PaidSubscriptionPeriod_Key,
	sum(SubscriptionMembers) as num

FROM 
	[EDW].[fact].[SubscriptionMembersFact]

where SubscriptionMembers > 0
  and SubscriptionMembersDate_Key >= 20180101

group by SubscriptionMembersDate_Key, PaidSubscriptionPeriod_Key
order by SubscriptionMembersDate_Key"

df <- sqlQuery(channel, query) %>%
  as_tibble() %>%
  mutate(status = ifelse(PaidSubscriptionPeriod_Key == 1,"Betalende","Gratis"))

df <- df %>%
  mutate(date = as.Date(as.character(SubscriptionMembersDate_Key), format = "%Y%m%d")) %>%
  select(date,num, status)

# udvikling 2018 - i dag
df %>% 
  ggplot(aes(date,num,colour = status)) +
  geom_line(size=1.5) +
  theme_minimal() +
  scale_color_viridis_d(option="C",end=.8)

# forecast antal kunder
df_ts <- df  %>% 
  filter(status=="Betalende") %>%
  as_tsibble()


day <- df_ts %>%
  model(
    arima = ARIMA(log(num))
  ) %>%
  forecast(h = "1 year") %>% 
  autoplot(filter(df_ts, year(date) >= 2017),level = c(80,95),size=1,alpha=.6) +
  theme_minimal()  +
  ylim(c(0,150000)) +
  ggtitle("Daily 1-year forecast")

# monthly
df_ts_mon <- df %>%
  mutate(day = day(df$date)) %>%
  dplyr::filter(day == 1 & status == "Betalende") %>%
  mutate(mon = yearmonth(date)) %>%
  as_tibble() %>%
  dplyr::select(-day,-date,-status) %>%
  as_tsibble()

month <- df_ts_mon %>%
  model(
    arima = ARIMA(log(num))
  ) %>%
  forecast(h = "1 year") %>% 
  autoplot(filter(df_ts_mon,year(mon) >= "2017 Jan"),level = c(80,95),size=1,alpha=.6) +
  theme_minimal() +
  ylim(c(0,150000))  +
  ggtitle("Monthly 1-year forecast")


gridExtra::grid.arrange(day,month,ncol=1)
