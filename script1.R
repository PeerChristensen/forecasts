

library(tidyverse)
library(RODBC)
library(lubridate)

library(forecast)
library(tsibble)
library(fable)
library(feasts)

credentials <- read_rds("credentials.rds")

channel <- odbcConnect(credentials[1],credentials[2],credentials[3])

query <- "SELECT  
SubscriptionMembersDate_Key,
sum(SubscriptionMembers) as num
FROM [EDW].[fact].[SubscriptionMembersFact]
where SubscriptionMembers > 0
and SubscriptionMembersDate_Key >= 20180101
group by SubscriptionMembersDate_Key
order by SubscriptionMembersDate_Key"

df <- sqlQuery(channel, query) %>%
  as_tibble()

df <- df %>%
  mutate(date = as.Date(as.character(SubscriptionMembersDate_Key), format = "%Y%m%d")) %>%
  select(date,num)

# udvikling 2018 - i dag
df %>% 
  ggplot(aes(date,num)) +
  geom_line()


# forecast antal kunder
df_ts <- df  %>% 
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

# season plots
df_ts %>% gg_season()


# monthly
df_ts_mon <- df_ts %>%
  mutate(day = day(df$date)) %>%
  dplyr::filter(day == 1) %>%
  mutate(mon = yearmonth(date)) %>%
  as_tibble() %>%
  dplyr::select(-day,-date) %>%
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

# tilgang
#daily
df %>%
  mutate(tilgang = num - lag(num)) %>%
  ggplot(aes(date,tilgang)) +
  geom_line()

#monthly
df %>%
  mutate(day = day(df$date)) %>%
  dplyr::filter(day == 1) %>%
  mutate(mon = yearmonth(date)) %>%
  mutate(num = num - lag(num)) %>%
  ggplot(aes(mon,num)) +
  geom_line()

# tilgang forecast
df_ts_tilg <- df  %>% 
  mutate(tilgang = num - lag(num)) %>%
  as_tsibble()

df_ts_tilg %>%
  model(
    arima = ARIMA(tilgang)
  ) %>%
  forecast(h = "1 year") %>% 
  autoplot(filter(df_ts_tilg, year(date) >= 2017),level = NULL,size=1,alpha=.6) +
  theme_minimal()
