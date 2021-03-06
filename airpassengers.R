
library(tidyverse)
#library(forecast)
#library(tsibble)
library(fable)
#library(feasts)


df_ts <- AirPassengers %>% as_tsibble()
range(df_ts$index)

df_ts %>%
  model(ARIMA(value)) %>% #fabletools
  forecast(h=12) %>%
  autoplot(df_ts)

df_ts2 <- df_ts %>%
  head(26)

df_ts2 %>%
  model(ARIMA(value)) %>%
  forecast(h=12) %>%
  autoplot(df_ts2) 
