

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
    arima = ARIMA(log(num)),
    Drift = NAIVE(num ~ drift())
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
    arima = ARIMA(log(num)),
    Drift = NAIVE(num ~ drift())
  ) %>%
  forecast(h = "1 year") %>% 
  autoplot(filter(df_ts_mon,year(mon) >= "2017 Jan"),level = c(80,95),size=1,alpha=.6) +
  theme_minimal() +
  ylim(c(0,150000))  +
  ggtitle("Monthly 1-year forecast")


gridExtra::grid.arrange(day,month,ncol=1)


# check residuals ( mean should be close to 0)
df_ts %>%
  model(arima = ARIMA(log(num))) %>% 
  augment() %>% 
  as_tibble() %>% 
  summarise(m=mean(.resid))

df_ts %>%
  model(arima = ARIMA(log(num))) %>% 
  augment() %>%
  autoplot(.resid)

df_ts %>% 
  model(arima = ARIMA(log(num))) %>% 
  augment() %>% 
  as_tibble() %>% 
  ggplot(aes(.resid)) + 
  geom_histogram()

# check autocorrelation between residuals
df_ts %>% 
  model(arima = ARIMA(log(num))) %>% 
  augment() %>%
  ACF(.resid) %>% 
  autoplot()

# shortcut
df_ts %>% 
  model(arima = ARIMA(log(num))) %>%
  gg_tsresiduals()

# add prediction intervals to output
df_ts %>% 
  model(arima = ARIMA(log(num))) %>%
  forecast(h="1 year") %>%
  hilo()



### validation
#train <- df_ts %>% filter(year(date) <= 2019)
train <- df_ts %>% filter(date <= as.Date("2019-08-01"))


fit <- train %>%
  model(
    Mean = MEAN(num),
    `Naïve` = NAIVE(num),
    `Seasonal naïve` = SNAIVE(num),
    Drift = RW(num ~ drift()),
    arima = ARIMA(log(num))
  )

fc <- fit %>%
  forecast(h = "6 months")

accuracy(fc, df_ts)

fc %>%
  autoplot(filter(df_ts, year(date) >= 2018),level = NULL,size=1)

# cross-validation - not working!
cv <- df_ts %>%
  slice(1:(n()-8)) %>%
  stretch_tsibble(.init = 3, .step = 1)

fc <- cv %>%
  model(
        arima = ARIMA(log(num))) %>%
  forecast(h="1 week")

accuracy(fc, df_ts)

# forecasting with seasonal components
fit <- df_ts %>%
  model(TSLM(num ~ trend() + season()))

fc <- forecast(fit, h = "6 months")

fc %>%
  autoplot(df_ts) +
  ggtitle("Forecasts of beer production using regression") +
  xlab("Year") + ylab("megalitres")

# non linear
fit_trends <- df_ts %>%
  model(
    linear = TSLM(num ~ trend()),
    exponential = TSLM(log(num) ~ trend()),
    piecewise = TSLM(num ~ trend()))


fc_trends <- fit_trends %>% forecast(h = "3 months")
fc_trends %>% autoplot(df_ts,level=NULL)

# ETS
fit <- df_ts %>%
  model(ETS(num))

fc <- fit %>%
  forecast(h="6 months")

fc %>% autoplot(df_ts)


# DIFFERENCING

#the KPSS test for stationarity suggests that differencing is required
df_ts %>%   features(num, unitroot_kpss)

# differencing makes the TS stationary
df_ts %>%
  mutate(diff_num = difference(num)) %>%
  features(diff_num, unitroot_kpss)

# how many differences are required for stationarity?
df_ts %>%
  features(num, unitroot_ndiffs)

# auto select arima model
# p=
#   order of the autoregressive part;
# d=
#   degree of first differencing involved;
# q=
#   order of the moving average part. 

fit <- df_ts %>%
  model(ARIMA(t ~ PDQ(0,0,0)))

report(fit)

fc <- fit %>%
  forecast(h="6 months")

fc %>% autoplot(df_ts)
