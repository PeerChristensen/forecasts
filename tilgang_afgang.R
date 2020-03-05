
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
	[EventDate_Key],
	et.CustomerEventTypeName,
    count(*) as n


FROM [EDW].[edw].[CustomerEventFact] f
INNER JOIN [EDW].[dim].[CustomerEventType] et ON et.CustomerEventType_Key = f.CustomerEventType_Key

WHERE 
et.CustomerEventTypeID IN (1, 3)
and EventDate_Key >= 20180101

group by [EventDate_Key],
	et.CustomerEventTypeName
order by [EventDate_Key],
	et.CustomerEventTypeName DESC"

df <- sqlQuery(channel, query) %>%
  as_tibble() %>%
  mutate(date = as.Date(as.character(EventDate_Key), format = "%Y%m%d")) %>%
  filter(date < today()) %>%
  select(date, type = CustomerEventTypeName,n) %>%
  mutate(type = fct_recode(type, Start = "Subscription Start", End = "Subscription End"))

# tilgang - afgang til i dag
df %>% 
  filter(n < 2000) %>% # outliers
  ggplot(aes(date,n,colour = type)) +
  geom_line(size=1, alpha= .8) +
  theme_minimal() +
  scale_color_viridis_d(option="C",end=.8) +
  facet_wrap(~type,ncol=1)

# difference 
df %>%
  pivot_wider(id_cols=date, names_from = type,values_from = n) %>%
  mutate(Difference = Start - End) %>%
  filter(Difference < 1000) %>% # outliers
  pivot_longer(cols = -date) %>%
  mutate(name = fct_relevel(factor(name),"Start", "End", "Difference")) %>%
  ggplot(aes(date,value, colour = name)) +
  theme_minimal() +
  geom_line() +
  facet_wrap(~name, ncol=1) +
  scale_color_viridis_d(option="C",end=.8)

# tsibble
df_ts <- df  %>% 
  pivot_wider(id_cols=date, names_from = type,values_from = n) %>%
  as_tsibble() %>%
  mutate(Difference = Start - End)
  

# ---------------------------------------------------------------------------------------------------
# ARIMA Forecasting

# 1. Plot the data and identify any unusual observations.
# 2. If necessary, transform the data (using a Box-Cox transformation) to stabilise the variance.
# 3. If the data are non-stationary, take first differences of the data until the data are stationary.
# 4. Examine the ACF/PACF: Is an ARIMA(p,d,0) or ARIMA(0,d,q) model appropriate?
# 5. Try your chosen model(s), and use the AICc to search for a better model.
# 6. Check the residuals from your chosen model by plotting the ACF of the residuals, and doing a portmanteau test of the residuals. If they do not look like white noise, try a modified model.
# 7. Once the residuals look like white noise, calculate forecasts.
# 8. plot

# The Hyndman-Khandakar algorithm only takes care of steps 3–5.
# --------------------------------------------------------------------------------------------------


#####################################################################################################

# forecast tilgang

#####################################################################################################

# --------------------------------------------------------------------------------------------------
# 1. (three unusual observations)
df_ts %>%
  ggplot(aes(Date,Start)) +
  geom_line()

# --------------------------------------------------------------------------------------------------
# 2. Box cox transformation (if variance is not constant)
# Update: Box-Cox transformation produces unreliable results

# first we find the right lambda value to stabilize seasonal variation
lambda <- df_ts %>%
  features(Start, features = guerrero) %>%
  pull(lambda_guerrero)

# df_ts <- df_ts %>%
#   mutate(Start_bc = box_cox(Start,lambda = lambda))

# --------------------------------------------------------------------------------------------------
# 3. - 5. fit ARIMA model

fit_tilgang <- df_ts %>%
  filter(Start < mean(Start) +2*sd(Start)) %>% # remove outliers
  tsibble::fill_gaps() %>% # fill gaps
  #model(arima = ARIMA(box_cox(Start,lambda=lambda)))
  model(arima = ARIMA(Start))
  
report(fit_tilgang)

# 6. Check residuals
fit_tilgang %>% gg_tsresiduals()

# 7. calculate forecasts
fc_tilgang <- fit_tilgang %>% 
  forecast(h=as.numeric(as.Date("2020-12-31") - today()+1))

# 8. plot
fc_tilgang %>% 
  autoplot(df_ts, level = NULL) +
  geom_smooth() +
  geom_smooth(method="lm",colour="green") +
  theme_minimal()  +
  ylim(c(0,1000)) +
  ggtitle("Tilgang pr. dag 1 års forecast")


#####################################################################################################

# forecast afgang

#####################################################################################################

# 1. (two unusual observations)
df_ts %>%
  ggplot(aes(date,End)) +
  geom_point()

# --------------------------------------------------------------------------------------------------
# 2. Box cox transformation (if variance is not constant)
# Update: Box-Cox transformation produces unreliable results

# first we find the right lambda value to stabilize seasonal variation
lambda <- df_ts %>%
  features(End, features = guerrero) %>%
  pull(lambda_guerrero)

# df_ts <- df_ts %>%
#   mutate(Start_bc = box_cox(Start,lambda = lambda))

# --------------------------------------------------------------------------------------------------
# 3. - 5. fit ARIMA model

fit_afgang <- df_ts %>%
  filter(End < mean(End) +2*sd(End)) %>% # remove outliers
  tsibble::fill_gaps() %>% # fill gaps
  #model(arima = ARIMA(box_cox(Start,lambda=lambda)))
  model(arima = ARIMA(End))

report(fit_afgang)

# 6. Check residuals
fit_afgang %>% gg_tsresiduals()

# 7. calculate forecasts
fc_afgang <- fit_afgang %>% 
  forecast(h=as.numeric(as.Date("2020-12-31") - today()+1))

# 8. plot
fc_afgang %>% 
  autoplot(df_ts, level = NULL) +
  geom_smooth() +
  geom_smooth(method = "lm",colour="green") +
  theme_minimal()  +
 # ylim(c(0,1000)) +
  ggtitle("Afgang pr. dag 1 års forecast")


#####################################################################################################

# forecast difference

#####################################################################################################

# 1. (three unusual observations)
df_ts %>%
  ggplot(aes(date,Difference)) +
  geom_point()

# --------------------------------------------------------------------------------------------------
# 2. Box cox transformation (if variance is not constant)
# Update: Box-Cox transformation produces unreliable results

# first we find the right lambda value to stabilize seasonal variation
lambda <- df_ts %>%
  features(End, features = guerrero) %>%
  pull(lambda_guerrero)

# df_ts <- df_ts %>%
#   mutate(Start_bc = box_cox(Start,lambda = lambda))

# --------------------------------------------------------------------------------------------------
# 3. - 5. fit ARIMA model

fit_diff <- df_ts %>%
  filter(Difference < mean(Difference) +2*sd(Difference)) %>% # remove outliers
  tsibble::fill_gaps() %>% # fill gaps
  #model(arima = ARIMA(box_cox(Start,lambda=lambda)))
  model(arima = ARIMA(Difference))


report(fit_diff)

augment(fit_diff) %>%
  features(.resid, ljung_box, lag = 7)

# 6. Check residuals
fit_diff %>% gg_tsresiduals()

# 7. calculate forecasts
fc_diff <- fit_diff %>% 
  forecast(h=as.numeric(as.Date("2020-12-31") - today()+1))

# 8. plot
fc_diff %>% 
  autoplot(df_ts, level = NULL) +
  geom_smooth() +
  theme_minimal()  +
   ylim(c(-100,1000)) +
  ggtitle("Difference pr. dag 1 års forecast")


#########################################################################################################

###### join forecasts

# betalende
fc_betalende <- read_csv("betalende_fc.csv")

# join
forecast <- fc_betalende %>% 
  inner_join(fc_tilgang,by="date") %>%
  inner_join(fc_afgang,by="date") %>%
  inner_join(fc_diff, by = "date") %>%
  select(date, Betalende = num, Start, End, Difference)

# save
write_csv2(forecast, "daily_subscribers_fc.csv")

# plot

forecast %>%
  pivot_longer(-date) %>%
  ggplot(aes(date,value)) +
  geom_line() +
  facet_wrap(~name,scales="free")

#########################################################################################################

##### EXTRA ####
tilgang <- df_ts %>%
  model(arima = ARIMA(Start_bc)) %>%
  forecast(h = "1 year") %>% 
  autoplot(df_ts,level = c(80,95),size=1,alpha=.6) +
  geom_smooth() +
  theme_minimal()  +
  #ylim(c(0,1000)) +
  ggtitle("Daily 1-year forecast")

# residuals
df_ts %>% 
  model(arima = ARIMA(log(Start))) %>%
  gg_tsresiduals()

df_ts %>%
  gg_tsdisplay(difference(Start), plot_type='partial')


#the KPSS test for stationarity suggests that differencing is required
df_ts %>%   features(t, unitroot_kpss)

# add prediction intervals to output
df_ts %>% 
  model(arima = ARIMA(log(t))) %>%
  forecast(h="1 year") %>%
  hilo()

### validation
train <- df_ts %>% filter(date <= as.Date("2019-08-01"))


fit <- train %>%
  model(
    Mean = MEAN(t),
    `Naïve` = NAIVE(t),
    `Seasonal naïve` = SNAIVE(t),
    Drift = RW(t ~ drift()),
    arima = ARIMA(log(t))
  )

fc <- fit %>%
  forecast(h = "6 months")

accuracy(fc, df_ts)

fc %>%
  autoplot(filter(df_ts, year(date) >= 2018),level = NULL,size=1) +
  ylim(c(0,1000))