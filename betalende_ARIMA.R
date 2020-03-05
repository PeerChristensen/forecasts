# Betalende ARIMA



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

# tsibble
df_ts <- df %>%
  pivot_wider(id_cols=date,names_from=status,values_from=num) %>% 
  mutate(Samlet = Betalende + Gratis) %>%
  as_tsibble()


# --------------------------------------------------------------------------------------------------
# BETALENDE
# --------------------------------------------------------------------------------------------------

# 1. ?
df_ts %>%
  ggplot(aes(date,Betalende)) +
  geom_point()

# --------------------------------------------------------------------------------------------------
# # 2. Box cox transformation (if variance is not constant)
# # Update: Box-Cox transformation produces unreliable results
# 
# #first we find the right lambda value to stabilize seasonal variation
lambda <- df_ts %>%
  features(Betalende, features = guerrero) %>%
  pull(lambda_guerrero)
# 
# # df_ts <- df_ts %>%
# #   mutate(Start_bc = box_cox(Start,lambda = lambda))

# --------------------------------------------------------------------------------------------------
# 3. - 5. fit ARIMA model

fit_betalende <- df_ts %>%
  #model(arima = ARIMA(box_cox(Start,lambda=lambda)))
  model(arima = ARIMA(Betalende))

report(fit_betalende)

# validate
train <- df_ts %>% filter(date <= as.Date("2019-09-02"))


fit <- train %>%
  model(
    Mean = MEAN(Betalende),
    `Naïve` = NAIVE(Betalende),
    `Seasonal naïve` = SNAIVE(Betalende),
    Drift = RW(Betalende ~ drift()),
    arima = ARIMA(log(Betalende)),
    arima2 = ARIMA(Betalende),
    arima3 = ARIMA(box_cox(Betalende,lambda = lambda)))

fc <- fit %>%
  forecast(h = "6 months")

accuracy(fc, df_ts)

fc %>%
  autoplot(filter(df_ts, year(date) >= 2018),level = NULL,size=1)

# cross-validation - MAPE = 2.26
cv <- df_ts %>%
  slice(1:(n()-30)) %>%
  stretch_tsibble(.init = 100, .step = 10)

fc <- cv %>%
  model(
    arima = ARIMA(Betalende)) %>%
  forecast(h=30)

accuracy(fc, df_ts)

# 6. Check residuals
fit_betalende %>% gg_tsresiduals()

# 7. calculate forecasts
fc_betalende <- fit_betalende %>% 
  forecast(h=as.numeric(as.Date("2020-12-31") - today()+1))

# 8. plot
fc_betalende %>% 
  autoplot(df_ts) +
  geom_smooth() +
  geom_smooth(method="lm",colour = "green") +
  theme_minimal()  +
  ggtitle("Betalende pr. dag forecast")

# --------------------------------------------------------------------------------------------------
# GRATIS
# --------------------------------------------------------------------------------------------------

# 1. ?
df_ts %>%
  ggplot(aes(date,Gratis)) +
  geom_point()

# --------------------------------------------------------------------------------------------------
# 2. Box cox transformation (if variance is not constant)
# Update: Box-Cox transformation produces unreliable results

#first we find the right lambda value to stabilize seasonal variation
# lambda <- df_ts %>%
#   features(num, features = guerrero) %>%
#   pull(lambda_guerrero)

# df_ts <- df_ts %>%
#   mutate(Start_bc = box_cox(Start,lambda = lambda))

# --------------------------------------------------------------------------------------------------
# 3. - 5. fit ARIMA model

fit_gratis <- df_ts %>%
  #model(arima = ARIMA(box_cox(Start,lambda=lambda)))
  model(arima = ARIMA(Gratis))

report(fit_gratis)

# cross-validation - MAPE = 19.6
fc <- cv %>%
  model(
    arima = ARIMA(Gratis)) %>%
  forecast(h=30)

accuracy(fc, df_ts)

# 6. Check residuals
fit_gratis %>% gg_tsresiduals()

# 7. calculate forecasts
fc_gratis <- fit_gratis %>% 
  forecast(h=as.numeric(as.Date("2020-12-31") - today()+1))

# 8. plot
fc_gratis %>% 
  autoplot(df_ts) +
  geom_smooth() +
  geom_smooth(method="lm",colour = "green") +
  theme_minimal()  +
  ggtitle("Gratismedlemmer pr. dag forecast")

# --------------------------------------------------------------------------------------------------
# SAMLET
# --------------------------------------------------------------------------------------------------

# 1. ?
df_ts %>%
  ggplot(aes(date,Samlet)) +
  geom_point()

# --------------------------------------------------------------------------------------------------
# 2. Box cox transformation (if variance is not constant)
# Update: Box-Cox transformation produces unreliable results

# #first we find the right lambda value to stabilize seasonal variation
# lambda <- df_ts %>%
#   features(num, features = guerrero) %>%
#   pull(lambda_guerrero)

# df_ts <- df_ts %>%
#   mutate(Start_bc = box_cox(Start,lambda = lambda))

# --------------------------------------------------------------------------------------------------
# 3. - 5. fit ARIMA model

fit_samlet <- df_ts %>%
  #model(arima = ARIMA(box_cox(Start,lambda=lambda)))
  model(arima = ARIMA(Samlet))

report(fit_samlet)

# cross-validation - MAPE = 3.06
fc <- cv %>%
  model(
    arima = ARIMA(Samlet)) %>%
  forecast(h=30)

accuracy(fc, df_ts)

# 6. Check residuals
fit_samlet %>% gg_tsresiduals()

# 7. calculate forecasts
fc_samlet <- fit_samlet %>% 
  forecast(h=as.numeric(as.Date("2020-12-31") - today()+1))

# 8. plot
fc_samlet %>% 
  autoplot(df_ts) +
  geom_smooth() +
  geom_smooth(method="lm",colour = "green") +
  theme_minimal()  +
  ggtitle("Tilgang pr. dag 1 års forecast")

# --------------------------------------------------------------------------------------------------

fc_streaming <- read_csv("streaming_fc.csv")

forecast <- fc_betalende %>%
  inner_join(fc_gratis,by = "date") %>%
  inner_join(fc_samlet, by = "date") %>%
  inner_join(fc_streaming, by = "date") %>%
  select(Date = date,Betalende_fc = Betalende,Gratis_fc = Gratis,Samlet_fc = Samlet, Streaming_fc = n) %>%
  mutate(Samlet_calc = Betalende_fc + Gratis_fc,
         Tilgang_afgang_calc = Samlet_fc - lag(Samlet_fc)) %>%
  as_tibble() %>%
  mutate_if(is.numeric,round) %>%
  select(Date,Streaming_fc,everything())

forecast %>% 
  pivot_longer(-Date) %>% 
  ggplot(aes(Date,value,colour= name)) + 
  geom_line(size=1.3) + 
  facet_wrap(~name,scale="free",ncol=1) +
  theme_minimal() +
  ggthemes::scale_colour_tableau()

# save
write_csv2(forecast,"streaming_bestand_fc.csv")


