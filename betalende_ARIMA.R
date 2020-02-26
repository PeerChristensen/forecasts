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
  filter(status == "Betalende") %>%
  select(date,num) %>%
  as_tsibble()

# --------------------------------------------------------------------------------------------------
# 1. ?
df_ts %>%
  ggplot(aes(date,num)) +
  geom_point()

# --------------------------------------------------------------------------------------------------
# 2. Box cox transformation (if variance is not constant)
# Update: Box-Cox transformation produces unreliable results

# first we find the right lambda value to stabilize seasonal variation
# lambda <- df_ts %>%
#   features(Start, features = guerrero) %>%
#   pull(lambda_guerrero)

# df_ts <- df_ts %>%
#   mutate(Start_bc = box_cox(Start,lambda = lambda))

# --------------------------------------------------------------------------------------------------
# 3. - 5. fit ARIMA model

fit_betalende <- df_ts %>%
  #model(arima = ARIMA(box_cox(Start,lambda=lambda)))
  model(arima = ARIMA(num))

report(fit_betalende)

# 6. Check residuals
fit_betalende %>% gg_tsresiduals()

# 7. calculate forecasts
fc_betalende <- fit_betalende %>% 
  forecast(h="1 year") 

# 8. plot
fc_betalende %>% 
  autoplot(df_ts, level = NULL) +
  geom_smooth() +
  theme_minimal()  +
  ggtitle("Tilgang pr. dag 1 års forecast")

# save
write_csv(fc_betalende,"betalende_fc.csv")


