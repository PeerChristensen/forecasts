# monthly forecast



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
  mutate(type = fct_recode(type, Start = "Subscription Start", End = "Subscription End")) %>%
  mutate(month = yearmonth(date)) %>%
  filter(month != yearmonth(today())) %>%
  group_by(month, type) %>%
  summarise(n = sum(n))

# tilgang - afgang til i dag
df %>% 
  ggplot(aes(month,n,colour = type)) +
  geom_line(size=1, alpha= .8) +
  theme_minimal() +
  scale_color_viridis_d(option="C",end=.8)

# difference 
df %>%
  pivot_wider(id_cols=month, names_from = type,values_from = n) %>%
  mutate(Difference = Start - End) %>%
  pivot_longer(cols = -month) %>%
  mutate(name = fct_relevel(factor(name),"Start", "End", "Difference")) %>%
  ggplot(aes(month,value, colour = name)) +
  theme_minimal() +
  geom_line(size = 1.5) +
  facet_wrap(~name, ncol=1) +
  scale_color_viridis_d(option="C",end=.8)


# tsibble
df_ts <- df  %>% 
  pivot_wider(id_cols=month, names_from = type,values_from = n) %>%
  as_tsibble() %>%
  mutate(Difference = Start - End)


#####################################################################################################

# forecast tilgang

#####################################################################################################

# --------------------------------------------------------------------------------------------------
# 1. (three unusual observations)
df_ts %>%
  ggplot(aes(month,Start)) +
  geom_point()

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
 # filter(Start < mean(Start) +2*sd(Start)) %>% # remove outliers
 # tsibble::fill_gaps() %>% # fill gaps
  model(
    arima = ARIMA(box_cox(Start,lambda=lambda)))

report(fit_tilgang)

# 6. Check residuals
fit_tilgang %>% gg_tsresiduals()

# 7. calculate forecasts
fc_tilgang <- fit_tilgang %>% 
  forecast(h=as.numeric(month(as.Date("2020-12-31")) - month(today())+1)) 

# 8. plot
fc_tilgang %>% 
  autoplot(df_ts) +
  geom_smooth() +
  theme_minimal()  +
  #ylim(c(0,1000)) +
  ggtitle("Tilgang pr. måned 1 års forecast")


#####################################################################################################

# forecast afgang

#####################################################################################################

# 1. (two unusual observations)
df_ts %>%
  ggplot(aes(month,End)) +
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
#  filter(End < mean(End) +2*sd(End)) %>% # remove outliers
 # tsibble::fill_gaps() %>% # fill gaps
  #model(arima = ARIMA(box_cox(Start,lambda=lambda)))
  model(arima = ARIMA(End))

report(fit_afgang)

# 6. Check residuals
fit_afgang %>% gg_tsresiduals()

# 7. calculate forecasts
fc_afgang <- fit_afgang %>% 
  forecast(h="1 year") 

# 8. plot
fc_afgang %>% 
  autoplot(df_ts, level = NULL) +
  geom_smooth() +
  theme_minimal()  +
  # ylim(c(0,1000)) +
  ggtitle("Afgang pr. måned 1 års forecast")


#####################################################################################################

# forecast difference

#####################################################################################################

# 1. (three unusual observations)
df_ts %>%
  ggplot(aes(month,Difference)) +
  geom_point()

# --------------------------------------------------------------------------------------------------
# 2. Box cox transformation (if variance is not constant)
# Update: Box-Cox transformation produces unreliable results

# # first we find the right lambda value to stabilize seasonal variation
# lambda <- df_ts %>%
#   features(End, features = guerrero) %>%
#   pull(lambda_guerrero)

# df_ts <- df_ts %>%
#   mutate(Start_bc = box_cox(Start,lambda = lambda))

# --------------------------------------------------------------------------------------------------
# 3. - 5. fit ARIMA model

fit_diff <- df_ts %>%
 # filter(Difference < mean(Difference) +2*sd(Difference)) %>% # remove outliers
#  tsibble::fill_gaps() %>% # fill gaps
  #model(arima = ARIMA(box_cox(Start,lambda=lambda)))
  model(arima = ARIMA(Difference))

report(fit_diff)

# 6. Check residuals
fit_diff %>% gg_tsresiduals()

# 7. calculate forecasts
fc_diff <- fit_diff %>% 
  forecast(h="1 year") 

# 8. plot
fc_diff %>% 
  autoplot(df_ts, level = NULL) +
  geom_smooth() +
  theme_minimal()  +
  # ylim(c(0,1000)) +
  ggtitle("Difference pr. måned 1 års forecast")

# join
forecast <- fc_tilgang %>%
  inner_join(fc_afgang,by="month") %>%
  inner_join(fc_diff, by = "month") %>%
  select(month, Start, End, Difference)

# save
write_csv2(forecast, "monthly_subscribers_fc.csv")

