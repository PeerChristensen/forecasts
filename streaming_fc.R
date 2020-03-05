# forecast streamingforbrug

library(tidyverse)
library(RODBC)
library(lubridate)


credentials <- read_rds("credentials.rds")

channel <- odbcConnect(credentials[1],credentials[2],credentials[3])

#	  ,timeday.TimeOfDayHour
#,timeday.TimeOfDayMinute

query <- "SELECT Date_Key as date,
	  count(*) as n


  FROM [EDW].[fact].[ReaderFact] as reader
  

  where Date_Key >= (SELECT CONVERT(INT, CONVERT(VARCHAR(8), GETDATE()-791, 112)))
and Customer_Key != -1

group by Date_Key"

df <- sqlQuery(channel,query)

df <- df %>%
  as_tibble() %>%
  mutate(date = as.Date(as.character(date), format = "%Y%m%d")) %>%
  filter(date < today()) 

# plot
df %>% 
  ggplot(aes(date,n)) +
  geom_line(size=1, alpha= .8) +
  theme_minimal() 


# tsibble
df_ts <- df  %>% 
  as_tsibble() %>%
  fill_gaps()


# --------------------------------------------------------------------------------------------------
# 2. Box cox transformation (if variance is not constant)
# Update: Box-Cox transformation produces unreliable results

# first we find the right lambda value to stabilize seasonal variation
lambda <- df_ts %>%
  features(n, features = guerrero) %>%
  pull(lambda_guerrero)

# df_ts <- df_ts %>%
#   mutate(Start_bc = box_cox(Start,lambda = lambda))

# --------------------------------------------------------------------------------------------------
# 3. - 5. fit ARIMA model

fit_streaming <- df_ts %>%
  # filter(Start < mean(Start) +2*sd(Start)) %>% # remove outliers
  tsibble::fill_gaps() %>% # fill gaps
  model(arima = ARIMA(box_cox(n,lambda=lambda)))
  #model(arima = ARIMA(n))

report(fit_streaming)

cv <- df_ts %>%
  slice(1:(n()-30)) %>%
  stretch_tsibble(.init = 100, .step = 10)

fc <- cv %>%
  model(
    Drift = RW(n ~ drift()),
    arima_log = ARIMA(log(n)),
    arima = ARIMA(n),
    arima_bc = ARIMA(box_cox(n,lambda=lambda))) %>%
  forecast(h=30)

accuracy(fc, df_ts)

all_fc <- df_ts %>%
  model(
    Drift = RW(n ~ drift()),
    arima_log = ARIMA(log(n)),
    arima = ARIMA(n),
    arima_bc = ARIMA(box_cox(n,lambda=lambda))) %>%
  forecast(h="6 months")

all_fc %>%
  autoplot(df_ts,level = NULL,size=1)

# 6. Check residuals
fit_streaming %>% gg_tsresiduals()

# 7. calculate forecasts
fc_streaming <- fit_streaming %>% 
  forecast(h=as.numeric(as.Date("2020-12-31") - today()+1))

# 8. plot
fc_streaming %>% 
  autoplot(df_ts) +
  geom_smooth() +
  theme_minimal()  +
  #ylim(c(0,1000)) +
  ggtitle("Streaming forecast")

write_csv(fc_streaming,"streaming_fc.csv")
