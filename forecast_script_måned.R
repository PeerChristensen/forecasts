# # -----------------------------------------------------------------------------------------
# # Forecast script
# # Peer Christensen
# # marts 2020
# 
# # forecasts på dagsniveau
# 
# # 1. Antal streaming buckets
# # 2. Antal betalende medlemmer
# # 3. Antal gratis medlemmer (samlet - betalende)
# # 4. Samlet antal medlemmer
# # 5. tilgang
# # 6. afgang
# 
# # mdlige beregninger
# 
# # 7. Omsætning (2.333 * antal betalende)
# # 8. Gennemsnitlig streamingcost pr. bog ( (streaming buckets * 29) /100 )
# # 9. Churn rate
# 
# # -----------------------------------------------------------------------------------------
# # Pakker
# # -----------------------------------------------------------------------------------------
# 
library(RODBC)
library(tidyverse)
library(lubridate)
library(forecast)
library(tsibble)
library(openxlsx)
library(emayili)
library(fable)
library(feasts)

theme_set(theme_minimal())
# 
# # -----------------------------------------------------------------------------------------
# # SQL login
# # -----------------------------------------------------------------------------------------
# 
# credentials <- read_rds("C:/Users/pech/Desktop/RProjects/ForecastAbo/credentials.rds")
# 
# channel <- odbcConnect(credentials[1],credentials[2],credentials[3])
# 
# # -----------------------------------------------------------------------------------------
# # Streaming query
# # -----------------------------------------------------------------------------------------
# 
# query1 <- "SELECT Date_Key as Date,
# 	  count(*) as nBuckets
# 
# 
#   FROM [EDW].[fact].[ReaderFact] as reader
# 
# 
#   where Date_Key >= 20180101
# and Customer_Key != -1
# 
# group by Date_Key
# order by Date_Key"
# 
# # -----------------------------------------------------------------------------------------
# # Medlemsantal query
# # -----------------------------------------------------------------------------------------
# 
# query2 <- "SELECT
# 	SubscriptionMembersDate_Key as Date,
# 	PaidSubscriptionPeriod_Key as Period,
# 	sum(SubscriptionMembers) as nSubscribers
# 
# FROM
# 	[EDW].[fact].[SubscriptionMembersFact]
# 
# where SubscriptionMembers > 0
#   and SubscriptionMembersDate_Key >= 20180101
# 
# group by SubscriptionMembersDate_Key, PaidSubscriptionPeriod_Key
# order by SubscriptionMembersDate_Key"
# 
# # -----------------------------------------------------------------------------------------
# # Tilgang-afgang query
# # -----------------------------------------------------------------------------------------
# 
# query3 <- "SELECT
# 	[EventDate_Key] as Date,
# 	et.CustomerEventTypeName as eventType,
#     count(*) as n
# 
# 
# FROM [EDW].[edw].[CustomerEventFact] f
# INNER JOIN [EDW].[dim].[CustomerEventType] et ON et.CustomerEventType_Key = f.CustomerEventType_Key
# 
# WHERE
# et.CustomerEventTypeID IN (1, 3)
# and EventDate_Key >= 20180101
# 
# group by [EventDate_Key],
# 	et.CustomerEventTypeName
# order by [EventDate_Key],
# 	et.CustomerEventTypeName DESC"
# 
# # -----------------------------------------------------------------------------------------
# # Antal i bestanden der streamer
# # -----------------------------------------------------------------------------------------
# 
# query4 <- "SELECT TOP 1000
# 
# 	 d.[Month] as month
# 	,count(distinct [Customer_Key]) as n
# 
#  FROM [EDW].[fact].[ReaderFact] f
#  INNER JOIN EDW.dim.[Date] d ON d.Date_Key = f.Date_Key
# 
#  where f.Date_Key >= 20180101
#  AND f.IsBucketRead_Key = 1
# 
#  group by d.[Month]
# 
#  order by d.[Month]"
# 
# # -----------------------------------------------------------------------------------------
# # get and preprocess data
# # -----------------------------------------------------------------------------------------
# 
# df1 <- sqlQuery(channel,query1) %>%
#   mutate(month = yearmonth(ymd(Date))) %>%
#   filter(month < yearmonth(today())) %>%
#   group_by(month) %>%
#   summarise(M_Buckets = round(sum(nBuckets) / 1000000,2))
# 
# df2 <- sqlQuery(channel,query2) %>%
#   mutate(Period  = fct_recode(as.factor(Period), Betalende = "1", Gratis = "2")) %>%
#   pivot_wider(names_from = Period, values_from = nSubscribers) %>%
#   drop_na() %>%
#   mutate(Date = ymd(Date)) %>%
#   mutate(month = yearmonth(ymd(Date))) %>%
#   filter(month < yearmonth(today())) %>%
#   group_by(month) %>%
#   mutate(Betalende_start = if_else(Date == min(Date),Betalende,NULL),
#          Betalende_slut = if_else(Date == max(Date),Betalende,NULL),
#          Gratis_start = if_else(Date == min(Date),Gratis,NULL),
#          Gratis_slut = if_else(Date == max(Date),Gratis,NULL)) %>%
#   summarise(Betalende_start = sum(Betalende_start,na.rm=T),
#             Betalende_slut = sum(Betalende_slut,na.rm=T),
#             Gratis_start = sum(Gratis_start,na.rm=T),
#             Gratis_slut = sum(Gratis_slut,na.rm=T))
# 
# df3 <- sqlQuery(channel,query3) %>%
#   mutate(eventType = fct_recode(eventType, Tilgang = "Subscription Start", Afgang = "Subscription End")) %>%
#   pivot_wider(names_from = eventType, values_from = n) %>%
#   mutate(month = yearmonth(ymd(Date))) %>%
#   filter(month < yearmonth(today())) %>%
#   select(-Date) %>%
#   group_by(month) %>%
#   summarise_all(sum)
# 
# 
# df4 <-  sqlQuery(channel,query4) %>%
#   mutate(month = yearmonth(as.character(month))) %>%
#   rename(AntalStreamere = n)
# 
# close(channel)
# 
# df <- df2 %>%
#   left_join(df1) %>%
#   left_join(df3) %>%
#   left_join(df4) %>%
#   mutate(SamletBestand_start = Betalende_start + Gratis_start,
#          SamletBestand_slut = Betalende_slut + Gratis_slut) %>%
#   mutate(month = yearmonth(month))

df <- read_csv2("forecast_data_2020-03-12.csv") %>%
  mutate(M_Buckets = as.numeric(M_Buckets),
         month = yearmonth(month))

df %>%
  pivot_longer(cols = -month) %>%
  ggplot(aes(month,value)) +
    geom_line() +
    facet_wrap(~name,scales="free") +
  theme_minimal()
    
# create tsibble
df_ts <- df  %>% 
  as_tsibble() %>%
  fill_gaps()

# -----------------------------------------------------------------------------------------
# Forecast streaming buckets
# -----------------------------------------------------------------------------------------

# fit ARIMA model
fit_streaming <- df_ts %>%
  model(
    arima = ARIMA(M_Buckets))

# calculate forecasts
fc_streaming <- fit_streaming %>% 
  forecast(h=month(as.Date("2020-12-31")) - month(today())+1) %>%
  hilo() %>% 
  as_tibble() %>%
  rename(M_Buckets_ci80 = `80%`, M_Buckets_ci95 = `95%`) %>%
  select(-.model)

# Plot
(M_Buckets_plot <- fit_streaming %>% 
  forecast(h=month(as.Date("2020-12-31")) - month(today())+1) %>%
  autoplot(df_ts, colour = "blue", size = 1) +
  geom_smooth(fill="blue",level = .95))

# -----------------------------------------------------------------------------------------
# Forecast Antal betalende medlemmer i starten af måneden
# -----------------------------------------------------------------------------------------

# fit ARIMA model
fit_betalende_start <- df_ts %>%
  model(arima = ARIMA(Betalende_start))

# calculate forecasts
fc_betalende_start <- fit_betalende_start %>% 
  forecast(h=month(as.Date("2020-12-31")) - month(today())+1) %>%
  hilo() %>% 
  as_tibble() %>%
  rename(Betalende_start_ci80 = `80%`, Betalende_start_ci95 = `95%`) %>%
  select(-.model)

# Plot
(Betalende_start_plot <- fit_betalende_start %>% 
    forecast(h=month(as.Date("2020-12-31")) - month(today())+1) %>%
    autoplot(df_ts, colour = "blue", size = 1) +
    geom_smooth(fill="blue",level = .95))

# -----------------------------------------------------------------------------------------
# Forecast Antal betalende medlemmer i slutningen af måneden
# -----------------------------------------------------------------------------------------

# fit ARIMA model
fit_betalende_slut <- df_ts %>%
  model(arima = ARIMA(Betalende_slut))

# calculate forecasts
fc_betalende_slut <- fit_betalende_slut %>% 
  forecast(h=month(as.Date("2020-12-31")) - month(today())+1) %>%
  hilo() %>% 
  as_tibble() %>%
  rename(Betalende_slut_ci80 = `80%`, Betalende_slut_ci95 = `95%`) %>%
  select(-.model)

# Plot
(Betalende_slut_plot <- fit_betalende_slut %>% 
    forecast(h=month(as.Date("2020-12-31")) - month(today())+1) %>%
    autoplot(df_ts, colour = "blue", size = 1) +
    geom_smooth(fill="blue",level = .95))

# -----------------------------------------------------------------------------------------
# Forecast Antal medlemmer (samlet) i starten af måneden
# -----------------------------------------------------------------------------------------

# fit ARIMA model
fit_samlet_start <- df_ts %>%
  model(arima = ARIMA(SamletBestand_start))

# calculate forecasts
fc_samlet_start <- fit_samlet_start %>% 
  forecast(h=month(as.Date("2020-12-31")) - month(today())+1) %>%
  hilo() %>% 
  as_tibble() %>%
  rename(SamletBestand_start_ci80 = `80%`, SamletBestand_start_ci95 = `95%`) %>%
  select(-.model)

# Plot
(samlet_start_plot <- fit_betalende_start %>% 
    forecast(h=month(as.Date("2020-12-31")) - month(today())+1) %>%
    autoplot(df_ts, colour = "blue", size = 1) +
    geom_smooth(fill="blue",level = .95))

# -----------------------------------------------------------------------------------------
# Forecast Antal medlemmer (samlet) i slutningen af måneden
# -----------------------------------------------------------------------------------------

# fit ARIMA model
fit_samlet_slut <- df_ts %>%
  model(arima = ARIMA(SamletBestand_slut))

# calculate forecasts
fc_samlet_slut <- fit_samlet_slut %>% 
  forecast(h=month(as.Date("2020-12-31")) - month(today())+1) %>%
  hilo() %>% 
  as_tibble() %>%
  rename(SamletBestand_slut_ci80 = `80%`, SamletBestand_slut_ci95 = `95%`) %>%
  select(-.model)

# Plot
(samlet_slut_plot <- fit_betalende_slut %>% 
    forecast(h=month(as.Date("2020-12-31")) - month(today())+1) %>%
    autoplot(df_ts, colour = "blue", size = 1) +
    geom_smooth(fill="blue",level = .95))

# -----------------------------------------------------------------------------------------
# Forecast Tilgang
# -----------------------------------------------------------------------------------------

# lambda <- df_ts %>%
#   features(Start, features = guerrero) %>%
#   pull(lambda_guerrero)
# 
# cv <- df_ts %>%
#   slice(1:(n()-30)) %>%
#   stretch_tsibble(.init = 100, .step = 10)
# 
# fc <- cv %>%
#   model(
#     Drift = RW(Start ~ drift()),
#     arima_log = ARIMA(log(Start)),
#     arima = ARIMA(Start),
#     arima_bc = ARIMA(box_cox(Start,lambda=lambda))) %>%
#   forecast(h=30)
# 
# accuracy(fc, df_ts)
# 
# all_fc <- df_ts %>% model(
#   Drift = RW(Start ~ drift()),
#   arima_log = ARIMA(log(Start)),
#   arima = ARIMA(Start),
#   arima_bc = ARIMA(box_cox(Start,lambda=lambda))) %>%
#   forecast(h="6 months")
# 
# all_fc %>%
#   autoplot(df_ts,level = NULL,size=1)

lambda <- df_ts %>%
  features(Tilgang, features = guerrero) %>%
  pull(lambda_guerrero)

# fit ARIMA model
fit_tilgang <- df_ts %>%
  #filter(Tilgang < mean(Tilgang) +2*sd(Tilgang)) %>% # remove outliers
  #tsibble::fill_gaps() %>% # fill gaps
  model(arima = ARIMA(box_cox(Tilgang,lambda)))

# 7. calculate forecasts
fc_tilgang <- fit_tilgang %>% 
  forecast(h=month(as.Date("2020-12-31")) - month(today())+1) %>%
  as_tibble() %>%
  select(-.model,-.distribution)

# Plot
(tilgang_plot <- fit_tilgang %>% 
    forecast(h=month(as.Date("2020-12-31")) - month(today())+1) %>%
    autoplot(df_ts, colour = "blue", size = 1) +
    geom_smooth(fill="blue",level = .95))

# -----------------------------------------------------------------------------------------
# Forecast Afgang
# -----------------------------------------------------------------------------------------

#####  
# cross-validation

# lambda <- df_ts %>%
#   features(End, features = guerrero) %>%
#   pull(lambda_guerrero)
# 
# cv <- df_ts %>%
#   filter(End < mean(End) +2*sd(End)) %>% # remove outliers
#   tsibble::fill_gaps() %>% # fill gaps
#   slice(1:(n()-30)) %>%
#   stretch_tsibble(.init = 100, .step = 10)
# 
# fc <- cv %>%
#   model(
#     Drift = RW(End ~ drift()),
#     arima_log = ARIMA(log(End)),
#     arima = ARIMA(End),
#     arima_bc = ARIMA(box_cox(End,lambda=lambda))) %>%
#   forecast(h=30)
# 
# accuracy(fc, df_ts)
# 
# all_fc <- df_ts %>% model(
#   Drift = RW(End ~ drift()),
#   arima_log = ARIMA(log(End)),
#   arima = ARIMA(End),
#   arima_bc = ARIMA(box_cox(End,lambda=lambda))) %>%
#   forecast(h="6 months")
# 
# all_fc %>%
#   autoplot(df_ts,level = NULL,size=1) cross-validation
#####

lambda <- df_ts %>%
  features(Afgang, features = guerrero) %>%
  pull(lambda_guerrero)

# fit ARIMA model
fit_afgang <- df_ts %>%
  #filter(Afgang < mean(Afgang) +2*sd(Afgang)) %>% # remove outliers
  #tsibble::fill_gaps() %>% # fill gaps
  model(arima = ARIMA(box_cox(Afgang,lambda)))

# 7. calculate forecasts
fc_afgang <- fit_afgang %>% 
  forecast(h=month(as.Date("2020-12-31")) - month(today())+1) %>%
  as_tibble() %>%
  select(-.model,-.distribution)

# Plot
(afgang_plot <- fit_afgang %>% 
    forecast(h=month(as.Date("2020-12-31")) - month(today())+1) %>%
    autoplot(df_ts, colour = "blue", size = 1) +
    geom_smooth(fill="blue",level = .95))

# -----------------------------------------------------------------------------------------
# Forecast af antal gratis medlemmer start
# -----------------------------------------------------------------------------------------

# lambda <- df_ts %>%
#   features(Gratis_start, features = guerrero) %>%
#   pull(lambda_guerrero)

# fit ARIMA model
fit_gratis_start <- df_ts %>%
  #filter(Afgang < mean(Afgang) +2*sd(Afgang)) %>% # remove outliers
  #tsibble::fill_gaps() %>% # fill gaps
  model(arima = ARIMA(Gratis_start))

# 7. calculate forecasts
fc_gratis_start <- fit_gratis_start %>% 
  forecast(h=month(as.Date("2020-12-31")) - month(today())+1) %>%
  as_tibble() %>%
  select(-.model,-.distribution)

# Plot
(gratis_start_plot <- fit_gratis_start %>% 
    forecast(h=month(as.Date("2020-12-31")) - month(today())+1) %>%
    autoplot(df_ts, colour = "blue", size = 1) +
    geom_smooth(fill="blue",level = .95))

# -----------------------------------------------------------------------------------------
# Forecast af antal gratis medlemmer start
# -----------------------------------------------------------------------------------------

# lambda <- df_ts %>%
#   features(Gratis_start, features = guerrero) %>%
#   pull(lambda_guerrero)

# fit ARIMA model
fit_gratis_slut <- df_ts %>%
  #filter(Afgang < mean(Afgang) +2*sd(Afgang)) %>% # remove outliers
  #tsibble::fill_gaps() %>% # fill gaps
  model(arima = ARIMA(Gratis_slut))

# 7. calculate forecasts
fc_gratis_slut <- fit_gratis_slut %>% 
  forecast(h=month(as.Date("2020-12-31")) - month(today())+1) %>%
  as_tibble() %>%
  select(-.model,-.distribution)

# Plot
(gratis_slut_plot <- fit_gratis_slut %>% 
    forecast(h=month(as.Date("2020-12-31")) - month(today())+1) %>%
    autoplot(df_ts, colour = "blue", size = 1) +
    geom_smooth(fill="blue",level = .95))

# -----------------------------------------------------------------------------------------
# Beregning af antal gratis medlemmer
# -----------------------------------------------------------------------------------------

Gratis_start <- fc_samlet_start$SamletBestand_start  - fc_betalende_start$Betalende_start

Gratis_slut <- fc_samlet_slut$SamletBestand_slut  - fc_betalende_slut$Betalende_slut

# sammenlign start
tibble(Gratis_start,fc_gratis =fc_gratis_start$Gratis_start,month = 1:length(Gratis_start)) %>%
  pivot_longer(-month) %>%
  ggplot(aes(month,value,colour=name)) +
  geom_line()

# sammenlign slut
tibble(Gratis_slut,fc_gratis =fc_gratis_slut$Gratis_slut,month = 1:length(Gratis_slut)) %>%
  pivot_longer(-month) %>%
  ggplot(aes(month,value,colour=name)) +
  geom_line()

# -----------------------------------------------------------------------------------------
# Beregning af omsætning
# -----------------------------------------------------------------------------------------

revenue <- fc_betalende_slut %>%
  mutate(Revenue = (((Betalende_slut + fc_betalende_start$Betalende_start) /2) * 2.4) * 30) %>%
  select(month,Revenue)

# -----------------------------------------------------------------------------------------
# Beregning af Gennemsnitlig streamingcost pr. enhed
# -----------------------------------------------------------------------------------------

streaming_cost <- fc_streaming %>%
  mutate(Cost = ((M_Buckets*1000000) * 27.5) / 100) %>%
  select(month, Cost)

# -----------------------------------------------------------------------------------------
# Beregning af churn rate
# -----------------------------------------------------------------------------------------

start_medlemmer <- fc_samlet_start %>%
  select(month,SamletBestand_start)

slut_medlemmer <- fc_samlet_slut %>%
  select(month,SamletBestand_slut)

nye_medlemmer <- slut_medlemmer$SamletBestand_slut - start_medlemmer$SamletBestand_start

churn <- fc_afgang %>%
  inner_join(start_medlemmer) %>%
  inner_join(slut_medlemmer) %>%
  mutate(nye_medlemmer = SamletBestand_slut - SamletBestand_start) %>%
  mutate(ChurnRate1 = Afgang / (SamletBestand_start + nye_medlemmer) * 100,
         ChurnRate2 = Afgang / (SamletBestand_start/2 + SamletBestand_slut/2) * 100) %>%
  select(month,ChurnRate1, ChurnRate2)

# -----------------------------------------------------------------------------------------
# Forecast af antal der streamer
# -----------------------------------------------------------------------------------------

# lambda <- df_ts %>%
#   features(AntalStreamere, features = guerrero) %>%
#   pull(lambda_guerrero)

# fit ARIMA model
fit_streamers <- df_ts %>%
  model(arima = ARIMA(AntalStreamere))

# 7. calculate forecasts
fc_streamers <- fit_streamers %>% 
  forecast(h=month(as.Date("2020-12-31")) - month(today())+1) %>%
  as_tibble() %>%
  select(-.model, -.distribution)

# Plot
(streamers_plot <- fit_streamers %>% 
    forecast(h=month(as.Date("2020-12-31")) - month(today())+1) %>%
    autoplot(df_ts, colour = "blue", size = 1) +
    geom_smooth(fill="blue",level = .95))

# -----------------------------------------------------------------------------------------
# validation
# -----------------------------------------------------------------------------------------

# cross-validation

cv_accuracies <- NULL

for (i in names(df[,-1])) {
  
  df_ts_i <- df %>% select(month,i) %>% as_tsibble()
  
  fc <- df_ts_i %>%
    slice(-n()) %>%
    stretch_tsibble(.init = 5,.step=1) %>%
    model(
      arima = ARIMA()
    ) %>%
    forecast(h = "1 month")
  
  acc <- accuracy(fc, df_ts_i)
  cv_accuracies = rbind(cv_accuracies,acc)
  
}

cv_accuracies <- cv_accuracies %>%
  mutate(name = names(df[,-1])) %>%
  select(name, .model, cvMAPE = MAPE)

# simple validation

accuracies <- NULL

for (i in names(df[,-1])) {
  
  df_ts_i <- df %>% select(month,i) %>% as_tsibble()
  
  train <- df_ts_i %>% filter(month <= yearmonth(today()-months(2)))
  
  fit <- train %>%
    model(
      arima = ARIMA()
    )
  
  fc <- fit %>%
    forecast(h = "1 month")
  
  acc <- accuracy(fc, df_ts_i)
  accuracies = rbind(accuracies,acc)
  
}

accuracies <- accuracies %>%
  mutate(name = names(df[,-1])) %>%
  select(name, .model, MAPE)

accuracy_output <- cv_accuracies %>%
  inner_join(accuracies)

# -----------------------------------------------------------------------------------------
# output
# -----------------------------------------------------------------------------------------

forecasts = list(fc_betalende_start,
                 fc_betalende_slut,
                 fc_gratis_start,
                 fc_gratis_slut,
                 fc_samlet_start,
                 fc_samlet_slut,
                 fc_tilgang,
                 fc_afgang,
                 fc_streaming,
                 fc_streamers,
                 revenue,
                 streaming_cost,
                 churn)

output <- forecasts %>% 
  reduce(inner_join,by="month") %>%
  mutate(Gratis_start_beregn. = Gratis_start,
         Gratis_slut_beregn. = Gratis_slut,
         AndelStreamere = AntalStreamere / ((SamletBestand_slut + SamletBestand_start) / 2) * 100) %>%
  select(month:Gratis_slut,
         Gratis_start_beregn.:Gratis_slut_beregn.,
         SamletBestand_start:AntalStreamere,
         AndelStreamere,
         Revenue:ChurnRate2) %>% 
  mutate_if(is.numeric, round,2)


#data <- list(Daily = daily_fc, Monthly = monthly)

filename <- glue::glue("C:/Users/pech/Desktop/RProjects/ForecastAbo/forecasts/forecasts_{today()}.xlsx")

#write.xlsx(output, file = filename)
write.xlsx(output, file = "testfile.xlsx")


# -----------------------------------------------------------------------------------------
# send email
# -----------------------------------------------------------------------------------------

# insert quote
quote_df <- read_csv("C:/Users/pech/Desktop/RProjects/ForecastAbo/famous_quotes.csv") %>%
  top_n(500) %>%
  sample_n(1)

quote <- quote_df$quotes
author <- quote_df$authors %>% str_remove(",")

quote_string <- paste0(quote," - ",author)

email <- envelope() %>%
  from("pech@saxo.com") %>%
  to("pech@saxo.com") %>% 
  cc("pech@saxo.com") %>%
  subject(glue::glue("Forecasts {today()}")) %>%
  text(glue::glue("Hej Peder\n\nHer kommer nye forecasts for Premiumbestanden samt diverse beregninger t.o.m. slutningen af 2020.\n\n\n{quote_string}")) %>% 
  attachment(filename)


creds <- read_rds("C:/Users/pech/Desktop/RProjects/ForecastAbo/email_credentials.rds")

smtp <- server(host     = creds[[1]],
               port     = creds[[2]],
               username = creds[[3]],
               password = creds[[4]],)

smtp(email, verbose = F)


