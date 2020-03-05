# -----------------------------------------------------------------------------------------
# Forecast script
# Peer Christensen
# marts 2020

# forecasts på dagsniveau

# 1. Antal streaming buckets
# 2. Antal betalende medlemmer
# 3. Antal gratis medlemmer (samlet - betalende)
# 4. Samlet antal medlemmer
# 5. tilgang
# 6. afgang

# månedlige beregninger

# 7. Omsætning (2.333 * antal betalende)
# 8. Gennemsnitlig streamingcost pr. bog ( (streaming buckets * 29) /100 )
# 9. Churn rate

# -----------------------------------------------------------------------------------------
# Pakker
# -----------------------------------------------------------------------------------------

library(RODBC)
library(tidyverse)
library(forecast)
library(tsibble)
library(openxlsx)
library(emayili)

# -----------------------------------------------------------------------------------------
# SQL login
# -----------------------------------------------------------------------------------------

credentials <- read_rds("credentials.rds")

channel <- odbcConnect(credentials[1],credentials[2],credentials[3])

# -----------------------------------------------------------------------------------------
# Streaming query
# -----------------------------------------------------------------------------------------

query1 <- "SELECT Date_Key as Date,
	  count(*) as nBuckets


  FROM [EDW].[fact].[ReaderFact] as reader
  

  where Date_Key >= 20180101
and Customer_Key != -1

group by Date_Key
order by Date_Key"

# -----------------------------------------------------------------------------------------
# Medlemsantal query
# -----------------------------------------------------------------------------------------

query2 <- "SELECT 
	SubscriptionMembersDate_Key as Date,
	PaidSubscriptionPeriod_Key as Period,
	sum(SubscriptionMembers) as nSubscribers

FROM 
	[EDW].[fact].[SubscriptionMembersFact]

where SubscriptionMembers > 0
  and SubscriptionMembersDate_Key >= 20180101

group by SubscriptionMembersDate_Key, PaidSubscriptionPeriod_Key
order by SubscriptionMembersDate_Key"

# -----------------------------------------------------------------------------------------
# Tilgang-afgang query
# -----------------------------------------------------------------------------------------

query3 <- "SELECT 
	[EventDate_Key] as Date,
	et.CustomerEventTypeName as eventType,
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

# -----------------------------------------------------------------------------------------
# Antal bestanden der streamer
# -----------------------------------------------------------------------------------------

query4 <- "SELECT TOP 1000  

	 d.[Month] as month
	,count(distinct [Customer_Key]) as n

 FROM [EDW].[fact].[ReaderFact] f
 INNER JOIN EDW.dim.[Date] d ON d.Date_Key = f.Date_Key

 where f.Date_Key >= 20180101
 AND f.IsBucketRead_Key = 1
 
 group by d.[Month]

 order by d.[Month]"

# -----------------------------------------------------------------------------------------
# get and preprocess data
# -----------------------------------------------------------------------------------------

df1 <- sqlQuery(channel,query1)

df2 <- sqlQuery(channel,query2) %>%
  mutate(Period  = fct_recode(as.factor(Period), Paid = "1", Free = "2")) %>%
  pivot_wider(names_from = Period, values_from = nSubscribers)

df3 <- sqlQuery(channel,query3) %>%
  mutate(eventType = fct_recode(eventType, Start = "Subscription Start", End = "Subscription End")) %>%
  pivot_wider(names_from = eventType, values_from = n)

df4 <-  sqlQuery(channel,query4)

close(channel)

df <- df2 %>%
  left_join(df1) %>%
  left_join(df3) %>%
  as_tibble() %>%
  mutate(Date = as.Date(as.character(Date), format = "%Y%m%d")) %>%
  filter(Date < today()) %>%
  mutate(TotalMembers = Paid + Free)

# # check Date gaps
# df %>%
#   mutate(diff = Date - lag(Date, 1) == 1) %>%
#   count(diff)
# 
# # check gaps
# df %>%filter_all(any_vars(is.na(.)))

# create tsibble
df_ts <- df  %>% 
  as_tsibble() %>%
  fill_gaps()

# -----------------------------------------------------------------------------------------
# Forecast streaming buckets
# -----------------------------------------------------------------------------------------

# Box-cox transformation (due to non-constant variance)
lambda <- df_ts %>%
  features(nBuckets, features = guerrero) %>%
  pull(lambda_guerrero)

# fit ARIMA model
fit_streaming <- df_ts %>%
  model(arima = ARIMA(box_cox(nBuckets,lambda=lambda)))

# calculate forecasts
fc_streaming <- fit_streaming %>% 
  forecast(h=as.numeric(as.Date("2020-12-31") - today()+1)) %>%
  hilo() %>% 
  as_tibble() %>%
  rename(nBuckets_ci80 = `80%`, nBuckets_ci95 = `95%`) %>%
  select(-.model)

# -----------------------------------------------------------------------------------------
# Forecast Antal betalende medlemmer
# -----------------------------------------------------------------------------------------

# fit ARIMA model
fit_betalende <- df_ts %>%
  model(arima = ARIMA(Paid))

# calculate forecasts
fc_betalende <- fit_betalende %>% 
  forecast(h=as.numeric(as.Date("2020-12-31") - today()+1)) %>%
  hilo() %>% 
  as_tibble() %>%
  rename(Paid_ci80 = `80%`, Paid_ci95 = `95%`) %>%
  select(-.model)

# slut antal betalende per måned
betalende_måned <- fc_betalende %>%
  group_by(month = month(Date)) %>%
  filter(Date == max(Date)) %>%
  ungroup() %>%
  mutate(month = month.name[month]) %>%
  select(month, Betalende = Paid)

# -----------------------------------------------------------------------------------------
# Forecast Antal medlemmer (samlet)
# -----------------------------------------------------------------------------------------

# fit ARIMA model
fit_samlet <- df_ts %>%
  model(arima = ARIMA(TotalMembers))

# calculate forecasts
fc_samlet <- fit_samlet %>% 
  forecast(h=as.numeric(as.Date("2020-12-31") - today()+1)) %>%
  hilo() %>% 
  as_tibble() %>%
  rename(TotalMembers_ci80 = `80%`, TotalMembers_ci95 = `95%`) %>%
  select(-.model)

# slut antal medlemmer per måned
bestand_måned <- fc_samlet %>%
  group_by(month = month(Date)) %>%
  filter(Date == max(Date)) %>%
  ungroup() %>%
  mutate(month = month.name[month]) %>%
  select(month, Bestand = TotalMembers)

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

# fit ARIMA model
fit_start <- df_ts %>%
  filter(Start < mean(Start) +2*sd(Start)) %>% # remove outliers
  tsibble::fill_gaps() %>% # fill gaps
  model(arima = ARIMA(log(Start)))

# 7. calculate forecasts
fc_start <- fit_start %>% 
  forecast(h=as.numeric(as.Date("2020-12-31") - today()+1)) %>%
  as_tibble() %>%
  select(-.model,-.distribution) %>%
  rename(tilgang = Start)

# tilgang (sum) per måned
tilgang_måned <- fc_start  %>%
  group_by(month = month(Date)) %>%
  summarise(tilgang = sum(tilgang)) %>%
  mutate(month = month.name[month]) 

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

# fit ARIMA model
fit_end <- df_ts %>%
  filter(End < mean(End) +2*sd(End)) %>% # remove outliers
  tsibble::fill_gaps() %>% # fill gaps
  model(arima = ARIMA(End))

# 7. calculate forecasts
fc_end <- fit_end %>% 
  forecast(h=as.numeric(as.Date("2020-12-31") - today()+1)) %>%
  as_tibble() %>%
  select(-.model, -.distribution) %>%
  rename(afgang = End)

# afgang (sum) per måned
afgang_måned <- fc_end  %>%
  group_by(month = month(Date)) %>%
  summarise(afgang = sum(afgang)) %>%
  mutate(month = month.name[month]) 

# -----------------------------------------------------------------------------------------
# Beregning af Samlet antal antal gratis medlemmer
# -----------------------------------------------------------------------------------------

gratis <- fc_samlet$TotalMembers - fc_betalende$Paid

# -----------------------------------------------------------------------------------------
# Beregning af omsætning
# -----------------------------------------------------------------------------------------

revenue <- fc_betalende %>%
  mutate(revenue = Paid * 2.33) %>%
  select(Date,revenue) %>%
  group_by(month =month(Date)) %>%
  summarise(revenue = sum(revenue)) %>%
  mutate(month = month.name[month])
  
# -----------------------------------------------------------------------------------------
# Beregning af Gennemsnitlig streamingcost pr. enhed
# -----------------------------------------------------------------------------------------

streaming_cost <- fc_streaming %>%
  mutate(cost = (nBuckets * 29) / 100) %>%
  select(Date, cost) %>%
  group_by(month =month(Date)) %>%
  summarise(cost = sum(cost)) %>%
  mutate(month = month.name[month])

# -----------------------------------------------------------------------------------------
# Beregning af churn rate
# -----------------------------------------------------------------------------------------

afgang <- fc_end %>%
  group_by(month = month(Date)) %>%
  summarise(afgang = sum(afgang)) %>%
  mutate(month = month.name[month])

start_medlemmer <- fc_samlet %>%
  group_by(month(Date)) %>%
  mutate(min = min(Date)) %>%
  ungroup() %>%
  filter(Date == min) %>%
  mutate(month = month.name[month(min)]) %>%
  select(month, start_medlemmer = TotalMembers)

# NB. samme som bestand_måned
slut_medlemmer <- fc_samlet %>%
  group_by(month(Date)) %>%
  mutate(max = max(Date)) %>%
  ungroup() %>%
  filter(Date == max) %>%
  mutate(month = month.name[month(max)]) %>%
  select(month, slut_medlemmer = TotalMembers)

nye_medlemmer <- slut_medlemmer %>%
  mutate(nye_medlemmer = slut_medlemmer - start_medlemmer$start_medlemmer) %>%
  select(month, nye_medlemmer)

churn <- afgang %>%
  inner_join(start_medlemmer) %>%
  inner_join(slut_medlemmer) %>%
  inner_join(nye_medlemmer) %>%
  mutate(churn_rate1 = afgang / (start_medlemmer + nye_medlemmer) * 100,
         churn_rate2 = afgang / (start_medlemmer/2 + slut_medlemmer/2) * 100) %>%
  select(month,churn_rate1, churn_rate2)

# -----------------------------------------------------------------------------------------
# Forecast af andel der streamer
# -----------------------------------------------------------------------------------------

# df4 %>%
#   mutate(month = yearmonth(as.Date(month))) %>%
#   filter(month != max(month)) %>%
#   ggplot(aes(month,n)) +
#   geom_line()
  
df4_ts <- df4 %>%
   mutate(month = yearmonth(as.Date(month))) %>%
   filter(month != max(month)) %>%
   as_tsibble()

# fit ARIMA model
fit_streamers <- df4_ts %>%
  model(arima = ARIMA(n))

# 7. calculate forecasts
fc_streamers <- fit_streamers %>% 
  forecast(h=as.numeric(month(as.Date("2020-12-31")) - month(today())+1)) %>%
  as_tibble() %>%
  select(-.model, -.distribution)

fc_streamers <- fc_streamers %>%
  as_tibble() %>%
  mutate(month = month.name[month(as.Date(month))]) %>%
  rename(nStreamers = n)

# -----------------------------------------------------------------------------------------
# output
# -----------------------------------------------------------------------------------------

daily_fc <- fc_betalende %>%
  mutate(Free = gratis) %>%
  inner_join(fc_samlet) %>%
  inner_join(fc_start) %>%
  inner_join(fc_end) %>%
  mutate_if(is.numeric, round)

monthly <- revenue %>%
  inner_join(streaming_cost) %>%
  inner_join(churn) %>%
  inner_join(betalende_måned) %>%
  inner_join(bestand_måned) %>%
  inner_join(tilgang_måned) %>%
  inner_join(afgang_måned) %>%
  inner_join(fc_streamers) %>%
  mutate(AndelStreamers = nStreamers / Bestand * 100) %>%
  mutate_at(vars(revenue, cost,Betalende,Bestand,tilgang,afgang,nStreamers), round)
  
data <- list(Daily = daily_fc, Monthly = monthly)

filename <- glue::glue("forecasts/forecasts_{today()}.xlsx")

write.xlsx(data, file = filename)

# -----------------------------------------------------------------------------------------
# send email
# -----------------------------------------------------------------------------------------

email <- envelope() %>%
  from("pech@saxo.com") %>%
  to("pech@saxo.com") %>% 
  cc("pech@saxo.com") %>%
  subject(glue::glue("Forecasts {today()}")) %>%
  text("Hej Peder\n\nHer kommer forecasts på Premiumbestanden\nsamt diverse beregninger t.o.m. slutningen af 2020.") %>% 
  attachment(filename)


creds <- read_rds("email_credentials.rds")

smtp <- server(host     = creds[[1]],
               port     = creds[[2]],
               username = creds[[3]],
               password = creds[[4]],)

smtp(email, verbose = TRUE)
