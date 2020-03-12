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

# mdlige beregninger

# 7. Omsætning (2.333 * antal betalende)
# 8. Gennemsnitlig streamingcost pr. bog ( (streaming buckets * 29) /100 )
# 9. Churn rate

# -----------------------------------------------------------------------------------------
# Pakker
# -----------------------------------------------------------------------------------------

library(RODBC)
library(tidyverse)
library(lubridate)
library(forecast)
library(tsibble)
library(openxlsx)
library(emayili)
library(fable)
library(feasts)

# -----------------------------------------------------------------------------------------
# SQL login
# -----------------------------------------------------------------------------------------

credentials <- read_rds("C:/Users/pech/Desktop/RProjects/ForecastAbo/credentials.rds")

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
# Antal i bestanden der streamer
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

df1 <- sqlQuery(channel,query1) %>%
  mutate(month = yearmonth(ymd(Date))) %>%
  filter(month < yearmonth(today())) %>%
  group_by(month) %>%
  summarise(M_Buckets = round(sum(nBuckets) / 1000000,2))

df2 <- sqlQuery(channel,query2) %>%
  mutate(Period  = fct_recode(as.factor(Period), Betalende = "1", Gratis = "2")) %>%
  pivot_wider(names_from = Period, values_from = nSubscribers) %>%
  drop_na() %>%
  mutate(Date = ymd(Date)) %>%
  mutate(month = yearmonth(ymd(Date))) %>%
  filter(month < yearmonth(today())) %>%
  group_by(month) %>%
  mutate(Betalende_start = if_else(Date == min(Date),Betalende,NULL),
         Betalende_slut = if_else(Date == max(Date),Betalende,NULL),
         Gratis_start = if_else(Date == min(Date),Gratis,NULL),
         Gratis_slut = if_else(Date == max(Date),Gratis,NULL)) %>%
  summarise(Betalende_start = sum(Betalende_start,na.rm=T),
            Betalende_slut = sum(Betalende_slut,na.rm=T),
            Gratis_start = sum(Gratis_start,na.rm=T),
            Gratis_slut = sum(Gratis_slut,na.rm=T))

df3 <- sqlQuery(channel,query3) %>%
  mutate(eventType = fct_recode(eventType, Tilgang = "Subscription Start", Afgang = "Subscription End")) %>%
  pivot_wider(names_from = eventType, values_from = n) %>%
  mutate(month = yearmonth(ymd(Date))) %>%
  filter(month < yearmonth(today())) %>%
  select(-Date) %>%
  group_by(month) %>%
  summarise_all(sum)
  

df4 <-  sqlQuery(channel,query4) %>%
  mutate(month = yearmonth(as.character(month))) %>%
  rename(AntalStreamere = n)

close(channel)

df <- df2 %>%
  left_join(df1) %>%
  left_join(df3) %>%
  left_join(df4) %>%
  mutate(SamletBestand_start = Betalende_start + Gratis_start,
         SamletBestand_slut = Betalende_slut + Gratis_slut) %>%
  mutate(month = yearmonth(month))
  
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

# Box-cox transformation (due to non-constant variance)
lambda <- df_ts %>%
  features(M_Buckets, features = guerrero) %>%
  pull(lambda_guerrero)

# fit ARIMA model
fit_streaming <- df_ts %>%
  #model(arima = ARIMA(box_cox(M_Buckets,lambda=lambda)))
  model(arima = ARIMA(M_Buckets))
  
# calculate forecasts
fc_streaming <- fit_streaming %>% 
  forecast(h=month(as.Date("2020-12-31")) - month(today())+1) %>%
  hilo() %>% 
  as_tibble() %>%
  rename(nBuckets_ci80 = `80%`, nBuckets_ci95 = `95%`) %>%
  select(-.model)

# Plot
fit_streaming %>% 
  forecast(h=month(as.Date("2020-12-31")) - month(today())+1) %>%
  autoplot(df_ts)

# -----------------------------------------------------------------------------------------
# Forecast Antal betalende medlemmer
# -----------------------------------------------------------------------------------------

# fit ARIMA model
fit_betalende <- df_ts %>%
  model(arima = ARIMA(Betalende))

# calculate forecasts
fc_betalende <- fit_betalende %>% 
  forecast(h=as.numeric(as.Date("2020-12-31") - today()+1)) %>%
  hilo() %>% 
  as_tibble() %>%
  rename(Betalende_ci80 = `80%`, Betalende_ci95 = `95%`) %>%
  select(-.model)

# slut antal betalende per md
betalende_md <- fc_betalende %>%
  group_by(month = month(Date)) %>%
  filter(Date == max(Date)) %>%
  ungroup() %>%
  mutate(month = month.name[month]) %>%
  select(month, Betalende)

# -----------------------------------------------------------------------------------------
# Forecast Antal medlemmer (samlet)
# -----------------------------------------------------------------------------------------

# fit ARIMA model
fit_samlet <- df_ts %>%
  model(arima = ARIMA(SamletBestand))

# calculate forecasts
fc_samlet <- fit_samlet %>% 
  forecast(h=as.numeric(as.Date("2020-12-31") - today()+1)) %>%
  hilo() %>% 
  as_tibble() %>%
  rename(SamletBestand_ci80 = `80%`, SamletBestand_ci95 = `95%`) %>%
  select(-.model)

# slut antal medlemmer per md
bestand_md <- fc_samlet %>%
  group_by(month = month(Date)) %>%
  filter(Date == max(Date)) %>%
  ungroup() %>%
  mutate(month = month.name[month]) %>%
  select(month, SamletBestand)

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
  model(arima = ARIMA(Start))

# 7. calculate forecasts
fc_start <- fit_start %>% 
  forecast(h=as.numeric(as.Date("2020-12-31") - today()+1)) %>%
  as_tibble() %>%
  select(-.model,-.distribution) %>%
  rename(tilgang = Start)

# tilgang (sum) per md
tilgang_md <- fc_start  %>%
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

# afgang (sum) per md
afgang_md <- fc_end  %>%
  group_by(month = month(Date)) %>%
  summarise(afgang = sum(afgang)) %>%
  mutate(month = month.name[month]) 

# -----------------------------------------------------------------------------------------
# Beregning af Samlet antal antal gratis medlemmer
# -----------------------------------------------------------------------------------------

Gratis <- fc_samlet$SamletBestand - fc_betalende$Betalende

# -----------------------------------------------------------------------------------------
# Beregning af omsætning
# -----------------------------------------------------------------------------------------

revenue <- fc_betalende %>%
  mutate(Revenue = Betalende * 2.4) %>%
  select(Date,Revenue) %>%
  group_by(month =month(Date)) %>%
  summarise(Revenue = sum(Revenue)) %>%
  mutate(month = month.name[month])

# -----------------------------------------------------------------------------------------
# Beregning af Gennemsnitlig streamingcost pr. enhed
# -----------------------------------------------------------------------------------------

streaming_cost <- fc_streaming %>%
  mutate(Cost = (nBuckets * 27.5) / 100) %>%
  select(Date, Cost) %>%
  group_by(month =month(Date)) %>%
  summarise(Cost = sum(Cost)) %>%
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
  select(month, start_medlemmer = SamletBestand)

# NB. samme som bestand_md
slut_medlemmer <- fc_samlet %>%
  group_by(month(Date)) %>%
  mutate(max = max(Date)) %>%
  ungroup() %>%
  filter(Date == max) %>%
  mutate(month = month.name[month(max)]) %>%
  select(month, slut_medlemmer = SamletBestand)

nye_medlemmer <- slut_medlemmer %>%
  mutate(nye_medlemmer = slut_medlemmer - start_medlemmer$start_medlemmer) %>%
  select(month, nye_medlemmer)

churn <- afgang %>%
  inner_join(start_medlemmer) %>%
  inner_join(slut_medlemmer) %>%
  inner_join(nye_medlemmer) %>%
  mutate(ChurnRate1 = afgang / (start_medlemmer + nye_medlemmer) * 100,
         ChurnRate2 = afgang / (start_medlemmer/2 + slut_medlemmer/2) * 100) %>%
  select(month,ChurnRate1, ChurnRate2)

# -----------------------------------------------------------------------------------------
# Forecast af antal der streamer
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
  rename(AntalStreamere = n)

# -----------------------------------------------------------------------------------------
# output
# -----------------------------------------------------------------------------------------

daily_fc <- fc_betalende %>%
  mutate(Gratis = Gratis) %>%
  inner_join(fc_samlet) %>%
  inner_join(fc_start) %>%
  inner_join(fc_end) %>%
  mutate_if(is.numeric, round)


monthly <- revenue %>%
  inner_join(streaming_cost) %>%
  inner_join(churn) %>%
  inner_join(betalende_md) %>%
  inner_join(bestand_md) %>%
  inner_join(tilgang_md) %>%
  inner_join(afgang_md) %>%
  inner_join(fc_streamers) %>%
  mutate(AndelStreamere = AntalStreamere / ((slut_medlemmer$slut_medlemmer + start_medlemmer$start_medlemmer) / 2) * 100) %>%
  mutate_at(vars(Revenue, Cost,Betalende,SamletBestand,tilgang,afgang,AntalStreamere), round) %>%
  mutate_at(vars(ChurnRate1, ChurnRate2,AndelStreamere), round,1)


data <- list(Daily = daily_fc, Monthly = monthly)

filename <- glue::glue("C:/Users/pech/Desktop/RProjects/ForecastAbo/forecasts/forecasts_{today()}.xlsx")

write.xlsx(data, file = filename)

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
  to("psl@saxo.com") %>% 
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

