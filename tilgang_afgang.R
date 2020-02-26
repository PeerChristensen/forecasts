
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
  select(date, type = CustomerEventTypeName,n)

# tilgang - afgang til i dag
df %>% 
  filter(n < 2000) %>% # outliers
  ggplot(aes(date,n,colour = type)) +
  geom_line(size=1, alpha= .8) +
  theme_minimal() +
  scale_color_viridis_d(option="C",end=.8)

# difference 
df %>%
  pivot_wider(id_cols=date, names_from = type,values_from = n) %>%
  mutate(Difference = `Subscription Start` - `Subscription End`) %>%
  filter(Difference < 1000) %>% # outliers
  pivot_longer(cols = -date) %>%
  mutate(name = fct_relevel(factor(name),"Subscription Start", "Subscription End", "Difference")) %>%
  ggplot(aes(date,value, colour = name)) +
  theme_minimal() +
  geom_line() +
  facet_wrap(~name, ncol=1) +
  scale_color_viridis_d(option="C",end=.8)

