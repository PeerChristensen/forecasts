# arima w/ (lagged) predictors insurance <- as_tsibble(fpp2::insurance, pivot_longer = FALSE)
fit <- insurance %>%
  # Restrict data so models use same fitting period
  mutate(Quotes = c(NA,NA,NA,Quotes[4:40])) %>%
  # Estimate models
  model(
  lag0 = ARIMA(Quotes ~  TV.advert),
  lag1= ARIMA(Quotes ~  TV.advert + lag(TV.advert)),
  lag2= ARIMA(Quotes ~ TV.advert + lag(TV.advert) + lag(TV.advert, 2)),
  lag3= ARIMA(Quotes ~ TV.advert + lag(TV.advert) + lag(TV.advert, 2) + lag(TV.advert, 3)))

accuracy(fit)
glance(fit)

fit_best <- insurance %>%
  model(
    lag1= ARIMA(Quotes ~  TV.advert + lag(TV.advert)))

report(fit_best)

insurance_future <- new_data(insurance, 20) %>%
  mutate(TV.advert = 1:20)

fit_best %>%
  forecast(insurance_future) 
