library(prophet)
library(dplyr)
library(forecast)
library(lubridate)

# make standard data ready for prophet
# transform to make stationary
data = AirPassengers
data = log1p(data)
# convert dates
dates_numeric <- as.numeric(time(data)) # convert dates to numeric format
dates_string <- format(date_decimal(dates_numeric), "%Y-%m-%d") # convert from numeric to string
dates_asDate = as.Date(as.Date(dates_string)) # convert from string to Date object
# create dataframe for prophet
df = data.frame(ds=dates_asDate, y=as.matrix(data)) # build dataframe

# fbprophet
# fit model and make forecast
m = prophet(df)
future <- make_future_dataframe(m, periods = 120, freq = 'month')
forecast <- predict(m, future)
# exponentiate forecast back to original scale
forecast = forecast %>%
  mutate_at(c('yhat', 'yhat_lower', 'yhat_upper'), expm1)
m$history$y = expm1(m$history$y)
plot(m, forecast)

# predict with auto.arima
data = ts(AirPassengers)
model = auto.arima(data)
forecast = forecast(model, h=120)
plot(forecast)
