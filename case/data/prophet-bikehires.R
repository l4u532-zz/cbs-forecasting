library(forecast)
library(prophet)
library(dplyr)

# load data
data = read.csv('tfl-daily-cycle-hires.csv')
head(data)  # check dataframe
# plot data
plot(data)
lines(data)

# create holidays dataframe
bank_holidays = read.csv('UK_holidays.csv', as.is=TRUE)
strike_days = read.csv('UK_tube-strikes.csv', as.is=TRUE)
holidays = bind_rows(bank_holidays, strike_days)  # combine to one dataframe

# prepare dataframe for prophet
names(data) = c('ds', 'y')

# predict
m = prophet(data, holidays = holidays)
future = make_future_dataframe(m, periods = 365)
forecast = predict(m, future)
plot(m, forecast)
prophet_plot_components(m, forecast)

# cross-validate
data.cv = cross_validation(m, horizon = 365, units = 'days')
head(data.cv) # show first few values of CV dataframe
accuracy(data.cv$y, data.cv$yhat)
