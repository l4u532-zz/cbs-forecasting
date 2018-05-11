library(prophet)
library(forecast)
library(dplyr)
# define holidays
playoffs = data_frame(
  holiday = 'playoff',
  ds = as.Date(c('2008-01-13', '2009-01-03', '2010-01-16',
                 '2010-01-24', '2010-02-07', '2011-01-08',
                 '2013-01-12', '2014-01-12', '2014-01-19',
                 '2014-02-02', '2015-01-11', '2016-01-17',
                 '2016-01-24', '2016-02-07')),
  lower_window = 0,
  upper_window = 1
)
superbowls = data_frame(
  holiday = 'superbowl',
  ds = as.Date(c('2010-02-07', '2014-02-02', '2016-02-07')),
  lower_window = 0,
  upper_window = 1
)
holidays = bind_rows(playoffs, superbowls)

# load data
df = read.csv('example_wp_peyton_manning.csv')

# predict
df['y'] = log(df['y'])
m = prophet(df, holidays = holidays)
future = make_future_dataframe(m, periods = 365)
forecast = predict(m, future)
plot(m, forecast)
prophet_plot_components(m, forecast)

# cross-validate
df.cv = cross_validation(m, horizon = 365, units = 'days')
head(df.cv) # show first few values of CV dataframe
df.cv = df.cv %>% # exponentiate logged values back to original
  mutate_if(is.numeric, exp)
accuracy(df.cv$y, df.cv$yhat)

