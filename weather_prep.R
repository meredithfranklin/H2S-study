# Script to prepare weather data for prediction
library(tidyverse)
library(httr)
library(jsonlite)

# load in data
data_visit1 <- read_csv('data_visit1_no_weather.csv')
data_visit2 <- read_csv('data_visit2_no_weather.csv')

# set arguments
api <- 'elLCtlloSD9HKH2KghuMEZDEHNBmwPkl'

# create UNIX time columns
data_visit1$date_prior_unix <- as.numeric(data_visit1$date_prior)
data_visit2$date_prior_unix <- as.numeric(data_visit2$date_prior)

result_tibble1 <- tibble(rowid = numeric(),
                        avg_temp = numeric(),
                        precip = numeric(),
                        wd = numeric(),
                        ws = numeric())
# loop through rows
for (i in 1:nrow(data_visit1)){
  get_url <- sprintf('https://timemachine.pirateweather.net/forecast/%s/%s,%s,%s?exclude=minutely,hourly',
                     api, data_visit1$lat[i], data_visit1$lon[i], data_visit1$date_prior_unix[i])
  res = GET(get_url)
  data = fromJSON(rawToChar(res$content))
  if (is.null(data$daily)) {
    result_tibble1 <- rbind(result_tibble1, tibble(rowid = i, avg_temp = NA, precip = NA, wd_avg = NA, ws_avg = NA))
  } else {
    result_tibble1 <- rbind(result_tibble1, tibble(rowid = i, 
                                                 avg_temp = mean(data$daily$data$temperatureHigh, data$daily$data$temperatureLow),
                                                 wd_avg = data$daily$data$windBearing,
                                                 ws_avg = data$daily$data$windSpeed,
                                                 precip = data$daily$data$precipAccumulation))
  }
  print(sprintf("Row %s completed", i))
}

data_visit1 <- cbind(data_visit1, result_tibble1)

result_tibble2 <- tibble(rowid = numeric(),
                         avg_temp = numeric(),
                         precip = numeric(),
                         wd = numeric(),
                         ws = numeric())
for (i in 1:nrow(data_visit2)){
  get_url <- sprintf('https://timemachine.pirateweather.net/forecast/%s/%s,%s,%s?exclude=minutely,hourly',
                     api, data_visit2$lat[i], data_visit2$lon[i], data_visit2$date_prior_unix[i])
  res = GET(get_url)
  data = fromJSON(rawToChar(res$content))
  if (is.null(data$daily)) {
    result_tibble2 <- rbind(result_tibble2, tibble(rowid = i, avg_temp = NA, precip = NA, wd_avg = NA, ws_avg = NA))
  } else {
    result_tibble2 <- rbind(result_tibble2, tibble(rowid = i, 
                                                 avg_temp = mean(data$daily$data$temperatureHigh, data$daily$data$temperatureLow),
                                                 wd_avg = data$daily$data$windBearing,
                                                 ws_avg = data$daily$data$windSpeed,
                                                 precip = data$daily$data$precipAccumulation))
  }
}

data_visit2 <- cbind(data_visit2, result_tibble2)

write_csv(data_visit1, 'data_visit1.csv')
write_csv(data_visit2, 'data_visit2.csv')