# Script to prepare weather data for prediction
library(tidyverse)
library(httr)
library(jsonlite)

# load in data
data_visit1 <- read_csv('data_visit1_no_weather.csv')
#data_visit2 <- read_csv('data_visit2_no_weather.csv')
# 
# set arguments
api <- '4Q7hFEmsDjk3orhrP46zKtmQ0BMGYw9s'

# create UNIX time columns
data_visit1$date_prior_unix <- as.numeric(data_visit1$date_prior)
#data_visit2$date_prior_unix <- as.numeric(data_visit2$date_prior)

result_tibble1 <- tibble(rowid = numeric(),
                        avg_temp = numeric(),
                        wd_avg = numeric(),
                        ws_avg = numeric(),
                        precip = numeric(),
                        dewpoint = numeric(),
                        hum = numeric())
# loop through rows
for (i in 1:nrow(data_visit1)){
  get_url <- sprintf('https://timemachine.pirateweather.net/forecast/%s/%s,%s,%s?exclude=minutely,hourly',
                     api, data_visit1$lat[i], data_visit1$lon[i], data_visit1$date_prior_unix[i])
  res = GET(get_url)
  data = fromJSON(rawToChar(res$content))
  if (typeof(data) == 'character') {
    result_tibble1 <- rbind(result_tibble1, tibble(rowid = i, avg_temp = NA, wd_avg = NA, ws_avg = NA, precip = NA, dewpoint = NA, hum = NA))
  } else if (is.null(data$daily)) {
    result_tibble1 <- rbind(result_tibble1, tibble(rowid = i, avg_temp = NA, wd_avg = NA, ws_avg = NA, precip = NA, dewpoint = NA, hum = NA))
  } else {
    result_tibble1 <- rbind(result_tibble1, tibble(rowid = i,
                                                 avg_temp = mean(data$daily$data$temperatureHigh, data$daily$data$temperatureLow),
                                                 wd_avg = data$daily$data$windBearing,
                                                 ws_avg = data$daily$data$windSpeed,
                                                 precip = data$daily$data$precipAccumulation,
                                                 dewpoint = data$daily$data$dewPoint) %>%
                                              mutate(hum = 100 * (exp((17.625 * (dewpoint - 32) * (5/9)) / (243.04 + (dewpoint - 32) * (5/9)))
                                                                       / exp((17.625 * (avg_temp - 32) * (5/9)) / (243.04 + (avg_temp - 32) * (5/9))))))
  }
  print(sprintf("Row %s completed", i))
  if (i %% 10 == 0) {write_csv(data_visit1 <- cbind(data_visit1[1:i, ], result_tibble1), 'data_visit1_temp1.csv')}
}

data_visit1 <- cbind(data_visit1, result_tibble1)

write_csv(data_visit1, 'data_visit1_part1_result.csv')

# result_tibble2 <- tibble(rowid = numeric(),
#                          avg_temp = numeric(),
#                          wd_avg = numeric(),
#                          ws_avg = numeric(),
#                          precip = numeric(),
#                          dewpoint = numeric(),
#                          hum = numeric())
# for (i in 1:nrow(data_visit2)){
#   get_url <- sprintf('https://timemachine.pirateweather.net/forecast/%s/%s,%s,%s?exclude=minutely,hourly',
#                      api, data_visit2$lat[i], data_visit2$lon[i], data_visit2$date_prior_unix[i])
#   res = GET(get_url)
#   data = fromJSON(rawToChar(res$content))
#   if (typeof(data) == 'character') {
#     result_tibble3 <- rbind(result_tibble3, tibble(rowid = i, avg_temp = NA, wd_avg = NA, ws_avg = NA, precip = NA, dewpoint = NA, hum = NA))
#   } else if (is.null(data$daily)) {
#     result_tibble3 <- rbind(result_tibble3, tibble(rowid = i, avg_temp = NA, wd_avg = NA, ws_avg = NA, precip = NA, dewpoint = NA, hum = NA))
#   } else {
#     result_tibble2 <- rbind(result_tibble2, tibble(rowid = i, 
#                                                  avg_temp = mean(data$daily$data$temperatureHigh, data$daily$data$temperatureLow),
#                                                  wd_avg = data$daily$data$windBearing,
#                                                  ws_avg = data$daily$data$windSpeed,
#                                                  precip = data$daily$data$precipAccumulation,
#                                                  dewpoint = data$daily$data$dewPoint) %>%
#                                               mutate(hum = 100 * (exp((17.625 * (dewpoint - 32) * (5/9)) / (243.04 + (dewpoint - 32) * (5/9))) 
#                                                                        / exp((17.625 * (avg_temp - 32) * (5/9)) / (243.04 + (avg_temp - 32) * (5/9))))))
#   }
# }
# 
# data_visit2 <- cbind(data_visit2, result_tibble2)

library(tidyverse)
library(httr)
library(jsonlite)

# for exposure data
data_exposure <- read_csv('data_exposure_no_met_part3.csv')

# set arguments
api <- '4Q7hFEmsDjk3orhrP46zKtmQ0BMGYw9s'

# create UNIX time columns
data_exposure$exposure_date_unix <- as.numeric(data_exposure$exposure_date)

result_tibble3 <- tibble(rowid = numeric(),
                         avg_temp = numeric(),
                         wd_avg = numeric(),
                         ws_avg = numeric(),
                         precip = numeric(),
                         dewpoint = numeric(),
                         hum = numeric())
# loop through rows
for (i in 2691:nrow(data_exposure)){
  get_url <- sprintf('https://timemachine.pirateweather.net/forecast/%s/%s,%s,%s?exclude=minutely,hourly',
                     api, data_exposure$lat[i], data_exposure$lon[i], data_exposure$exposure_date_unix[i])
  res = GET(get_url)
  data = fromJSON(rawToChar(res$content))
  if (typeof(data) == 'character') {
    result_tibble3 <- rbind(result_tibble3, tibble(rowid = i, avg_temp = NA, wd_avg = NA, ws_avg = NA, precip = NA, dewpoint = NA, hum = NA))
  } else if (is.null(data$daily)) {
    result_tibble3 <- rbind(result_tibble3, tibble(rowid = i, avg_temp = NA, wd_avg = NA, ws_avg = NA, precip = NA, dewpoint = NA, hum = NA))
  } else {
    result_tibble3 <- rbind(result_tibble3, tibble(rowid = i, 
                                                   avg_temp = mean(data$daily$data$temperatureHigh, data$daily$data$temperatureLow),
                                                   wd_avg = data$daily$data$windBearing,
                                                   ws_avg = data$daily$data$windSpeed,
                                                   precip = data$daily$data$precipAccumulation,
                                                   dewpoint = data$daily$data$dewPoint) %>%
                              mutate(hum = 100 * (exp((17.625 * (dewpoint - 32) * (5/9)) / (243.04 + (dewpoint - 32) * (5/9))) 
                                                       / exp((17.625 * (avg_temp - 32) * (5/9)) / (243.04 + (avg_temp - 32) * (5/9))))))
  }
  print(sprintf("Row %s completed", i))
  if (i %% 10 == 0) {write_csv(cbind(data_exposure[2691:i, ], result_tibble3), 'data_exposure_temp3c.csv')}
}

data_exposure <- cbind(data_exposure, result_tibble3)

write_csv(data_exposure, 'data_exposure_part1.csv')
 
# write_csv(data_visit1, 'data_visit1.csv')
# write_csv(data_visit2, 'data_visit2.csv')