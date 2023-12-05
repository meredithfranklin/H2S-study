# Test weather fetching
library(tidyverse)
library(httr)
library(jsonlite)
library(sf)

api <- 'elLCtlloSD9HKH2KghuMEZDEHNBmwPkl'
sample_arg <- tibble(date = as.numeric(as.POSIXct('2021-10-01', tz = 'America/Los_Angeles')),
                     lat = '33.929104597856',
                     lon = '-118.426470877697')

# Sample request
start_time <- Sys.time()
get_url <- sprintf('https://timemachine.pirateweather.net/forecast/%s/%s,%s,%s?exclude=minutely,hourly',
                   api, sample_arg$lat, sample_arg$lon, sample_arg$date)
res = GET(get_url)
end_time <- Sys.time()
print(end_time - start_time)
data = fromJSON(rawToChar(res$content))

result <- tibble(date = data$daily$data$time,
                 avg_temp = mean(data$daily$data$temperatureHigh, data$daily$data$temperatureLow),
                 wd = data$daily$data$windBearing,
                 ws = data$daily$data$windSpeed,
                 precip = data$daily$data$precipAccumulation)

write_csv(result, 'test_result.csv')
