# Script to prepare weather data for prediction
library(tidyverse)
library(httr)
library(jsonlite)
library(circular)

# load in data
data_visit <- read_csv('Geocoded_visits12_04.01.24.csv')

data_visit <- data_visit%>%
  filter(!(Y > 33.99)) %>%
  select(X, Y, Visit1, Visit2) %>%
  distinct()

# First fetch the exposure period
daily_exposure_result <- tibble(Visit1 = character(),
                                Visit2 = character(), 
                                latitude = numeric(),
                                longitude = numeric(),
                                day = numeric(),
                                daily_temp = numeric(),
                                daily_hum = numeric(),
                                daily_precip = numeric(),
                                daily_ws = numeric(),
                                daily_wd = numeric())


# loop through rows
for (i in 1:nrow(data_visit)){

  get_url <- sprintf("https://customer-archive-api.open-meteo.com/v1/archive?latitude=%s&longitude=%s&start_date=2021-10-01&end_date=2021-11-30&hourly=temperature_2m,relative_humidity_2m,precipitation,wind_speed_10m,wind_direction_10m&temperature_unit=fahrenheit&wind_speed_unit=mph&precipitation_unit=inch&timezone=America%%2FLos_Angeles&apikey=S4sRENjfQvm2aH86",
                     data_visit$Y[i], data_visit$X[i])
  res = GET(get_url)
  data = fromJSON(rawToChar(res$content))
  if (!(is.null(try(data$error)))){
    ith_daily_exposure_result <- tibble(Visit1 = data_visit$Visit1[i],
                                        Visit2 = data_visit$Visit2[i],
                                        latitude = data_visit$Y[i],
                                        longitude = data_visit$X[i],
                                        day = NA,
                                        daily_temp = NA,
                                        daily_hum = NA,
                                        daily_precip = NA,
                                        daily_ws = NA,
                                        daily_wd = NA)
  }
  else {
    hourly_result <- tibble(latitude = data_visit$Y[i],
                            longitude = data_visit$X[i],
                            elevation = data$elevation,
                            hourly = data$hourly$time,
                            hourly_temp = data$hourly$temperature_2m,
                            hourly_hum = data$hourly$relative_humidity_2m,
                            hourly_precip = data$hourly$precipitation,
                            hourly_ws = data$hourly$wind_speed_10m,
                            hourly_wd = data$hourly$wind_direction_10m)
    
    ith_daily_exposure_result <- hourly_result %>%
      mutate(date = as.POSIXct(hourly)) %>%
      mutate(day = format(date, "%Y-%m-%d"))
    
    ith_daily_exposure_result$hourly_wd <- as.numeric(ith_daily_exposure_result$hourly_wd)

    ith_daily_exposure_result <- ith_daily_exposure_result %>%
      group_by(latitude, longitude, day) %>%
      summarise(daily_temp = mean(hourly_temp, na.rm = T),
                daily_hum = mean(hourly_hum, na.rm = T),
                daily_precip = sum(hourly_precip, na.rm = T),
                daily_ws = mean(hourly_ws, na.rm = T),
                daily_wd = as.numeric(mean(circular(hourly_wd, units = 'degrees'), na.rm=TRUE)),
                .groups = 'drop')
    
    ith_daily_exposure_result <- ith_daily_exposure_result %>%
      mutate(daily_wd = if_else(daily_wd < 0, daily_wd + 360, daily_wd),
             Visit1 = data_visit$Visit1[i],
             Visit2 = data_visit$Visit2[i]) %>%
      relocate(Visit1, Visit2)
  }

  daily_exposure_result <- rbind(daily_exposure_result, ith_daily_exposure_result)
  
  print(sprintf("Row %s completed", i))
}

write_csv(daily_exposure_result, 'data_exposure_weather.csv')

# Then fetch month prior to visit 1
daily_visit1_result <- tibble(Visit1 = data_visit$Visit1[i],
                              Visit2 = data_visit$Visit2[i],
                              latitude = numeric(),
                              longitude = numeric(),
                              day = numeric(),
                              daily_temp = numeric(),
                              daily_hum = numeric(),
                              daily_precip = numeric(),
                              daily_ws = numeric(),
                              daily_wd = numeric())

visit_datestring <- as.POSIXct(data_visit$Visit1, tz = 'America/Los_Angeles', format = '%m/%d/%y %H:%S')
month_prior_datestring <- format(visit_datestring - 31*60*60*24, '%Y-%m-%d') 

# loop through rows
for (i in 1:nrow(data_visit)){
  
  
  
  get_url <- sprintf("https://customer-archive-api.open-meteo.com/v1/archive?latitude=%s&longitude=%s&start_date=%s&end_date=%s&hourly=temperature_2m,relative_humidity_2m,precipitation,wind_speed_10m,wind_direction_10m&temperature_unit=fahrenheit&wind_speed_unit=mph&precipitation_unit=inch&timezone=America%%2FLos_Angeles&apikey=S4sRENjfQvm2aH86",
                     data_visit$Y[i], data_visit$X[i], month_prior_datestring[i], visit_datestring[i])
  res = GET(get_url)
  data = fromJSON(rawToChar(res$content))
  if (!(is.null(try(data$error)))){
    ith_daily_visit1_result <- tibble(Visit1 = data_visit$Visit1[i],
                                      Visit2 = data_visit$Visit2[i],
                                      latitude = data_visit$Y[i],
                                      longitude = data_visit$X[i],
                                      day = NA,
                                      daily_temp = NA,
                                      daily_hum = NA,
                                      daily_precip = NA,
                                      daily_ws = NA,
                                      daily_wd = NA)
  }
  else {
    hourly_result <- tibble(latitude = data_visit$Y[i],
                            longitude = data_visit$X[i],
                            elevation = data$elevation,
                            hourly = data$hourly$time,
                            hourly_temp = data$hourly$temperature_2m,
                            hourly_hum = data$hourly$relative_humidity_2m,
                            hourly_precip = data$hourly$precipitation,
                            hourly_ws = data$hourly$wind_speed_10m,
                            hourly_wd = data$hourly$wind_direction_10m)
    
    ith_daily_visit1_result <- hourly_result %>%
      mutate(date = as.POSIXct(hourly)) %>%
      mutate(day = format(date, "%Y-%m-%d"))
    
    ith_daily_visit1_result$hourly_wd <- as.numeric(ith_daily_visit1_result$hourly_wd)
    
    ith_daily_visit1_result <- ith_daily_visit1_result %>%
      group_by(latitude, longitude, day) %>%
      summarise(daily_temp = mean(hourly_temp, na.rm = T),
                daily_hum = mean(hourly_hum, na.rm = T),
                daily_precip = sum(hourly_precip, na.rm = T),
                daily_ws = mean(hourly_ws, na.rm = T),
                daily_wd = as.numeric(mean(circular(hourly_wd, units = 'degrees'), na.rm=TRUE)),
                .groups = 'drop')
    
    ith_daily_visit1_result <- ith_daily_visit1_result %>%
      mutate(daily_wd = if_else(daily_wd < 0, daily_wd + 360, daily_wd),
             Visit1 = data_visit$Visit1[i],
             Visit2 = data_visit$Visit2[i]) %>%
      relocate(Visit1, Visit2)
  }
  
  daily_visit1_result <- rbind(daily_visit1_result, ith_daily_visit1_result)
  
  print(sprintf("Row %s completed", i))
}

write_csv(daily_visit1_result, 'data_visit1_weather.csv')

# Then fetch month prior to visit 2
daily_visit2_result <- tibble(Visit1 = data_visit$Visit1[i],
                              Visit2 = data_visit$Visit2[i],
                              latitude = numeric(),
                              longitude = numeric(),
                              day = numeric(),
                              daily_temp = numeric(),
                              daily_hum = numeric(),
                              daily_precip = numeric(),
                              daily_ws = numeric(),
                              daily_wd = numeric())

visit_datestring <- as.POSIXct(data_visit$Visit2, tz = 'America/Los_Angeles', format = '%m/%d/%y %H:%S')
month_prior_datestring <- format(visit_datestring - 31*60*60*24, '%Y-%m-%d') 

# loop through rows
for (i in 1:nrow(data_visit)){
  
  get_url <- sprintf("https://customer-archive-api.open-meteo.com/v1/archive?latitude=%s&longitude=%s&start_date=%s&end_date=%s&hourly=temperature_2m,relative_humidity_2m,precipitation,wind_speed_10m,wind_direction_10m&temperature_unit=fahrenheit&wind_speed_unit=mph&precipitation_unit=inch&timezone=America%%2FLos_Angeles&apikey=S4sRENjfQvm2aH86",
                     data_visit$Y[i], data_visit$X[i], month_prior_datestring[i], visit_datestring[i])
  res = GET(get_url)
  data = fromJSON(rawToChar(res$content))
  if (!(is.null(try(data$error)))){
    ith_daily_visit2_result <- tibble(Visit1 = data_visit$Visit1[i],
                                      Visit2 = data_visit$Visit2[i],
                                      latitude = data_visit$Y[i],
                                      longitude = data_visit$X[i],
                                      day = NA,
                                      daily_temp = NA,
                                      daily_hum = NA,
                                      daily_precip = NA,
                                      daily_ws = NA,
                                      daily_wd = NA)
  }
  else {
    hourly_result <- tibble(latitude = data_visit$Y[i],
                            longitude = data_visit$X[i],
                            elevation = data$elevation,
                            hourly = data$hourly$time,
                            hourly_temp = data$hourly$temperature_2m,
                            hourly_hum = data$hourly$relative_humidity_2m,
                            hourly_precip = data$hourly$precipitation,
                            hourly_ws = data$hourly$wind_speed_10m,
                            hourly_wd = data$hourly$wind_direction_10m)
    
    ith_daily_visit2_result <- hourly_result %>%
      mutate(date = as.POSIXct(hourly)) %>%
      mutate(day = format(date, "%Y-%m-%d"))
    
    ith_daily_visit2_result$hourly_wd <- as.numeric(ith_daily_visit2_result$hourly_wd)
    
    ith_daily_visit2_result <- ith_daily_visit2_result %>%
      group_by(latitude, longitude, day) %>%
      summarise(daily_temp = mean(hourly_temp, na.rm = T),
                daily_hum = mean(hourly_hum, na.rm = T),
                daily_precip = sum(hourly_precip, na.rm = T),
                daily_ws = mean(hourly_ws, na.rm = T),
                daily_wd = as.numeric(mean(circular(hourly_wd, units = 'degrees'), na.rm=TRUE)),
                .groups = 'drop')
    
    ith_daily_visit2_result <- ith_daily_visit2_result %>%
      mutate(daily_wd = if_else(daily_wd < 0, daily_wd + 360, daily_wd),
             Visit1 = data_visit$Visit1[i],
             Visit2 = data_visit$Visit2[i]) %>%
      relocate(Visit1, Visit2)
  }
  
  daily_visit2_result <- rbind(daily_visit2_result, ith_daily_visit2_result)
  
  print(sprintf("Row %s completed", i))
}

write_csv(daily_visit2_result, 'data_visit2_weather.csv')