library(tidyverse)
library(httr)
library(jsonlite)
library(sf)
library(circular)


# Load in data
zipcode_grid_df_cosmos <- read_csv('zipcode_grid_df_cosmos.csv')
#zipcode_grid_fetched_cosmos <- read_csv('zipcode_grid_fetched_cosmos.csv')

# Code to fetch weather data
# For first time
zipcode_grid_fetched_cosmos <- tibble(longitude = numeric(), latitude = numeric())

zipcode_grid_unfetched_cosmos <- zipcode_grid_df_cosmos %>%
  anti_join(zipcode_grid_fetched_cosmos, join_by(lon == longitude, lat == latitude))

hourly_result <- tibble(latitude = numeric(),
                        longitude = numeric(),
                        hourly = numeric(),
                        hourly_temp = numeric(),
                        hourly_hum = numeric(),
                        hourly_precip = numeric(),
                        hourly_ws = numeric(),
                        hourly_wd = numeric())
counter <- 0
start_time <- Sys.time()
for (i in 1:nrow(zipcode_grid_unfetched_cosmos)) {
  if (counter == 150) {
    break
  }
  reset <- FALSE
  # In order to follow the 600 calls per minute restriction from open-meteo
  if (counter != 0 && counter %% 9 == 0) {
    end_time <- Sys.time()
    sec_until_60s <- 60 - (as.numeric(end_time) - as.numeric(start_time))
    print(paste0('Completed ', counter, ' coordinates'))
    print(paste0('Waiting ', sec_until_60s, ' seconds'))
    Sys.sleep(sec_until_60s + 3) # give 3s buffer
    reset <- TRUE
  }
  
  # In order to follow the 5000 calls per hour restriction from open-meteo
  if (counter != 0 && counter %% 55 == 0) {
    end_time <- Sys.time()
    sec_until_60m <- 3600 - (as.numeric(end_time) - as.numeric(start_time))
    print(paste0('Completed ', counter, ' coordinates'))
    print(paste0('Waiting ', sec_until_60m, ' seconds'))
    Sys.sleep(sec_until_60m + 30) # give 30s buffer
    reset <- TRUE
  }
  
  if (reset == T) {
    start_time <- Sys.time()
  }
  
  counter <- counter + 1
  
  
  get_url <- sprintf("https://archive-api.open-meteo.com/v1/archive?latitude=%s&longitude=%s&start_date=2018-01-01&end_date=2022-12-31&hourly=temperature_2m,relative_humidity_2m,precipitation,wind_speed_10m,wind_direction_10m&temperature_unit=fahrenheit&wind_speed_unit=mph&precipitation_unit=inch&timezone=America%%2FLos_Angeles",
                     zipcode_grid_unfetched_cosmos$lat[i], zipcode_grid_unfetched_cosmos$lon[i])
  res = GET(get_url)
  data = fromJSON(rawToChar(res$content))
  hourly_result <- rbind(hourly_result,
                         tibble(latitude = zipcode_grid_unfetched_cosmos$lat[i],
                                longitude = zipcode_grid_unfetched_cosmos$lon[i],
                                elevation = data$elevation,
                                hourly = data$hourly$time,
                                hourly_temp = data$hourly$temperature_2m,
                                hourly_hum = data$hourly$relative_humidity_2m,
                                hourly_precip = data$hourly$precipitation,
                                hourly_ws = data$hourly$wind_speed_10m,
                                hourly_wd = data$hourly$wind_direction_10m))
}

# result <- rbind(zipcode_grid_fetched_cosmos,
#                 hourly_result)
saveRDS(hourly_result, 'zipcode_grid_fetched_cosmos.rds')