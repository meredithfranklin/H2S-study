# Script to prepare data for prediction
library(tidyverse)
library(sf)
library(raster)
library(units)
library(lubridate)
library(lwgeom)
select <- dplyr::select

data_exposure_with_weather <- read_csv('data_exposure_weather.csv')
data_visit1_with_weather <- read_csv('data_visit1_weather.csv')
data_visit2_with_weather <- read_csv('data_visit2_weather.csv')

# Set dates to POSIXct
data_exposure_with_weather$Visit1 <- as.POSIXct(data_exposure_with_weather$Visit1, tz = 'America/Los_Angeles', format = '%m/%d/%y %H:%S')
data_exposure_with_weather$Visit2 <- as.POSIXct(data_exposure_with_weather$Visit2, tz = 'America/Los_Angeles', format = '%m/%d/%y %H:%S')
data_visit1_with_weather$Visit1 <- as.POSIXct(data_visit1_with_weather$Visit1, tz = 'America/Los_Angeles', format = '%m/%d/%y %H:%S')
data_visit1_with_weather$Visit2 <- as.POSIXct(data_visit1_with_weather$Visit2, tz = 'America/Los_Angeles', format = '%m/%d/%y %H:%S')
data_visit2_with_weather$Visit1 <- as.POSIXct(data_visit2_with_weather$Visit1, tz = 'America/Los_Angeles', format = '%m/%d/%y %H:%S')
data_visit2_with_weather$Visit2 <- as.POSIXct(data_visit2_with_weather$Visit2, tz = 'America/Los_Angeles', format = '%m/%d/%y %H:%S')

# Convert CRS if needed
CRS_UTM <- CRS("+proj=utm +zone=11 ellps=WGS84")

data_exposure_with_weather <- data_exposure_with_weather %>%
  st_as_sf(coords = c('longitude', 'latitude')) %>%
  st_set_crs("+proj=longlat +datum=WGS84") %>%
  mutate(longitude = unlist(map(geometry,1)),
         latitude = unlist(map(geometry,2))) %>%
  st_transform(CRS_UTM) %>%
  mutate(mon_utm_x = unlist(map(geometry,1)),
         mon_utm_y = unlist(map(geometry,2))) %>%
  st_drop_geometry()

data_visit1_with_weather <- data_visit1_with_weather %>%
  st_as_sf(coords = c('longitude', 'latitude')) %>%
  st_set_crs("+proj=longlat +datum=WGS84") %>%
  mutate(longitude = unlist(map(geometry,1)),
         latitude = unlist(map(geometry,2))) %>%
  st_transform(CRS_UTM) %>%
  mutate(mon_utm_x = unlist(map(geometry,1)),
         mon_utm_y = unlist(map(geometry,2))) %>%
  st_drop_geometry()

data_visit2_with_weather <- data_visit2_with_weather %>%
  st_as_sf(coords = c('longitude', 'latitude')) %>%
  st_set_crs("+proj=longlat +datum=WGS84") %>%
  mutate(longitude = unlist(map(geometry,1)),
         latitude = unlist(map(geometry,2))) %>%
  st_transform(CRS_UTM) %>%
  mutate(mon_utm_x = unlist(map(geometry,1)),
         mon_utm_y = unlist(map(geometry,2))) %>%
  st_drop_geometry()

# Helper Functions 
get_sf_loc <- function(utm_x, utm_y) {
  sf_loc <- st_as_sf(tibble(utm_x = utm_x, utm_y = utm_y), 
                     coords = c('utm_x', 'utm_y')) %>%
    st_set_crs(CRS_UTM)
  return(sf_loc)
}

get_ll_loc <- function(utm_x, utm_y) {
  ll_loc <- st_as_sf(tibble(utm_x = utm_x, utm_y = utm_y), 
                     coords = c('utm_x', 'utm_y')) %>%
    st_set_crs(CRS_UTM) %>%
    st_transform("+proj=longlat +datum=WGS84")
  return(ll_loc)
}

# WRP Data
la_wrp <- read_sf('LA_WRP.shp', layer = 'LA_WRP')

# Convert both water treatment plants and monitor coordinates to UTM
la_wrp <- st_transform(la_wrp, CRS_UTM)

get_dist_wrp <- function(location) {
  return(tibble(dist_wrp = apply(st_distance(location, la_wrp), 1, min),
                capacity = la_wrp$cpcty_m[st_nearest_feature(location, la_wrp)]))
}

get_angle_wrp <- function(location) {
  angles <- tibble(location, la_wrp$geometry[st_nearest_feature(location, la_wrp)]) %>%
    pivot_longer(cols = everything()) %>% 
    pull(value) %>% # extract coordinates only
    st_transform(4326) %>% # convert to lat/lon for function below
    st_geod_azimuth() %>%
    set_units('degrees') %>% # convert to degrees
    drop_units()
  angles <- angles[c(T, F)] # keep only odd index, valid pairs
  angles <- if_else(angles < 0, angles + 360, angles)
  return(angles)
}

# Get MinDist to Refinery
#st_read('Petroleum_Refinery.shp')
refineries <- read_sf('Petroleum_Refinery.shp', layer = 'Petroleum_Refinery')
refineries <- refineries %>%
  select(Company, Site, Latitude, Longitude) %>%
  mutate(refinery = case_when(Site == 'Carson' ~ 'Marathon (Carson)',
                              Site == 'El Segundo' ~ 'Chevron El Segundo',
                              Site == 'Wilmington Asphalt Plant' ~ 'Valero Refinery',
                              Site == 'Wilmington Refinery' ~ 'Marathon (Wilmington)',
                              Site == 'Torrance' ~ 'Torrance Refinery',
                              Site == 'Wilmington' ~ 'Phillips 66 (Wilmington)')) %>%
  st_transform(CRS_UTM)

get_dist_ref <- function(location) {
  return(apply(st_distance(location, refineries), 1, min))
}

get_angle_ref <- function(location) {
  angles <- tibble(location, refineries$geometry[st_nearest_feature(location, refineries)]) %>%
    pivot_longer(cols = everything()) %>% 
    pull(value) %>% # extract coordinates only
    st_transform(4326) %>% # convert to lat/lon for function below
    st_geod_azimuth() %>%
    set_units('degrees') %>% # convert to degrees
    drop_units()
  angles <- angles[c(T, F)] # keep only odd index, valid pairs
  angles <- if_else(angles < 0, angles + 360, angles)
  return(angles)
}

# Get production data
well_prod <- read_csv('LA well prod Mar2024 Well Monthly Production.CSV', show_col_types = FALSE)
full_well_info <- read_csv('well_active_inactive_prod_since_2000.CSV', show_col_types = FALSE)
active_well_info <- read_csv('LA well prod Mar2024 Well Headers.CSV', show_col_types = FALSE)

inactive_well_info <- anti_join(full_well_info, active_well_info, join_by(API14)) %>%
  rename(lon = `Surface Hole Longitude (WGS84)`,
         lat = `Surface Hole Latitude (WGS84)`) %>%
  select(`API14`, lon, lat) %>%
  distinct() %>%
  st_as_sf(coords = c('lon', 'lat')) %>%
  st_set_crs(4326) %>%
  st_transform(CRS_UTM)

well_prod <- well_prod %>%
  mutate('Monthly Production Date' = force_tz(`Monthly Production Date`, tz = 'America/Los_Angeles')) %>%
  left_join(active_well_info, join_by(`API/UWI` == API14))

well_prod <- well_prod %>%
  select(`API/UWI`, `Monthly Production Date`, `Monthly Oil`, `Monthly Gas`,
         `Monthly Water`, `Monthly BOE`, Days, `Operator Company Name.x`,
         `Production Type.x`, `Well Status.x`, `Drill Type`,
         `Surface Hole Latitude (WGS84)`, `Surface Hole Longitude (WGS84)`) %>%
  rename(lon = `Surface Hole Longitude (WGS84)`,
         lat = `Surface Hole Latitude (WGS84)`)

# project distinct well locations to UTM
active_well_location <- well_prod %>%
  select(`API/UWI`, lon, lat) %>%
  distinct() %>%
  st_as_sf(coords = c('lon', 'lat')) %>%
  st_set_crs(4326) %>%
  st_transform(CRS_UTM)

prod_end_month <- strftime(well_prod[which.max(well_prod$`Monthly Production Date`), ]$`Monthly Production Date` + 60*60*24*31, format = '%Y-%m-%d')
well_prod$`Monthly Production Date` <- strftime(well_prod$`Monthly Production Date`, format = '%Y-%m-%d')

get_well_prod <- function(location, date) {
  # if date is later than a certain month, return NA
  if (date > prod_end_month) {
    return(tibble(active_2km = NA,
                  monthly_oil_2km = NA,
                  monthly_gas_2km = NA))
  }
  # get the wells within 2km to location
  location_well <- active_well_location %>%
    mutate(dist_to_loc = apply(st_distance(location, active_well_location), 2, min)) %>%
    filter(dist_to_loc <= 2000)
  
  loc_well_prod <- well_prod %>%
    right_join(location_well %>% select(-geometry), join_by(`API/UWI`))
  
  loc_well_prod <- loc_well_prod %>%
    filter(`Monthly Production Date` == date) %>%
    group_by(`Monthly Production Date`) %>%
    summarise(active_2km = n(),
              monthly_oil_2km = sum(`Monthly Oil`, na.rm = TRUE),
              monthly_gas_2km = sum(`Monthly Gas`, na.rm = TRUE))
  
  # consider the case when there is no well prod data
  if (nrow(loc_well_prod) == 0) {
    return(tibble(active_2km = 0,
                  monthly_oil_2km = 0,
                  monthly_gas_2km = 0))
  }
  return(loc_well_prod)
}


get_well_inactive <- function(location) {
  # get inactive wells within 2km
  inactive_well_api <- drop_units(st_distance(location, inactive_well_info))
  inactive_well_api <- inactive_well_info[which(inactive_well_api <= 2000), ] %>% st_drop_geometry()
  return(nrow(inactive_well_api))
}

# Get elevation
elevation <- raster('N33W119.hgt')

get_elevation <- function(location_ll) {
  return(raster::extract(elevation, location_ll))
}

# Get EVI
evi <- raster('MOD13Q1.061__250m_16_days_EVI_doy2023145_aid0001_2.tif')

evi <- projectRaster(evi, crs = CRS_UTM)
# fill NA pixels
dist <- distance(evi)  

direct <- direction(evi, from=FALSE)

evi_na <- is.na(evi) 

na_x <- init(evi_na, 'x')
na_y <- init(evi_na, 'y')

co_x <- na_x + dist * sin(direct)
co_y <- na_y + dist * cos(direct)

co <- cbind(co_x[], co_y[]) 

NAVals <- raster::extract(evi, co, method='simple') 
evi_NAVals <- evi # initiate new raster
evi_NAVals[] <- NAVals # store values in raster

evi_filled <- cover(x=evi, y = evi_NAVals)

evi_filled <- projectRaster(evi_filled, crs = "+proj=longlat +datum=WGS84")

get_evi <- function(location){
  return(raster::extract(evi_filled, location) * 0.0001)
}

# Get Number of Odor Complaints
odor <- read_csv('odorcomplaintdata_2018_2023.csv')

odor$date <- force_tz(odor$date, tz = 'America/Los_Angeles')
odor$date <- strftime(odor$date, format = '%Y-%m-%d')

la_county <- read_sf('CA_Zipcode.shp')

la_county_trans <- st_transform(la_county, CRS_UTM)

get_zipcode <- function(location) {
  return(apply(st_intersects(la_county_trans, location, sparse = FALSE), 2,
               function(col) {
                 la_county_trans[which(col), ]$ZIP_CODE
               }))
}

get_odor <- function(zipcode, date_prior) {
  odor <- odor[odor$zip == zipcode, ]
  odor <- odor %>% filter(date == date_prior)
  if (nrow(odor) == 0) {
    return(0)
  } else {
    return(odor$num_odor_complaints)
  }
}


# Distance to Dominguez Channel
# Read in the shape file, it already has a CRS
st_read('DominguezChannel_Carson.shp')
d_channel <- read_sf('DominguezChannel_Carson.shp', layer = 'DominguezChannel_Carson')
d_channel <- st_transform(d_channel, CRS_UTM) # convert to UTM crs


# prepare data for each visit

# First the data exposure data set

# Get the time-independent variables
data_exposure <- data_exposure_with_weather %>%
  select(Visit1, Visit2, mon_utm_x, mon_utm_y, latitude, longitude) %>%
  distinct() %>%
  mutate(sf_loc = get_sf_loc(mon_utm_x, mon_utm_y),
         ll_loc = get_ll_loc(mon_utm_x, mon_utm_y)) %>%
  mutate(dist_wrp = get_dist_wrp(sf_loc)$dist_wrp,
         MinDist = get_dist_ref(sf_loc),
         elevation = get_elevation(ll_loc),
         EVI = get_evi(ll_loc),
         capacity = get_dist_wrp(sf_loc)$capacity,
         dist_dc = apply(st_distance(sf_loc, d_channel), 1, min),
         zipcode = get_zipcode(sf_loc),
         angle_ref = get_angle_ref(sf_loc),
         angle_wrp = get_angle_wrp(sf_loc))

data_exposure <- data_exposure %>%
  rowwise() %>%
  mutate(inactive_2km = get_well_inactive(sf_loc)) %>%
  ungroup()

data_exposure <- data_exposure %>%
  left_join(data_exposure_with_weather, join_by(Visit1, Visit2, mon_utm_x, mon_utm_y, latitude, longitude))

wind_diff <- abs(data_exposure$angle_ref - data_exposure$daily_wd)
wind_diff <- ifelse(wind_diff > 180, 360 - wind_diff, wind_diff)
data_exposure$daily_downwind_ref <- as.integer(wind_diff <= 15)

wind_diff <- abs(data_exposure$angle_wrp - data_exposure$daily_wd)
wind_diff <- ifelse(wind_diff > 180, 360 - wind_diff, wind_diff)
data_exposure$daily_downwind_wrp <- as.integer(wind_diff <= 15)

data_exposure$month_floor <- floor_date(data_exposure$day, 'month')

# get monthly oil
data_exposure_month <- data_exposure %>%
  select(sf_loc, Visit1, Visit2, month_floor) %>%
  distinct()

data_exposure_month <- data_exposure_month %>%
  rowwise() %>%
  mutate(well_prod_data = get_well_prod(sf_loc, month_floor)) %>%
  ungroup()

data_exposure <- data_exposure %>%
  left_join(data_exposure_month, join_by(sf_loc, Visit1, Visit2, month_floor))

data_exposure <- data_exposure %>%
  mutate(active_2km = well_prod_data$active_2km,
         monthly_oil_2km = well_prod_data$monthly_oil_2km,
         monthly_gas_2km = well_prod_data$monthly_gas_2km) %>%
  select(-well_prod_data)

data_exposure <- data_exposure %>%
  rowwise() %>%
  mutate(num_odor_complaints = get_odor(zipcode, day))

data_exposure <- data_exposure %>%
  st_drop_geometry() %>%
  select(-c(sf_loc, ll_loc))

write_csv(data_exposure, 'data_exposure_pred_ready.csv')

# Then the visit 1 data

# Get the time-independent variables
data_visit1 <- data_visit1_with_weather %>%
  filter(!is.na(Visit1)) %>%
  select(Visit1, Visit2, mon_utm_x, mon_utm_y, latitude, longitude) %>%
  distinct() %>%
  mutate(sf_loc = get_sf_loc(mon_utm_x, mon_utm_y),
         ll_loc = get_ll_loc(mon_utm_x, mon_utm_y)) %>%
  mutate(dist_wrp = get_dist_wrp(sf_loc)$dist_wrp,
         MinDist = get_dist_ref(sf_loc),
         elevation = get_elevation(ll_loc),
         EVI = get_evi(ll_loc),
         capacity = get_dist_wrp(sf_loc)$capacity,
         dist_dc = apply(st_distance(sf_loc, d_channel), 1, min),
         zipcode = get_zipcode(sf_loc),
         angle_ref = get_angle_ref(sf_loc),
         angle_wrp = get_angle_wrp(sf_loc))

data_visit1 <- data_visit1 %>%
  rowwise() %>%
  mutate(inactive_2km = get_well_inactive(sf_loc)) %>%
  ungroup()

data_visit1 <- data_visit1 %>%
  left_join(data_visit1_with_weather, join_by(Visit1, Visit2, mon_utm_x, mon_utm_y, latitude, longitude))

wind_diff <- abs(data_visit1$angle_ref - data_visit1$daily_wd)
wind_diff <- ifelse(wind_diff > 180, 360 - wind_diff, wind_diff)
data_visit1$daily_downwind_ref <- as.integer(wind_diff <= 15)

wind_diff <- abs(data_visit1$angle_wrp - data_visit1$daily_wd)
wind_diff <- ifelse(wind_diff > 180, 360 - wind_diff, wind_diff)
data_visit1$daily_downwind_wrp <- as.integer(wind_diff <= 15)

data_visit1$month_floor <- floor_date(data_visit1$day, 'month')

# get monthly oil
data_visit1_month <- data_visit1 %>%
  select(sf_loc, Visit1, Visit2, month_floor) %>%
  distinct()

data_visit1_month <- data_visit1_month %>%
  rowwise() %>%
  mutate(well_prod_data = get_well_prod(sf_loc, month_floor)) %>%
  ungroup()

data_visit1 <- data_visit1 %>%
  left_join(data_visit1_month, join_by(sf_loc, Visit1, Visit2, month_floor))

data_visit1 <- data_visit1 %>%
  mutate(active_2km = well_prod_data$active_2km,
         monthly_oil_2km = well_prod_data$monthly_oil_2km,
         monthly_gas_2km = well_prod_data$monthly_gas_2km) %>%
  select(-well_prod_data)

data_visit1 <- data_visit1 %>%
  rowwise() %>%
  mutate(num_odor_complaints = get_odor(zipcode, day))

data_visit1 <- data_visit1 %>%
  st_drop_geometry() %>%
  select(-c(sf_loc, ll_loc))

write_csv(data_visit1, 'data_visit1_pred_ready.csv')


# Finally visit 2 data

# Get the time-independent variables
data_visit2 <- data_visit2_with_weather %>%
  filter(!is.na(Visit2)) %>%
  select(Visit1, Visit2, mon_utm_x, mon_utm_y, latitude, longitude) %>%
  distinct() %>%
  mutate(sf_loc = get_sf_loc(mon_utm_x, mon_utm_y),
         ll_loc = get_ll_loc(mon_utm_x, mon_utm_y)) %>%
  mutate(dist_wrp = get_dist_wrp(sf_loc)$dist_wrp,
         MinDist = get_dist_ref(sf_loc),
         elevation = get_elevation(ll_loc),
         EVI = get_evi(ll_loc),
         capacity = get_dist_wrp(sf_loc)$capacity,
         dist_dc = apply(st_distance(sf_loc, d_channel), 1, min),
         zipcode = get_zipcode(sf_loc),
         angle_ref = get_angle_ref(sf_loc),
         angle_wrp = get_angle_wrp(sf_loc))

data_visit2 <- data_visit2 %>%
  rowwise() %>%
  mutate(inactive_2km = get_well_inactive(sf_loc)) %>%
  ungroup()

data_visit2 <- data_visit2 %>%
  left_join(data_visit2_with_weather, join_by(Visit1, Visit2, mon_utm_x, mon_utm_y, latitude, longitude))

wind_diff <- abs(data_visit2$angle_ref - data_visit2$daily_wd)
wind_diff <- ifelse(wind_diff > 180, 360 - wind_diff, wind_diff)
data_visit2$daily_downwind_ref <- as.integer(wind_diff <= 15)

wind_diff <- abs(data_visit2$angle_wrp - data_visit2$daily_wd)
wind_diff <- ifelse(wind_diff > 180, 360 - wind_diff, wind_diff)
data_visit2$daily_downwind_wrp <- as.integer(wind_diff <= 15)

data_visit2$month_floor <- floor_date(data_visit2$day, 'month')

# get monthly oil
data_visit2_month <- data_visit2 %>%
  select(sf_loc, Visit1, Visit2, month_floor) %>%
  distinct()

data_visit2_month <- data_visit2_month %>%
  rowwise() %>%
  mutate(well_prod_data = get_well_prod(sf_loc, month_floor)) %>%
  ungroup()
data_visit2 <- data_visit2_with_weather %>%
  filter(!is.na(Visit2)) %>%
  select(Visit1, Visit2, mon_utm_x, mon_utm_y, latitude, longitude) %>%
  distinct() %>%
  mutate(sf_loc = get_sf_loc(mon_utm_x, mon_utm_y),
         ll_loc = get_ll_loc(mon_utm_x, mon_utm_y)) %>%
  mutate(dist_wrp = get_dist_wrp(sf_loc)$dist_wrp,
         MinDist = get_dist_ref(sf_loc),
         elevation = get_elevation(ll_loc),
         EVI = get_evi(ll_loc),
         capacity = get_dist_wrp(sf_loc)$capacity,
         dist_dc = apply(st_distance(sf_loc, d_channel), 1, min),
         zipcode = get_zipcode(sf_loc),
         angle_ref = get_angle_ref(sf_loc),
         angle_wrp = get_angle_wrp(sf_loc))

data_visit2 <- data_visit2 %>%
  rowwise() %>%
  mutate(inactive_2km = get_well_inactive(sf_loc)) %>%
  ungroup()

data_visit2 <- data_visit2 %>%
  left_join(data_visit2_with_weather, join_by(Visit1, Visit2, mon_utm_x, mon_utm_y, latitude, longitude))

wind_diff <- abs(data_visit2$angle_ref - data_visit2$daily_wd)
wind_diff <- ifelse(wind_diff > 180, 360 - wind_diff, wind_diff)
data_visit2$daily_downwind_ref <- as.integer(wind_diff <= 15)

wind_diff <- abs(data_visit2$angle_wrp - data_visit2$daily_wd)
wind_diff <- ifelse(wind_diff > 180, 360 - wind_diff, wind_diff)
data_visit2$daily_downwind_wrp <- as.integer(wind_diff <= 15)

data_visit2$month_floor <- floor_date(data_visit2$day, 'month')

# get monthly oil
data_visit2_month <- data_visit2 %>%
  select(sf_loc, Visit1, Visit2, month_floor) %>%
  distinct()

data_visit2_month <- data_visit2_month %>%
  rowwise() %>%
  mutate(well_prod_data = get_well_prod(sf_loc, month_floor)) %>%
  ungroup()

data_visit2 <- data_visit2 %>%
  left_join(data_visit2_month, join_by(sf_loc, Visit1, Visit2, month_floor))

data_visit2 <- data_visit2 %>%
  mutate(active_2km = well_prod_data$active_2km,
         monthly_oil_2km = well_prod_data$monthly_oil_2km,
         monthly_gas_2km = well_prod_data$monthly_gas_2km) %>%
  select(-well_prod_data)

data_visit2 <- data_visit2 %>%
  rowwise() %>%
  mutate(num_odor_complaints = get_odor(zipcode, day))

data_visit2 <- data_visit2 %>%
  st_drop_geometry() %>%
  select(-c(sf_loc, ll_loc))

write_csv(data_visit2, 'data_visit2_pred_ready.csv')

data_visit2 <- data_visit2 %>%
  left_join(data_visit2_month, join_by(sf_loc, Visit1, Visit2, month_floor))

data_visit2 <- data_visit2 %>%
  mutate(active_2km = well_prod_data$active_2km,
         monthly_oil_2km = well_prod_data$monthly_oil_2km,
         monthly_gas_2km = well_prod_data$monthly_gas_2km) %>%
  select(-well_prod_data)

data_visit2 <- data_visit2 %>%
  rowwise() %>%
  mutate(num_odor_complaints = get_odor(zipcode, day))

data_visit2 <- data_visit2 %>%
  st_drop_geometry() %>%
  select(-c(sf_loc, ll_loc))

write_csv(data_visit2, 'data_visit2_pred_ready.csv')
