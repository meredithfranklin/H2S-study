# Script to prepare data for prediction
library(tidyverse)
library(sf)
library(raster)
library(units)
library(lubridate)
select <- dplyr::select

# carson_participant_data <- read_csv('carson_participant_clean.csv')
# carson_participant_data <- carson_participant_data %>% select(X, Y, StudyID, Visit1, Visit2)
# carson_participant_data$Visit1 <- as.POSIXct(carson_participant_data$Visit1, tz = 'America/Los_Angeles', format = '%m/%d/%y %H:%S')
# carson_participant_data$Visit2 <- as.POSIXct(carson_participant_data$Visit2, tz = 'America/Los_Angeles', format = '%m/%d/%y %H:%S')
# Load in sample input coordinate dataset
carson_participant_data <- tibble(X = c(-118.2514, -118.1929),
                       Y = c(33.85663, 33.76697),
                       StudyID = c(1084, 1038),
                       Visit1 = c("12/6/22 0:00", "1/31/23 0:00"),
                       Visit2 = c("7/18/23 0:00", NA))

# Set dates to POSIXct
carson_participant_data$Visit1 <- as.POSIXct(carson_participant_data$Visit1, tz = 'America/Los_Angeles', format = '%m/%d/%y %H:%S')
# carson_participant_data$Visit2 <- as.POSIXct(carson_participant_data$Visit2, tz = 'America/Los_Angeles', format = '%m/%d/%y %H:%S')
# Convert CRS if needed
CRS_UTM <- CRS("+proj=utm +zone=11 ellps=WGS84")

carson_participant_data <- carson_participant_data %>%
  mutate(lon = X,
         lat = Y)

carson_participant_data <- carson_participant_data %>%
  st_as_sf(coords = c('X', 'Y')) %>%
  st_set_crs("+proj=longlat +datum=WGS84") %>%
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

# Get production data
well_prod <- read_csv('LA well prod Dec2023 Well Monthly Production.CSV', show_col_types = FALSE)
full_well_info <- read_csv('well_active_inactive_prod_since_2000.CSV', show_col_types = FALSE)
active_well_info <- read_csv('LA well prod Dec2023 Well Headers.CSV', show_col_types = FALSE)

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

get_well_prod <- function(location, date) {
  # if date is later than a certain month, return NA
  if (date > max(well_prod$`Monthly Production Date`) + 31){
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

la_county <- read_sf('Zip_Codes_(LA_County).shp')

la_county_trans <- st_transform(la_county, CRS_UTM)

get_zipcode <- function(location) {
  return(apply(st_intersects(la_county_trans, location, sparse = FALSE), 2,
               function(col) {
                 la_county_trans[which(col), ]$ZIPCODE
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
# Get the time-independent variables
data_visit1 <- carson_participant_data %>%
  select(StudyID, Visit1, mon_utm_x, mon_utm_y, lon, lat) %>%
  filter(!is.na(Visit1)) %>%
  filter(!(lat > 33.99))

data_visit1 <- data_visit1 %>%
  mutate(sf_loc = get_sf_loc(mon_utm_x, mon_utm_y),
         ll_loc = get_ll_loc(mon_utm_x, mon_utm_y)) %>%
  mutate(dist_wrp = get_dist_wrp(sf_loc)$dist_wrp,
         MinDist = get_dist_ref(sf_loc),
         elevation = get_elevation(ll_loc),
         EVI = get_evi(ll_loc),
         capacity = get_dist_wrp(sf_loc)$capacity,
         dist_dc = apply(st_distance(sf_loc, d_channel), 1, min),
         inactive_2km = get_well_inactive(sf_loc),
         zipcode = get_zipcode(sf_loc))

# expand the data with past 4 weeks
data_visit1 <- data_visit1 %>%
  cross_join(tibble(days_prior = 0:28))

data_visit1$date_prior <- data_visit1$Visit1 - data_visit1$days_prior*60*60*24

data_visit1$month_floor <- floor_date(data_visit1$date_prior, 'month')

# get monthly oil
data_visit1_month <- data_visit1 %>%
  select(sf_loc, StudyID, Visit1, month_floor) %>%
  distinct()

data_visit1_month <- data_visit1_month %>%
  rowwise() %>%
  mutate(well_prod_data = get_well_prod(sf_loc, month_floor)) %>%
  ungroup()

data_visit1 <- data_visit1 %>%
  left_join(data_visit1_month, join_by(sf_loc, StudyID, Visit1, month_floor))

data_visit1 <- data_visit1 %>%
  mutate(active_2km = well_prod_data$active_2km,
         monthly_oil_2km = well_prod_data$monthly_oil_2km,
         monthly_gas_2km = well_prod_data$monthly_gas_2km) %>%
  select(-well_prod_data)

data_visit1 <- data_visit1 %>%
  rowwise() %>%
  mutate(num_odor_complaints = get_odor(zipcode, date_prior))

data_visit1 <- data_visit1 %>%
  st_drop_geometry() %>%
  select(-c(sf_loc, ll_loc))

# # Same for visit2
# # Get the time-independent variables
# data_visit2 <- carson_participant_data %>%
#   select(StudyID, Visit2, mon_utm_x, mon_utm_y) %>%
#   filter(!is.na(Visit2))
# 
# data_visit2 <- data_visit2 %>%
#   mutate(sf_loc = get_sf_loc(mon_utm_x, mon_utm_y),
#          ll_loc = get_ll_loc(mon_utm_x, mon_utm_y)) %>%
#   mutate(dist_wrp = get_dist_wrp(sf_loc)$dist_wrp,
#          MinDist = get_dist_ref(sf_loc),
#          elevation = get_elevation(ll_loc),
#          EVI = get_evi(sf_loc),
#          capacity = get_dist_wrp(sf_loc)$capacity,
#          dist_dc = apply(st_distance(sf_loc, d_channel), 1, min),
#          inactive_2km = get_well_inactive(sf_loc),
#          zipcode = get_zipcode(sf_loc))
# 
# # expand the data with past 4 weeks
# data_visit2 <- data_visit2 %>%
#   cross_join(tibble(days_prior = 0:28))
# 
# data_visit2$date_prior <- data_visit2$Visit2 - data_visit2$days_prior*60*60*24
# 
# data_visit2$month_floor <- floor_date(data_visit2$date_prior, 'month')
# 
# # get monthly oil
# data_visit2_month <- data_visit2 %>%
#   select(sf_loc, StudyID, Visit2, month_floor) %>%
#   distinct()
# 
# data_visit2_month <- data_visit2_month %>%
#   rowwise() %>%
#   mutate(well_prod_data = get_well_prod(sf_loc, month_floor)) %>%
#   ungroup()
# 
# data_visit2 <- data_visit2 %>%
#   left_join(data_visit2_month, join_by(sf_loc, StudyID, Visit2, month_floor))
# 
# data_visit2 <- data_visit2 %>%
#   mutate(active_2km = well_prod_data$active_2km,
#          monthly_oil_2km = well_prod_data$monthly_oil_2km,
#          monthly_gas_2km = well_prod_data$monthly_gas_2km) %>%
#   select(-well_prod_data)
# 
# data_visit2 <- data_visit2 %>%
#   rowwise() %>%
#   mutate(num_odor_complaints = get_odor(zipcode, date_prior))
# 
# data_visit2 <- data_visit2 %>%
#   st_drop_geometry() %>%
#   select(-c(sf_loc, ll_loc))

# Also for the two month of exposure, Oct 2021 and Nov 2021
data_exposure <- carson_participant_data %>%
  select(StudyID, Visit1, mon_utm_x, mon_utm_y, lon, lat) %>%
  filter(!is.na(Visit1)) %>%
  filter(!(lat > 33.99))

data_exposure <- data_exposure %>%
  mutate(sf_loc = get_sf_loc(mon_utm_x, mon_utm_y),
         ll_loc = get_ll_loc(mon_utm_x, mon_utm_y)) %>%
  mutate(dist_wrp = get_dist_wrp(sf_loc)$dist_wrp,
         MinDist = get_dist_ref(sf_loc),
         elevation = get_elevation(ll_loc),
         EVI = get_evi(ll_loc),
         capacity = get_dist_wrp(sf_loc)$capacity,
         dist_dc = apply(st_distance(sf_loc, d_channel), 1, min),
         inactive_2km = get_well_inactive(sf_loc),
         zipcode = get_zipcode(sf_loc))

# expand the data with past 4 weeks
data_exposure <- data_exposure %>%
  cross_join(tibble(exposure_date = 
                      ceiling_date(seq(as.POSIXct('2021-10-01', tz = 'America/Los_Angeles'), 
                                       as.POSIXct('2021-11-30', tz = 'America/Los_Angeles'), 
                                       by = 'day'), 
                                   unit = 'day')))

data_exposure$month_floor <- floor_date(data_exposure$exposure_date, 'month')

# get monthly oil
data_exposure_month <- data_exposure %>%
  select(sf_loc, StudyID, Visit1, month_floor) %>%
  distinct()

data_exposure_month <- data_exposure_month %>%
  rowwise() %>%
  mutate(well_prod_data = get_well_prod(sf_loc, month_floor)) %>%
  ungroup()

data_exposure <- data_exposure %>%
  left_join(data_exposure_month, join_by(sf_loc, StudyID, Visit1, month_floor))

data_exposure <- data_exposure %>%
  mutate(active_2km = well_prod_data$active_2km,
         monthly_oil_2km = well_prod_data$monthly_oil_2km,
         monthly_gas_2km = well_prod_data$monthly_gas_2km) %>%
  select(-well_prod_data)

data_exposure <- data_exposure %>%
  rowwise() %>%
  mutate(num_odor_complaints = get_odor(zipcode, exposure_date))

data_exposure <- data_exposure %>%
  st_drop_geometry() %>%
  select(-c(sf_loc, ll_loc))

write_csv(data_visit1, 'data_visit1_no_weather.csv')
# write_csv(data_visit2, 'data_visit2_no_weather.csv')