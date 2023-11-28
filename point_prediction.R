# Script to predict H2S concentration at given coordinates
library(tidyverse)
library(sf)
library(raster)
library(httr)
library(jsonlite)

# Load in XGBoost model with disaster indicator
fit.xgb_da_log_h2s_dis_ind <- readRDS('rfiles/fit.xgb_da_log_h2s_dis_ind.rds')

# Load in sample input coordinate dataset
sample_coord <- tibble(lon = c(-118.4262, -118.4264, -118.4265, -118.4206, 
                               -118.4207, -118.4208, -118.4208, -118.4209, 
                               -118.4210, -118.4211),
                       lat = c(33.91558, 33.92460, 33.92910, 33.90212, 33.90663, 
                               33.91113, 33.91564, 33.92015, 33.92466, 33.92917))
# Convert CRS if needed
CRS_UTM <- CRS("+proj=utm +zone=11 ellps=WGS84")

sample_coord <- sample_coord %>%
  st_as_sf(coords = c('lon', 'lat')) %>%
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
la_wrp <- read_sf('shapefiles/LA_WRP.shp', layer = 'LA_WRP')

# Convert both water treatment plants and monitor coordinates to UTM
la_wrp <- st_transform(la_wrp, CRS_UTM)

get_dist_wrp <- function(location) {
  return(tibble(dist_wrp = apply(st_distance(location, la_wrp), 1, min),
                capacity = la_wrp$cpcty_m[st_nearest_feature(location, la_wrp)]))
}

# Get MinDist to Refinery
st_read('shapefiles/Petroleum_Refinery.shp')
refineries <- read_sf('shapefiles/Petroleum_Refinery.shp', layer = 'Petroleum_Refinery')
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
well_prod <- read_csv('data/LA_well_prod_Oct2023 Well Monthly Production.CSV', show_col_types = FALSE)
full_well_info <- read_csv('data/well_active_inactive_prod_since_2000.CSV', show_col_types = FALSE)
active_well_info <- read_csv('data/LA_well_prod_Oct2023 Well Headers.CSV', show_col_types = FALSE)

inactive_well_info <- anti_join(full_well_info, active_well_info, join_by(API14)) %>%
  rename(lon = `Surface Hole Longitude (WGS84)`,
         lat = `Surface Hole Latitude (WGS84)`) %>%
  select(`API14`, lon, lat) %>%
  distinct() %>%
  st_as_sf(coords = c('lon', 'lat')) %>%
  st_set_crs(4326) %>%
  st_transform(CRS_UTM)

well_prod <- well_prod %>%
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

get_well_prod <- function(location, disaster) {
  # get the wells within 2km to location
  location_well <- well_location %>%
    mutate(dist_to_loc = apply(st_distance(location, well_location), 2, min)) %>%
    filter(dist_to_loc <= 2000)
  
  loc_well_prod <- well_prod %>%
    right_join(location_well %>% select(-geometry), join_by(`API/UWI`))
  
  if (disaster == 1) {
    loc_well_prod <- loc_well_prod %>%
      filter(`Monthly Production Date` == '2021-10-01') %>%
      group_by(`Monthly Production Date`) %>%
      summarise(active_2km = n(),
                monthly_oil_2km = sum(`Monthly Oil`, na.rm = TRUE),
                monthly_gas_2km = sum(`Monthly Gas`, na.rm = TRUE))
  } else {
    loc_well_prod <- loc_well_prod %>%
      filter(`Monthly Production Date` == '2022-10-01') %>%
      group_by(`Monthly Production Date`) %>%
      summarise(active_2km = n(),
                monthly_oil_2km = sum(`Monthly Oil`, na.rm = TRUE),
                monthly_gas_2km = sum(`Monthly Gas`, na.rm = TRUE))
  }
  if (nrow(loc_well_prod)  == 0) {
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
elevation <- raster('shapefiles/N33W119.hgt')

get_elevation <- function(location_ll) {
  return(raster::extract(elevation, location_ll))
}

# Get EVI
evi <- raster('shapefiles/MOD13Q1.061__250m_16_days_EVI_doy2023145_aid0001.tif')

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

evi_filled <- cover(x=evi, y= evi_NAVals)

get_evi <- function(location){
  return(raster::extract(evi_filled, location) * 0.0001)
}

# Get Number of Odor Complaints
odor <- read_csv('data/SCAQMD_odorcomplaints_2018_2022.csv')

get_zipcode <- function(location) {
  return(apply(st_intersects(la_county_trans, location, sparse = FALSE), 2,
               function(col) {
                 la_county_trans[which(col), ]$ZIPCODE
               }))
}

get_odor <- function(location, disaster) {
  zipcode <- get_zipcode(location)
  odor <- odor[odor$zip == zipcode, ]
  
  if (disaster == 1) {
    return(odor %>% filter(between(date, as.Date('2021-10-01'), as.Date('2021-10-31'))))
  } else {
    return(odor %>% filter(between(date, as.Date('2022-10-01'), as.Date('2022-10-31'))))
  }
}


# Distance to Dominguez Channel
# Read in the shape file, it already has a CRS
st_read('shapefiles/DominguezChannel_Carson.shp')
d_channel <- read_sf('shapefiles/DominguezChannel_Carson.shp', layer = 'DominguezChannel_Carson')
d_channel <- st_transform(d_channel, CRS_UTM) # convert to UTM crs

api <- 'elLCtlloSD9HKH2KghuMEZDEHNBmwPkl'
get_weather <- function(location, time) {
  # get time in UNIX
  visit_time <- as.numeric(time)
  for (i in 1:28) {
    t <- visit_time-86400*i
    get_url <- sprintf('https://timemachine.pirateweather.net/forecast/%s/%s,%s,%s?exclude=minutely,hourly',
                       api, unlist(map(location$geometry,1)), unlist(map(location$geometry,2)), t)
    res = GET(get_url)
    data = fromJSON(rawToChar(res$content))
  }
}

# prepare disaster period data
pred_data_disaster <- sample_coord %>%
  mutate(sf_loc = get_sf_loc(mon_utm_x, mon_utm_y),
         ll_loc = get_ll_loc(mon_utm_x, mon_utm_y)) %>%
  mutate(month = '10',
         year = as.integer(2021),
         dist_wrp = get_dist_wrp(sf_loc)$dist_wrp,
         MinDist = get_dist_ref(sf_loc),
         elevation = get_elevation(ll_loc),
         EVI = get_evi(sf_loc),
         capacity = get_dist_wrp(sf_loc)$capacity,
         dist_dc = apply(st_distance(sf_loc, d_channel), 1, min))

pred_data_disaster <- pred_data_disaster %>%
  rowwise() %>%
  mutate(well_prod_data = get_well_prod(sf_loc, 1),
         inactive_2km = get_well_inactive(sf_loc)) %>%
  ungroup()

pred_data_disaster <- pred_data_disaster %>%
  mutate(active_2km = well_prod_data$active_2km,
         monthly_oil_2km = well_prod_data$monthly_oil_2km,
         monthly_gas_2km = well_prod_data$monthly_gas_2km) %>%
  select(-well_prod_data)

odor_data <- tibble(mon_utm_x = double(), mon_utm_y = double(), date = date(),
                    num_odor_complaint = double())
for (i in 1:nrow(pred_data_disaster)){
  sf_loc <- get_sf_loc(pred_data_disaster$mon_utm_x[i], pred_data_disaster$mon_utm_y[i])
  odor_data <- rbind(odor_data,
                     tibble(mon_utm_x = pred_data_disaster$mon_utm_x[i],
                            mon_utm_y = pred_data_disaster$mon_utm_y[i]) %>%
                       cross_join(get_odor(sf_loc, 1)))
}

disaster_dates <- tibble(day = seq(as.Date('2021-10-01'), as.Date('2021-10-31'),"days")) %>%
                  mutate(weekday = relevel(factor(wday(day, label=TRUE), ordered = FALSE), ref = "Sun"))

pred_data_disaster <- pred_data_disaster %>%
  cross_join(disaster_dates)

pred_data_disaster <- pred_data_disaster %>%
  left_join(odor_data %>% select(mon_utm_x, mon_utm_y, date, num_odor_complaints),
            join_by(mon_utm_x, mon_utm_y, day == date))

pred_data_disaster <- pred_data_disaster %>%
  mutate(num_odor_complaints = coalesce(num_odor_complaints, 0))

pred_data_disaster <- pred_data_disaster %>%
  st_drop_geometry() %>%
  select(-c(x, y, values, sf_loc, ll_loc))

pred_data_disaster$EVI <- coalesce(pred_data_disaster$EVI, mean(pred_data_disaster$EVI, na.rm=TRUE))