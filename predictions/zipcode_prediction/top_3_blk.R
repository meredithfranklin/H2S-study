# This script computes the 3 most populated blocks in each zip code
# and finds the centeroid of each block to fetch weather data

library(tidyverse)
library(sf)

################ Get Zip Codes ################

daily_full <- readRDS('data/daily_full.rds')

CRS_UTM <- CRS("+proj=utm +zone=11 ellps=WGS84")

mon_location <- daily_full %>%
  select(Monitor, mon_utm_x, mon_utm_y) %>%
  distinct() %>%
  st_as_sf(coords = c('mon_utm_x', 'mon_utm_y')) %>%
  st_set_crs(CRS_UTM)

la_county <- read_sf('shapefiles/Zip_Codes_(LA_County)/Zip_Codes_(LA_County).shp')

la_county_trans <- st_transform(la_county, CRS_UTM)      # apply transformation to polygons sf

# intersect and extract state name
mon_location$county <- apply(st_intersects(la_county_trans, mon_location, sparse = FALSE), 2, 
                             function(col) { 
                               la_county_trans[which(col), ]$ZIPCODE
                             })
# These are the counties with a Monitor
la_county_present <- la_county_trans[la_county_trans$ZIPCODE %in% unique(mon_location$county), ]
# Add some more
additional_zip <- c(90278, 90260, 90501, 90505, 90710, 90746, 90248, 90806, 
                    90807, 90247, 90710, 90502, 90717, 90250, 90249, 90506, 
                    90747, 90755, 90731, 90732, 90802, 90277, 90254, 90261)
la_county_full <- rbind(la_county_present, 
                        la_county_trans[la_county_trans$ZIPCODE %in% additional_zip, ])

############### Load census and clean ##################
census_2020 <- read_csv('nhgis0001_csv/nhgis0001_ds258_2020_block.csv')

census_2020 <- census_2020 %>%
  filter(ZCTAA %in% la_county_full$ZIPCODE) %>%
  select(ZCTAA, BLOCKA, INTPTLAT, INTPTLON, U7H001) %>%
  arrange(ZCTAA, desc(U7H001)) 

top_3_blk <- census_2020 %>% 
  group_by(ZCTAA) %>% 
  slice(1:3)

top_3_location <- top_3_blk %>%
  select(INTPTLAT, INTPTLON) %>%
  distinct() %>%
  st_as_sf(coords = c('INTPTLON', 'INTPTLAT')) %>%
  st_set_crs("+proj=longlat +datum=WGS84")

top_3_blk_zip_graph <- ggplot() +
  geom_sf(data = la_county_full, aes(fill = ZIPCODE)) +
  geom_sf(data = top_3_location)

top_3_blk_zip_graph

ggsave('top_3_blk_zip_graph.png', plot = top_3_blk_zip_graph)

write_csv(top_3_blk, 'top_3_blk_without_metero.csv')

# We have 89 blocks here for 90 zip codes, as 90747 has only two blocks
# The number of fetches for years 2020-2022: 97455
# The number of fetches at 500m resolution: 812.1 hours