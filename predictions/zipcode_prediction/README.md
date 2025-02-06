# Folder Purpose
Predict zipcode H2S exposures based on locations of LA patients.

# Procedure

1. zipcode_grid_prep.rmd:
    1. **Input**: "lacounty_EDpatients_ZIPcodes.csv", "../../shapefiles/CA_Zipcode.shp", "../../shapefiles/CA_Zipcode_points.shp"
        - Reads in patient zipcodes and merge with shapefiles containing zipcode information.
    3. Filter for zipcodes with centroids in: -118.46 <= lon <= -118.14, 33.7 <= lat <= 33.99.
    4. Convert to UTM CRS and use stars package to convert selected zipcodes from sf to stars (data cubes).
    5. Convert stars to dataframe (lat & lon) and filter for data cubes within the filtered zipcodes.
    6. **Output**: "zipcode_grid_df.csv"
2. zipcode_weather_prep.rmd:
    1. **Input**: "zipcode_grid_df.csv", "zipcode_grid_fetched_weather.rds"
        - Reads in the zipcode grid dataframe and any existing weather data that could be downloaded earlier.
    2. For each grid, fetches hourly weather (temperature, humidity, precipitation, wind speed, wind direction) from 2018-01-01 to 2022-12-31.
    3. Aggregate and obtain daily weather dataset.
    4. **Output** "zipcode_grid_fetched_weather.rds"
3. zipcode_prediction.rmd:
    1. **Input**: "zipcode_grid_fetched_weather.rds", "../../rfiles/fit.xgb_da_log_h2s_dis_ind.rds", (files associated with variables creations).
        - Reads in weather data and xgboost model
    2. Computes the rest of the predictor variables for each grid.
    3. Fits the model and acquire daily predictions for each grid.
    4. Post-prediction cleaning:
        1. Map grid prediction to zipcode prediction by taking a weighted average of grids that intersect with the zipcode.
    5. **Output** "zipcode_grid_result.rds", "zipcode_daily_average.rds", "zipcode_daily_average.csv"
