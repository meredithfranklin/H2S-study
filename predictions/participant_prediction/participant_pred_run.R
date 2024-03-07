# Script to run prediction
library(tidyverse)
library(lubridate)
library(fastDummies)
# Load in XGBoost model with disaster indicator
fit.xgb_da_log_h2s_dis_ind <- readRDS('rfiles/fit.xgb_da_log_h2s_dis_ind.rds')

# Load in data
data_visit1 <- read_csv('data_visit1_result.csv')
#data_visit2 <- read_csv('data_visit2.csv')

# format data
data_visit1_mat <- data_visit1 %>% 
  mutate(year = year(date_prior),
         month = month(date_prior),
         weekday = relevel(factor(wday(data_visit1$date_prior, label=TRUE), ordered = FALSE), ref = "Sun")) %>%
  mutate(month_01 = if_else(month == 1, as.integer(1), as.integer(0)),
         month_02 = if_else(month == 2, as.integer(1), as.integer(0)),
         month_03 = if_else(month == 3, as.integer(1), as.integer(0)),
         month_04 = if_else(month == 4, as.integer(1), as.integer(0)),
         month_05 = if_else(month == 5, as.integer(1), as.integer(0)),
         month_06 = if_else(month == 6, as.integer(1), as.integer(0)),
         month_07 = if_else(month == 7, as.integer(1), as.integer(0)),
         month_08 = if_else(month == 8, as.integer(1), as.integer(0)),
         month_09 = if_else(month == 9, as.integer(1), as.integer(0)),
         month_10 = if_else(month == 10, as.integer(1), as.integer(0)),
         month_11 = if_else(month == 11, as.integer(1), as.integer(0)),
         month_12 = if_else(month == 12, as.integer(1), as.integer(0)),
         year_2020 = if_else(year == 2020, as.integer(1), as.integer(0)),
         year_2021 = if_else(year == 2021, as.integer(1), as.integer(0)),
         year_2022 = if_else(year == 2022, as.integer(1), as.integer(0)),
         year_2023 = if_else(year == 2023, as.integer(1), as.integer(0))) %>%
  mutate(disaster = if_else(year == '2021' & month %in% c(10, 11, 12), 1, 0),
         MinDist = 1/(MinDist^2),
         dist_wrp = 1/(dist_wrp^2),
         dist_dc = 1/(dist_dc^2)) %>%
  select(-month)


data_visit1_mat <- fastDummies::dummy_cols(data_visit1_mat,
                                           remove_selected_columns = TRUE)

# predict
data_visit1_pred <- data_visit1 %>%
  mutate(xgb_dis_ind_predict = predict(fit.xgb_da_log_h2s_dis_ind, newdata = data_visit1_mat))

# ------------------------
# format data 
data_exposure_mat <- complete_set %>% 
  mutate(year = year(exposure_date),
         month = month(exposure_date),
         weekday = relevel(factor(wday(complete_set$exposure_date, label=TRUE), ordered = FALSE), ref = "Sun")) %>%
  mutate(month_01 = if_else(month == 1, as.integer(1), as.integer(0)),
         month_02 = if_else(month == 2, as.integer(1), as.integer(0)),
         month_03 = if_else(month == 3, as.integer(1), as.integer(0)),
         month_04 = if_else(month == 4, as.integer(1), as.integer(0)),
         month_05 = if_else(month == 5, as.integer(1), as.integer(0)),
         month_06 = if_else(month == 6, as.integer(1), as.integer(0)),
         month_07 = if_else(month == 7, as.integer(1), as.integer(0)),
         month_08 = if_else(month == 8, as.integer(1), as.integer(0)),
         month_09 = if_else(month == 9, as.integer(1), as.integer(0)),
         month_10 = if_else(month == 10, as.integer(1), as.integer(0)),
         month_11 = if_else(month == 11, as.integer(1), as.integer(0)),
         month_12 = if_else(month == 12, as.integer(1), as.integer(0)),
         year_2020 = if_else(year == 2020, as.integer(1), as.integer(0)),
         year_2021 = if_else(year == 2021, as.integer(1), as.integer(0)),
         year_2022 = if_else(year == 2022, as.integer(1), as.integer(0)),
         year_2023 = if_else(year == 2023, as.integer(1), as.integer(0))) %>%
  mutate(disaster = if_else(year == '2021' & month %in% c(10, 11, 12), 1, 0),
         MinDist = 1/(MinDist^2),
         dist_wrp = 1/(dist_wrp^2),
         dist_dc = 1/(dist_dc^2)) %>%
  rename(daily_downwind_ref = downwind_ref,
         daily_downwind_wrp = downwind_wrp,
         avg_hum = hum) %>%
  select(-month)


data_exposure_mat <- fastDummies::dummy_cols(data_exposure_mat,
                                           remove_selected_columns = TRUE)

# predict
data_exposure_pred <- complete_set %>%
  mutate(xgb_dis_ind_predict = predict(fit.xgb_da_log_h2s_dis_ind, newdata = data_exposure_mat))

