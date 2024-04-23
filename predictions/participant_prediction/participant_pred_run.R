# Script to run prediction
library(tidyverse)
library(lubridate)
library(fastDummies)
# Load in XGBoost model with disaster indicator
fit.xgb_da_log_h2s_dis_ind <- readRDS('fit.xgb_da_log_h2s_dis_ind.rds')

# Load in data
data_exposure <- read_csv('data_exposure_pred_ready.csv')
data_visit1 <- read_csv('data_visit1_pred_ready.csv')
data_visit2 <- read_csv('data_visit2_pred_ready.csv')

# ------------------------
# format data 
data_exposure_mat <- data_exposure %>% 
  mutate(year = year(day),
         month = month(day),
         weekday = relevel(factor(wday(data_exposure$day, label=TRUE), ordered = FALSE), ref = "Sun")) %>%
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
  rename(avg_hum = daily_hum,
         wd_avg = daily_wd,
         ws_avg = daily_ws,
         avg_temp = daily_temp,
         precip = daily_precip) %>%
  select(-month)


data_exposure_mat <- fastDummies::dummy_cols(data_exposure_mat,
                                           remove_selected_columns = TRUE)

# predict
data_exposure_pred <- data_exposure %>%
  mutate(daily_h2s_log_prediction = predict(fit.xgb_da_log_h2s_dis_ind, newdata = data_exposure_mat))

data_exposure_pred$daily_h2s_prediction <- exp(data_exposure_pred$daily_h2s_log_prediction)

write_csv(data_exposure_pred, 'data_exposure_prediction_result.csv')

# -------------------------
# format data
data_visit1_mat <- data_visit1 %>% 
  mutate(year = year(day),
         month = month(day),
         weekday = relevel(factor(wday(data_visit1$day, label=TRUE), ordered = FALSE), ref = "Sun")) %>%
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
  rename(avg_hum = daily_hum,
         wd_avg = daily_wd,
         ws_avg = daily_ws,
         avg_temp = daily_temp,
         precip = daily_precip) %>%
  select(-month)


data_visit1_mat <- fastDummies::dummy_cols(data_visit1_mat,
                                           remove_selected_columns = TRUE)

# predict
data_visit1_pred <- data_visit1 %>%
  mutate(daily_h2s_log_prediction = predict(fit.xgb_da_log_h2s_dis_ind, newdata = data_visit1_mat))

data_visit1_pred$daily_h2s_prediction <- exp(data_visit1_pred$daily_h2s_log_prediction)

write_csv(data_visit1_pred, 'data_visit1_prediction_result.csv')
# -------------------------
data_visit2_complete <- data_visit2 %>% filter(complete.cases(.))
data_visit2_incomplete <- data_visit2 %>% filter(!complete.cases(.))

# format data
data_visit2_mat <- data_visit2_complete %>%
  mutate(year = year(day),
         month = month(day),
         weekday = relevel(factor(wday(data_visit2_complete$day, label=TRUE), ordered = FALSE), ref = "Sun")) %>%
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
  rename(avg_hum = daily_hum,
         wd_avg = daily_wd,
         ws_avg = daily_ws,
         avg_temp = daily_temp,
         precip = daily_precip) %>%
  select(-month)


data_visit2_mat <- fastDummies::dummy_cols(data_visit2_mat,
                                           remove_selected_columns = TRUE)

# predict
data_visit2_pred <- data_visit2_complete %>%
  mutate(daily_h2s_log_prediction = predict(fit.xgb_da_log_h2s_dis_ind, newdata = data_visit2_mat))

data_visit2_pred$daily_h2s_prediction <- exp(data_visit2_pred$daily_h2s_log_prediction)

data_visit2_pred <- rbind(data_visit2_pred, 
                          data_visit2_incomplete %>% 
                            mutate(daily_h2s_log_prediction = NA, 
                            daily_h2s_prediction = NA))

write_csv(data_visit2_pred, 'data_visit2_prediction_result.csv')
