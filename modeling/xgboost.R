# XGB Models
library(tidyverse)
library(caret)
library(fastDummies)
select <- dplyr::select

daily_full <- readRDS('../../data/daily_full.rds') %>%
  mutate(Refinery = str_replace_all(str_replace_all(Refinery, '[()]', ''), ' ', '_'),
         Monitor = str_replace_all(Monitor, ' ', '_'),
         weekday = weekday,
         daily_downwind_ref = as.integer(daily_downwind_ref),
         daily_downwind_wrp = as.integer(daily_downwind_wrp),
         MinDist = 1/(MinDist^2),
         dist_wrp = 1/(dist_wrp^2),
         dist_dc = 1/(dist_dc^2))

predictors <- c('H2S_daily_avg', 'month', 'year', 'weekday', 'wd_avg', 'ws_avg', 
                'daily_downwind_ref', 'dist_wrp', 'MinDist',
                'mon_utm_x', 'mon_utm_y', 'day', 'monthly_oil_2km', 'monthly_gas_2km', 
                'active_2km', 'inactive_2km', 'daily_downwind_wrp', 'elevation', 'EVI', 'num_odor_complaints',
                'dist_dc', 'capacity', 'avg_temp', 'avg_hum', 'precip') 

predictors_no_met <- c('H2S_daily_avg', 'month', 'year', 'weekday', 'dist_wrp', 'MinDist',
                'mon_utm_x', 'mon_utm_y', 'day', 'monthly_oil_2km', 'monthly_gas_2km', 
                'active_2km', 'inactive_2km', 'elevation', 'EVI', 'num_odor_complaints',
                'dist_dc', 'capacity') 

predictors_no_hum <- c('H2S_daily_avg', 'month', 'year', 'weekday', 'wd_avg', 'ws_avg', 
                       'daily_downwind_ref', 'dist_wrp', 'MinDist',
                       'mon_utm_x', 'mon_utm_y', 'day', 'monthly_oil_2km', 'monthly_gas_2km', 
                       'active_2km', 'inactive_2km', 'daily_downwind_wrp', 'elevation', 'EVI', 'num_odor_complaints',
                       'dist_dc', 'capacity', 'avg_temp', 'precip') 

# Since Feb 2022
train <- daily_full %>%
  filter(day >= '2022-01-31') %>%
  filter(complete.cases(.))

train <- fastDummies::dummy_cols(train %>%
                                   select(-c(Refinery, Monitor, day, H2S_daily_max, H2S_monthly_average,
                                             monitor_lat, monitor_lon, county, dist_213)),
                                 remove_selected_columns = TRUE)

# Try for a continuous month
tune_grid <- expand.grid(nrounds = c(100, 200, 500),
                         max_depth = c(3, 4, 5),
                         eta = c(0.1, 0.3),
                         gamma = c(0.01, 0.001),
                         colsample_bytree = c(0.5, 1),
                         min_child_weight = 0,
                         subsample = c(0.5, 0.75, 1))

# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", 
                        number=10,
                        verboseIter=TRUE, 
                        search='grid',
                        savePredictions = 'final')

fit.xgb_da <- train(H2S_daily_avg~.,
                 method = 'xgbTree',
                 data = train,
                 trControl=control,
                 tuneGrid = tune_grid,
                 tuneLength = 10, importance=TRUE, verbosity = 0, verbose=FALSE)
saveRDS(fit.xgb_da, 'rfiles/fit.xgb_da.rds')

# Disaster Only
train <- daily_full[complete.cases(daily_full),] %>%
  filter(year == '2021', month %in% c('10', '11', '12'))

train <- fastDummies::dummy_cols(train %>%
                                   select(-c(Refinery, Monitor, day, H2S_daily_max, H2S_monthly_average,
                                             monitor_lat, monitor_lon, county, dist_213, year)),
                                 remove_selected_columns = TRUE)

fit.xgb_da_dis <- train(H2S_daily_avg~.,
                 method = 'xgbTree',
                 data = train,
                 trControl=control,
                 tuneGrid = tune_grid,
                 tuneLength = 10, importance=TRUE, verbosity = 0, verbose=FALSE)
saveRDS(fit.xgb_da_dis, 'rfiles/fit.xgb_da_dis.rds')

# Exclude Disaster
train <- daily_full[complete.cases(daily_full),] %>%
  filter(!(year == '2021' & month %in% c('10', '11', '12')))

train <- fastDummies::dummy_cols(train %>%
                                   select(-c(Refinery, Monitor, day, H2S_daily_max, H2S_monthly_average,
                                             monitor_lat, monitor_lon, county, dist_213)),
                                 remove_selected_columns = TRUE)

fit.xgb_da_excl_dis <- train(H2S_daily_avg~.,
                 method = 'xgbTree',
                 data = train,
                 trControl=control,
                 tuneGrid = tune_grid,
                 tuneLength = 10, importance=TRUE, verbosity = 0, verbose=FALSE)
saveRDS(fit.xgb_da_excl_dis, 'rfiles/fit.xgb_da_excl_dis.rds')


# Everything w. Disaster Indicator
everything <- daily_full %>% 
  select(all_of(predictors)) %>% 
  mutate(disaster = if_else(year == '2021' & month %in% c('10', '11', '12'), 1, 0)) %>% 
  filter(complete.cases(.)) 

train <- everything

train <- fastDummies::dummy_cols(train %>%
                                   select(-c(day)),
                                 remove_selected_columns = TRUE)

# 10 Fold for Everything models
set.seed(90)
folds_1 <- createFolds(which(train$disaster == 1), k = 10)
folds_0 <- createFolds(which(train$disaster == 0), k = 10)
folds <- list()
for (i in 1:10){
  folds <- append(folds, list(c(which(train$disaster == 1)[folds_1[[i]]], 
                           which(train$disaster == 0)[folds_0[[i]]])))
}

# Separate trainControl for everything models, stratified sampling folds
control_everything <- trainControl(method="cv", 
                        number=10,
                        indexOut = folds,
                        verboseIter=TRUE, 
                        search='grid',
                        savePredictions = 'final')

fit.xgb_da_full_dis_ind <- train(H2S_daily_avg~.,
                 method = 'xgbTree',
                 data = train,
                 trControl=control_everything,
                 tuneGrid = tune_grid,
                 tuneLength = 10, importance=TRUE, verbosity = 0, verbose=FALSE)
saveRDS(fit.xgb_da_full_dis_ind, 'rfiles/fit.xgb_da_full_dis_ind.rds')

# Everything w.o Disaster Indicator
train <- everything %>% 
  select(-disaster)

train <- fastDummies::dummy_cols(train %>%
                                   select(-c(day)),
                                 remove_selected_columns = TRUE)

fit.xgb_da_full <- train(H2S_daily_avg~.,
                 method = 'xgbTree',
                 data = train,
                 trControl=control_everything,
                 tuneGrid = tune_grid,
                 tuneLength = 10, importance=TRUE, verbosity = 0, verbose=FALSE)
saveRDS(fit.xgb_da_full, 'rfiles/fit.xgb_da_full.rds')

# Box Cox
# Since Feb 2022
daily_avg_train_sincefeb2022 <- daily_full %>% 
  filter(day >= '2022-01-31') %>%
  select(all_of(predictors)) %>%
  filter(complete.cases(.))

train <- daily_avg_train_sincefeb2022 %>% 
  mutate(H2S_daily_avg = log(H2S_daily_avg))

train <- fastDummies::dummy_cols(train %>%
                                   select(-c(day)),
                                 remove_selected_columns = TRUE)

fit.xgb_da_log_h2s_sincefeb2022 <- train(H2S_daily_avg~.,
                 method = 'xgbTree',
                 data = train,
                 trControl=control,
                 tuneGrid = tune_grid,
                 tuneLength = 10, importance=TRUE, verbosity = 0, verbose=FALSE)
saveRDS(fit.xgb_da_log_h2s_sincefeb2022, 'rfiles/fit.xgb_da_log_h2s_sincefeb2022.rds')

# Disaster Only
disaster <- daily_full %>% 
  filter(year == '2021', month %in% c('10', '11', '12')) %>% 
  select(all_of(predictors)) %>% 
  filter(complete.cases(.))

train <- disaster %>% 
  mutate(H2S_daily_avg = log(H2S_daily_avg))

train <- fastDummies::dummy_cols(train %>%
                                   select(-c(day)),
                                 remove_selected_columns = TRUE)

fit.xgb_da_log_h2s_dis <- readRDS('rfiles/fit.xgb_da_log_h2s_dis.rds')
fit.xgb_da_log_h2s_dis <- train(H2S_daily_avg~.,
                 method = 'xgbTree',
                 data = train,
                 trControl=control,
                 tuneGrid = tune_grid,
                 tuneLength = 10, importance=TRUE, verbosity = 0, verbose=FALSE)
saveRDS(fit.xgb_da_log_h2s_dis, 'rfiles/fit.xgb_da_log_h2s_dis.rds')

# Exclude Disaster
excl_disaster <- daily_full %>% 
  filter(!(year == '2021' & month %in% c('10', '11', '12'))) %>% 
  select(all_of(predictors)) %>% 
  filter(complete.cases(.))

train <- excl_disaster %>% 
  mutate(H2S_daily_avg = log(H2S_daily_avg))

train <- fastDummies::dummy_cols(train %>%
                                   select(-c(day)),
                                 remove_selected_columns = TRUE)

fit.xgb_da_log_h2s_excl_dis <- train(H2S_daily_avg~.,
                 method = 'xgbTree',
                 data = train,
                 trControl=control,
                 tuneGrid = tune_grid,
                 tuneLength = 10, importance=TRUE, verbosity = 0, verbose=FALSE)
saveRDS(fit.xgb_da_log_h2s_excl_dis, 'rfiles/fit.xgb_da_log_h2s_excl_dis.rds')

# Everything w. Disaster Indicator
everything <- daily_full %>%
  select(all_of(predictors)) %>%
  mutate(disaster = if_else(year == '2021' & month %in% c('10', '11', '12'), 1, 0)) %>%
  filter(complete.cases(.))

train <- everything %>%
  mutate(H2S_daily_avg = log(H2S_daily_avg))

train <- fastDummies::dummy_cols(train %>%
                                   select(-c(day)),
                                 remove_selected_columns = TRUE)

fit.xgb_da_log_h2s_dis_ind <- train(H2S_daily_avg~.,
                 method = 'xgbTree',
                 data = train,
                 trControl=control_everything,
                 tuneGrid = tune_grid,
                 tuneLength = 10, importance=TRUE, verbosity = 0, verbose=FALSE)
saveRDS(fit.xgb_da_log_h2s_dis_ind, 'rfiles/fit.xgb_da_log_h2s_dis_ind.rds')

# Everything w.o Disaster Indicator
train <- everything %>% 
  select(-disaster) %>%
  mutate(H2S_daily_avg = log(H2S_daily_avg))

train <- fastDummies::dummy_cols(train %>%
                                   select(-c(day)),
                                 remove_selected_columns = TRUE)

fit.xgb_da_log_h2s_full <- train(H2S_daily_avg~.,
                 method = 'xgbTree',
                 data = train,
                 trControl=control_everything,
                 tuneGrid = tune_grid,
                 tuneLength = 10, importance=TRUE, verbosity = 0, verbose=FALSE)
saveRDS(fit.xgb_da_log_h2s_full, 'rfiles/fit.xgb_da_log_h2s_full.rds')

# # 80/20 split on everything log models
# # Everything w. Disaster Indicator
# set.seed(123)
# everything_80train <- daily_full %>% 
#   select(all_of(predictors)) %>% 
#   mutate(disaster = if_else(year == '2021' & month %in% c('10', '11', '12'), 1, 0)) %>% 
#   filter(complete.cases(.)) %>%
#   slice_sample(prop = 0.8) 
# 
# everything_20test <- anti_join(daily_full %>% 
#                                select(all_of(predictors)) %>% 
#                                mutate(disaster = if_else(year == '2021' & month %in% c('10', '11', '12'), 1, 0)) %>% 
#                                filter(complete.cases(.)),
#                                everything_80train)
# 
# train <- everything_80train %>% 
#   mutate(H2S_daily_avg = log(H2S_daily_avg))
# 
# train <- fastDummies::dummy_cols(train %>%
#                                    select(-c(day)),
#                                  remove_selected_columns = TRUE)
# 
# test <- everything_20test  %>% 
#   mutate(H2S_daily_avg = log(H2S_daily_avg))
# 
# test <- fastDummies::dummy_cols(test %>%
#                                   select(-c(day)),
#                                 remove_selected_columns = TRUE)
# 
# # 10 Fold for Everything models
# set.seed(90)
# folds_1 <- createFolds(which(train$disaster == 1), k = 10)
# folds_0 <- createFolds(which(train$disaster == 0), k = 10)
# folds <- list()
# for (i in 1:10){
#   folds <- append(folds, list(c(which(train$disaster == 1)[folds_1[[i]]], 
#                                 which(train$disaster == 0)[folds_0[[i]]])))
# }
# 
# # Separate trainControl for everything models, stratified sampling folds
# control_everything_8020 <- trainControl(method="cv", 
#                                    number=10,
#                                    indexOut = folds,
#                                    verboseIter=TRUE, 
#                                    search='grid',
#                                    savePredictions = 'final')
# 
# fit.xgb_da_log_h2s_dis_ind_8020 <- train(H2S_daily_avg~.,
#                                     method = 'xgbTree',
#                                     data = train,
#                                     trControl=control_everything_8020,
#                                     tuneGrid = tune_grid,
#                                     tuneLength = 10, importance=TRUE, verbosity = 0, verbose=FALSE)
# saveRDS(fit.xgb_da_log_h2s_dis_ind_8020, 'rfiles/fit.xgb_da_log_h2s_dis_ind_8020.rds')
# 
# # Everything w.o Disaster Indicator
# train <- train %>% select(-disaster)
# test <- test %>% select(-disaster)
# fit.xgb_da_log_h2s_full_8020 <- train(H2S_daily_avg~.,
#                                  method = 'xgbTree',
#                                  data = train,
#                                  trControl=control_everything_8020,
#                                  tuneGrid = tune_grid,
#                                  tuneLength = 10, importance=TRUE, verbosity = 0, verbose=FALSE)
# saveRDS(fit.xgb_da_log_h2s_full_8020, 'rfiles/fit.xgb_da_log_h2s_full_8020.rds')

# Since Feb 2022 without meteorological
# train <- daily_full %>%
#   select(all_of(predictors_no_met)) %>%
#   filter(day >= '2022-01-31') %>%
#   mutate(H2S_daily_avg = log(H2S_daily_avg)) %>%
#   filter(complete.cases(.)) 
# 
# train <- fastDummies::dummy_cols(train %>%
#                                    select(-c(day)),
#                                  remove_selected_columns = TRUE)
# 
# fit.xgb_da_log_h2s_sincefeb2022_no_met <- train(H2S_daily_avg~.,
#                                            method = 'xgbTree',
#                                            data = train,
#                                            trControl=control,
#                                            tuneGrid = tune_grid,
#                                            tuneLength = 10, importance=TRUE, verbosity = 0, verbose=FALSE)
# saveRDS(fit.xgb_da_log_h2s_sincefeb2022_no_met, 'rfiles/fit.xgb_da_log_h2s_sincefeb2022_no_met.rds')

# Everything Models without meteorological

# Everything w. Disaster Indicator
everything_no_met <- daily_full %>% 
  select(all_of(predictors_no_met)) %>% 
  mutate(disaster = if_else(year == '2021' & month %in% c('10', '11', '12'), 1, 0)) %>% 
  filter(complete.cases(.)) 

train <- everything_no_met %>% 
  mutate(H2S_daily_avg = log(H2S_daily_avg))

train <- fastDummies::dummy_cols(train %>%
                                   select(-c(day)),
                                 remove_selected_columns = TRUE)

# 10 Fold for Everything models
set.seed(90)
folds_1 <- createFolds(which(train$disaster == 1), k = 10)
folds_0 <- createFolds(which(train$disaster == 0), k = 10)
folds <- list()
for (i in 1:10){
  folds <- append(folds, list(c(which(train$disaster == 1)[folds_1[[i]]], 
                                which(train$disaster == 0)[folds_0[[i]]])))
}

# Separate trainControl for everything models, stratified sampling folds
control_everything_no_met <- trainControl(method="cv", 
                                   number=10,
                                   indexOut = folds,
                                   verboseIter=TRUE, 
                                   search='grid',
                                   savePredictions = 'final')

fit.xgb_da_log_h2s_dis_ind_no_met <- train(H2S_daily_avg~.,
                                    method = 'xgbTree',
                                    data = train,
                                    trControl=control_everything_no_met,
                                    tuneGrid = tune_grid,
                                    tuneLength = 10, importance=TRUE, verbosity = 0, verbose=FALSE)
saveRDS(fit.xgb_da_log_h2s_dis_ind_no_met, 'rfiles/fit.xgb_da_log_h2s_dis_ind_no_met.rds')

# Everything w.o Disaster Indicator
# train <- everything_no_met %>% 
#   select(-disaster) %>%
#   mutate(H2S_daily_avg = log(H2S_daily_avg))
# 
# train <- fastDummies::dummy_cols(train %>%
#                                    select(-c(day)),
#                                  remove_selected_columns = TRUE)
# 
# fit.xgb_da_log_h2s_full_no_met <- train(H2S_daily_avg~.,
#                                  method = 'xgbTree',
#                                  data = train,
#                                  trControl=control_everything_no_met,
#                                  tuneGrid = tune_grid,
#                                  tuneLength = 10, importance=TRUE, verbosity = 0, verbose=FALSE)
# saveRDS(fit.xgb_da_log_h2s_full_no_met, 'rfiles/fit.xgb_da_log_h2s_full_no_met.rds')

