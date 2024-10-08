# XGB Models 
# 2024-09-07

library(tidyverse)
library(stringr)
library(tidyr)
library(dplyr)
library(caret)
library(fastDummies)
select <- dplyr::select

savedir <- '../rfiles/xgboost_v2/'
#savedir <- 'xgboost/'
daily_full <- readRDS('../data/daily_full_20230930.rds')

daily_full <- daily_full %>%
  mutate(Monitor = str_replace_all(Monitor, ' ', '_'),
         weekday = weekday,
         daily_downwind_ref = as.integer(daily_downwind_ref),
         daily_downwind_wrp = as.integer(daily_downwind_wrp),
         dist_ref = 1/(dist_ref^2),
         dist_wrp = 1/(dist_wrp^2),
         dist_dc = 1/(dist_dc^2))

predictors <- c('month', 'year', 'weekday', 'wd_avg', 'ws_avg', 
                'daily_downwind_ref', 'dist_wrp', 'dist_ref',
                'mon_utm_x', 'mon_utm_y', 'monthly_oil_2km', 'monthly_gas_2km', 
                'active_2km', 'inactive_2km', 'daily_downwind_wrp', 'elevation', 'EVI', 'num_odor_complaints',
                'dist_dc', 'closest_wrp_capacity', 'daily_temp', 'daily_hum', 'daily_precip') 

# Daily Average
# Since Feb 2022
since_feb2022 <- daily_full %>% 
  filter(day >= '2022-01-31') %>%
  select(all_of(c('H2S_daily_avg', predictors))) %>% 
  filter(complete.cases(.))

train <- since_feb2022

train <- fastDummies::dummy_cols(train,
                                 remove_selected_columns = TRUE)

# Try for a continuous month
tune_grid <- expand.grid(nrounds = c(300, 500, 700),
                         max_depth = c(4, 5, 6),
                         eta = c(0.1, 0.3),
                         gamma = c(0.01, 0.001),
                         colsample_bytree = c(0.5, 0.75, 1),
                         min_child_weight = 0,
                         subsample = c(0.5, 0.75, 1))

# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", 
                        number=10,
                        verboseIter=TRUE, 
                        search='grid',
                        savePredictions = 'final')

fit.xgb_da_sincefeb2022 <- train(H2S_daily_avg~.,
                    method = 'xgbTree',
                    data = train,
                    trControl=control,
                    tuneGrid = tune_grid,
                    tuneLength = 10, importance=TRUE, verbosity = 0, verbose=FALSE)
saveRDS(fit.xgb_da_sincefeb2022, paste0(savedir, 'fit.xgb_da_sincefeb2022', '.rds'))
gc();rm(fit.xgb_da_sincefeb2022)

# Disaster Only
disaster <- daily_full %>% 
  filter(year == '2021', month %in% c('10', '11', '12')) %>% 
  select(c('H2S_daily_avg', predictors)) %>% 
  filter(complete.cases(.))

train <- disaster

train <- fastDummies::dummy_cols(train,
                                 remove_selected_columns = TRUE)

fit.xgb_da_dis <- train(H2S_daily_avg~.,
                        method = 'xgbTree',
                        data = train,
                        trControl=control,
                        tuneGrid = tune_grid,
                        tuneLength = 10, importance=TRUE, verbosity = 0, verbose=FALSE)
saveRDS(fit.xgb_da_dis,  paste0(savedir, 'fit.xgb_da_dis', '.rds'))
gc();rm(fit.xgb_da_dis)

# Exclude Disaster
excl_disaster <- daily_full %>% 
  filter(!(year == '2021' & month %in% c('10', '11', '12'))) %>% 
  select(c('H2S_daily_avg', predictors)) %>% 
  filter(complete.cases(.))

train <- excl_disaster

train <- fastDummies::dummy_cols(train,
                                 remove_selected_columns = TRUE)

fit.xgb_da_excl_dis <- train(H2S_daily_avg~.,
                             method = 'xgbTree',
                             data = train,
                             trControl=control,
                             tuneGrid = tune_grid,
                             tuneLength = 10, importance=TRUE, verbosity = 0, verbose=FALSE)
saveRDS(fit.xgb_da_excl_dis,  paste0(savedir, 'fit.xgb_da_excl_dis', '.rds'))
gc();rm(fit.xgb_da_excl_dis)

# Everything w. Disaster Indicator
everything <- daily_full %>%
  select(c('H2S_daily_avg', predictors)) %>%
  mutate(disaster = if_else(year == '2021' & month %in% c('10', '11', '12'), 1, 0)) %>%
  filter(complete.cases(.))

train <- everything

train <- fastDummies::dummy_cols(train,
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
saveRDS(fit.xgb_da_full_dis_ind,  paste0(savedir, 'fit.xgb_da_full_dis_ind', '.rds'))
gc();rm(fit.xgb_da_full_dis_ind)

# Everything w.o Disaster Indicator
train <- everything %>% 
  select(-disaster)

train <- fastDummies::dummy_cols(train,
                                 remove_selected_columns = TRUE)

fit.xgb_da_full <- train(H2S_daily_avg~.,
                         method = 'xgbTree',
                         data = train,
                         trControl=control_everything,
                         tuneGrid = tune_grid,
                         tuneLength = 10, importance=TRUE, verbosity = 0, verbose=FALSE)
saveRDS(fit.xgb_da_full, paste0(savedir, 'fit.xgb_da_full', '.rds'))
gc();rm(fit.xgb_da_full)

# Log H2S average
# Since Feb 2022
train <- since_feb2022 %>% 
  mutate(H2S_daily_avg = log(H2S_daily_avg))

train <- fastDummies::dummy_cols(train,
                                 remove_selected_columns = TRUE)

fit.xgb_da_log_h2s_sincefeb2022 <- train(H2S_daily_avg~.,
                                         method = 'xgbTree',
                                         data = train,
                                         trControl=control,
                                         tuneGrid = tune_grid,
                                         tuneLength = 10, importance=TRUE, verbosity = 0, verbose=FALSE)
saveRDS(fit.xgb_da_log_h2s_sincefeb2022,  paste0(savedir, 'fit.xgb_da_log_h2s_sincefeb2022', '.rds'))
gc();rm(fit.xgb_da_log_h2s_sincefeb2022)

# Disaster Only
train <- disaster %>% 
  mutate(H2S_daily_avg = log(H2S_daily_avg))

train <- fastDummies::dummy_cols(train,
                                 remove_selected_columns = TRUE)

fit.xgb_da_log_h2s_dis <- train(H2S_daily_avg~.,
                                method = 'xgbTree',
                                data = train,
                                trControl=control,
                                tuneGrid = tune_grid,
                                tuneLength = 10, importance=TRUE, verbosity = 0, verbose=FALSE)
saveRDS(fit.xgb_da_log_h2s_dis,  paste0(savedir, 'fit.xgb_da_log_h2s_dis', '.rds'))
gc();rm(fit.xgb_da_log_h2s_dis)

# Exclude Disaster
train <- excl_disaster %>% 
  mutate(H2S_daily_avg = log(H2S_daily_avg))

train <- fastDummies::dummy_cols(train,
                                 remove_selected_columns = TRUE)

fit.xgb_da_log_h2s_excl_dis <- train(H2S_daily_avg~.,
                                     method = 'xgbTree',
                                     data = train,
                                     trControl=control,
                                     tuneGrid = tune_grid,
                                     tuneLength = 10, importance=TRUE, verbosity = 0, verbose=FALSE)
saveRDS(fit.xgb_da_log_h2s_excl_dis,  paste0(savedir, 'fit.xgb_da_log_h2s_excl_dis', '.rds'))
gc();rm(fit.xgb_da_log_h2s_excl_dis)

# Everything w. Disaster Indicator
train <- everything %>%
  mutate(H2S_daily_avg = log(H2S_daily_avg))

train <- fastDummies::dummy_cols(train,
                                 remove_selected_columns = TRUE)

fit.xgb_da_log_h2s_dis_ind <- train(H2S_daily_avg~.,
                                    method = 'xgbTree',
                                    data = train,
                                    trControl=control_everything,
                                    tuneGrid = tune_grid,
                                    tuneLength = 10, importance=TRUE, verbosity = 0, verbose=FALSE)
saveRDS(fit.xgb_da_log_h2s_dis_ind,  paste0(savedir, 'fit.xgb_da_log_h2s_dis_ind', '.rds'))
gc();rm(fit.xgb_da_log_h2s_dis_ind)

# Everything w.o Disaster Indicator
train <- everything %>% 
  select(-disaster) %>%
  mutate(H2S_daily_avg = log(H2S_daily_avg))

train <- fastDummies::dummy_cols(train,
                                 remove_selected_columns = TRUE)

fit.xgb_da_log_h2s_full <- train(H2S_daily_avg~.,
                                 method = 'xgbTree',
                                 data = train,
                                 trControl=control_everything,
                                 tuneGrid = tune_grid,
                                 tuneLength = 10, importance=TRUE, verbosity = 0, verbose=FALSE)
saveRDS(fit.xgb_da_log_h2s_full,  paste0(savedir, 'fit.xgb_da_log_h2s_full', '.rds'))
gc();rm(fit.xgb_da_log_h2s_full)

#------------------------Daily Max-----------------------------#
# Daily max
# Since Feb 2022
since_feb2022 <- daily_full %>% 
  filter(day >= '2022-01-31') %>%
  select(c('H2S_daily_max', predictors)) %>% 
  filter(complete.cases(.))

train <- since_feb2022

train <- fastDummies::dummy_cols(train,
                                 remove_selected_columns = TRUE)

# Try for a continuous month
tune_grid <- expand.grid(nrounds = c(300, 500, 700),
                         max_depth = c(4, 5, 6),
                         eta = c(0.1, 0.3),
                         gamma = c(0.01, 0.001),
                         colsample_bytree = c(0.5, 0.75, 1),
                         min_child_weight = 0,
                         subsample = c(0.5, 0.75, 1))

# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", 
                        number=10,
                        verboseIter=TRUE, 
                        search='grid',
                        savePredictions = 'final')

fit.xgb_dm_sincefeb2022 <- train(H2S_daily_max~.,
                    method = 'xgbTree',
                    data = train,
                    trControl=control,
                    tuneGrid = tune_grid,
                    tuneLength = 10, importance=TRUE, verbosity = 0, verbose=FALSE)
saveRDS(fit.xgb_dm_sincefeb2022,  paste0(savedir, 'fit.xgb_dm_sincefeb2022', '.rds'))
gc();rm(fit.xgb_dm_sincefeb2022)

# Disaster Only
disaster <- daily_full %>% 
  filter(year == '2021', month %in% c('10', '11', '12')) %>% 
  select(c('H2S_daily_max', predictors)) %>% 
  filter(complete.cases(.))

train <- disaster

train <- fastDummies::dummy_cols(train,
                                 remove_selected_columns = TRUE)

fit.xgb_dm_dis <- train(H2S_daily_max~.,
                        method = 'xgbTree',
                        data = train,
                        trControl=control,
                        tuneGrid = tune_grid,
                        tuneLength = 10, importance=TRUE, verbosity = 0, verbose=FALSE)
saveRDS(fit.xgb_dm_dis,  paste0(savedir, 'fit.xgb_dm_dis', '.rds'))
gc();rm(fit.xgb_dm_dis)

# Exclude Disaster
excl_disaster <- daily_full %>% 
  filter(!(year == '2021' & month %in% c('10', '11', '12'))) %>% 
  select(c('H2S_daily_max', predictors)) %>% 
  filter(complete.cases(.))

train <- excl_disaster

train <- fastDummies::dummy_cols(train,
                                 remove_selected_columns = TRUE)

fit.xgb_dm_excl_dis <- train(H2S_daily_max~.,
                             method = 'xgbTree',
                             data = train,
                             trControl=control,
                             tuneGrid = tune_grid,
                             tuneLength = 10, importance=TRUE, verbosity = 0, verbose=FALSE)
saveRDS(fit.xgb_dm_excl_dis,  paste0(savedir, 'fit.xgb_dm_excl_dis', '.rds'))
gc();rm(fit.xgb_dm_excl_dis)

# Everything w. Disaster Indicator
everything <- daily_full %>%
  select(c('H2S_daily_max', predictors)) %>%
  mutate(disaster = if_else(year == '2021' & month %in% c('10', '11', '12'), 1, 0)) %>%
  filter(complete.cases(.))

train <- everything

train <- fastDummies::dummy_cols(train,
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

fit.xgb_dm_full_dis_ind <- train(H2S_daily_max~.,
                                 method = 'xgbTree',
                                 data = train,
                                 trControl=control_everything,
                                 tuneGrid = tune_grid,
                                 tuneLength = 10, importance=TRUE, verbosity = 0, verbose=FALSE)
saveRDS(fit.xgb_dm_full_dis_ind,  paste0(savedir, 'fit.xgb_dm_full_dis_ind', '.rds'))
gc();rm(fit.xgb_dm_full_dis_ind)

# Everything w.o Disaster Indicator
train <- everything %>% 
  select(-disaster)

train <- fastDummies::dummy_cols(train,
                                 remove_selected_columns = TRUE)

fit.xgb_dm_full <- train(H2S_daily_max~.,
                         method = 'xgbTree',
                         data = train,
                         trControl=control_everything,
                         tuneGrid = tune_grid,
                         tuneLength = 10, importance=TRUE, verbosity = 0, verbose=FALSE)
saveRDS(fit.xgb_dm_full,  paste0(savedir, 'fit.xgb_dm_full', '.rds'))
gc();rm(fit.xgb_dm_full)

# Log H2S average
# Since Feb 2022
train <- since_feb2022 %>% 
  mutate(H2S_daily_max = log(H2S_daily_max))

train <- fastDummies::dummy_cols(train,
                                 remove_selected_columns = TRUE)

fit.xgb_dm_log_h2s_sincefeb2022 <- train(H2S_daily_max~.,
                                         method = 'xgbTree',
                                         data = train,
                                         trControl=control,
                                         tuneGrid = tune_grid,
                                         tuneLength = 10, importance=TRUE, verbosity = 0, verbose=FALSE)
saveRDS(fit.xgb_dm_log_h2s_sincefeb2022,  paste0(savedir, 'fit.xgb_dm_log_h2s_sincefeb2022', '.rds'))
gc();rm(fit.xgb_dm_log_h2s_sincefeb2022)

# Disaster Only
train <- disaster %>% 
  mutate(H2S_daily_max = log(H2S_daily_max))

train <- fastDummies::dummy_cols(train,
                                 remove_selected_columns = TRUE)

fit.xgb_dm_log_h2s_dis <- train(H2S_daily_max~.,
                                method = 'xgbTree',
                                data = train,
                                trControl=control,
                                tuneGrid = tune_grid,
                                tuneLength = 10, importance=TRUE, verbosity = 0, verbose=FALSE)
saveRDS(fit.xgb_dm_log_h2s_dis,  paste0(savedir, 'fit.xgb_dm_log_h2s_dis', '.rds'))
gc();rm(fit.xgb_dm_log_h2s_dis)

# Exclude Disaster
train <- excl_disaster %>% 
  mutate(H2S_daily_max = log(H2S_daily_max))

train <- fastDummies::dummy_cols(train,
                                 remove_selected_columns = TRUE)

fit.xgb_dm_log_h2s_excl_dis <- train(H2S_daily_max~.,
                                     method = 'xgbTree',
                                     data = train,
                                     trControl=control,
                                     tuneGrid = tune_grid,
                                     tuneLength = 10, importance=TRUE, verbosity = 0, verbose=FALSE)
saveRDS(fit.xgb_dm_log_h2s_excl_dis,  paste0(savedir, 'fit.xgb_dm_log_h2s_excl_dis', '.rds'))
gc();rm(fit.xgb_dm_log_h2s_excl_dis)

# Everything w. Disaster Indicator
train <- everything %>%
  mutate(H2S_daily_max = log(H2S_daily_max))

train <- fastDummies::dummy_cols(train,
                                 remove_selected_columns = TRUE)

fit.xgb_dm_log_h2s_dis_ind <- train(H2S_daily_max~.,
                                    method = 'xgbTree',
                                    data = train,
                                    trControl=control_everything,
                                    tuneGrid = tune_grid,
                                    tuneLength = 10, importance=TRUE, verbosity = 0, verbose=FALSE)
saveRDS(fit.xgb_dm_log_h2s_dis_ind,  paste0(savedir, 'fit.xgb_dm_log_h2s_dis_ind', '.rds'))
gc();rm(fit.xgb_dm_log_h2s_dis_ind)

# Everything w.o Disaster Indicator
train <- everything %>% 
  select(-disaster) %>%
  mutate(H2S_daily_max = log(H2S_daily_max))

train <- fastDummies::dummy_cols(train,
                                 remove_selected_columns = TRUE)

fit.xgb_dm_log_h2s_full <- train(H2S_daily_max~.,
                                 method = 'xgbTree',
                                 data = train,
                                 trControl=control_everything,
                                 tuneGrid = tune_grid,
                                 tuneLength = 10, importance=TRUE, verbosity = 0, verbose=FALSE)
saveRDS(fit.xgb_dm_log_h2s_full,  paste0(savedir, 'fit.xgb_dm_log_h2s_full', '.rds'))
gc();rm(fit.xgb_dm_log_h2s_full)
