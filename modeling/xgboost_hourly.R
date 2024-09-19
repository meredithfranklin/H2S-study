# XGB Models hourly
# 2024-09-15

library(tidyverse)
library(stringr)
library(tidyr)
library(dplyr)
library(caret)
library(fastDummies)
# library(doParallel)

select <- dplyr::select

#n_cores <- detectCores(); n_cores
#registerDoParallel(cores = 8)

hourly_full_raw <- readRDS('hourly_full_20230930.rds')

hourly_full <- hourly_full_raw %>%
  mutate(Monitor = str_replace_all(Monitor, ' ', '_'),
         weekday = weekday,
         hourly_downwind_ref = as.integer(hourly_downwind_ref),
         hourly_downwind_wrp = as.integer(hourly_downwind_wrp),
         dist_ref = 1/(dist_ref^2),
         dist_wrp = 1/(dist_wrp^2),
         dist_dc = 1/(dist_dc^2)) %>%
  filter(!is.na(H2S_hourly_avg))

predictors <- c('month', 'year', 'weekday', 'wd_avg', 'ws_avg', 
                'hourly_downwind_ref', 'dist_wrp', 'dist_ref',
                'mon_utm_x', 'mon_utm_y', 'monthly_oil_2km', 'monthly_gas_2km', 
                'active_2km', 'inactive_2km', 'hourly_downwind_wrp', 'elevation', 'EVI', 'num_odor_complaints',
                'dist_dc', 'closest_wrp_capacity', 'hourly_temp', 'hourly_hum', 'hourly_precip') 

############### Hourly Avg ####################
since_feb2022 <- hourly_full %>% 
  filter(day >= '2022-01-31') %>%
  select(all_of(c('H2S_hourly_avg', predictors))) %>% 
  filter(complete.cases(.))

train <- since_feb2022

train <- fastDummies::dummy_cols(train,
                                 remove_selected_columns = TRUE)

tune_grid <- expand.grid(nrounds = c(50, 100),
                         max_depth = c(3, 4),
                         eta = c(0.2, 0.4),
                         gamma = c(0.01, 0.001),
                         colsample_bytree = c(0.8, 1),
                         min_child_weight = 0,
                         subsample = c(0.75, 1))

# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", 
                        number=10,
                        verboseIter=TRUE, 
                        search='grid',
                        savePredictions = 'final',
                        #allowParallel = TRUE
                        )

fit.xgb_ha <- train(H2S_hourly_avg~.,
                    method = 'xgbTree',
                    data = train,
                    trControl=control,
                    tuneGrid = tune_grid,
                    tuneLength = 10, importance=TRUE, verbosity = 0, verbose=FALSE)
saveRDS(fit.xgb_ha, 'fit.xgb_ha_sincefeb2022.rds')
gc();rm(fit.xgb_ha)

# Disaster Only
disaster <- hourly_full %>% 
  filter(year == '2021', month %in% c('10', '11', '12')) %>% 
  select(c('H2S_hourly_avg', predictors)) %>% 
  filter(complete.cases(.))

train <- disaster

train <- fastDummies::dummy_cols(train,
                                 remove_selected_columns = TRUE)

fit.xgb_ha_dis <- train(H2S_hourly_avg~.,
                        method = 'xgbTree',
                        data = train,
                        trControl=control,
                        tuneGrid = tune_grid,
                        tuneLength = 10, importance=TRUE, verbosity = 0, verbose=FALSE)
saveRDS(fit.xgb_ha_dis, 'fit.xgb_ha_dis.rds')
gc();rm(fit.xgb_ha_dis)

# Exclude Disaster
excl_disaster <- hourly_full %>% 
  filter(!(year == '2021' & month %in% c('10', '11', '12'))) %>% 
  select(c('H2S_hourly_avg', predictors)) %>% 
  filter(complete.cases(.))

train <- excl_disaster

train <- fastDummies::dummy_cols(train,
                                 remove_selected_columns = TRUE)

fit.xgb_ha_excl_dis <- train(H2S_hourly_avg~.,
                             method = 'xgbTree',
                             data = train,
                             trControl=control,
                             tuneGrid = tune_grid,
                             tuneLength = 10, importance=TRUE, verbosity = 0, verbose=FALSE)

saveRDS(fit.xgb_ha_excl_dis, 'fit.xgb_ha_excl_dis.rds')
gc();rm(fit.xgb_ha_excl_dis)

# Everything w. Disaster Indicator
everything <- hourly_full %>%
  select(c('H2S_hourly_avg', predictors)) %>%
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
                                   savePredictions = 'final',
                                   #allowParallel = TRUE
                                   )

fit.xgb_ha_dis_ind <- train(H2S_hourly_avg~.,
                                 method = 'xgbTree',
                                 data = train,
                                 trControl=control_everything,
                                 tuneGrid = tune_grid,
                                 tuneLength = 10, importance=TRUE, verbosity = 0, verbose=FALSE)
saveRDS(fit.xgb_ha_dis_ind, 'fit.xgb_ha_dis_ind.rds')
gc();rm(fit.xgb_ha_dis_ind)

# Everything w.o Disaster Indicator
train <- everything %>% 
  select(-disaster)

train <- fastDummies::dummy_cols(train,
                                 remove_selected_columns = TRUE)

fit.xgb_ha_full <- train(H2S_hourly_avg~.,
                         method = 'xgbTree',
                         data = train,
                         trControl=control_everything,
                         tuneGrid = tune_grid,
                         tuneLength = 10, importance=TRUE, verbosity = 0, verbose=FALSE)
saveRDS(fit.xgb_ha_full, 'fit.xgb_ha_full.rds')
gc();rm(fit.xgb_ha_full)

# Log H2S hourly average
# Since Feb 2022
train <- since_feb2022 %>% 
  mutate(H2S_hourly_avg = log(H2S_hourly_avg))

train <- fastDummies::dummy_cols(train,
                                 remove_selected_columns = TRUE)

fit.xgb_log_ha_sincefeb2022 <- train(H2S_hourly_avg~.,
                                         method = 'xgbTree',
                                         data = train,
                                         trControl=control,
                                         tuneGrid = tune_grid,
                                         tuneLength = 10, importance=TRUE, verbosity = 0, verbose=FALSE)
saveRDS(fit.xgb_log_ha_sincefeb2022, 'fit.xgb_log_ha_sincefeb2022.rds')
gc();rm(fit.xgb_log_ha_sincefeb2022)

# Disaster Only
train <- disaster %>% 
  mutate(H2S_hourly_avg = log(H2S_hourly_avg))

train <- fastDummies::dummy_cols(train,
                                 remove_selected_columns = TRUE)

fit.xgb_log_ha_dis <- train(H2S_hourly_avg~.,
                                method = 'xgbTree',
                                data = train,
                                trControl=control,
                                tuneGrid = tune_grid,
                                tuneLength = 10, importance=TRUE, verbosity = 0, verbose=FALSE)
saveRDS(fit.xgb_log_ha_dis, 'fit.xgb_log_ha_dis.rds')
gc();rm(fit.xgb_log_ha_dis)

# Exclude Disaster
train <- excl_disaster %>% 
  mutate(H2S_hourly_avg = log(H2S_hourly_avg))

train <- fastDummies::dummy_cols(train,
                                 remove_selected_columns = TRUE)

fit.xgb_log_ha_excl_dis <- train(H2S_hourly_avg~.,
                                     method = 'xgbTree',
                                     data = train,
                                     trControl=control,
                                     tuneGrid = tune_grid,
                                     tuneLength = 10, importance=TRUE, verbosity = 0, verbose=FALSE)
saveRDS(fit.xgb_log_ha_excl_dis, 'fit.xgb_log_ha_excl_dis.rds')
gc();rm(fit.xgb_log_ha_excl_dis)

# Everything w. Disaster Indicator
train <- everything %>%
  mutate(H2S_hourly_avg = log(H2S_hourly_avg))

train <- fastDummies::dummy_cols(train,
                                 remove_selected_columns = TRUE)

fit.xgb_log_ha_dis_ind <- train(H2S_hourly_avg~.,
                                    method = 'xgbTree',
                                    data = train,
                                    trControl=control_everything,
                                    tuneGrid = tune_grid,
                                    tuneLength = 10, importance=TRUE, verbosity = 0, verbose=FALSE)
saveRDS(fit.xgb_log_ha_dis_ind, 'fit.xgb_log_ha_dis_ind.rds')
gc();rm(fit.xgb_log_ha_dis_ind)

# Everything w.o Disaster Indicator
train <- everything %>% 
  select(-disaster) %>%
  mutate(H2S_hourly_avg = log(H2S_hourly_avg))

train <- fastDummies::dummy_cols(train,
                                 remove_selected_columns = TRUE)

fit.xgb_log_ha_full <- train(H2S_hourly_avg~.,
                                 method = 'xgbTree',
                                 data = train,
                                 trControl=control_everything,
                                 tuneGrid = tune_grid,
                                 tuneLength = 10, importance=TRUE, verbosity = 0, verbose=FALSE)
saveRDS(fit.xgb_log_ha_full, 'fit.xgb_log_ha_full.rds')
gc();rm(fit.xgb_log_ha_full)

############### Hourly Max ####################
since_feb2022 <- hourly_full %>% 
  filter(day >= '2022-01-31') %>%
  select(all_of(c('H2S_hourly_max', predictors))) %>% 
  filter(complete.cases(.))

train <- since_feb2022

train <- fastDummies::dummy_cols(train,
                                 remove_selected_columns = TRUE)

fit.xgb_hm_sincefeb2022 <- train(H2S_hourly_max~.,
                    method = 'xgbTree',
                    data = train,
                    trControl=control,
                    tuneGrid = tune_grid,
                    tuneLength = 10, importance=TRUE, verbosity = 0, verbose=FALSE)
saveRDS(fit.xgb_hm_sincefeb2022, 'fit.xgb_hm_sincefeb2022.rds')
gc();rm(fit.xgb_hm_sincefeb2022)

# Disaster Only
disaster <- hourly_full %>% 
  filter(year == '2021', month %in% c('10', '11', '12')) %>% 
  select(c('H2S_hourly_max', predictors)) %>% 
  filter(complete.cases(.))

train <- disaster

train <- fastDummies::dummy_cols(train,
                                 remove_selected_columns = TRUE)

fit.xgb_hm_dis <- train(H2S_hourly_max~.,
                        method = 'xgbTree',
                        data = train,
                        trControl=control,
                        tuneGrid = tune_grid,
                        tuneLength = 10, importance=TRUE, verbosity = 0, verbose=FALSE)
saveRDS(fit.xgb_hm_dis, 'fit.xgb_hm_dis.rds')
gc();rm(fit.xgb_hm_dis)

# Exclude Disaster
excl_disaster <- hourly_full %>% 
  filter(!(year == '2021' & month %in% c('10', '11', '12'))) %>% 
  select(c('H2S_hourly_max', predictors)) %>% 
  filter(complete.cases(.))

train <- excl_disaster

train <- fastDummies::dummy_cols(train,
                                 remove_selected_columns = TRUE)

fit.xgb_hm_excl_dis <- train(H2S_hourly_max~.,
                             method = 'xgbTree',
                             data = train,
                             trControl=control,
                             tuneGrid = tune_grid,
                             tuneLength = 10, importance=TRUE, verbosity = 0, verbose=FALSE)

saveRDS(fit.xgb_hm_excl_dis, 'fit.xgb_hm_excl_dis.rds')
gc();rm(fit.xgb_hm_excl_dis)

# Everything w. Disaster Indicator
everything <- hourly_full %>%
  select(c('H2S_hourly_max', predictors)) %>%
  mutate(disaster = if_else(year == '2021' & month %in% c('10', '11', '12'), 1, 0)) %>%
  filter(complete.cases(.))

train <- everything

train <- fastDummies::dummy_cols(train,
                                 remove_selected_columns = TRUE)

fit.xgb_hm_dis_ind <- train(H2S_hourly_max~.,
                                 method = 'xgbTree',
                                 data = train,
                                 trControl=control_everything,
                                 tuneGrid = tune_grid,
                                 tuneLength = 10, importance=TRUE, verbosity = 0, verbose=FALSE)
saveRDS(fit.xgb_hm_dis_ind, 'fit.xgb_hm_dis_ind.rds')
gc();rm(fit.xgb_hm_dis_ind)

# Everything w.o Disaster Indicator
train <- everything %>% 
  select(-disaster)

train <- fastDummies::dummy_cols(train,
                                 remove_selected_columns = TRUE)

fit.xgb_hm_full <- train(H2S_hourly_max~.,
                         method = 'xgbTree',
                         data = train,
                         trControl=control_everything,
                         tuneGrid = tune_grid,
                         tuneLength = 10, importance=TRUE, verbosity = 0, verbose=FALSE)
saveRDS(fit.xgb_hm_full, 'fit.xgb_hm_full.rds')
gc();rm(fit.xgb_hm_full)

# Log H2S hourly max
# Since Feb 2022
train <- since_feb2022 %>% 
  mutate(H2S_hourly_max = log(H2S_hourly_max))

train <- fastDummies::dummy_cols(train,
                                 remove_selected_columns = TRUE)

fit.xgb_log_hm_sincefeb2022 <- train(H2S_hourly_max~.,
                                         method = 'xgbTree',
                                         data = train,
                                         trControl=control,
                                         tuneGrid = tune_grid,
                                         tuneLength = 10, importance=TRUE, verbosity = 0, verbose=FALSE)
saveRDS(fit.xgb_log_hm_sincefeb2022, 'fit.xgb_log_hm_sincefeb2022.rds')
gc();rm(fit.xgb_log_hm_sincefeb2022)

# Disaster Only
train <- disaster %>% 
  mutate(H2S_hourly_max = log(H2S_hourly_max))

train <- fastDummies::dummy_cols(train,
                                 remove_selected_columns = TRUE)

fit.xgb_log_hm_dis <- train(H2S_hourly_max~.,
                                method = 'xgbTree',
                                data = train,
                                trControl=control,
                                tuneGrid = tune_grid,
                                tuneLength = 10, importance=TRUE, verbosity = 0, verbose=FALSE)
saveRDS(fit.xgb_log_hm_dis, 'fit.xgb_log_hm_dis.rds')
gc();rm(fit.xgb_log_hm_dis)

# Exclude Disaster
train <- excl_disaster %>% 
  mutate(H2S_hourly_max = log(H2S_hourly_max))

train <- fastDummies::dummy_cols(train,
                                 remove_selected_columns = TRUE)

fit.xgb_log_hm_excl_dis <- train(H2S_hourly_max~.,
                                     method = 'xgbTree',
                                     data = train,
                                     trControl=control,
                                     tuneGrid = tune_grid,
                                     tuneLength = 10, importance=TRUE, verbosity = 0, verbose=FALSE)
saveRDS(fit.xgb_log_hm_excl_dis, 'fit.xgb_log_hm_excl_dis.rds')
gc();rm(fit.xgb_log_hm_excl_dis)

# Everything w. Disaster Indicator
train <- everything %>%
  mutate(H2S_hourly_max = log(H2S_hourly_max))

train <- fastDummies::dummy_cols(train,
                                 remove_selected_columns = TRUE)

fit.xgb_log_hm_dis_ind <- train(H2S_hourly_max~.,
                                    method = 'xgbTree',
                                    data = train,
                                    trControl=control_everything,
                                    tuneGrid = tune_grid,
                                    tuneLength = 10, importance=TRUE, verbosity = 0, verbose=FALSE)
saveRDS(fit.xgb_log_hm_dis_ind, 'fit.xgb_log_hm_dis_ind.rds')
gc();rm(fit.xgb_log_hm_dis_ind)

# Everything w.o Disaster Indicator
train <- everything %>% 
  select(-disaster) %>%
  mutate(H2S_hourly_max = log(H2S_hourly_max))

train <- fastDummies::dummy_cols(train,
                                 remove_selected_columns = TRUE)

fit.xgb_log_hm_full <- train(H2S_hourly_max~.,
                                 method = 'xgbTree',
                                 data = train,
                                 trControl=control_everything,
                                 tuneGrid = tune_grid,
                                 tuneLength = 10, importance=TRUE, verbosity = 0, verbose=FALSE)
saveRDS(fit.xgb_log_hm_full, 'fit.xgb_log_hm_full.rds')
gc();rm(fit.xgb_log_hm_full)