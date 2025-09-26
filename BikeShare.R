library(tidyverse)
library(tidymodels)
library(vroom)
library(patchwork)
library(dplyr)
library(ggplot2)
library(car)
library(corrplot)
library(ggcorrplot)
library(lubridate)
library(glmnet)
library(rpart)
library(ranger)
library(bonsai)
library(dbarts)
library(lightgbm)
library(agua) 

h2o::h2o.init()

L <- 3
K <- 3

# --- Load data ---
bikeshare <- vroom("GItHub/KaggleBikeShare/train.csv")
test      <- vroom("GItHub/KaggleBikeShare/test.csv")

# --- Transform training data ---
bikeshare <- bikeshare %>%
  mutate(count = log(count))

# --- Recipe ---
bike_rec <- recipe(count ~ temp + humidity + windspeed + season + weather +
                     holiday + workingday + datetime,
                   data = bikeshare) %>%
  step_mutate(weather = ifelse(weather == 4, 3, weather)) %>%
  step_mutate(
    season     = as.factor(season),
    holiday    = as.factor(holiday),
    workingday = as.factor(workingday),
    weather    = as.factor(weather)
  ) %>%
  step_mutate(hour = hour(datetime)) %>%
  step_mutate(hour_sin = sin(2 * pi * hour / 24),
              hour_cos = cos(2 * pi * hour / 24)) %>%
  step_rm(hour) %>%
  step_mutate(sunny = ifelse(weather == 1 & between(hour(datetime), 5, 20), 1, 0),
              sunny = as.factor(sunny)) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_poly(temp, degree = 2) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_rm(datetime)

# ------------------------
# Commented-out models
# ------------------------

## Example Random Forest (not used)
# preg_model <- rand_forest(
#   mtry  = tune(),
#   min_n = tune(),
#   trees = 1000
# ) %>%
#   set_engine("ranger") %>%
#   set_mode("regression")

## Example BART (not used)
# bart_model <- parsnip::bart() %>%
#   set_engine("dbarts") %>%
#   set_mode("regression")
# bart_wf <- workflow() %>%
#   add_recipe(bike_rec) %>%
#   add_model(bart_model)
# bart_grid <- grid_regular(
#   trees(range = c(20, 200)), 
#   levels = L
# )
# folds <- vfold_cv(bikeshare, v = K, repeats = 1)
# bart_results <- bart_wf %>%
#   tune_grid(
#     resamples = folds,
#     grid = bart_grid,
#     metrics = metric_set(rmse, mae)
#   )
# best_bart <- bart_results %>% select_best(metric = "rmse")
# final_bart_wf <- bart_wf %>%
#   finalize_workflow(best_bart) %>%
#   fit(data = bikeshare)
# final_bart <- final_bart_wf %>%
#   predict(new_data = test) %>%
#   mutate(count = exp(.pred)) %>%
#   mutate(count = pmax(0, count)) %>%
#   select(count)

## Example LightGBM Boosting (not used)
# boost_model <- boost_tree(tree_depth=tune(),
#                           trees=tune(),
#                           learn_rate=tune()) %>%
#   set_engine("lightgbm") %>%
#   set_mode("regression")
# boost_wf <- workflow() %>%
#   add_recipe(bike_rec) %>%
#   add_model(boost_model)

# ------------------------
# Active Model: h2o AutoML
# ------------------------

auto_model <- auto_ml() %>%
  set_engine("h2o", max_runtime_secs = 360, max_models = 10) %>%
  set_mode("regression")

automl_wf <- workflow() %>%
  add_recipe(bike_rec) %>%
  add_model(auto_model) %>%
  fit(data = bikeshare)   # use bikeshare directly

# --- Predict with h2o AutoML ---
final_h2o <- automl_wf %>%
  predict(new_data = test) %>%
  mutate(count = exp(.pred)) %>%       # back-transform
  mutate(count = pmax(0, count)) %>%   # enforce non-negative
  select(count)

# --- Kaggle submission ---
kaggle_submission <- bind_cols(
  test %>% select(datetime),
  final_h2o
) %>%
  mutate(datetime = as.character(format(datetime)))

vroom_write(kaggle_submission, file = "./H2O_AutoML_Preds.csv", delim = ",")
head(kaggle_submission)
