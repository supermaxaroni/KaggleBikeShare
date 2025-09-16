## load libraries
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

bikeshare <- vroom("GItHub/KaggleBikeShare/train.csv")
test      <- vroom("GItHub/KaggleBikeShare/test.csv")

bikeshare <- bikeshare %>%
  mutate(count = log(count))

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

## Penalized regression model
preg_model <- linear_reg(penalty=.05, mixture=.3) %>% #Set model and tuning
  set_engine("glmnet") # Function to fit in R
preg_wf <- workflow() %>%
add_recipe(bike_rec) %>%
add_model(preg_model)
preg_fit <- fit(preg_wf, data=bikeshare)
bike_predictions <-predict(preg_fit, new_data=test) %>%
  mutate(.pred = exp(.pred)) %>%       # back-transform
  mutate(.pred = pmax(0, .pred)) %>%   # enforce non-negative
  rename(count = .pred)


# Model
#bike_lm <- linear_reg() %>%
 # set_engine("lm")

# Workflow
#bike_wf <- workflow() %>%
 # add_recipe(bike_rec) %>%
  #add_model(bike_lm)

# Fit
#bike_fit <- fit(bike_wf, data = bikeshare)

# Predict on test
#predict(bike_fit, new_data = test) %>%
#  mutate(.pred = exp(.pred)) %>%       # back-transform
#  mutate(.pred = pmax(0, .pred)) %>%   # enforce non-negative
#  rename(count = .pred)

# Kaggle submission
kaggle_submission <- bind_cols(
  test %>% 
  select(datetime),bike_predictions) %>%
  mutate(datetime = as.character(format(datetime)))

vroom_write(kaggle_submission, file = "./LinearPreds.csv", delim = ",")

head(kaggle_submission)


