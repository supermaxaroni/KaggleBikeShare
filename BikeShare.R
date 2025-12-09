# Clean workspace
rm(list = ls())

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(vroom)
library(ggmosaic)
library(embed)
library(doParallel)

# Parallel processing -----------------------------------------------------
registerDoParallel(cores = parallel::detectCores())

ggg_train <- vroom("./train.csv", show_col_types = FALSE) %>%
  mutate(ACTION = as.factor(ACTION))
ggg_test <- vroom("./test.csv", show_col_types = FALSE)

my_recipe <- recipe(ACTION ~ ., data=amazon_train) %>%
  step_mutate_at(all_numeric_predictors(), fn = factor) %>% # turn all numeric features into factors
  step_other(all_nominal_predictors(), threshold = .01) %>% # combines categorical values that occur <5% into an "other" value
  step_dummy(all_nominal_predictors()) %>% # dummy variable encoding
  step_lencode_mixed(all_nominal_predictors(), outcome = vars(target_var)) #target encoding
# also step_lencode_glm() and step_lencode_bayes()


# NOTE: some of these step functions are not appropriate to use together

# apply the recipe to your data
prep <- prep(my_recipe)
baked <- bake(prep, new_data = amazon_train)

## recipe

my_recipe <- recipe(ACTION ~ ., data=amazon_train) %>%
  step_mutate_at(all_numeric_predictors(), fn = factor) %>% # turn all numeric features into factors
  #step_other(all_nominal_predictors(), threshold = .001) %>% 
  step_lencode_mixed(all_nominal_predictors(), outcome = vars(ACTION))

## Create a workflow with model & recipe

my_mod_rf <- rand_forest(mtry = tune(),
                         min_n=tune(),
                         trees=500) %>%
  set_engine("ranger") %>%
  set_mode("classification")


amazon_workflow <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(my_mod_rf)

## Set up grid of tuning values

tuning_grid <- grid_regular(mtry(range = c(1,(ncol(amazon_train)-1))),
                            min_n(),
                            levels = 3)

## Set up K-fold CV

folds <- vfold_cv(amazon_train, v = 3, repeats=1)

## Find best tuning parameters

CV_results <- amazon_workflow %>%
  tune_grid(resamples=folds,
            grid=tuning_grid,
            metrics=metric_set(roc_auc))

best_tune_rf <- CV_results %>%
  select_best(metric = "roc_auc")

## Finalize workflow and predict

final_wf <- amazon_workflow %>%
  finalize_workflow(best_tune_rf) %>%
  fit(data=amazon_train)

amazon_predictions_rf <- final_wf %>% predict(new_data=amazon_test,
                                              type="prob")

ap_rf <- amazon_predictions_rf %>% #This predicts
  bind_cols(., amazon_test) %>% #Bind predictions with test data
  select(id, .pred_1) %>% #Just keep datetime and predictions
  rename(Action=.pred_1)



vroom_write(x=ap_rf, file="rfamazon.csv", delim=",")

write.csv(ap_rf, file = "amazonrandomforestsfinal.csv", quote = FALSE, row.names = FALSE)

save(file="./rfamazon.csv", list=c("best_tune_rf", "amazon_predictions_rf"))


