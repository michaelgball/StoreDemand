library(tidyverse)
library(tidymodels)
library(vroom)

trainSet <- vroom("train.csv")
testSet <- vroom("test.csv")

trainSet <- trainSet %>%
  filter(store==1,item==1)
my_recipe <- recipe(sales~date, data=trainSet) %>%
  step_date(date, features="doy") %>%
  step_range(date_doy, min=0, max=pi) %>%
  step_mutate(sinDOY=sin(date_doy), cosDOY=cos(date_doy))%>%
  step_lag(lag=365)


library(rpart)
library(ranger)
forest_mod <- rand_forest(mtry = tune(),
                          min_n=tune(),
                          trees=500) %>% #Type of model
  set_engine("ranger") %>% # What R function to use
  set_mode("regression")

forest_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(forest_mod)

## Set up grid of tuning values
tuning_grid <- grid_regular(mtry(c(1,4)), min_n(), levels = 5)

## Set up K-fold CV
folds <- vfold_cv(trainSet, v = 5, repeats=1)

## Run the CV
CV_results <- forest_wf %>%
  tune_grid(resamples=folds,grid=tuning_grid,metrics=metric_set(smape))

## Find Best Tuning Parameters
bestTune <- CV_results %>%
  select_best("smape")

collect_metrics(CV_results) %>%
  filter(mtry == 1, min_n == 40)%>%
  pull(mean)
