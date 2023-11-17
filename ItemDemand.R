library(tidyverse)
library(tidymodels)
library(vroom)

trainSet <- vroom("train.csv")
testSet <- vroom("test.csv")

p1 <- trainSet %>%
  filter(store == 1) %>%
  filter(item == 1) %>%
  pull(sales) %>%
  forecast::ggAcf(., lag.max=2*365)

p2 <- trainSet %>%
  filter(store == 2) %>%
  filter(item == 15) %>%
  pull(sales) %>%
  forecast::ggAcf(., lag.max=2*365)

p3 <- trainSet %>%
  filter(store == 5,item ==30) %>%
  pull(sales) %>%
  forecast::ggAcf(., lag.max=2*365)

p4 <- trainSet %>%
  filter(store == 10, item == 50) %>%
  pull(sales) %>%
  forecast::ggAcf(., lag.max=2*365)

(p1 + p2)/(p3 + p4)


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

##Exponential Smoothing
trainSet <- vroom("train.csv")
testSet <- vroom("test.csv")
library(modeltime) #Extensions of tidymodels to time series
library(timetk) #Some nice time series functions

train <- trainSet %>% filter(store==8, item==37)
test <- testSet %>% filter(store==8, item==37)
cv_split <- time_series_split(train, assess="3 months", cumulative = TRUE)
cv_split %>%
tk_time_series_cv_plan() %>% #Put into a data frame
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)

es_model <- exp_smoothing() %>%
set_engine("ets") %>%
fit(sales~date, data=train)

## Cross-validate to tune model
cv_results <- modeltime_calibrate(es_model,new_data = testing(cv_split))

## Visualize CV results
p3 <- cv_results %>%
modeltime_forecast(new_data = testing(cv_split),actual_data = train) %>%
plot_modeltime_forecast(.interactive=TRUE)

## Evaluate the accuracy
cv_results %>%
modeltime_accuracy() %>%
table_modeltime_accuracy(.interactive = FALSE)

## Refit to all data then forecast
es_fullfit <- cv_results %>%
modeltime_refit(data = train)

es_preds <- es_fullfit %>%
modeltime_forecast(h = "3 months") %>%
rename(date=.index, sales=.value) %>%
select(date, sales) %>%
full_join(., y=test, by="date") %>%
select(id, sales)

p4 <- es_fullfit %>%
modeltime_forecast(h = "3 months", actual_data = train) %>%
plot_modeltime_forecast(.interactive=FALSE)


plotly::subplot(p1,p3,p2,p4,nrows=2)



nStores <- max(trainSet$store)
nItems <- max(trainSet$item)
for(s in 1:nStores){
  for(i in 1:nItems){
    storeItemTrain <- trainSet %>%
    filter(store==s, item==i)
    storeItemTest <- testSet %>%
    filter(store==s, item==i)
    
    ## Fit storeItem models here
      trainSet %>%
      pull(sales) %>%
      forecast::ggAcf(., lag.max=2*365)
    ## Predict storeItem sales

    ## Save storeItem predictions
    if(s==1 & i==1){
      all_preds <- preds
    } else {
      all_preds <- bind_rows(all_preds, preds)
    }
    
  }
}

