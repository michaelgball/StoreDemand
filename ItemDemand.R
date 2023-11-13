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


library(timetk)
trainSet %>%
plot_time_series(date, sales, .interactive=FALSE)
