

library(tidyverse)
library(vroom)
library(tidymodels)
#library(patchwork)
#library(DataExplorer)

bikeDataTrain = vroom("train.csv")

bikeDataTrain = bikeDataTrain  %>% mutate(
  
  weather = ifelse(weather  >3, 3, weather)
  
) %>% select(-c(casual, registered))

View(bikeDataTrain)

rFormula = count ~ datetime + season + holiday + workingday + weather + temp + atemp + humidity + windspeed

my_recipe = recipe(rFormula, data = bikeDataTrain) %>%
  step_corr(all_numeric_predictors(), threshold = 0.3) %>% 
  step_time(datetime, features = c("hour")) %>% 
  step_dummy(all_nominal_predictors()) %>%
  step_rm(datetime) %>%
  step_zv(all_predictors()) %>% 
  step_num2factor(weather, levels = c("1","2","3", "4")) %>%
  step_num2factor(season, levels = c("1","2","3"))

preppedRecipe = prep(my_recipe)

bakedDataTrain = bake(preppedRecipe, new_data = NULL) %>% select(count, everything())

a = 2