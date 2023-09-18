

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
  step_num2factor(weather, levels = c("1","2","3")) %>%
  step_num2factor(season, levels = c("1","2","3", "4"))

preppedRecipe = prep(my_recipe)

bakedDataTrain = bake(preppedRecipe, new_data = NULL) %>% select(count, everything())

# switching to tidymodels because will be easier to cross over in the future
my_mod = linear_reg() %>%
  set_engine("lm") # Engine = what R function to use

bike_workflow = workflow() %>% add_recipe(preppedRecipe) %>%
  add_model(my_mod) %>%
  fit(data = bikeDataTrain)


testData = vroom("test.csv")
bikePreds = predict(bike_workflow, new_data = testData)

sub = testData %>% mutate(
  preds = ifelse(bikePreds$.pred < 0, 0, bikePreds$.pred),
  datetime = as.character(format(datetime))
                                   
  ) %>% select(datetime, preds) 


colnames(sub) = c("datetime", "count") 
vroom_write(sub, "linRegSub1.csv", delim = ",")

write.csv(sub, "screwVroom.csv")

