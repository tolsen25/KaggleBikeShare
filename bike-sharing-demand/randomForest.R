
library(tidyverse)
library(vroom)
library(rpart)
library(tidymodels)
#library(patchwork)
#library(DataExplorer)

bikeDataTrain_rf = vroom("train.csv")
testData_rf = vroom("test.csv")




logTrain_rf = bikeDataTrain_rf %>%
  mutate(count = log(count))


#View(bikeDataTrain)

rFormula_rf = count ~ datetime + season + holiday + workingday + weather + temp + atemp + humidity + windspeed

my_recipe_rf = recipe(rFormula_rf, data = logTrain_rf) %>%
  step_corr(all_numeric_predictors(), threshold = 0.3) %>% 
  step_mutate(weather = ifelse(weather  >3, 3, weather)) %>%
  #step_mutate(count = log(count)) %>% # log of the count
  step_time(datetime, features = c("hour")) %>% 
  step_rm(datetime) %>%
  step_zv(all_predictors()) %>% 
  step_dummy(all_nominal_predictors())  %>%
  step_num2factor(weather, levels = c("1","2","3")) %>%
  step_num2factor(season, levels = c("1","2","3", "4")) 
#step_dummy(all_nominal_predictors()) %>%
#step_normalize(all_numeric_predictors()) not needed for trees

my_mod_rf = rand_forest(mtry = tune(), min_n = tune(), trees = 555) %>%
  set_engine("ranger") %>%
  set_mode("regression")


preg_wf_rf = workflow() %>% add_recipe(my_recipe_rf) %>%
  add_model(my_mod_rf) 
# what is the difference between L,v/k, and repeats

tuning_grid_rf = grid_regular(mtry(range = c(1,7)), min_n(), levels = 5)

folds_rf = vfold_cv(logTrain_rf, v = 10, repeats = 1)

CV_results_rf = preg_wf_rf %>% tune_grid(resamples = folds_rf, grid = tuning_grid_rf,
                                     metrics = metric_set(rmse,mae))


# final_wf = preg_wf %>% finalize_workflow(bestTune) %>%
#   fit(data = logTrain)

bestTune_rf = CV_results_rf %>% select_best("rmse")

final_wf_rf = preg_wf_rf %>% finalize_workflow(bestTune_rf) %>%
  fit(data = logTrain_rf)

logSub_rf = final_wf_rf %>% predict(new_data = testData_rf) # saves count = .pred



#logSub = final_wf %>% predict(new_data = testData)

sub_rf = testData_rf %>% mutate(
  count = exp(logSub_rf$.pred),
  datetime = as.character(format(datetime))
  
) %>% select(datetime, count) 

vroom_write(sub_rf, "forestRegSub1.csv", delim = ",")


