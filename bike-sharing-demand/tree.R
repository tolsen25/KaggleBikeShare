
library(tidyverse)
library(vroom)
library(rpart)
library(tidymodels)
#library(patchwork)
#library(DataExplorer)

bikeDataTrain2 = vroom("train.csv")
testData2 = vroom("test.csv")




# logTrain2 = bikeDataTrain2 %>%
#   mutate(count = log(count))


#View(bikeDataTrain)

rFormula2 = count ~ datetime + season + holiday + workingday + weather + temp + atemp + humidity + windspeed

my_recipe2 = recipe(rFormula2, data = logTrain2) %>%
  step_corr(all_numeric_predictors(), threshold = 0.3) %>% 
  step_mutate(weather = ifelse(weather  >3, 3, weather)) %>%
  #\step_mutate(count = log(count)) %>% # log of the count
  step_time(datetime, features = c("hour")) %>% 
  step_rm(datetime) %>%
  step_zv(all_predictors()) %>% 
  step_num2factor(weather, levels = c("1","2","3")) %>%
  step_num2factor(season, levels = c("1","2","3", "4"))
#step_dummy(all_nominal_predictors()) %>%
#step_normalize(all_numeric_predictors()) not needed for trees

my_mod2 = decision_tree(tree_depth = tune(),
                       cost_complexity = tune(),
                       min_n = tune()) %>%
  set_engine("rpart") %>%
  set_mode("regression")


preg_wf2 = workflow() %>% add_recipe(my_recipe2) %>%
  add_model(my_mod2) 
# what is the difference between L,v/k, and repeats

tuning_grid2 = grid_regular(tree_depth(), cost_complexity(), min_n(), levels = 5)

folds2 = vfold_cv(logTrain2, v = 10, repeats = 1)

CV_results2 = preg_wf2 %>% tune_grid(resamples = folds2, grid = tuning_grid2,
                                   metrics = metric_set(rmse,mae))


# final_wf = preg_wf %>% finalize_workflow(bestTune) %>%
#   fit(data = logTrain)

bestTune2 = CV_results2 %>% select_best("rmse")

final_wf2 = preg_wf2 %>% finalize_workflow(bestTune2) %>%
  fit(data = logTrain2)

logSub2 = final_wf2 %>% predict(new_data = testData2) # saves count = .pred



#logSub = final_wf %>% predict(new_data = testData)

sub2 = testData2 %>% mutate(
  count = exp(logSub2$.pred),
  datetime = as.character(format(datetime))
  
) %>% select(datetime, count) 

vroom_write(sub2, "treeRegSub1.csv", delim = ",")


