
library(tidyverse)
library(vroom)
library(rpart)
library(tidymodels)
#library(patchwork)
#library(DataExplorer)

bikeDataTrainFinal = vroom("train.csv")
testDataFinal = vroom("test.csv")




logTrainFinal= bikeDataTrainFinal %>%
  mutate(count = log(count))


#View(bikeDataTrain)

rFormulaFinal = count ~ datetime + season + holiday + workingday + weather + temp + atemp + humidity + windspeed

my_recipeFinal = recipe(rFormulaFinal, data = logTrainFinal) %>%
  step_corr(all_numeric_predictors(), threshold = 0.3) %>% 
  step_mutate(weather = ifelse(weather  >3, 3, weather)) %>%
  #\step_mutate(count = log(count)) %>% # log of the count
  step_time(datetime, features = c("hour")) %>% 
  step_date(datetime, features = c("year")) %>%
  step_rm(datetime) %>%
  step_zv(all_predictors()) %>% 
  step_num2factor(weather, levels = c("1","2","3")) %>%
  step_num2factor(season, levels = c("1","2","3", "4"))


kNeighbrs = nearest_neighbor(neighbors = tune()) %>%
  set_engine("kknn") %>%
  set_mode("regression")

tuning_gridFinal = grid_regular(neighbors(), levels = 5)

kWorflow = workflow() %>% add_recipe(my_recipeFinal) %>% add_model(kNeighbrs)

finalFolds = vfold_cv(logTrainFinal, v = 10, repeats = 1)

CV_resultsFinal = kWorflow %>% tune_grid(resamples = finalFolds, grid = tuning_gridFinal,
                                     metrics = metric_set(rmse,mae))


bestTuneFinal = CV_resultsFinal %>% select_best("rmse")

final_wfFinal = kWorflow %>% finalize_workflow(bestTuneFinal) %>%
  fit(data = logTrainFinal)

logSubFinal = final_wfFinal %>% predict(new_data = testDataFinal) # saves count = .pred



subFinal = testDataFinal %>% mutate(
  count = exp(logSubFinal$.pred),
  datetime = as.character(format(datetime))
  
) %>% select(datetime, count) 

vroom_write(subFinal, "kknn.csv", delim = ",")
