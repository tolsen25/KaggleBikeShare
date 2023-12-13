
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


#View(bikeDataTrain)


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

bartMod = bart(mode = "regression", engine = 'dbarts', trees = 22)

bartWf = workflow() %>% add_recipe(my_recipeFinal) %>% add_model(bartMod) %>%
  fit(logTrainFinal)

bartPredict = bartWf %>% predict(new_data = testDataFinal)



subFinal = testDataFinal %>% mutate(
  count = exp(bartPredict$.pred),
  datetime = as.character(format(datetime))
  
) %>% select(datetime, count) 

vroom_write(subFinal, "bart.csv", delim = ",")


