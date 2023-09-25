

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
testData = testData  %>% mutate(
  
  weather = ifelse(weather  >3, 3, weather)
  
)

bikePreds = predict(bike_workflow, new_data = testData)

sub = testData %>% mutate(
  preds = ifelse(bikePreds$.pred < 0, 0, bikePreds$.pred),
  datetime = as.character(format(datetime))
                                   
  ) %>% select(datetime, preds) 


colnames(sub) = c("datetime", "count") 
vroom_write(sub, "linRegSub1.csv", delim = ",")

write.csv(sub, "screwVroom.csv")



# Poisson Reg -------------------------------------------------------------

library(poissonreg)

poi_mod = poisson_reg() %>%
  set_engine("glm")

bike_pois_workflow = workflow() %>% 
  add_recipe(my_recipe) %>%
  add_model(poi_mod) %>% 
  fit(data = bikeDataTrain)


poiPreds = predict(bike_pois_workflow, new_data = testData)

sub = testData %>% mutate(
  preds = poiPreds$.pred,
  datetime = as.character(format(datetime))
  
) %>% select(datetime, preds) 


colnames(sub) = c("datetime", "count") 
vroom_write(sub, "poiRegSub1.csv", delim = ",")
 


# Penalized Regression ----------------------------------------------------

# squared penalty vs absolute value penalty,
# squared model don't want coeffs to be too big
# abs value not quite as big of a penalty
# works well for high dimensional problems
# want to normalize all numeric variables
# can't take categorical variables



bikeDataTrain = vroom("train.csv")

logTrain = bikeDataTrain %>%
  mutate(count = log(count))
# bikeDataTrain = bikeDataTrain  %>% mutate(
#   
#   weather = ifelse(weather  >3, 3, weather)
#   
# ) %>% select(-c(casual, registered))
# 
# #View(bikeDataTrain)

rFormula = count ~ datetime + season + holiday + workingday + weather + temp + atemp + humidity + windspeed

my_recipe = recipe(rFormula, data = logTrain) %>%
  step_corr(all_numeric_predictors(), threshold = 0.3) %>% 
  step_mutate(weather = ifelse(weather  >3, 3, weather)) %>%
  #step_mutate(count = log(count)) %>% # log of the count
  step_time(datetime, features = c("hour")) %>% 
  step_rm(datetime) %>%
  step_zv(all_predictors()) %>% 
  step_num2factor(weather, levels = c("1","2","3")) %>%
  step_num2factor(season, levels = c("1","2","3", "4")) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors())

preppedRecipe = prep(my_recipe)
#bake(preppedRecipe, logTrain)

preg_model = linear_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet")

preg_wf = workflow() %>% add_recipe(my_recipe) %>%
  add_model(preg_model) %>% fit(data = logTrain)
# what is the difference between L,v/k, and repeats

tuning_grid = grid_regular(penalty(), mixture(), levels = 5)

folds = vfold_cv(logTrain, v = 10, repeats = 1)

CV_results = preg_wf %>% tune_grid(resamples = folds, grid = tuning_grid,
                                   metrics = metric_set(rmse,mae,rsq))

collect_metrics(CV_results) %>%
  filter(.metric == "rmse") %>%
  ggplot(data=., aes(x=penalty, y = mean, color =factor(mixture)))+
  geom_line()

bestTune = CV_results %>% select_best("rmse")

final_wf = preg_wf %>% finalize_workflow(bestTune) %>%
  fit(data = logTrain)

logSub = final_wf %>% predict(new_data = testData)



#logSub = predict(preg_wf, new_data = testData)
#     mutate(.pred = exp(.pred)))

sub = testData %>% mutate(
  count = exp(logSub$.pred),
  datetime = as.character(format(datetime))
  
) %>% select(datetime, count) 

vroom_write(sub, "tunedPenalty.csv", delim = ",")

#bakedDataTrain = bake(preppedRecipe, new_data = NULL) %>% select(count, everything())

bikeDataTrain = vroom("train.csv")
# Tuning Penalized Regression ---------------------------------------------

logTrain = bikeDataTrain %>%
  mutate(count = log(count))
# bikeDataTrain = bikeDataTrain  %>% mutate(
#   
#   weather = ifelse(weather  >3, 3, weather)
#   
# ) %>% select(-c(casual, registered))
# 
# #View(bikeDataTrain)

rFormula = count ~ datetime + season + holiday + workingday + weather + temp + atemp + humidity + windspeed

my_recipe = recipe(rFormula, data = logTrain) %>%
  step_corr(all_numeric_predictors(), threshold = 0.3) %>% 
  step_mutate(weather = ifelse(weather  >3, 3, weather)) %>%
  step_mutate(count = log(count)) %>% # log of the count
  step_time(datetime, features = c("hour")) %>% 
  step_rm(datetime) %>%
  step_zv(all_predictors()) %>% 
  step_num2factor(weather, levels = c("1","2","3")) %>%
  step_num2factor(season, levels = c("1","2","3", "4")) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors())



preppedRecipe = prep(my_recipe)

preg_model = linear_reg(penalty = 0, mixture = 0) %>%
  set_engine("glmnet")

preg_wf = workflow() %>% add_recipe(my_recipe) %>%
  add_model(preg_model) %>% fit(data = logTrain)





# formatting stuff --------------------------------------------------------

sub = testData %>% mutate(
  count = exp(logSub$.pred),
  datetime = as.character(format(datetime))
  
) %>% select(datetime, count) 

vroom_write(sub, "penRegSub1.csv", delim = ",")




