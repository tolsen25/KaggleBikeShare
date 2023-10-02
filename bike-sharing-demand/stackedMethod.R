library(tidyverse)
library(vroom)
library(rpart)
library(tidymodels)
library(stacks)
library(poissonreg)
#library(patchwork)
#library(DataExplorer)


bikeDataTrain_stk = vroom("train.csv")
testData = vroom("test.csv")


logTrain_stk = bikeDataTrain_stk %>%
  mutate(count = log(count))
folds = vfold_cv(logTrain_stk, v = 10, repeats = 1)

#View(bikeDataTrain)

rFormula_stk = count ~ datetime + season + holiday + workingday + weather + temp + atemp + humidity + windspeed

my_recipe = recipe(rFormula_stk, data = logTrain_stk) %>%
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





# control settings
untunedModel = control_stack_grid()
tunedModel = control_stack_resamples() # alr

lin_model = linear_reg() %>%
  set_engine("lm")

#preppedRecipe = prep(my_recipe)

# lin model
linRegModel_wf = workflow() %>% add_model(lin_model) %>% 
  add_recipe(my_recipe) 
  
linRegModel = fit_resamples(linRegModel_wf, resamples = folds,
                            metrics = metric_set(rmse,mae,rsq),
                            control = tunedModel)



# penalized regression
penReg_model = linear_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet")


penReg_wf = workflow() %>% add_recipe(my_recipe) %>%
  add_model(penReg_model)
# what is the difference between L,v/k, and repeats

tuning_grid = grid_regular(penalty(), mixture(), levels = 5)


penReg_allModels = penReg_wf %>% tune_grid(resamples = folds, grid = tuning_grid,
                                   metrics = metric_set(rmse,mae,rsq), control = untunedModel)


# random forest
my_mod_rf = rand_forest(mtry = tune(), min_n = tune(), trees = 555) %>%
  set_engine("ranger") %>%
  set_mode("regression")


preg_wf_rf = workflow() %>% add_recipe(my_recipe) %>%
  add_model(my_mod_rf) 
# what is the difference between L,v/k, and repeats

tuning_grid_rf = grid_regular(mtry(range = c(1,7)), min_n(), levels = 5)


CV_results_rf = preg_wf_rf %>% tune_grid(resamples = folds, grid = tuning_grid_rf,
                                         metrics = metric_set(rmse,mae), control = untunedModel)

my_stack = stacks() %>%
  add_candidates(linRegModel) %>%
  add_candidates(penReg_allModels) %>%
  add_candidates(CV_results_rf)

stack_mod = my_stack %>%
  blend_predictions() %>%
  fit_members()

stackSub = stack_mod %>% predict(new_data = testData)


sub = testData %>% mutate(
  count = exp(stackSub$.pred),
  datetime = as.character(format(datetime))
  
) %>% select(datetime, count) 

vroom_write(sub, "stackSub1.csv", delim = ",")


