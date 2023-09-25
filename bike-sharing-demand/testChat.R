# Install and load the required packages
#install.packages("tidymodels")
library(tidymodels)

# Load your data
train_data <- read.csv("train.csv")
test_data <- read.csv("test.csv")

# Define cross-validation folds
folds <- vfold_cv(train_data, v = 5)  # You can adjust the number of folds

# Preprocess the data
data_recipe <- recipe(count ~ ., data = train_data) %>%
  #step_time(all_predictors(), feature_extract = "datetime") %>%
  step_time(datetime, features = c("hour")) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_zv(all_predictors()) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes())

# Create a model specification
model_spec <- 
  decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("regression")

# Create a workflow
bike_workflow <- 
  workflow() %>%
  add_recipe(data_recipe) %>%
  add_model(model_spec)

# Train and tune the model
# bike_tune_results <- tune_grid(
#   bike_workflow,
#   resamples = folds,  # Use the defined cross-validation folds
#   grid = tibble(maxdepth = c(5, 10, 15),
#                 minsplit = c(5, 10, 15))
# )

# Fit the best model
best_model <- select_best(bike_tune_results, "rmse")

# Make predictions on the test data
test_predictions <- predict(best_model, test_data)

sub = test_data %>% mutate(
  preds = test_predictions$.pred,
  datetime = as.character(format(datetime))
  
) %>% select(datetime, preds) 


colnames(sub) = c("datetime", "count") 


# Save the predictions to a submission file
write.csv(sub, "submission.csv", row.names = FALSE)
