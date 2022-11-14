# Prerequisite from last time (run if you are running just this)
library(tidyverse)
library(tidymodels)

rent <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-05/rent.csv') %>% 
  mutate(logprice = log(price))

set.seed(42)

rent_split <- initial_split(rent, prop = 0.8)

rent_train <- training(rent_split)
rent_test <- testing(rent_split)

rent_recipe <- recipe(logprice ~ year + beds + baths + sqft + county, 
                      data = rent_train) %>% 
  step_impute_mean(all_numeric_predictors()) %>% 
  step_other(county) %>%
  step_naomit(county) %>%
  step_dummy(county)


# Fit the best model (according to the analysis from last time) -----------

rent_boost <- boost_tree(mode = "regression", 
                         engine = "xgboost",
                         trees = 1239,
                         learn_rate = 0.0655)

boost_model_fit <- workflow() %>% 
  add_model(rent_boost) %>% 
  add_recipe(rent_recipe) %>% 
  fit(rent_train %>% 
        slice_sample(prop = 0.1))

best_model_prediction <- predict(boost_model_fit, new_data = rent_test)

# Manual demonstration

test_w_pred <- rent_test %>% 
  mutate(.pred = best_model_prediction$.pred) %>% 
  select(logprice, .pred)

rmse <- test_w_pred %>% 
  mutate(sq = (logprice - .pred)^2) %>% 
  summarize(sqrt(mean(sq)))

rsq <- cor(test_w_pred$logprice, test_w_pred$.pred)^2

# Using the metrics function from yardstick

metrics(test_w_pred,
        truth = logprice,
        estimate = .pred)

# In actual $ values

metrics(test_w_pred %>% 
          mutate_all(exp),
        truth = logprice,
        estimate = .pred)

# Saving a slim model -----------------------------------------------------

saveRDS(boost_model_fit, "saved objects/big_model.RDS")

boost_model_fit_butchered <- butcher::butcher(boost_model_fit)

saveRDS(boost_model_fit_butchered, "saved objects/butchered_model.RDS")
boost_model_fit_butchered <- readRDS("saved objects/butchered_model.RDS")

predict(boost_model_fit_butchered, new_data = rent_test)

# Deploying as an API (vetiver) -------------------------------------------

boost_model_fit_butchered_vetiver <- vetiver::vetiver_model(boost_model_fit_butchered, "boost_rent")

library(plumber)
pr() %>% 
  vetiver::vetiver_api(boost_model_fit_butchered_vetiver, path = "/predict_rent") %>% 
  pr_run()
