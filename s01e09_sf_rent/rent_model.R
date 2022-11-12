library(tidyverse)
library(tidymodels)


# Read the dataset --------------------------------------------------------

rent <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-05/rent.csv') %>% 
  mutate(logprice = log(price))

# Some exploratory analysis -----------------------------------------------

rent %>% glimpse()

rent %>% count(room_in_apt)

rent %>% count(baths)

rent %>% count(is.na(sqft))

rent %>% 
  filter(year %in% c(2004, 2012, 2016)) %>% 
  ggplot(aes(price)) + 
  geom_histogram() + 
  scale_x_log10() + 
  facet_wrap(~year)


# Building a new model to predict the rent --------------------------------

set.seed(42)

rent_split <- initial_split(rent, prop = 0.8)

rent_train <- training(rent_split)
rent_test <- testing(rent_split)

rent_cv <- vfold_cv(rent_train, v = 10)


# Create a model tuning specification -------------------------------------

rent_trees <- decision_tree(tree_depth = tune(),
                            cost_complexity = tune()) %>% 
  set_engine("rpart") %>% 
  set_mode("regression")

rent_boost <- boost_tree(mode = "regression", 
                         engine = "xgboost",
                         trees = tune(),
                         learn_rate = tune())

rent_lm <- linear_reg(mode = "regression", engine = "lm")


# Create a search grid ----------------------------------------------------

rent_trees_grid <- grid_latin_hypercube(tree_depth(),
                                        cost_complexity())

rent_boost_grid <- grid_latin_hypercube(trees(),
                                        learn_rate())

# Generate a recipe -------------------------------------------------------

rent_recipe <- recipe(logprice ~ year + beds + baths + sqft + county, 
                      data = rent_train) %>% 
  step_impute_mean(all_numeric_predictors()) %>% 
  step_naomit(county) %>% 
  step_other(county)

# Double check if recipe is ok --------------------------------------------

rent_recipe %>% 
  prep() %>% 
  bake(new_data = rent_train) %>% 
  count(county)

# Generate workflow -------------------------------------------------------

rent_lm_workflow <- workflow() %>% 
  add_model(rent_lm) %>% 
  add_recipe(rent_recipe)

rent_lm_fit <- rent_lm_workflow %>% 
  fit(rent_train)

# Getting a glimpse of the original underlaying model
extract_fit_engine(rent_lm_fit) %>% summary()

# Resample controls -------------------------------------------------------

ctrl <- control_resamples(save_pred = TRUE)

# Conduct resamples -------------------------------------------------------

resamples_lm <- rent_lm %>% 
  tune_grid(rent_recipe, resamples = rent_cv, control = ctrl)

rent_lm_predict <- collect_predictions(resamples_lm)

collect_metrics(resamples_lm)
