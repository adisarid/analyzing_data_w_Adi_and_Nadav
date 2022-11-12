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
                                        cost_complexity(), size = 5)

rent_boost_grid <- grid_latin_hypercube(trees(),
                                        learn_rate())

# Generate a recipe -------------------------------------------------------

rent_recipe <- recipe(logprice ~ year + beds + baths + sqft + county, 
                      data = rent_train) %>% 
  step_impute_mean(all_numeric_predictors()) %>% 
  step_other(county) %>%
  step_naomit(county) %>%
  step_dummy(county)

# Double check if recipe is ok --------------------------------------------

rent_recipe %>% 
  prep() %>% 
  bake(new_data = rent_train)

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

# Up to here part 1 ----


# Create boosting and random forest models --------------------------------

# > Generate tree workflow ----

resamples_trees <- rent_trees %>% 
  tune_grid(rent_recipe, resamples = rent_cv, control = ctrl, 
            grid = rent_trees_grid)

# Did it in advanced:
resamples_trees <- readRDS("saved objects/resamples_trees.RDS")

# rent_trees_predict <- collect_predictions(resamples_trees)

tree_metrics <- collect_metrics(resamples_trees)

# > Generate boosting workflow ----

resamples_boosting <- rent_boost %>% 
  tune_grid(rent_recipe, resamples = rent_cv, control = ctrl, grid = rent_boost_grid)

# Did it in advanced:
resamples_boosting <- readRDS("saved objects/resamples_boosting.RDS")

boost_metrics <- collect_metrics(resamples_boosting)

boost_metrics %>% 
  ggplot(aes(x = trees, y = mean)) + 
  facet_wrap(~.metric, scales = "free_y") +
  geom_point() + 
  geom_line()

boost_metrics %>% 
  ggplot(aes(x = learn_rate, y = mean)) + 
  facet_wrap(~.metric, scales = "free_y") +
  geom_point() + 
  geom_line() +
  scale_x_continuous(labels = scales::scientific_format())

boost_metrics %>% 
  select(trees, learn_rate, mean, .metric) %>% 
  pivot_wider(id_cols = c(trees, learn_rate), names_from = .metric, values_from = mean) %>% 
  arrange(desc(rsq))


# exporting objects for later use -----------------------------------------

# saveRDS(resamples_trees, file = "saved objects/resamples_trees.RDS")
# saveRDS(resamples_boosting, file = "saved objects/resamples_boosting.RDS")










# Fit the best model ------------------------------------------------------
# replace 999 with best values
rent_boost <- boost_tree(mode = "regression", 
                         engine = "xgboost",
                         trees = 1239,
                         learn_rate = 0.0655)

boost_model_fit <- workflow() %>% 
  add_model(rent_boost) %>% 
  add_recipe(rent_recipe) %>% 
  fit(rent_train)

rent_tree <- decision_tree(tree_depth = 13,
                           cost_complexity = 7.8e-7) %>% 
  set_engine("rpart") %>% 
  set_mode("regression")

tree_model_fit <- workflow() %>% 
  add_model(rent_tree) %>% 
  add_recipe(rent_recipe) %>% 
  fit(rent_train)

best_model_prediction <- predict(boost_model_fit, newdata = rent_test)