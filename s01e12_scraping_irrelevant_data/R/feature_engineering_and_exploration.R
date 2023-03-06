library(tidyverse)

irr <- read_csv("data/extracted_irrelevant_data.csv") %>% 
  mutate(recommendation = case_when(
    str_detect(title, "לא להעביר") ~ "avoid",
    str_detect(title, "לכאן ולכאן") ~ "neutral",
    str_detect(title, "ללא המלצה") ~ "neutral",
    str_detect(title, "\\(תרמית\\)") ~ "avoid",
    str_detect(title, "אפשר להעביר") ~ "true")) %>% 
  filter(!is.na(recommendation)) %>% 
  filter(!is.na(original_text))

ggplot(irr, aes(recommendation)) + 
  geom_bar()


# Splitting to train/test -------------------------------------------------

library(tidymodels)

set.seed(544)

irr_split <- initial_split(irr, prop = 0.8)
irr_train <- training(irr_split) %>% 
  mutate(avoid = factor((recommendation == "avoid")))
irr_test <- testing(irr_split) %>% 
  mutate(avoid = factor(recommendation == "avoid"))


# Analysis on the training dataset ----------------------------------------

irr_train %>% 
  mutate(msg_length = str_length(original_text)) %>% 
  ggplot(aes(y = msg_length, x = recommendation)) + 
  geom_boxplot() + 
  scale_y_log10()

# > Message length --------------------------------------------------------

irr_train %>% 
  group_by(recommendation) %>% 
  summarize(mean_msg_length = mean(str_length(original_text)))

# First insight: the message length is important - shorter messages tend to be false.


# > Checking exclamation and question marks -------------------------------

irr_exclamation <- irr_train %>% 
  mutate(msg_length = str_length(original_text),
         num_exclamation = str_count(original_text, pattern = "!"),
         num_question = str_count(original_text, pattern = "\\?"),
         num_dots = str_count(original_text, pattern = "\\.\\.\\."),
         prcnt_exclamation = num_exclamation/msg_length,
         prcnt_question = num_question/msg_length,
         prcnt_dots = num_dots/msg_length,
         num_total = num_exclamation + num_question + num_dots,
         prcnt_total = prcnt_exclamation + prcnt_question + prcnt_dots)

# irr_exclamation %>% 
#   ggplot(aes(y = prcnt_total, x = recommendation)) + 
#   geom_boxplot() +
#   scale_y_log10()
# 
# irr_exclamation %>% 
#   ggplot(aes(y = prcnt_exclamation, x = recommendation)) + 
#   geom_boxplot() + 
#   scale_y_log10()
# 
# irr_exclamation %>% 
#   ggplot(aes(y = prcnt_question, x = recommendation)) + 
#   geom_boxplot() + 
#   scale_y_log10()

irr_exclamation %>% 
  select(recommendation, num_exclamation:num_dots) %>% 
  pivot_longer(cols = -recommendation, names_to = "character_type", values_to = "num_characters") %>% 
  count(recommendation, character_type, num_characters) %>% 
  group_by(recommendation, character_type) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(x = num_characters, y = prop)) + 
  geom_col() +
  facet_grid(rows = vars(character_type), cols = vars(recommendation)) + 
  coord_cartesian(xlim = c(0, 10))

# Insight #2: Seems as though these characters are not good separators between true/avoid.


# > Using keywords --------------------------------------------------------

library(tidytext)

irr_text_analysis <- irr_train %>% 
  select(article, recommendation, original_text) %>% 
  tidytext::unnest_tokens(word, original_text)

irr_word_counts <- irr_text_analysis %>% 
  group_by(recommendation) %>% 
  count(word, sort = T) %>% 
  group_by(recommendation) %>% 
  mutate(prop = n/sum(n))

visualize_word_separation <- function(word_for_check){
  
  word_table <- irr_train %>% 
    mutate(is_word_appear = str_detect(original_text, word_for_check))
  
  word_sample <- sum(word_table$is_word_appear)
  
  word_table %>% 
    group_by(is_word_appear) %>% 
    count(recommendation) %>% 
    ggplot(aes(x = is_word_appear, y = n, fill = recommendation)) + 
    geom_col(position = "fill") + 
    labs(caption = glue::glue("The word {word_for_check} appears in {word_sample} observations."))
}

visualize_word_separation("מספר")

# Insight #3: It seems that the word "מספר" is meaningful in identifying avoid

visualize_word_separation("הודעה")
visualize_word_separation("אנשים")
visualize_word_separation("וירוס")

report_word_separation <- function(word_for_check){
  irr_train %>% 
    mutate(is_word_appear = str_detect(original_text, word_for_check)) %>% 
    group_by(is_word_appear) %>% 
    count(recommendation) %>% 
    group_by(recommendation) %>% 
    mutate(prop = n/sum(n)) %>% 
    select(-n) %>% 
    filter(!is_word_appear) %>% 
    pivot_wider(names_from = recommendation, values_from = prop) %>% 
    select(-is_word_appear)
}

report_word_separation("מספר")
report_word_separation("הודעה")

report_word_separation('ש"ח')
visualize_word_separation('ש"ח')

word_separation_summary <- irr_word_counts %>% 
  arrange(desc(n)) %>% 
  group_by(recommendation) %>% 
  slice(1:50) %>% 
  mutate(num = seq_along(n)) %>% 
  ungroup() %>% 
  distinct(word) %>% 
  mutate(sep = map(word, report_word_separation, .progress = T))

word_separation_summary %>% 
  unnest(sep) %>% 
  ungroup() %>% 
  mutate(avoid/((neutral+true)/2)) %>% 
  View()
  
visualize_word_separation('לא')
visualize_word_separation('לו')

# Insight #4: It seems that words that we expected to not influence the recommendation are influential
#             Specifically Hebrew stop words - TODO: VERIFY later on.
#             If this is indeed accurate, should use these words in the model training.



# Using regular expression to detect phone numbers ------------------------

irr_train %>% 
  mutate(is_number_present = str_detect(original_text, "^[\\+]?[(]?[0-9]{3}[)]?[-\\s\\.]?[0-9]{3}[-\\s\\.]?[0-9]{4,6}$")) %>% 
  count(is_number_present)

# Insight #5: seems the regex didn't find any phone numbers. After viewing a few texts - the author might be deleting the phone numbers

# Finalizing features -----------------------------------------------------

# We will use the following features: msg length, and the words: מספר, וירוס, לא, לו, ש"ח, שקלים, to, and

irr_train_processed <- irr_train %>% 
  mutate(is_mispar = str_detect(original_text, "מספר"),
         is_virus = str_detect(original_text, "וירוס|קורונה"),
         is_not = str_detect(original_text, "לא"),
         is_forhim = str_detect(original_text, "לו"),
         is_currency = str_detect(original_text, 'ש"ח|שקלים'),
         is_eng = str_detect(original_text, "to|and")) %>% 
  mutate(msg_length = str_length(original_text)) %>% 
  select(starts_with("is_"), msg_length, avoid) %>% 
  mutate(across(starts_with("is_"),
                ~{.*1}))

# Build a cross validation basis
irr_cv <- vfold_cv(irr_train_processed, v = 10)

# Build the recipe based on the aforementioned insights

irr_recipe <- recipe(avoid ~ ., data = irr_train_processed) %>% 
  step_log(msg_length)
  
# Test recipe
irr_recipe %>% 
  prep() %>% 
  bake(new_data = irr_train_processed)

# Create a logistic regression model
irr_glm <- logistic_reg(engine = "glm")

# Generate workflow (for logistic regression)

irr_logistic_regression_wflow <- workflow() %>% 
  add_model(irr_glm) %>% 
  add_recipe(irr_recipe)

# Fit the actual model

irr_glm_fit <- irr_logistic_regression_wflow %>% 
  fit(irr_train_processed)

glm_res <- extract_fit_engine(irr_glm_fit)
glm_res %>% summary()

# Resampling definitions

ctrl <- control_resamples(save_pred = T)

# Conduct resampling

resamples_glm <- irr_glm %>% 
  tune_grid(irr_recipe, resamples = irr_cv, control = ctrl)

collect_metrics(resamples_glm)
collect_predictions(resamples_glm)

irr_train_processed %>% 
  mutate(fitted = glm_res$fitted.values) %>% 
  roc_curve(fitted, truth = avoid, event_level = "second") %>% 
  ggplot(aes(x = 1-specificity, y = sensitivity)) + 
  geom_line() + 
  geom_abline(slope = 1, intercept = 0)


# Boosting model ----------------------------------------------------------

# Create a logistic regression model
irr_boost <- boost_tree(mode = "classification",
                        engine = "xgboost")

# Generate workflow (for logistic regression)

irr_boost_regression_wflow <- workflow() %>% 
  add_model(irr_boost) %>% 
  add_recipe(irr_recipe)

# Fit the actual model

irr_boost_fit <- irr_boost_regression_wflow %>% 
  fit(irr_train_processed)

# Conduct resampling

resamples_boost <- irr_boost %>% 
  tune_grid(irr_recipe, resamples = irr_cv, control = ctrl)

boost_res <- extract_fit_engine(irr_boost_fit)

boost_predictions <- predict(boost_res, newdata = irr_recipe %>% 
          prep() %>% 
          bake(new_data = irr_train_processed) %>% 
          select(-avoid) %>% 
          as.matrix())

# Create a combined ROC ---------------------------------------------------

glm_roc <- irr_train_processed %>% 
  mutate(fitted = glm_res$fitted.values) %>% 
  roc_curve(fitted, truth = avoid, event_level = "second") %>% 
  mutate(model = "Logistic regression (train)")

boost_roc <- irr_train_processed %>% 
  mutate(fitted = boost_predictions) %>% 
  roc_curve(fitted, truth = avoid) %>% 
  mutate(model = "Boosting (train)")

glm_roc %>% 
  bind_rows(boost_roc) %>% 
  ggplot(aes(x = 1-specificity, y = sensitivity, color = model)) + 
  geom_line() + 
  geom_abline(slope = 1, intercept = 0)


# Examining the test set ROC ----------------------------------------------

irr_test_processed <- irr_test %>% 
  mutate(is_mispar = str_detect(original_text, "מספר"),
         is_virus = str_detect(original_text, "וירוס|קורונה"),
         is_not = str_detect(original_text, "לא"),
         is_forhim = str_detect(original_text, "לו"),
         is_currency = str_detect(original_text, 'ש"ח|שקלים'),
         is_eng = str_detect(original_text, "to|and")) %>% 
  mutate(msg_length = str_length(original_text)) %>% 
  select(starts_with("is_"), msg_length, avoid) %>% 
  mutate(across(starts_with("is_"),
                ~{.*1}))

glm_test_pred <- predict(glm_res, newdata = irr_test_processed, type = "response")
boost_test_pred <- predict(boost_res, 
                           newdata = irr_test_processed %>% 
                             select(-avoid) %>% 
                             as.matrix())

glm_test_roc <- irr_test_processed %>% 
  mutate(fitted = glm_test_pred) %>% 
  roc_curve(fitted, truth = avoid, event_level = "second") %>% 
  mutate(model = "Logistic regression (test)")

boost_test_roc <- irr_test_processed %>% 
  mutate(fitted = boost_test_pred) %>% 
  roc_curve(fitted, truth = avoid) %>% 
  mutate(model = "Boosting (test)")

glm_roc %>% 
  bind_rows(boost_roc,
            glm_test_roc,
            boost_test_roc) %>% 
  ggplot(aes(x = 1-specificity, y = sensitivity, color = model)) + 
  geom_line() + 
  geom_abline(slope = 1, intercept = 0)
