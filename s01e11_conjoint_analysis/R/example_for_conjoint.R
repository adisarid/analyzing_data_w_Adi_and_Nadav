library(tidyverse)

smartwatch <- read_csv("data/smartwatch_conjoint_raw.csv") %>%
  set_names(c("response_id",
              "set_number",
              "card_number",
              "battery_life",
              "gps_capabilites",
              "esim_connectivity",
              "robustness",
              "choice")) %>% 
  mutate(choice = choice/100)

demographic_data <- read_csv("data/demographics.csv") %>% 
  set_names(c("response_id",
              "age",
              "gender",
              "sports_activity")) %>% 
  mutate(age = runif(101, min = 18, max = 70))

smart_watch_demographics <- smartwatch %>% 
  left_join(demographic_data)

# Running a logistic regression model
smartwatch_model <- glm(formula = choice ~ 
                          battery_life + gps_capabilites + 
                          esim_connectivity + robustness + 
                          age + sports_activity + gender,
                        family = binomial(link = "logit"),
                        data = smart_watch_demographics)

summary(smartwatch_model)

smartwatch_model %>% 
  broom::tidy() %>% 
  mutate(exp_estimate = exp(estimate)) %>% 
  View()

# Comparing two alternatives ----------------------------------------------

new_predictions <- tibble(
  battery_life = "7 ימים",
  gps_capabilites = "ללא GPS",
  esim_connectivity = "ניתן לבצע שיחות ללא טלפון",
  robustness = "עמידות לחול ומים",
  age = 45,
  sports_activity = "מעל חמש שעות",
  gender = "אשה") %>% 
  bind_rows(
    tibble(
      battery_life = "14 ימים",
      gps_capabilites = "ללא GPS",
      esim_connectivity = "ניתן לבצע שיחות ללא טלפון",
      robustness = "עמידות לחול ומים",
      age = 45,
      sports_activity = "מעל חמש שעות",
      gender = "אשה")
  )

predict(smartwatch_model, 
        newdata = new_predictions, 
        type = "response")

# Odds ratio comparison
(0.3518971/(1-0.3518971))/(0.3483264/(1-0.3483264))
