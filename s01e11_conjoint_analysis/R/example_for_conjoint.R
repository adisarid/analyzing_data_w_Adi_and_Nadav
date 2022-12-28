library(tidyverse)

smartwatch <- read_csv("https://reporting.alchemer.com/reportsview/?key=471756-13847170-687fdff0b5d483c1a744b9860f2e1002") %>%
  set_names(c("response_id",
              "set_number",
              "card_number",
              "battery_life",
              "gps_capabilites",
              "esim_connectivity",
              "robustness",
              "choice")) %>% 
  mutate(choice = choice/100)

demographic_data <- read_csv("https://reporting.alchemer.com/reportsview/?key=471756-13847202-0a2a33bdad9c7a20b9fcb24ad75dc242") %>% 
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
