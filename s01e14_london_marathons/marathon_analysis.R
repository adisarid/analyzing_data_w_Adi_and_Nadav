library(tidyverse)


winners <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-25/winners.csv')
london_marathon <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-25/london_marathon.csv') %>% 
  janitor::clean_names()

glimpse(winners)

# Do atheletes improve results over time (i.e., not the same athelete)

winners %>% 
  janitor::clean_names() %>%
  group_by(year, category) %>%
  summarize(best_result = min(time)) %>% 
  ggplot(aes(x = year, y = best_result/3600, color = category)) + 
  geom_line() + 
  geom_smooth(se = FALSE) + 
  ylab("Marathon time [hours]")

# Recent results
winners %>% 
  janitor::clean_names() %>% group_by(category) %>% slice_tail(n = 1)

# Earliest results
winners %>% 
  janitor::clean_names() %>% group_by(category) %>% slice_head(n = 1)


# Country distribution per decade -----------------------------------------

winners %>%
  janitor::clean_names() %>%
  mutate(decade = case_when(year < 1990 ~ "80s",
                            year < 2000 ~ "90s",
                            year < 2010 ~ "00s",
                            T ~ "10s+")) %>%
  mutate(decade = fct_inorder(decade)) %>%
  group_by(decade) %>%
  distinct(nationality, category) %>%
  ungroup() %>%
  count(decade, category)




# Analysis of the marathons themselves ------------------------------------

london_marathon %>% glimpse()

# Proportions of accepted/started over time

london_marathon %>% 
  mutate(across(accepted:finishers,
                ~{. / applicants})) %>%
  select(year, accepted:finishers) %>%
  pivot_longer(cols = -year, 
               names_to = "type",
               values_to = "proportion") %>%
  ggplot(aes(x = year, y = proportion, color = type)) + 
  geom_line() + 
  geom_point() + 
  scale_y_continuous(labels = scales::percent_format(1))
  
# How much was raised?

london_marathon %>% 
  ggplot(aes(year, y = raised)) + 
  geom_col()

# Increase in applicants over time

london_marathon %>%
  filter(year <= 2019) %>% 
  ggplot(aes(x = year, y = starters)) +
  geom_line() + 
  geom_point()

london_marathon %>%
  filter(year <= 2019) %>% 
  ggplot(aes(x = year, y = applicants)) +
  geom_line() + 
  geom_point()

london_marathon %>%
  filter(year <= 2019) %>%
  select(year, applicants:finishers) %>%
  pivot_longer(cols = -year, names_to = "type", values_to = "runners") %>%
  ggplot(aes(x = year, y = runners)) + 
  geom_line() + 
  geom_point() +
  facet_wrap(~type, scales = "free_y")








  
