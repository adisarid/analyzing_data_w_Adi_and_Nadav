library(tidyverse)


# This one is about programming languages! --------------------------------

languages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-21/languages.csv')

languages %>% 
  count(type, sort = T) %>% 
  mutate(type = fct_inorder(type)) %>% 
  ggplot(aes(y = type, x = n)) + 
  geom_col()

languages %>% 
  filter(type == "queryLanguage")


# Relationship of jobs and users ------------------------------------------

languages %>% 
  filter(type == "pl") %>% 
  filter(number_of_jobs > 0) %>% 
  ggplot(aes(x = number_of_users, y = number_of_jobs)) + 
  geom_point() + 
  stat_smooth(method = "lm") +
  scale_x_log10() + 
  scale_y_log10()


# Languages with 0 jobs ---------------------------------------------------

languages %>% 
  filter(number_of_jobs == 0) %>% 
  filter(type == "pl") %>% 
  View()


# Relationship of #user with year of creation -----------------------------

live_pl <- languages %>% 
  filter(type == "pl") %>% 
  filter(number_of_users > 10000) %>% 
  mutate(log_users = log10(number_of_users)) %>% 
  mutate(years_existed = 2023 - appeared)

ggplot(live_pl, aes(x = appeared, y = log_users, label = pldb_id, color = is_open_source)) + 
  geom_text(check_overlap = T, nudge_y = 0.075) + 
  geom_point()


# Validations year appeared vs wikipedia year appeared --------------------

live_pl %>% 
  mutate(diff_year = appeared - wikipedia_appeared) %>% 
  ggplot(aes(diff_year)) + 
  geom_histogram()

live_pl %>% 
  mutate(diff_year = appeared - wikipedia_appeared) %>% 
  filter(diff_year < -30) %>% glimpse()


# Analysis of origin ------------------------------------------------------

languages %>% 
  filter(type == "pl") %>% 
  add_count(origin_community, sort = T) %>% 
  filter(!is.na(origin_community)) %>% 
  mutate(is_live = number_of_users >= 10000) %>% 
  select(pldb_id, origin_community, n, is_live) %>% 
  group_by(origin_community, n) %>% 
  summarize(languages = paste0(pldb_id, collapse = "; ")) %>% 
  arrange(desc(n)) %>% 
  View()

# Visualization of live languages vs dead ones per origin community ----

base_languages_live_dead <- languages %>% 
  filter(type == "pl") %>% 
  filter(!is.na(origin_community)) %>% 
  mutate(is_live = number_of_users >= 10000) %>% 
  select(pldb_id, origin_community, is_live) %>%
  group_by(origin_community) %>% 
  summarize(total_live = sum(is_live),
            total_dead = sum(!is_live)) %>% 
  filter(total_live >= 2) %>%
  pivot_longer(cols = c(total_live, total_dead)) 

base_languages_labels <- languages %>% 
  filter(type == "pl") %>% 
  filter(!is.na(origin_community)) %>% 
  mutate(is_live = number_of_users >= 10000) %>% 
  select(pldb_id, origin_community, is_live) %>% 
  group_by(origin_community, is_live) %>% 
  summarize(language_names = paste0(pldb_id, collapse = "\n")) %>% 
  mutate(name = if_else(is_live,
                        "total_live",
                        "total_dead")) %>% 
  select(name, language_names, origin_community)


base_languages_live_dead %>% 
  left_join(base_languages_labels) %>% 
  ggplot(aes(x = origin_community, y = value, fill = name, label = language_names)) + 
  geom_col() + 
  geom_text(position = position_stack(vjust = 0.5), size = 3) + 
  theme_bw() + 
  theme(legend.position = "right") + 
  ggtitle("Programming languages - dead or alive?", subtitle = "Top contributing communities with at least two languages with >10K user base")



