library(tidyverse)

netflix <- read_csv("Data/ViewingActivity.csv") %>% 
  janitor::clean_names()
  
glimpse(netflix)



# Who watched what --------------------------------------------------------

netflix %>% 
  count(profile_name)

netflix %>% 
  filter(start_time >= "2023-01-01") %>% 
  count(profile_name)

watching_time <- netflix %>% 
  mutate(year = year(start_time)) %>% 
  group_by(profile_name, year) %>% 
  summarize(total_duration_hr = as.numeric(sum(duration))/(3600))

watching_time %>% 
  ggplot(aes(x = year, y = total_duration_hr)) + 
  geom_col() +
  facet_wrap(~profile_name)


# Adi's specific titles ---------------------------------------------------

netflix_adi <- netflix %>% 
  filter(profile_name == "Adi") %>% 
  mutate(title_fix = str_remove_all(title, "(:.*)")) %>% 
  mutate(year = year(start_time))

top_titles <- netflix_adi %>% 
  group_by(year, title_fix) %>% 
  summarize(total_duration_hr = as.numeric(sum(duration))/(3600)) %>% 
  arrange(year, desc(total_duration_hr)) %>% 
  group_by(year) %>% 
  slice(1:5) %>% 
  mutate(title_wrap = str_wrap(title_fix, width = 20)) %>% 
  ungroup() %>% 
  add_count(title_wrap)

ggplot(top_titles,
       aes(x = total_duration_hr, y = title_wrap, fill = factor(n))) +
  geom_col() + 
  facet_wrap(~year, scales = "free_y")

# Platform by year --------------------------------------------------------

netflix_adi %>% 
  mutate(year = floor_date(start_time, unit = "year")) %>% 
  filter(device_type != "Mobile",
         !str_detect(device_type, "Tablet")) %>% 
  mutate(device_type = if_else(str_detect(device_type, "Android"),
                               "Android",
                               device_type)) %>% 
  group_by(year, device_type) %>% 
  summarize(total_duration_hr = as.numeric(sum(duration))/(3600)) %>% 
  ggplot(aes(x = year, y = total_duration_hr, color = device_type)) + 
  geom_line() + 
  geom_point()


# Hourly patterns ---------------------------------------------------------

netflix_adi %>% 
  mutate(hour = hour(start_time)) %>% 
  mutate(viewtime_fct = case_when(between(hour, 7, 11) ~ "Morning",
                                  between(hour, 11, 16) ~ "Noon",
                                  between(hour, 16, 19) ~ "Afternoon",
                                  between(hour, 19, 21) ~ "Evening",
                                  T ~ "Night")) %>% 
  mutate(viewtime_fct = factor(viewtime_fct,
                               levels = c("Morning",
                                          "Noon",
                                          "Afternoon",
                                          "Evening",
                                          "Night"))) %>% 
  group_by(year, viewtime_fct) %>% 
  summarize(total_duration_hr = as.numeric(sum(duration))/(3600)) %>% 
  ggplot(aes(x = year, y = total_duration_hr, fill = viewtime_fct)) + 
  geom_col(position = position_fill()) +
  scale_fill_brewer(palette = "Reds")
