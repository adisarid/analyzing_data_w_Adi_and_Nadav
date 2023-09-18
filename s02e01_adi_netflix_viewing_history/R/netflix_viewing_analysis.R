library(tidyverse)
library(ggtext)

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

# With 2023 corrected for partial year
netflix_adi_chart_data <- netflix_adi %>% 
  mutate(year = year(floor_date(start_time, unit = "year"))) %>% 
  filter(device_type != "Mobile",
         !str_detect(device_type, "Tablet")) %>% 
  mutate(device_type = if_else(str_detect(device_type, "Android"),
                               "Android",
                               device_type)) %>% 
  group_by(year, device_type) %>% 
  summarize(total_duration_hr = as.numeric(sum(duration))/(3600)) %>% 
  mutate(total_duration_hr = if_else(year == 2023, 
                                     total_duration_hr/7*12, total_duration_hr)) %>% 
  mutate(device_type = case_when(device_type == "Android" ~ "Mobile",
                                 device_type == "Chrome PC (Cadmium)" ~ "PC",
                                 T ~ "Smart TV"))

library(ggtext)

netflix_adi_chart_data %>% 
  ggplot(aes(x = year, y = total_duration_hr, color = device_type, linewidth = device_type)) + 
  geom_line() + 
  geom_point(size = 2.5) + 
  theme_minimal() + 
  xlab("") + 
  ylab("Hours") + 
  labs(title = "Total <span style = 'color:#db0000;'>Netflix</span> duration 2019-2023* (Adi's profile)",
       subtitle = "PC and Smart TV declining in favor of Mobile",
       caption = "* The 2023 data was factored to accomodate for 12 months (actual data exists up to July 30th)") + 
  theme(legend.position = "none", 
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()) + 
  geom_text(data = netflix_adi_chart_data %>% 
              filter(year == 2023) %>% 
              mutate(total_duration_hr = if_else(device_type == "Smart TV",
                                                 total_duration_hr + 2.5,
                                                 total_duration_hr)), 
                           aes(label = device_type), nudge_x = 0.15, hjust = 0) + 
  coord_cartesian(xlim = c(2019, 2023.5)) + 
  scale_x_continuous(breaks = 2019:2023) + 
  scale_linewidth_manual(values = c("Mobile" = 1.25, "PC" = 0.5, "Smart TV" = 0.5)) + 
  scale_color_brewer(palette = "Set2") +
  theme(plot.title = element_markdown())

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
