library(tidyverse)

sets <- read_csv("data/sets.csv") %>% 
  filter(num_parts > 10)
themes <- read_csv("data/themes.csv")

set_theme_releases <- sets %>% 
  left_join(themes %>% 
              rename(theme_name = name), 
            by = c("theme_id" = "id"))

# Theme development over time
set_theme_releases %>% 
  count(year, theme_name)

# Different themes per year
set_theme_releases %>% 
  distinct(year, theme_name) %>% 
  count(year) %>% 
  ggplot(aes(x = year, y = n)) + 
  geom_col()+ 
  theme_bw() + 
  ggtitle("Number of Lego themes per year",
          subtitle = "Significant increase in theme variation since 00'")

# Star wars + space theme popularity
set_theme_releases %>% 
  mutate(theme = case_when(str_detect(name, "Space | space ") ~ "Space",
                           str_detect(theme_name, "Star") ~ "Star Wars")) %>% 
  filter(!is.na(theme)) %>% 
  count(year, theme) %>% 
  group_by(year, theme) %>% 
  summarize(n_sets = sum(n)) %>% 
  ggplot(aes(x = year, y = n_sets, fill = theme)) + 
  geom_col() + 
  facet_wrap(~ theme, scales = "free_y") + 
  theme_bw() +
  theme(legend.position = "none")

# The most complex sets ever
set_theme_releases %>% 
  arrange(desc(num_parts)) %>% 
  slice(1:10) %>% 
  View()

# Set complexity over time
set_theme_releases %>% 
  group_by(year) %>% 
  summarize(max_pieces = max(num_parts)) %>% 
  ggplot(aes(x = year, y = max_pieces)) +
  geom_col() + 
  theme_bw() + 
  ggtitle("Largest lego set per year (number of pieces)")

# Loess smoothing
set_theme_releases %>% 
  group_by(year) %>% 
  summarize(max_pieces = max(num_parts)) %>% 
  ggplot(aes(x = year, y = max_pieces)) +
  geom_smooth() + 
  theme_bw() + 
  ylab("Largest set size") +
  ggtitle("Lego set sizes per year (smoothing model)")

# Average set size
set_theme_releases %>% 
  ggplot(aes(x = year, y = num_parts)) + 
  geom_smooth(method = "lm") + 
  ylab("Number of Parts") + 
  xlab("Year") + 
  ggtitle("Average set size - Linear model") + 
  theme_bw()

set_theme_releases %>% 
  ggplot(aes(x = year, y = num_parts)) + 
  geom_smooth(method = "loess") + 
  ylab("Number of Parts") + 
  xlab("Year") + 
  ggtitle("Average set size - Loess model") + 
  theme_bw()

# Average number of set size (scatter plot)
set_theme_releases %>% 
  group_by(year) %>% 
  summarize(mean_num_size = mean(num_parts)) %>% 
  ggplot(aes(x = year, y = mean_num_size)) + 
  geom_point() +
  theme_bw()

# Moving average of set size
set_theme_releases %>% 
  group_by(year) %>% 
  summarize(mean_num_size = mean(num_parts)) %>% 
  filter(between(year, 1953, 2023)) %>% 
  mutate(ma3 = forecast::ma(mean_num_size, order = 3),
         ma10 = forecast::ma(mean_num_size, order = 10),
         ma15 = forecast::ma(mean_num_size, order = 15)) %>% 
  select(year, starts_with("ma")) %>% 
  pivot_longer(cols = starts_with("ma")) %>% 
  ggplot(aes(x = year, y = value, color = name)) + 
  geom_line() + 
  theme_bw() + 
  ggtitle("Moving average - lego set size (number of pieces)")
  


# Pricing?
read_csv("data/part_relationships.csv")

parts <- read_csv("data/parts.csv")
