library(tidyverse)
all_files <- tibble(filename = dir("c:/temp/weekly_moh_epi_reports/", pattern = "*.xls*")) %>% 
  separate(filename, sep = "_", into = letters[1:5], remove = FALSE) %>% 
  select(filename, c, d, e) %>% 
  mutate(year = as.numeric(c)) %>% 
  mutate(d = as.numeric(str_remove(d, "IWER"))) %>% 
  mutate(e = as.numeric(if_else(str_detect(e, "xls"), NA_character_, e))) %>% 
  mutate(week = coalesce(d, e)) %>% 
  select(filename, year, week)

# Check which are missing
all_combos <- crossing(year = 2001:2022, week = 1:52) %>% 
  filter(!(year == 2022 & week >= 15)) %>% 
  left_join(all_files)

all_combos %>% 
  filter(is.na(filename)) %>% View()
