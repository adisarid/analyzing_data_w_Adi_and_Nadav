library(tidyverse)

epi <- read_csv("data/epi_data_merged.csv")

glimpse(epi)

poliomyelitis <- epi %>% 
  filter(str_detect(disease_type, "Poliomyelitis")) %>% 
  pivot_longer(cols = Jerusalem:Ashqelon, names_to = "district", values_to = "cases") %>% 
  group_by(disease_type, year) %>% 
  summarize(total_cases = sum(cases))

salmonela <- epi %>% 
  filter(str_detect(disease_type, "Salmonellosis")) %>% 
  pivot_longer(cols = Jerusalem:Ashqelon, names_to = "district", values_to = "cases") %>% 
  group_by(disease_type, year) %>% 
  summarize(total_cases = sum(cases))


  
salmonela %>% 
  filter(year < 2022) %>% 
  summarize(mean(total_cases))

salmonela_weekly <- epi %>% 
  filter(str_detect(disease_type, "Salmonellosis")) %>% 
  pivot_longer(cols = Jerusalem:Ashqelon, names_to = "district", values_to = "cases") %>% 
  mutate(week_num = as.numeric(week_num)) %>% 
  group_by(disease_type, week_num, year) %>% 
  summarize(total_cases = sum(cases))

salmonela_weekly %>% 
  mutate(date = year + week_num/53) %>% 
  ggplot(aes(x = date, y = total_cases, color = factor(year))) + 
  geom_line()


salmonela_weekly %>% 
  mutate(date = year + week_num/53) %>% 
  ggplot(aes(x = week_num, y = total_cases, color = factor(year))) + 
  geom_line()

salmonela_weekly %>% 
  filter(year >= 2021) %>% 
  mutate(date = year + week_num/53) %>% 
  ggplot(aes(x = week_num, y = total_cases, color = factor(year))) + 
  geom_line()


# Diseases by district ----------------------------------------------------

epi %>% 
  filter(str_detect(disease_type, "Chlamydia")) %>% 
  pivot_longer(cols = Jerusalem:Ashqelon, names_to = "district", values_to = "cases") %>% 
  group_by(district) %>% 
  summarize(total_cases = sum(cases)) %>% 
  mutate(district = fct_reorder(district, total_cases)) %>% 
  ggplot(aes(x = district, y = total_cases)) + 
  geom_col() +
  coord_flip()
  
chlamydia_weekly <- epi %>% 
  filter(str_detect(disease_type, "Chlamydia")) %>% 
  pivot_longer(cols = Jerusalem:Ashqelon, names_to = "district", values_to = "cases") %>% 
  mutate(week_num = as.numeric(week_num)) %>% 
  group_by(disease_type, week_num, year) %>% 
  summarize(total_cases = sum(cases))

chlamydia_weekly %>% 
  filter(year >= 2019, year < 2022) %>% 
  mutate(date = year + week_num/53) %>% 
  ggplot(aes(x = week_num, y = total_cases, color = factor(year))) + 
  geom_line()


# Changes 2020<->2021 -----------------------------------------------------

changes_20_21 <- epi %>% 
  filter(year >= 2020, year <= 2021) %>% 
  pivot_longer(cols = Jerusalem:Ashqelon, names_to = "district", values_to = "cases") %>% 
  group_by(disease_type, disease_type_heb, year) %>% 
  summarize(total_cases = sum(cases)) %>% 
  pivot_wider(id_cols = c(disease_type, disease_type_heb),
              names_from = year,
              values_from = total_cases) %>% 
  mutate(year_diff = (`2021` - `2020`)/`2020`) %>% 
  arrange(year_diff) %>% 
  filter(`2020` >= 100) %>% 
  ungroup() %>% 
  mutate(disease_type_heb = fct_reorder(disease_type_heb, year_diff))

changes_20_21 %>% 
  ggplot(aes(x = year_diff, y = disease_type_heb)) +
  geom_col()


# Just the top and bottom 3 but for more years ----------------------------

use_disease <- changes_20_21 %>% 
  slice(1:3, 15:17) %>% 
  select(disease_type)

epi %>% 
  semi_join(use_disease) %>% 
  filter(year <= 2021) %>% 
  pivot_longer(cols = Jerusalem:Ashqelon, names_to = "district", values_to = "cases") %>% 
  group_by(disease_type, disease_type_heb, year) %>% 
  summarize(total_cases = sum(cases)) %>% 
  ggplot(aes(x = year, y = total_cases)) + 
  geom_line() + 
  facet_wrap(~disease_type_heb, scales = "free_y")


# Work on Criptosporidiosis -------------------------------------------------------

epi %>% 
  filter(disease_type == "Criptosporidiosis") %>% 
  pivot_longer(cols = Jerusalem:Ashqelon, names_to = "district", values_to = "cases") %>% 
  group_by(district, year) %>% 
  summarize(total_cases = sum(cases)) %>% 
  ggplot(aes(x = year, y = total_cases)) + 
  geom_line() + 
  facet_wrap(~district, scales = "free_y")

# A shiny app next week? maybe.