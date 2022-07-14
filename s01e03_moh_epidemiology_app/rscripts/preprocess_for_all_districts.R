library(tidyverse)

# Preprocess and loading --------------------------------------------------

epi_data <- read_csv("data/epi_data_merged.csv")

# This code was only run once.
# epi_data_all_districts <- epi_data %>% 
#   select(disease_type, Jerusalem:Ashqelon, week_num, year) %>% 
#   pivot_longer(cols = Jerusalem:Ashqelon, names_to = "district", values_to = "cases") %>% 
#   group_by(disease_type, week_num, year) %>% 
#   summarize(all_districts = sum(cases))
# 
# new_epi_data <- epi_data %>% 
#   left_join(epi_data_all_districts)
# 
# write_csv(new_epi_data, "data/epi_data_merged_all_districts.csv")
