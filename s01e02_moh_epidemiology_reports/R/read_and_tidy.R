# Read files from data

library(tidyverse)

read_and_tidy <- function(filename){
  
  # extract year and week number
  find_start <- str_locate(filename, "files_weekly-epidemiology_") # so that you can make do with any subdirectory length
  year <- str_sub(filename, start = find_start[2]+1, end = find_start[2]+4)
  week_num <- str_remove_all(str_sub(filename, start = find_start[2]+5),
                             glue::glue("{year}|.xlsx|.xls|_|IWER"))
  
  # read file
  
  file_data <- readxl::read_excel(
    filename,
    range = "A41:T128") %>% 
    rename(disease_type = `Week No.`,
           disease_type_heb = `שבוע מס'`) %>% 
    mutate(Jerusalem = as.numeric(Jerusalem)) %>% 
    filter(!is.na(Jerusalem)) %>% 
    mutate(across(-c(disease_type, disease_type_heb), as.numeric)) %>% 
    select(-17, -18, -19) %>% 
    mutate(week_num = week_num) %>% 
    mutate(year = year)
  
  file_data
}

# demonstrate reading:

epi_data_merged <- map_df(dir("data/", full.names = T) %>% tail(3),
                          read_and_tidy)

# write_excel_csv(epi_data_merged, "data/epi_data_merged.csv")

# Totals
epi_per_il <- epi_data_merged %>% 
  pivot_longer(cols = Jerusalem:Ashqelon, names_to = "district", values_to = "cases") %>% 
  group_by(disease_type, disease_type_heb, week_num, year) %>% 
  summarize(total_cases = sum(cases)) %>% 
  mutate(week_num = as.numeric(week_num)) %>% 
  arrange(disease_type, year, week_num)
