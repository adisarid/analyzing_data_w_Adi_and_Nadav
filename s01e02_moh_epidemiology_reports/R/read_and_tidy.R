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

epi_data_merged <- map_df(dir("data/", full.names = T) %>% tail(385),
                          read_and_tidy)
