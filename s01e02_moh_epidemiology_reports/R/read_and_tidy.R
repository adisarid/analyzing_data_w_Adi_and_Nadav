# Read files from data

library(tidyverse)

read_and_tidy <- function(week_num, year){
  file_data <- readxl::read_excel(
    glue::glue("data/files_weekly-epidemiology_{year}_IWER_{stringr::str_pad('1', width = 2, pad = '0')}_{year}.xlsx"),
    range = "A41:T128") %>% 
    rename(disease_type = `Week No.`,
           disease_type_heb = `שבוע מס'`) %>% 
    mutate(Jerusalem = as.numeric(Jerusalem)) %>% 
    filter(!is.na(Jerusalem)) %>% 
    mutate(across(-c(disease_type, disease_type_heb), as.numeric)) %>% 
    select(-17, -18, -19) %>% 
    mutate(week_num = week_num)
  
  file_data
}

# demonstrate reading of current year:
year2022 <- map_df(1:23, 
                   ~{
                     read_and_tidy(.x, 2022)
                   })

