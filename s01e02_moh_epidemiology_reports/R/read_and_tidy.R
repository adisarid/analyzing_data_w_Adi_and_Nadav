# Read files from data

library(tidyverse)

read_and_tidy <- function(week_num, year, directory_listing = dir("data")){
  
  # file name might be one of a few options, check which is it:
  file_name_opt1 <- glue::glue("data/files_weekly-epidemiology_{year}_IWER_{stringr::str_pad(week_num, width = 2, pad = '0')}_{year}.xlsx")
  file_name_opt2 <- glue::glue("data/files_weekly-epidemiology_{year}_IWER_{week_num}_{year}.xlsx")
  file_name_opt3 <- glue::glue("data/files_weekly-epidemiology_{year}_IWER_{week_num}_{year}.xls")
  file_name_opt4 <- glue::glue("data/files_weekly-epidemiology_{year}_IWER_{stringr::str_pad(week_num, width = 2, pad = '0')}_{year}.xls")
  file_name_opt5 <- glue::glue("data/files_weekly-epidemiology_{year}_IWER{stringr::str_pad(week_num, width = 2, pad = '0')}_{year}.xlsx")
  file_name_opt6 <- glue::glue("data/files_weekly-epidemiology_{year}_IWER{week_num}_{year}.xlsx")
  file_name_opt7 <- glue::glue("data/files_weekly-epidemiology_{year}_IWER{week_num}_{year}.xls")
  file_name_opt8 <- glue::glue("data/files_weekly-epidemiology_{year}_IWER{stringr::str_pad(week_num, width = 2, pad = '0')}_{year}.xls")
  
  
  
  if (file_name_opt1 %in% directory_listing){
    file_name <- file_name_opt1
  } else if (file_name_opt2 %in% directory_listing){
    file_name <- file_name_opt2
  } else if (file_name_opt3 %in% directory_listing){
    file_name <- file_name_opt3
  } else if (file_name_opt4 %in% directory_listing){
    file_name <- file_name_opt4
  } else if (file_name_opt5 %in% directory_listing){
    file_name <- file_name_opt5
  } else if (file_name_opt6 %in% directory_listing){
    file_name <- file_name_opt6
  } else if (file_name_opt7 %in% directory_listing){
    file_name <- file_name_opt7
  } else if (file_name_opt8 %in% directory_listing){
    file_name <- file_name_opt8
  } else {
    stop(glue::glue("Could not find file {year}-{week_num} in directory_listing"))
  }
  
  file_data <- readxl::read_excel(
    file_name,
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

# demonstrate reading of current year:
loop_base <- crossing(week_num = 1:52, year = 2015:2021) %>% 
  filter(year <= 2021 | 
           year == 2022 & week_num <= 23)

epi_data_merged <- map2_df(loop_base$week_num, loop_base$year,
                           ~{
                             read_and_tidy(.x, .y)
                           })
