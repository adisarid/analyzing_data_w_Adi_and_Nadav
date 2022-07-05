base_url <- "https://www.gov.il/BlobFolder/dynamiccollectorresultitem/"

# file_end <- "iwer-5-2001/he/files_weekly-epidemiology_2001_IWER05_2001.xls"
# https://www.gov.il/BlobFolder/dynamiccollectorresultitem/iwer-50-2020/he/files_weekly-epidemiology_2020_IWER_50_2020.xlsx

library(tidyverse)
library(polite)
library(rvest)

# WARNING: This results in getting banned pretty quickly...
# consider switching to `polite`. For now, adding Sys.sleep.
for (j in 2014){
  for (i in 1:52){
    
    if (j >= 2015 & j < 2018) {
      file_name <- glue::glue("files_weekly-epidemiology_{j}_IWER{str_pad(i, width = 2, pad = '0')}_{j}.xlsx")
      full_path <- glue::glue("{base_url}iwer-{i}-{j}/he/{file_name} ")
    }
    else if (j < 2014){
      file_name <- glue::glue("files_weekly-epidemiology_{j}_IWER{i}_{j}.xls")
      full_path <- glue::glue("{base_url}iwer-{i}-{j}/he/{file_name} ")
    } else if (j == 2018) {
      file_name <- glue::glue("files_weekly-epidemiology_{j}_IWER{i}_{j}.xlsx")
      full_path <- glue::glue("{base_url}iwer-{str_pad(i, width = 2, pad = '0')}-{j}/he/{file_name} ")
    } else if (j == 2019){
      file_name <- glue::glue("files_weekly-epidemiology_{j}_IWER{i}_{j}.xlsx")
      full_path <- glue::glue("{base_url}iwer-{i}-{j}/he/{file_name} ")
    } else if (j == 2020) {
      file_name <- glue::glue("files_weekly-epidemiology_{j}_IWER_{str_pad(i, width = 2, pad = '0')}_{j}.xlsx")
      full_path <- glue::glue("{base_url}iwer-{i}-{j}/he/{file_name} ")
    } else if (j >= 2021) {
      file_name <- glue::glue("files_weekly-epidemiology_{j}_IWER_{str_pad(i, width = 2, pad = '0')}_{j}.xlsx")
      full_path <- glue::glue("{base_url}iwer-{str_pad(i, width = 2, pad = '0')}-{j}/he/{file_name} ")
    } else if (j == 2014) {
      file_name <- glue::glue("files_weekly-epidemiology_{j}_IWER_{str_pad(i, width = 2, pad = '0')}_{j}.xlsx")
      full_path <- glue::glue("{base_url}iwer-{i}-{j}/he/{file_name} ")
    } else {
      file_name <- glue::glue("files_weekly-epidemiology_{j}_IWER_{str_pad(i, width = 2, pad = '0')}_{j}.xlsx")
      full_path <- glue::glue("{base_url}iwer-{i}-{j}/he/{file_name} ")
    }
    
    cat(glue::glue("\nChecking {file_name}: "))
    
    if (!(file_name %in% dir("c:/temp/weekly_moh_epi_reports/"))){
      
      file1 <- httr::GET(full_path)
      file_content <- httr::content(file1)
      
      tryCatch(
        expr = {
          writeBin(file_content, glue::glue("c:/temp/weekly_moh_epi_reports/{file_name}"))
          print("Success.")
        },
        error = function(e){cat(glue::glue("Error-{i}-{j}\n"))}
      )
      
      Sys.sleep(2)
      
    }
  }
}

