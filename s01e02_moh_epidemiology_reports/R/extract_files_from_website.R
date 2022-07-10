# File extraction script

# The data is available here:
# https://www.gov.il/he/Departments/DynamicCollectors/weekly-epidemiological-report?skip=0

# The data can be programmatically downloaded using a template formation and a loop, i.e.:
# Here we demonstrate downloading reports for weeks 15-23.

library(tidyverse)

# This template has changed a bit over the years (e.g., xls -> xlsx) but other changes as well.
template <- "https://www.gov.il/BlobFolder/dynamiccollectorresultitem/iwer-{week_number}-{year}/he/files_weekly-epidemiology_{year}_IWER_{week_number}_{year}.xlsx"
year <- 2022

for (week_number in 20:22){
  file_location <- glue::glue(template)
  httr::GET(file_location,
            httr::write_disk(glue::glue("data/files_weekly-epidemiology_{year}_IWER_{week_number}_{year}.xlsx"), overwrite = TRUE))
  
  # we have to wait a bit to avoid banning
  Sys.sleep(0.3)
}
