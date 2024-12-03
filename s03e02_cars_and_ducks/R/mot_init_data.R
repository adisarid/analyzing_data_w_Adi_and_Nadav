# Script for loading the csv into duckdb
library(tidyverse)
library(duckdb)
library(duckplyr)


# Check structure
# file1 <- read_delim("data/053cea08-09bc-40ec-8f7a-156f0677aff3.csv", 
# delim = "|", 
# locale = locale(encoding = "windows-1255"), 
# quote = "") # there is an issue with the גפ"מ, hence not accepting quotes.
  

# file1_processed <- file1 |> 
#   mutate(across(where(is.character), ~str_remove_all(., '\"'))) # fix redundant quotes

# file1_factorized <- file1_processed |> 
#   mutate(
#     across(
#       c(sug_degem, ramat_gimur,
#         degem_manoa, baalut, tzeva_rechev, sug_delek_nm,kinuy_mishari), 
#       as_factor)) |> 
#   mutate(
#     across(
#       c(
#         mispar_rechev, 
#         shnat_yitzur),
#       as.numeric)) |> 
#   mutate(
#     across(
#       c(mivchan_acharon_dt, tokef_dt),
#       lubridate::ymd
#     )
#   ) |> 
#   mutate(moed_aliya_lakvish = lubridate::ym(moed_aliya_lakvish))

# file2 <- read_delim("data/0866573c-40cd-4ca8-91d2-9dd2d7a492e5.csv", 
# n_max = 100, 
# delim = "|", 
# locale = locale(encoding = "utf-8"))

# file1 seems to be the main file and file2 seems to be additions.
# sum(names(file1) %in% names(file2))
# sum(names(file2) %in% names(file1))

# Initialize the duckdb

# con <- dbConnect(duckdb())

# ## Not working
# ducks <- dbWriteTable(con, "mot_tbl", file1_factorized)

# dbExecute(con, "EXPORT DATABASE 'data/duckdb'")

# dbDisconnect(con)

# Read db from files:
con <- dbConnect(duckdb())
dbExecute(con, "IMPORT DATABASE 'data/duckdb'")
dbListTables(con)

fuel_type <- tbl(con, "mot_tbl") |> 
  count(sug_delek_nm) |> 
  collect()

# Fuel type as a function of moed aliha lakvish
fuel_type_date <- tbl(con, "mot_tbl") |> 
  mutate(moed_aliya_lakvish_year = lubridate::year(moed_aliya_lakvish)) |> 
  group_by(moed_aliya_lakvish_year) |> 
  count(sug_delek_nm) |> 
  mutate(prop = n/sum(n)) |> 
  filter(moed_aliya_lakvish_year >= 2010) |> 
  collect()

# Chart the data
fuel_type_date |> 
  mutate(sug_delek_nm = 
    recode_factor(
      sug_delek_nm,
      "בנזין" = "Benzin",
      "גפמ" = "LPG",
      "דיזל" = "Diesel",
      "חשמל" = "Fully Electric",
      "חשמל/בנזין" = "Hybrid",
      "חשמל/דיזל" = "Hybrid"
  )) |> 
  filter(sug_delek_nm != "Benzin") |> 
  filter(sug_delek_nm != "Diesel") |> 
  ggplot(aes(x = moed_aliya_lakvish_year, y = prop, 
    fill = sug_delek_nm)) + 
  geom_col() + 
  theme_bw() + 
  xlab("Year") + 
  ylab("Proportion of cars") + 
  scale_y_continuous(labels = scales::percent) +
  ggtitle(
    "Adoption of Electric Vehicles in Israel",
    subtitle = "Since 2010") + 
  guides(fill = guide_legend("Fuel type")) + 
  theme(legend.justification = "top")

# Top brands on the road:
brand_per_year <- tbl(con, "mot_tbl") |> 
  mutate(moed_aliya_lakvish_year = lubridate::year(moed_aliya_lakvish)) |> 
  group_by(moed_aliya_lakvish_year) |> 
  count(tozeret_nm) |> 
  filter(!is.na(moed_aliya_lakvish_year)) |> 
  collect() |> 
  separate(tozeret_nm, into = c("tozeret", "origin", "rm1", "rm2", "rm3", "rm4"), sep = " |-|_") |> 
    mutate(new_tozeret = 
      case_when(tozeret == "דאבל" ~ "דאבל יו אם איי",
                tozeret == "די" ~ "די אס",
                tozeret == "אף" ~ "אף אי דאבל",
                tozeret == "בי" ~ "בי ווי די",
                tozeret == "ב" ~ "ב מ וו",
                tozeret == "אל" ~ "אל אי וי",
                tozeret == "קיי" ~ "קיי גי"
  )) |> 
    mutate(tozeret = coalesce(new_tozeret, tozeret)) |> 
  select(moed_aliya_lakvish_year, tozeret, n) |> 
  arrange(desc(n)) |> 
  rename(year = moed_aliya_lakvish_year) |> 
  ungroup() |> 
  mutate(tozeret = fct_inorder(tozeret))

library(gganimate)

brand_per_year |> 
  group_by(year) |> 
  slice(1:10) |> 
  filter(year == 2022) |> 
  ggplot(aes(x = n, y = tozeret)) + 
  geom_col()
