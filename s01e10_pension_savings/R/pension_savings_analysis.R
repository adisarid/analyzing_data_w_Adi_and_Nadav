library(tidyverse)

# The 2018 file is at a slightly different format, hence reading it manually.
# It is also a bit messed up so omitting certain rows

gemel2018 <- read_csv("data/gemel_2018.csv", skip = 1, guess_max = 10) %>% 
  filter(!is.na(FUND_ID)) %>% 
  select(-1)
  
# Now reading all the rest and adding the 2018 file
all_gemel_data_raw <- map_df(dir("data")[-3], 
                         ~{
                           read_csv(paste0("data/", .x), 
                                    col_types = cols(), 
                                    locale = locale(encoding = "windows-1255"))
                         }) %>% 
  bind_rows(gemel2018) %>% 
  arrange(FUND_NAME, REPORT_PERIOD)



altshuler_stocks <- all_gemel_data_raw %>% 
  filter(FUND_NAME == 
             "אלטשולר שחם גמל מניות") %>% 
  select(REPORT_PERIOD, MONTHLY_YIELD) %>% 
  mutate(year = floor(REPORT_PERIOD/100),
         month = (REPORT_PERIOD/100 - year)*100) %>% 
  mutate(date = lubridate::ymd(glue::glue("{year}-{str_pad(round(month), width = 2, side = 'left', pad = '0')}-01"))) %>% 
  mutate(cumulative_value = cumprod(MONTHLY_YIELD/100 + 1))

altshuler_stocks_monthly <- altshuler_stocks %>% 
  ggplot(aes(x = date, y = MONTHLY_YIELD)) + 
  geom_col() + 
  ggtitle("Monthly yield אלטשולר שחם גמל מניות") + 
  saridr::theme_sarid()

altshuler_stocks_cumulative <- altshuler_stocks %>% 
  ggplot(aes(x = date, y = cumulative_value)) + 
  geom_line() + 
  ggtitle("Total savings אלטשולר שחם גמל מניות") + 
  saridr::theme_sarid()

library(patchwork)
altshuler_stocks_cumulative / altshuler_stocks_monthly
  
