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
  mutate(cumulative_value = cumprod(MONTHLY_YIELD/100 + 1) - 1)

altshuler_stocks_monthly <- altshuler_stocks %>% 
  ggplot(aes(x = date, y = MONTHLY_YIELD)) + 
  geom_col() + 
  ggtitle("Monthly yield אלטשולר שחם גמל מניות")

altshuler_stocks_cumulative <- altshuler_stocks %>% 
  ggplot(aes(x = date, y = cumulative_value)) + 
  geom_line() + 
  ggtitle("Total savings אלטשולר שחם גמל מניות") + 
  scale_y_continuous(labels = scales::percent)

library(patchwork)
altshuler_stocks_cumulative / altshuler_stocks_monthly
  

# Past 5 vs. next 5 -------------------------------------------------------

all_funds_yield <- all_gemel_data_raw %>% 
  group_by(FUND_NAME) %>% 
  select(FUND_NAME, REPORT_PERIOD, MONTHLY_YIELD) %>% 
  mutate(year = floor(REPORT_PERIOD/100),
         month = (REPORT_PERIOD/100 - year)*100) %>% 
  mutate(date = lubridate::ymd(glue::glue("{year}-{str_pad(round(month), width = 2, side = 'left', pad = '0')}-01"))) %>% 
  filter(date >= "2012-01-01") %>% 
  mutate(cumulative_value = cumprod(MONTHLY_YIELD/100 + 1) - 1) %>% 
  select(FUND_NAME, date, cumulative_value) %>% 
  filter(!is.na(cumulative_value))

funds_with_2012 <- all_funds_yield %>% 
  filter(date == "2012-01-01") %>% 
  distinct(FUND_NAME)

funds_with_2022 <- all_funds_yield %>% 
  filter(date == "2022-10-01") %>% 
  distinct(FUND_NAME)

funds_with_longitudal_date <- intersect(funds_with_2012$FUND_NAME,
                                        funds_with_2022$FUND_NAME)


top_funds_17 <- all_funds_yield %>% 
  filter(FUND_NAME %in% funds_with_longitudal_date) %>% 
  filter(date == "2017-01-01") %>% 
  arrange(desc(cumulative_value)) %>% 
  ungroup() %>% 
  mutate(rank = seq_along(date))

# Compute yield from 2017 to 2022
all_funds_yield_from2017 <- all_gemel_data_raw %>% 
  group_by(FUND_NAME) %>% 
  select(FUND_NAME, REPORT_PERIOD, MONTHLY_YIELD) %>% 
  mutate(year = floor(REPORT_PERIOD/100),
         month = (REPORT_PERIOD/100 - year)*100) %>% 
  mutate(date = lubridate::ymd(glue::glue("{year}-{str_pad(round(month), width = 2, side = 'left', pad = '0')}-01"))) %>% 
  filter(date >= "2017-01-01") %>% 
  mutate(cumulative_value = cumprod(MONTHLY_YIELD/100 + 1) - 1) %>% 
  select(FUND_NAME, date, cumulative_value) %>% 
  filter(!is.na(cumulative_value)) %>% 
  filter(FUND_NAME %in% funds_with_longitudal_date)

top_funds_17to22 <- all_funds_yield_from2017 %>% 
  filter(date == "2022-10-01") %>% 
  arrange(desc(cumulative_value)) %>% 
  ungroup() %>% 
  mutate(rank = seq_along(date))

# Chart yield pre and post 17 ---------------------------------------------

unified_funds_yield <- top_funds_17 %>% 
  rename(yield_upto17 = cumulative_value,
         rank_upto17 = rank) %>% 
  select(-date) %>% 
  left_join(top_funds_17to22 %>% 
              rename(yield_from17 = cumulative_value,
                     rank_from17 = rank) %>% 
              select(-date)) %>% 
  filter(rank_upto17 <= 5)

unified_funds_yield %>% 
  select(FUND_NAME, contains("yield")) %>% 
  pivot_longer(cols = contains("yield"), names_to = "sample_time", values_to = "yield") %>% 
  mutate(sample_time = factor(sample_time, levels = c("yield_from17",
                                                      "yield_upto17"))) %>% 
  mutate(sample_time = if_else(sample_time == "yield_upto17",
                               0, 1)) %>% 
  left_join(unified_funds_yield %>% 
              select(FUND_NAME, rank_upto17, rank_from17)) %>% 
  mutate(label = if_else(sample_time == 0, rank_upto17,
                         rank_from17)) %>% 
  filter(!str_detect(FUND_NAME, "לאומי קופה")) %>% 
  mutate(small_nudge = if_else(sample_time == 0, -0.05, 0.05)) %>% 
  mutate(FUND_NAME = paste0("\u202b", str_sub(FUND_NAME, end = 24), "...")) %>% 
  ggplot(aes(x = sample_time, y = yield, color = FUND_NAME, label = label)) + 
  geom_point() + 
  geom_line() + 
  geom_text(show.legend = FALSE, size = 4, aes(x = sample_time + small_nudge),
            check_overlap = T) +
  scale_x_continuous(breaks = c(0, 1), 
                     labels = c("12-17", "17-22")) + 
  theme(legend.position = "right") + 
  scale_y_continuous(labels = scales::percent) + 
  ylab("תשואה") + 
  xlab("נקודת זמן") + 
  theme_light() + 
  guides(color = guide_legend("שם הקופה")) + 
  ggtitle(
    "ביצועי קופות גמל נבחרות בחמש השנים האחרונות",
    subtitle = "\u202bאיפה נמצאות היום (2022) חמש קופות הגמל שהובילו בתשואות של 2012-2017?"
    ) + 
  labs(caption =  "\u202bאם בחרתם קופת גמל בשנת 2017 לפי ביצועי 5 שנים, איפה תהיו היום?\n\u202bמבוסס על נתוני גמלנט https://data.gov.il/dataset/gemelnet\n\n\u202bעדי ונדב מנתחים נתונים https://www.youtube.com/@adinadavdata") +
  theme(legend.position = "right", 
        plot.title = element_text(hjust = 1),
        plot.subtitle = element_text(hjust = 1), 
        legend.text.align = 0, legend.title.align = 0.2)



# Playing with the data and distribution for all funds --------------------

unified_funds_yield_all <- top_funds_17 %>% 
  rename(yield_upto17 = cumulative_value,
         rank_upto17 = rank) %>% 
  select(-date) %>% 
  left_join(top_funds_17to22 %>% 
              rename(yield_from17 = cumulative_value,
                     rank_from17 = rank) %>% 
              select(-date))

unified_funds_yield_all %>% 
  filter(rank_upto17 <= 10) %>% 
  mutate(is_still_top10 = rank_from17 <= 10) %>%
  mutate(movement = rank_upto17 - rank_from17) %>% 
  summarize(mean(movement),
            median(movement))
