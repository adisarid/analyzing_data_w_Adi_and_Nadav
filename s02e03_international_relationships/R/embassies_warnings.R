library(tidyverse)


# Recognition and establishment? ----------------------------------------------

embassies <- readxl::read_excel("data/embassies_ministry_of_foreign_affairs.xlsx")

embassies %>% 
  count(`סטטוס_יחסים`)

# By year
embassies %>% 
  mutate(init_year = lubridate::year(`תאריך_כינון_יחסים`)) %>% 
  count(init_year) %>% 
  ggplot(aes(x = init_year, y = n)) +
  geom_col() + 
  theme_bw() + 
  ggtitle("Dates of international relationship estblishments with Israel") + 
  xlab("Year") + 
  ylab("Number of Countries") + 
  scale_x_continuous(breaks = seq(from = 1950,
                                  to = 2020,
                                  by = 10))
  
# By decade
embassies %>% 
  mutate(init_year = lubridate::year(`תאריך_כינון_יחסים`)) %>% 
  mutate(init_decade = floor((init_year - 1900)/10) * 10) %>% 
  count(init_decade) %>% 
  ggplot(aes(x = init_decade, y = n)) +
  geom_col() + 
  theme_bw() + 
  ggtitle("Decades of relationship estblishment with Israel") + 
  xlab("Decade") + 
  ylab("Number of Countries") + 
  scale_x_continuous(breaks = seq(40, 120, by = 10), 
                     labels = c(paste0(seq(40, 90, by = 10), "'"),
                                "00'", "10'", "20'"))


# Which coutries started relationships during the 90s
embassies %>% 
  mutate(init_year = lubridate::year(`תאריך_כינון_יחסים`)) %>% 
  mutate(init_decade = floor((init_year - 1900)/10) * 10) %>% 
  filter(init_decade == 90) %>% 
  View()


# Definition of countries as difficult - by status
embassies %>% 
  count(`סטטוס_יחסים`, 
        `מדינה_קשת_שרות`) %>%
  na.omit() %>% 
  ggplot(aes(x = `סטטוס_יחסים`,
             y = n,
             fill = `מדינה_קשת_שרות`)) + 
  geom_col(position = position_fill()) + 
  ggtitle("Distribution of service difficulty by country relationship") + 
  xlab("סטטוס יחסים") + 
  geom_label(aes(label = n), position = position_fill(), show.legend = FALSE)


# Definition of countries as difficult - by continent
embassies %>% 
  count(`יבשת`, 
        `מדינה_קשת_שרות`) %>%
  na.omit() %>% 
  ggplot(aes(x = `יבשת`,
             y = n,
             fill = `מדינה_קשת_שרות`)) + 
  geom_col(position = position_fill()) + 
  ggtitle("Distribution of service difficulty by continent") + 
  xlab("סטטוס יחסים") + 
  geom_label(aes(label = n), position = position_fill(), show.legend = FALSE)


# Some more in depth on countries

embassies %>% 
  filter(`יבשת` == 
           "אמריקה") %>% 
  arrange(`מדינה_קשת_שרות`) %>% 
  select(`מדינה_קשת_שרות`, everything()) %>% 
  View()


# Joining the data of warnings --------------------------------------------

travel_warnings <- readxl::read_excel("data/travel-warnings.xlsx") %>% 
  filter(`משרד` ==  
           'מל"ל') %>% 
  mutate(danger_level_hi = case_when(str_detect(recommendations, "רמה 4") ~ 4,
                                     str_detect(recommendations, "רמה 3") ~ 3,
                                     str_detect(recommendations, "רמה 2") ~ 2,
                                     T ~ 1)) %>% 
  mutate(danger_level_low = case_when(str_detect(recommendations,"רמה 2") ~ 2,
                                      str_detect(recommendations, "רמה 3") ~ 3,
                                      str_detect(recommendations, "רמה 4") ~ 4,
                                      T ~ 1))
  
travel_warn_cln <- travel_warnings %>% 
  select(country, starts_with("danger_level"))

warn_embassy <- travel_warn_cln %>% 
  full_join(embassies,
            by = c("country" = 
                     "שם_מדינה_במאגר")) %>% 
  select(country, contains("danger_level"), contains("שרות"))


warn_embassy %>% 
  count(`מדינה_קשת_שרות`,
        danger_level_low) %>% 
  na.omit() %>% 
  ggplot(aes(x = danger_level_low, 
             y = n)) + 
  geom_col() + 
  facet_wrap(~`מדינה_קשת_שרות`) + 
  ggtitle("Comparison of warnings between service difficulties") + 
  theme_bw() + 
  xlab("Danger level (min)") + 
  ylab("Number of countries")


low_danger_level_plot <- warn_embassy %>% 
  count(`מדינה_קשת_שרות`,
        danger_level_low) %>% 
  na.omit() %>% 
  ggplot(aes(x = `מדינה_קשת_שרות`, 
             y = n,
             fill = factor(danger_level_low))) + 
  geom_col(position = position_fill()) + 
  ggtitle("Comparison of warnings between service difficulties (level min)") + 
  theme_bw() + 
  xlab("Service difficulty") + 
  ylab("Distribution") + 
  scale_fill_brewer(palette = "RdYlGn", direction = -1) + 
  scale_y_continuous(labels = scales::percent) + 
  guides(fill = guide_legend("Danger level")) + 
  theme(legend.justification = "top")
  

hi_danger_level_plot <- warn_embassy %>% 
  count(`מדינה_קשת_שרות`,
        danger_level_hi) %>% 
  na.omit() %>% 
  ggplot(aes(x = `מדינה_קשת_שרות`, 
             y = n,
             fill = factor(danger_level_hi))) + 
  geom_col(position = position_fill()) + 
  ggtitle("Comparison of warnings between service difficulties (level max)") + 
  theme_bw() + 
  xlab("Service difficulty") + 
  ylab("Distribution") + 
  scale_fill_brewer(palette = "RdYlGn", direction = -1) + 
  scale_y_continuous(labels = scales::percent) + 
  guides(fill = guide_legend("Danger level")) + 
  theme(legend.justification = "top")


library(patchwork)
low_danger_level_plot / hi_danger_level_plot

