library(tidyverse)

happiness <- readxl::read_excel("data/DataForTable2.1WHR2023.xls") %>% 
  janitor::clean_names()

glimpse(happiness)


# Distribution of generosity per year ------------------------------------

happiness %>% 
  filter(year >= 2019) %>% 
  ggplot(aes(generosity)) +
  geom_histogram() + 
  facet_wrap(~year)

# Changes per year

happiness %>% 
  ggplot(aes(generosity, x = factor(year))) + 
  geom_boxplot()

# Top five generous countries per year ------------------------------------

happiness %>% 
  filter(year >= 2006) %>% 
  arrange(year, desc(generosity)) %>% 
  group_by(year) %>% 
  slice_head(n = 5) %>%
  ungroup() %>% 
  count(country_name, sort = T)

top5_2022 <- happiness %>% 
  filter(year == 2022) %>% 
  arrange(year, desc(generosity)) %>%
  slice_head(n = 5)

bottom5_2022 <- happiness %>% 
  filter(year == 2022) %>% 
  filter(!is.na(log_gdp_per_capita)) %>% 
  arrange(year, desc(generosity)) %>%
  slice_tail(n = 5)

# Log gdp chart -----------------------------------------------------------

happiness %>% 
  filter(year == 2022) %>% 
  filter(!is.na(log_gdp_per_capita)) %>% 
  mutate(country_name = fct_reorder(country_name, log_gdp_per_capita)) %>% 
  mutate(extreme5 = case_when(country_name %in% top5_2022$country_name ~ "top5",
                              country_name %in% bottom5_2022$country_name ~ "bottom5",
                              T ~ 'Somewhere')) %>% 
  ggplot(aes(x = country_name, y = log_gdp_per_capita,
             fill = extreme5)) +
  geom_col() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


# Correlograma ------------------------------------------------------------

measure_correlations <- happiness %>% 
  select(where(is.numeric)) %>% 
  select(-year) %>% 
  cor(use = "pairwise.complete.obs")

ggcorrplot::ggcorrplot(measure_correlations, 
                       show.diag = FALSE, 
                       method = "circle",
                       lab = TRUE)


# GDP Change in various countries --------------------------------------

gdp_lagged_changes <- happiness %>% 
  group_by(country_name) %>% 
  mutate(lgdp2 = log_gdp_per_capita - lag(log_gdp_per_capita)) %>% 
  mutate(lgdp3 = log_gdp_per_capita - lag(log_gdp_per_capita, n = 2)) %>% 
  mutate(lgdp4 = log_gdp_per_capita - lag(log_gdp_per_capita, n = 3)) %>% 
  ungroup()

lm(formula = life_ladder ~ .,
   data = gdp_lagged_changes %>%
     select(life_ladder, starts_with("lgdp"))) %>% 
  summary()


# Where is Israel? --------------------------------------------------------

# This is not sorted
happiness %>% 
  filter(year == 2022) %>% 
  select(-year) %>% 
  pivot_longer(cols = -country_name) %>% 
  mutate(is_israel = country_name == "Israel") %>% 
  ggplot(aes(x = country_name, y = value, fill = is_israel)) + 
  geom_col() + 
  theme(axis.text.x = element_blank()) + #element_text(angle = 90, hjust = 1, vjust = 0.5, size = 5)) + 
  scale_fill_manual(values = c("Grey", "Blue")) + 
  facet_wrap(~name, scales = "free_y")

# A tip and trick to sort this:
# Nadav says this is the "Chart of the year!" :)
happiness %>% 
  filter(year == 2022) %>% 
  select(-year) %>% 
  pivot_longer(cols = -country_name) %>% 
  mutate(highlighted_countries = 
           if_else(country_name %in% c("Israel", "Mexico", "Japan", "United States", "United Kingdom", "United Arab Emirates", "Greece"),
                   country_name,
                   "Other countries")) %>%
  mutate(set_alpha = if_else(highlighted_countries == "Other countries",
                             "not",
                             "highlight")) %>% 
  arrange(desc(value), name) %>% 
  group_by(name) %>% 
  mutate(country_position = seq_along(value)) %>% 
  ggplot(aes(x = factor(country_position), y = value, fill = highlighted_countries, alpha = set_alpha)) +
  geom_col() + 
  theme(axis.text.x = element_blank()) +
  facet_wrap(~name, scales = "free_y") + 
  scale_alpha_manual(values = c("not" = 0.2, "highlight" = 1))
