library(tidyverse)

emissions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-05-21/emissions.csv')

glimpse(emissions)

emissions %>% 
  group_by(parent_type, year) %>% 
  # filter(parent_type != "Nation State") %>% 
  summarize(emission_MtCO2e = sum(total_emissions_MtCO2e)) %>% 
  ggplot(aes(x = year, y = emission_MtCO2e, fill = parent_type)) + 
  geom_area() + 
  theme_bw() + 
  theme(legend.justification = "top", legend.position = c(0.2, 0.95)) + 
  ggtitle("Total Carbon emissions 1854-2022",
          subtitle = "Emissions in Metric Ton CO2 equivalent") + 
  labs(caption = "Data from tidytuesday") + 
  guides(fill = guide_legend("Entity type")) + 
  xlab("Year") + 
  ylab("MtCO2e")
  

# Check about the identity of state owned vs. nation state
emissions %>% 
  distinct(parent_type, parent_entity) %>% 
  View()

# Trying to recreate the chart of Carbon Majors Home
recreated_plot <- emissions %>% 
  mutate(commodity_fct = if_else(str_detect(commodity, "Coal"),
                                 "Coal",
                                 commodity)) %>% 
  mutate(commodity_fct = factor(commodity_fct,
                                levels = 
                                  c("Cement",
                                    "Natural Gas", 
                                    "Oil & NGL",
                                    "Coal"
                                    ))) %>% 
  group_by(commodity_fct, year) %>% 
  summarize(emission_MtCO2e = sum(total_emissions_MtCO2e)) %>% 
  ggplot(aes(x = year, y = emission_MtCO2e, fill = commodity_fct)) + 
  geom_area() + 
  theme_bw() + 
  theme(legend.justification = "top", legend.position = c(0.2, 0.95)) + 
  ggtitle("Total Carbon emissions 1854-2022",
          subtitle = "Emissions in Metric Ton CO2 equivalent") + 
  labs(caption = "Data from tidytuesday") + 
  guides(fill = guide_legend("Commodity")) + 
  scale_fill_manual(values = c("Cement" = "#797778",
                               "Natural Gas" = "#3175B4",
                               "Coal" = "#262E31",
                               "Oil & NGL" = "#72361B")) +
  xlab("Year") + 
  ylab("MtCO2e")


# Distribution instead of absolute values ---------------------------------

emissions %>% 
  mutate(commodity_fct = if_else(str_detect(commodity, "Coal"),
                                 "Coal",
                                 commodity)) %>% 
  mutate(commodity_fct = factor(commodity_fct,
                                levels = 
                                  c("Cement",
                                    "Natural Gas", 
                                    "Oil & NGL",
                                    "Coal"
                                  ))) %>% 
  group_by(commodity_fct, year) %>% 
  summarize(emission_MtCO2e = sum(total_emissions_MtCO2e)) %>% 
  ggplot(aes(x = year, y = emission_MtCO2e, fill = commodity_fct)) + 
  geom_col(position = position_fill(), width = 1) + 
  theme_bw() + 
  theme(legend.justification = "top") + 
  ggtitle("Total Carbon emissions 1854-2022",
          subtitle = "Emissions distribution 1855-2022") + 
  labs(caption = "Data from tidytuesday") + 
  guides(fill = guide_legend("Commodity")) + 
  scale_fill_manual(values = c("Cement" = "#797778",
                               "Natural Gas" = "#3175B4",
                               "Coal" = "#262E31",
                               "Oil & NGL" = "#72361B")) +
  scale_y_continuous(labels = scales::percent_format(1)) +
  xlab("Year") + 
  ylab("Distribution")


# Overall emissions -------------------------------------------------------

total_for_plotly <- emissions %>% 
  group_by(year) %>% 
  summarize(emission_MtCO2e = sum(total_emissions_MtCO2e)) %>% 
  ggplot(aes(x = year, y = emission_MtCO2e)) + 
  geom_line(color = "#DE5A19") + 
  theme_bw() + 
  ggtitle("Total Carbon emissions 1854-2022",
          subtitle = "Emissions in Metric Ton CO2 equivalent") + 
  labs(caption = "Data from tidytuesday") + 
  xlab("Year") + 
  ylab("MtCO2e")

plotly::ggplotly(total_for_plotly)


# Analyzing the time series -----------------------------------------------

emissions_ts <- emissions %>% 
  group_by(year) %>% 
  summarize(emission_MtCO2e = sum(total_emissions_MtCO2e)) %>% 
  mutate(year_diff = emission_MtCO2e - lag(emission_MtCO2e)) %>% 
  mutate(moving_avg = forecast::ma(emission_MtCO2e, order = 10))

emissions_ts %>% 
  select(year, emission_MtCO2e, moving_avg) %>% 
  pivot_longer(-year) %>% 
  ggplot(aes(x = year, y = value, color = name)) + 
  geom_line() + 
  theme_bw()


# Trying to forecast in the ets framework ---------------------------------

emissions_model <- forecast::ets(ts(emissions_ts$emission_MtCO2e),
                                 opt.crit = "lik") 

forecast::forecast(emissions_model)

autoplot(emissions_model) + 
  theme_bw()
autoplot(forecast::forecast(emissions_model)) + 
  theme_bw()
