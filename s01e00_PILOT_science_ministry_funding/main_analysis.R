library(tidyverse)

research <- readxl::read_excel("data/-2012-2021-.-xlsx.xlsx")

glimpse(research)

research %>% 
  count(`מטבע`)


# Money vs. year ----------------------------------------------------------

research %>% 
  count(`התכנית`, sort = T) %>% 
  slice(1:20) %>% 
  mutate(program = fct_inorder(`התכנית`)) %>% 
  ggplot(aes(y = program, x = n)) + 
  geom_col()


research_by_institution <- research %>% 
  mutate(program = `התכנית`,
         budget_year = `שנה תקציבית`,
         budget = parse_number(`תקציב ההסכם`),
         institution = `מוסד`) %>% 
  mutate(institution = if_else(str_detect(institution, "הטכניון"),
                               "הטכניון - מכון טכנולוגי לישראל",
                               institution)) %>% 
  group_by(budget_year, institution) %>% 
  summarize(total_budget = sum(budget, na.rm = T))
  
top_funded_institutions <- research_by_institution %>% 
  group_by(institution) %>% 
  summarize(total_budget = sum(total_budget)) %>% 
  arrange(desc(total_budget)) %>% 
  slice(c(1:7, 9))
  

research_by_institution %>% 
  filter(institution %in% top_funded_institutions$institution) %>% 
  ggplot(aes(x = budget_year, y = total_budget/(1e6))) + 
  geom_col() + 
  facet_wrap(~institution)

# Rising stars?

research %>% 
  mutate(program = `התכנית`,
         budget_year = `שנה תקציבית`,
         budget = parse_number(`תקציב ההסכם`),
         institution = `מוסד`) %>% 
  mutate(year_fct = if_else(budget_year >= 2019, "yr19_21",
                            "yr12_18")) %>% 
  group_by(institution, year_fct) %>% 
  summarize(total_budget = sum(budget, na.rm = T)) %>% 
  pivot_wider(id_cols = institution, names_from = year_fct, values_from = total_budget,
              values_fill = list(total_budget = 0)) %>% 
  mutate(diff = (yr19_21/3)/(yr12_18/7)) %>% 
  filter(yr12_18 != 0, yr19_21 != 0) %>% 
  ungroup() %>% 
  mutate(institution = fct_reorder(institution, diff)) %>% 
  ggplot(aes(y = institution, x = diff)) + 
  geom_col() + 
  geom_vline(xintercept = 1)


# text analysis -----------------------------------------------------------

research %>% 
  mutate(subject = `נושא`,
         id = `מספר סידורי`) %>% 
  select(subject, id) %>% 
  tidytext::unnest_tokens(word, subject) %>% 
  count(word, sort = T) %>% View

# Gender per year ---------------------------------------------------------

research %>% 
  mutate(program = `התכנית`,
         budget_year = `שנה תקציבית`,
         budget = parse_number(`תקציב ההסכם`),
         gender = `מגדר`) %>% 
  group_by(budget_year, gender) %>% 
  summarize(total_budget = sum(budget, na.rm = T)) %>% 
  ggplot(aes(x = budget_year, y = total_budget, fill = gender)) + 
  geom_col(position = position_fill())
