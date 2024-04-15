library(tidyverse)

fixed_headers <- crossing(profession = 
           fct_inorder(c("realestate",
             "socialworkers",
             "law",
             "accounting",
             "engineering",
             "paramedical",
             "psyc",
             "pharma",
             "nursing",
             "md")),
         sub = c("female", "male", "total")) %>% 
  mutate(header_name = glue::glue("{profession}_{sub}")) %>% 
  pull(header_name) %>% 
  c(., c("muni_num", "muni_name"))

occ_init <- readxl::read_excel("data/בעלי רישיונות עיסוק בישובים ומועצות אזוריות 2022.xlsx",
                          range = "A8:AF265", col_names = fixed_headers) %>% 
  filter(!is.na(muni_num)) %>% 
  mutate(across(realestate_female:md_total, as.numeric)) %>% 
  mutate(across(realestate_female:md_total, ~if_else(is.na(.), 0, .)))

occ_long <- occ_init %>% 
  pivot_longer(cols = realestate_female:md_total, names_to = "profession", 
               values_to = "licensed_n") %>% 
  separate(profession, into = c("profession", "gender"), sep = "_")

# Distribution - absolute numbers
occ_long %>% 
  filter(gender != "total") %>% 
  group_by(profession, gender) %>% 
  summarize(licensed_n = sum(licensed_n)) %>% 
  ggplot(aes(fill = gender, x = profession, y = licensed_n)) + 
  geom_col() + 
  ggtitle("Distribution of genders per profession") + 
  theme_bw()

# Distribution - percentage
occ_long %>% 
  filter(gender != "total") %>% 
  group_by(profession, gender) %>% 
  summarize(licensed_n = sum(licensed_n)) %>% 
  group_by(profession) %>% 
  mutate(percent = licensed_n / sum(licensed_n)) %>% 
  arrange(desc(gender), percent) %>% 
  ungroup() %>% 
  mutate(profession = fct_inorder(profession)) %>% 
  ggplot(aes(fill = gender, x = profession, y = licensed_n)) + 
  geom_col(position = position_fill()) + 
  ggtitle("Distribution of genders per profession") + 
  theme_bw() + 
  scale_y_continuous(labels = scales::percent) + 
  ylab("Gender distribution [%]") + 
  xlab("Profession") + 
  theme(legend.justification = "top")


# Distribution - decluttering and simplifying
annotated_plot <- occ_long %>% 
  filter(gender != "total") %>% 
  mutate(profession = str_to_title(profession)) %>% 
  group_by(profession, gender) %>% 
  summarize(licensed_n = sum(licensed_n)) %>% 
  group_by(profession) %>% 
  mutate(percent = licensed_n / sum(licensed_n)) %>% 
  arrange(desc(gender), percent) %>% 
  ungroup() %>% 
  mutate(profession = fct_inorder(profession)) %>% 
  filter(gender == "male") %>% 
  ggplot(aes(x = profession, y = percent)) + 
  geom_col(fill = "royalblue") +
  ggtitle("Distribution of genders per profession",
          "Israeli occupational licensing are clearly gender-imbalanced") + 
  theme_bw() + 
  scale_y_continuous(labels = scales::percent) + 
  ylab("Gender distribution [%]") + 
  xlab("Profession") + 
  theme(legend.justification = "top", 
        axis.text.x = element_text(size = 12)) + 
  annotate("text", 
           x = 9, y = 0.85, label = "Predominantly male ♂ professions\n(>60% males)") + 
  coord_cartesian(ylim = c(0, 1)) +
  annotate("text", 
           x = 3, y = 0.5, label = "Predominantly female ♀ professions\n(<40% males)") + 
  annotate("text", 
           x = 6.5, y = 0.7, label = "Gender-balanced ⚥ professions\n(~50% males)")

ggsave(filename = "plot/annotated_plot_gender.png",
       width = 30,
       height = 15,
       units = "cm",
       plot = annotated_plot)

