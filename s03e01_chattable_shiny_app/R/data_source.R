aroma <- read_csv("data/aroma_menu.csv") %>% 
  left_join(read_csv("data/dish_dict.csv")) %>% 
  mutate(serial = seq_along(item)) %>% 
  mutate(dish_type = case_when(between(serial, 1, 19) ~ "Hot drinks",
                               between(serial, 20, 49) ~ "Baked",
                               between(serial, 50, 75) ~ "Sandwiches",
                               between(serial, 76, 80) ~ "Salads",
                               between(serial, 81, 87) ~ "Breakfast",
                               between(serial, 88, 92) ~ "Bowl",
                               between(serial, 93, 101) ~ "Toast",
                               between(serial, 102, 107) ~ "Soup",
                               between(serial, 108, 145) ~ "Cold drinks",
                               between(serial, 146, 149) ~ "Bread",
                               between(serial, 150, 154) ~ "Condiments"))
