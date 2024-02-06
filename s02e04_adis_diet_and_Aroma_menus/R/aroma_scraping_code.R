library(rvest)


# Extract list of all Aroma items  ----------------------------------------

aroma_menu <- read_html("https://www.aroma.co.il/menus/")

menu_items <- aroma_menu |>
  html_elements(".img_block") |>
  html_elements("a") |> 
  html_text2()

menu_item_links <- aroma_menu |>
  html_elements(".img_block a") |>
  html_attr("href")

# Function to extract nutritional values ----------------------------------

nutritional_tbl <- function(item_link = "https://www.aroma.co.il/food/%d7%a7%d7%a4%d7%95%d7%a6%d7%99%d7%a0%d7%95/"){
  item_html <- read_html(item_link)
  
  item_nutrition_tbl <- item_html |>
    html_element("table") |>
    html_table()
}


# Loop over all products --------------------------------------------------

library(purrr)
nutritional_tbl_delayed <- slowly(nutritional_tbl, rate = rate_delay(pause = 0.2))

all_menu_nut <- map(menu_item_links, 
                    nutritional_tbl_delayed,
                    .progress = T)

all_menu_nut_fixed <- map(all_menu_nut,
                          ~{.x |>
                              mutate_all(as.character)})

library(tidyverse)
menu_with_names <- tibble(item = menu_items) %>% 
  mutate(nutrients = all_menu_nut_fixed) %>% 
  unnest(nutrients) %>% 
  select(item, `סימון תזונתי`, `מנה`) %>% 
  filter(!(`סימון תזונתי` %in% 
             c("מתוכן:",
               "מתוכם:"))) %>% 
  mutate(`מנה` = parse_number(`מנה`)) %>% 
  pivot_wider(id_cols = item, values_from = `מנה`, names_from = `סימון תזונתי`)

# write_csv(menu_with_names, "data/menu_with_names.csv")