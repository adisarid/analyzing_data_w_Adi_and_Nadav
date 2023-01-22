# Scraping live demonstration

library(tidyverse)
library(rvest)
library(glue)

extract_page <- function(post_number) {
  
  page <- read_html(glue("https://irrelevant.org.il/?p={post_number}"))
  
  entry_title <- page %>% 
    html_elements(".entry-title") %>% 
    html_text2()
  
  original_text <- page %>% 
    html_elements(".entry") %>% 
    html_text()
  
  origin_start_location <- str_locate_all(original_text, "המקור:")
  
  explain_text <- str_sub(original_text, start = 10, end = origin_start_location[[1]][1])
  source_text <- str_sub(original_text, start = origin_start_location[[1]][2] + 1)
  
  tibble(post_number = post_number,
         entry_title = entry_title,
         explain_text = explain_text,
         source_text = source_text)
}

results <- extract_page(99)

safe_extraction <- safely(extract_page,
                          otherwise = NA)

possible_extraction <- possibly(extract_page)

for (post_num in c(300, 3562, 100)){
  
  post_content <- safe_extraction(post_num)
  
  if (is.null(post_content$error)){
    results <- bind_rows(results,
                         post_content$result)
  }
}

extracted_data <- map(c(90:100, 300),
                      possible_extraction, 
                      .progress = TRUE)
extracted_tibble <- extracted_data %>% 
  list_rbind() %>% 
  mutate(recommendation = case_when(
    str_detect(entry_title, "לא להעביר") ~ "avoid",
    str_detect(entry_title, "לכאן ולכאן") ~ "neutral",
    str_detect(entry_title, "תרמית") ~ "fraud",
    T ~ "true"))

write_csv(extracted_tibble, "data/example_extracted_tibble.csv")


# Premade file example ----------------------------------------------------

irrelevant_data <- read_csv("data/extracted_irrelevant_data.csv") %>% 
  mutate(recommendation = case_when(
    str_detect(title, "לא להעביר") ~ "avoid",
    str_detect(title, "לכאן ולכאן") ~ "neutral",
    str_detect(title, "ללא המלצה") ~ "neutral",
    str_detect(title, "תרמית") ~ "fraud",
    str_detect(title, "אפשר להעביר") ~ "true"))

ggplot(irrelevant_data, aes(recommendation)) + 
  geom_bar()

irrelevant_data %>% 
  filter(recommendation == "fraud") %>% 
  pull(article)


# Comparison of msg length by recommendation ------------------------------

irrelevant_data %>% 
  filter(!is.na(recommendation)) %>% 
  filter(recommendation %in% c("avoid", "neutral", "true")) %>% 
  mutate(msg_length = str_length(original_text)) %>% 
  ggplot(aes(x = msg_length, color = recommendation)) + 
  geom_density() + 
  scale_x_log10()
