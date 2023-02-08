library(tidyverse)

irr <- read_csv("data/extracted_irrelevant_data.csv") %>% 
  mutate(recommendation = case_when(
    str_detect(title, "לא להעביר") ~ "avoid",
    str_detect(title, "לכאן ולכאן") ~ "neutral",
    str_detect(title, "ללא המלצה") ~ "neutral",
    str_detect(title, "\\(תרמית\\)") ~ "avoid",
    str_detect(title, "אפשר להעביר") ~ "true")) %>% 
  filter(!is.na(recommendation)) %>% 
  filter(!is.na(original_text))

ggplot(irr, aes(recommendation)) + 
  geom_bar()


# Splitting to train/test -------------------------------------------------

library(tidymodels)

set.seed(544)

irr_split <- initial_split(irr, prop = 0.8)
irr_train <- training(irr_split)
irr_test <- testing(irr_split)


# Analysis on the training dataset ----------------------------------------

irr_train %>% 
  mutate(msg_length = str_length(original_text)) %>% 
  ggplot(aes(y = msg_length, x = recommendation)) + 
  geom_boxplot() + 
  scale_y_log10()

# > Message length --------------------------------------------------------

irr_train %>% 
  group_by(recommendation) %>% 
  summarize(mean_msg_length = mean(str_length(original_text)))

# First insight: the message length is important - shorter messages tend to be false.


# > Checking exclamation and question marks -------------------------------

irr_exclamation <- irr_train %>% 
  mutate(msg_length = str_length(original_text),
         num_exclamation = str_count(original_text, pattern = "!"),
         num_question = str_count(original_text, pattern = "\\?"),
         num_dots = str_count(original_text, pattern = "\\.\\.\\."),
         prcnt_exclamation = num_exclamation/msg_length,
         prcnt_question = num_question/msg_length,
         prcnt_dots = num_dots/msg_length,
         num_total = num_exclamation + num_question + num_dots,
         prcnt_total = prcnt_exclamation + prcnt_question + prcnt_dots)

# irr_exclamation %>% 
#   ggplot(aes(y = prcnt_total, x = recommendation)) + 
#   geom_boxplot() +
#   scale_y_log10()
# 
# irr_exclamation %>% 
#   ggplot(aes(y = prcnt_exclamation, x = recommendation)) + 
#   geom_boxplot() + 
#   scale_y_log10()
# 
# irr_exclamation %>% 
#   ggplot(aes(y = prcnt_question, x = recommendation)) + 
#   geom_boxplot() + 
#   scale_y_log10()

irr_exclamation %>% 
  select(recommendation, num_exclamation:num_dots) %>% 
  pivot_longer(cols = -recommendation, names_to = "character_type", values_to = "num_characters") %>% 
  count(recommendation, character_type, num_characters) %>% 
  group_by(recommendation, character_type) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(x = num_characters, y = prop)) + 
  geom_col() +
  facet_grid(rows = vars(character_type), cols = vars(recommendation)) + 
  coord_cartesian(xlim = c(0, 10))

# Insight #2: Seems as though these characters are not good separators between true/avoid.


# > Using keywords --------------------------------------------------------

library(tidytext)

irr_text_analysis <- irr_train %>% 
  select(article, recommendation, original_text) %>% 
  tidytext::unnest_tokens(word, original_text)

irr_word_counts <- irr_text_analysis %>% 
  group_by(recommendation) %>% 
  count(word, sort = T) %>% 
  group_by(recommendation) %>% 
  mutate(prop = n/sum(n))

visualize_word_separation <- function(word_for_check){
  irr_train %>% 
    mutate(is_word_appear = str_detect(original_text, word_for_check)) %>% 
    group_by(is_word_appear) %>% 
    count(recommendation) %>% 
    ggplot(aes(x = is_word_appear, y = n, fill = recommendation)) + 
    geom_col(position = "fill")
}

visualize_word_separation("מספר")

# Insight #3: It seems that the word "מספר" is meaningful in identifying avoid

visualize_word_separation("הודעה")
visualize_word_separation("אנשים")

report_word_separation <- function(word_for_check){
  irr_train %>% 
    mutate(is_word_appear = str_detect(original_text, word_for_check)) %>% 
    group_by(is_word_appear) %>% 
    count(recommendation) %>% 
    group_by(recommendation) %>% 
    mutate(prop = n/sum(n)) %>% 
    select(-n) %>% 
    filter(!is_word_appear) %>% 
    pivot_wider(names_from = recommendation, values_from = prop) %>% 
    select(-is_word_appear)
}

report_word_separation("מספר")
report_word_separation("הודעה")

report_word_separation('ש"ח')
visualize_word_separation('ש"ח')

word_separation_summary <- irr_word_counts %>% 
  arrange(desc(n)) %>% 
  group_by(recommendation) %>% 
  slice(1:50) %>% 
  mutate(num = seq_along(n)) %>% 
  ungroup() %>% 
  distinct(word) %>% 
  mutate(sep = map(word, report_word_separation, .progress = T))

word_separation_summary %>% 
  unnest(sep) %>% 
  ungroup() %>% 
  mutate(avoid/((neutral+true)/2)) %>% 
  View()
  
visualize_word_separation('לא')
visualize_word_separation('לו')

# Insight #4: It seems that words that we expected to not influence the recommendation are influential
#             Specifically Hebrew stop words - TODO: VERIFY later on.
#             If this is indeed accurate, should use these words in the model training.