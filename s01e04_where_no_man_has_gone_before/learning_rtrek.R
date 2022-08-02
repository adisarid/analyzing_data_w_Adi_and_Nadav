library(tidyverse)
install.packages("rtrek")
library(rtrek)




# Experimenting with the stapi --------------------------------------------

stapiEntities$colnames[1]


stapi("animal", page_count = TRUE)

stapi("character", page = 1)
stapi("animal", page = 1)

all_st_animals <- map_df(.x = 1:4, .f = 
                           function(x) stapi("animal", x))


install.packages("trekcolors")
install.packages("trekfont")


# Load fonts --------------------------------------------------------------
# TODO: Check later about updating the fonts
library(showtext)
font <- c("Khan", "StarNext", "FederationDS9Title", "Federation", "Klingon", "ModernVulcan", "TNGcast", "FederationStarfleet")
path <- system.file(paste0("fonts/", font, ".ttf"), package = "trekfont")
for(i in seq_along(font)) font_add(font[i], path[i])


# Looking at trek animals with a chart ------------------------------------

all_st_animals %>% 
  count(earthAnimal, feline)

all_st_animals %>% 
  summarize(across(earthAnimal:feline, mean)) %>% 
  pivot_longer(cols = everything(), names_to = "property", values_to = "prop") %>% 
  ggplot(aes(x = property, y = prop)) + 
  geom_col(fill = trekcolors::trekpals$lcars_2357[1]) + 
  theme(panel.background = element_rect(fill = trekcolors::trekpals$lcars_2357[2])) + 
  scale_y_continuous(labels = scales::percent) + 
  ggtitle("Animal type distribution in star trek")
  

# Secondary characters ----------------------------------------------------

# Load st transcripts

script <- st_transcripts()

tng_scripts <- script %>% 
  filter(series == "TNG") %>% 
  unnest(text) %>% 
  mutate(character = str_remove_all(character, " V.o.| (V.o.)")) %>% 
  mutate(num_words = str_count(line, pattern = " ") + 1)

episodes_per_character <- tng_scripts %>% 
  distinct(character, season, number) %>% 
  mutate(character = if_else(str_detect(character, '"Q"') |
                               str_detect(character, 'Q '), 
                             'Q', character)) %>% 
  count(character, sort = T) %>% 
  filter(n < 100) %>% 
  filter(!str_detect(character, "Com Voice|(Cont'd)| (V.o.)| V.o.")) %>% 
  filter(!str_detect(character, fixed("(V.o"))) %>% 
  filter(!str_detect(character, fixed("O.s"))) %>% 
  slice(1:50) %>% 
  mutate(character = fct_inorder(character)) %>% 
  ggplot(aes(y = character, x = n)) + 
  geom_col()
  

# Count characters by words -----------------------------------------------

words_per_character <- tng_scripts %>% 
  mutate(character = if_else(str_detect(character, '"Q"') |
                               str_detect(character, 'Q '), 
                             'Q', character)) %>% 
  filter(!str_detect(character, "Com Voice|(Cont'd)| (V.o.)| V.o.")) %>% 
  filter(!str_detect(character, fixed("(V.o"))) %>% 
  filter(!str_detect(character, fixed("O.s"))) %>% 
  group_by(character) %>% 
  summarize(total_words = sum(num_words)) %>% 
  mutate(character = fct_reorder(character, -total_words)) %>% 
  slice_max(order_by = total_words, n = 50)

ggplot(words_per_character, 
       aes(y = character, x = total_words)) + 
  geom_col() + 
  scale_x_log10()
