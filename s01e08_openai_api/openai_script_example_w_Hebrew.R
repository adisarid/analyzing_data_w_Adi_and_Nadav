library(tidyverse)
library(reticulate)

# One time operation to generate a python environment
# reticulate::conda_create(envname = "open_ai_example_w_Hebrew", packages = c("openai", "boto3"), python_version = "3.9")

reticulate::use_condaenv("open_ai_example_w_Hebrew")

# Import packages ----

openai <- import("openai")
boto3 <- import("boto3")

openai_key <- read_file("credentials/openai.key")
aws_creds <- readr::read_csv("credentials/new_user_credentials.csv") %>% 
  select(3:4) %>% 
  janitor::clean_names()

openai$api_key <- openai_key

# connect to the translation service
translate <- boto3$client(service_name = 'translate', 
                          region_name = 'eu-central-1', use_ssl = TRUE,
                          aws_access_key_id = aws_creds$access_key_id,
                          aws_secret_access_key = aws_creds$secret_access_key)

# Define the prompt story in Hebrew
prompt_story_hebrew <- "שתיתי קפה של בוקר, נכנסתי לתא הטייס, והמראתי עם החללית שלי לירח"

# translate the the prompt to English
translated_prompt_EN <- translate$translate_text(Text = prompt_story_hebrew,
                                                 SourceLanguageCode = "he",
                                                 TargetLanguageCode = "en")

response <- openai$Completion$create(
  model = "text-davinci-002",
  prompt = translated_prompt_EN$TranslatedText,
  temperature = 0.7,
  max_tokens = 250L
)

response_list <- jsonlite::fromJSON(as.character(response$choices[[1]]))

eng_response <- str_replace_all(
  paste0(translated_prompt_EN$TranslatedText, 
         response_list$text), 
  "\n\n", " ")

write_file(eng_response, file = "the_moon_story_Eng.txt")

# Translate back to Hebrew
translated_answer_Hebrew <- translate$translate_text(Text = eng_response,
                                                     SourceLanguageCode = "en",
                                                     TargetLanguageCode = "he")

translated_answer_Hebrew$TranslatedText

write_file(translated_answer_Hebrew$TranslatedText, "the_moon_story_Heb.txt")

# Trying out prompt in Hebrew ----

response_hebrew <- openai$Completion$create(
  model = "text-davinci-002",
  prompt = prompt_story_hebrew,
  temperature = 0.7,
  max_tokens = 250L
)


openai_hebrew_response <- jsonlite::fromJSON(as.character(response_hebrew$choices[[1]]))


eng_response <- str_replace_all(
  paste0(prompt_story_hebrew,
         openai_hebrew_response$text), 
  "\n\n", " ")
