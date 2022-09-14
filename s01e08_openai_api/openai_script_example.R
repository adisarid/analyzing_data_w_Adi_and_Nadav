library(tidyverse)
library(reticulate)

# One time operation to generate a python environment
# reticulate::conda_create(envname = "open_ai_example", packages = "openai", python_version = "3.9")

reticulate::use_condaenv("open_ai_example")

openai <- import("openai")

openai_key <- read_file("credentials/openai.key")

openai$version

openai$api_key <- openai_key

response <- openai$Completion$create(
  model = "text-davinci-002",
  prompt = "This is a story about zombies, but not just any zombies. These zombies used to hide in the desert, eat snakes and hunt lions",
  temperature = 0.7,
  max_tokens = 250L
)

response_list <- jsonlite::fromJSON(as.character(response$choices[[1]]))

cat(
  paste("This is a story about zombies, but not just any zombies. These zombies used to hide in the desert, eat snakes and hunt lions",
        response_list$text
  ))

