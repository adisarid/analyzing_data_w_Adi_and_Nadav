library(tidyverse)
library(openai)

# openai_key <- Sys.getenv("OPENAI_API_KEY")

# TODO: Add a description about generating an openai API key.

# Using openai API as a "code monkey"

prompt <- "Build a function that creates a ggplot2 comparing the mpg and 
disp from mtcars data (in R)."

instructions <- "You are a programming wiz in multiple languages. 
You write efficient well documented code with validation checks.
You only provide the function text and commented comments, 
i.e., don't provide preamble only provide lines of code and inside code comments."

# Using the openai package

new_function <- create_chat_completion(model = "gpt-3.5-turbo",
                                       messages = list(
                                         list(
                                           "role" = "system",
                                           "content" = instructions
                                         ),
                                         list(
                                           "role" = "user",
                                           "content" = prompt
                                         )
                                       )
)

write_file(new_function$choices$message.content, file = "new_function.R", append = T)
rstudioapi::navigateToFile("new_function.R")


# Turning the code into a function

openai_codemonkey <- function(prompt,
                              file,
                              programming_language = "R",
                              model = "gpt-3.5-turbo",
                              openai_api_key = Sys.getenv("OPENAI_API_KEY")){
  
  # Test that a key is available
  assertthat::assert_that({openai_api_key != ""}, 
                          msg = "No key is provided - check that a key in the environment")
  
  instructions <- "You are a programming wiz in multiple languages. 
    You write efficient well documented code with validation checks.
    You only provide the function text and commented comments,
    i.e., don't provide preamble only provide lines of code and inside code comments.
  "
  
  new_function <- create_chat_completion(model = "gpt-3.5-turbo",
                                         messages = list(
                                           list(
                                             "role" = "system",
                                             "content" = instructions
                                           ),
                                           list(
                                             "role" = "user",
                                             "content" = 
                                               glue::glue("{prompt} in the {programming_language} language")
                                           )
                                         )
  )
  
  tmp_function <- str_match(new_function$choices$message.content,
                              "```([^`]+)```")[[2]]
  
  # Identify and remove the first few strings which are expected to be the language
  language_chars <- nchar(programming_language)
  clean_function <- str_sub(tmp_function, start = language_chars + 1)
  
  write_file(clean_function, file = file, append = F)
  rstudioapi::navigateToFile(file)
  
}



openai_codemonkey(prompt = 
                    "a function that gets a title as the argument and prints the title n times (n is also an argument of the function)",
                  file = "write_title.R",
                  programming_language = "R")


openai_codemonkey(prompt = 
                    "a function that gets a title as the argument and prints the title n times (n is also an argument of the function)",
                  file = "write_title.py",
                  programming_language = "python")

openai_codemonkey(prompt = 
                    "a function that gets a title as the argument and prints the title n times (n is also an argument of the function)",
                  file = "write_title.cpp",
                  programming_language = "C++")

openai_codemonkey(prompt = 
                    "a function that gets a title as the argument and prints the title n times (n is also an argument of the function)",
                  file = "write_title.jl",
                  programming_language = "julia")
