library(tidyverse)
library(rvest)

# Example for extracting data from a specific page ----

html_text <- read_html("https://irrelevant.org.il/?p=300")

entry_title <- html_text %>% 
  html_elements(".entry-title") %>% 
  html_text2()

entry_text <- html_text %>% 
  html_elements(".entry") %>% 
  html_text2()

# Identify position of splitting text ("המקור:")
original_text_location <- str_locate_all(entry_text, "המקור:\n")

explaining_text <- str_sub(entry_text, end = original_text_location[[1]][1])
the_original_text <- str_sub(entry_text, start = original_text_location[[1]][2])


# Figure out a non-working page -------------------------------------------

# It just returns an exception, which is fine, we'll use a "safely wrapper"
# around our function.

read_html("https://irrelevant.org.il/?p=3562") # this is a broken link

# Create a function which does all that -----------------------------------

retrieve_record <- function(post_number){
  
  # read the raw html with rvest
  html <- read_html(glue::glue("https://irrelevant.org.il/?p={post_number}"))
  
  entry_title <- html %>% 
    html_elements(".entry-title") %>% 
    html_text2()
  
  entry_text <- html %>% 
    html_elements(".entry") %>% 
    html_text2()
  
  # Identify position of splitting text ("המקור:")
  original_text_location <- str_locate_all(entry_text, "המקור:\n")
  
  explaining_text <- str_sub(entry_text, end = original_text_location[[1]][1])
  the_original_text <- str_sub(entry_text, start = original_text_location[[1]][2])
  
  # return the results as a list
  
  list(title = entry_title, 
       explaining_text = explaining_text,
       original_text = the_original_text)
  
}

safely_retrieve <- safely(retrieve_record, 
                          otherwise = NA)
## examples
# safely_retrieve(300) # successful run
# safely_retrieve(3562) # graceful failure

# Extract multiple records ------------------------------------------------

scraping_results <- map(1:5954,
                        safely_retrieve,
                        .progress = TRUE)

saveRDS(scraping_results, "data/scraping_results.RDS")
