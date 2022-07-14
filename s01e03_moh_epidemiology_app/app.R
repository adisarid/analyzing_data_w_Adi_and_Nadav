library(shiny)
library(tidyverse)


# Preprocess and loading --------------------------------------------------

epi_data <- read_csv("data/epi_data_merged_all_districts.csv") %>% 
  rename(`All Districts` = all_districts)

disease_type_list <- epi_data %>% 
  distinct(disease_type)

district_list <- c("All Districts", 
                   names(epi_data)[2:16])


# Define a function which makes the plot ----------------------------------

# this creates the basic table
disease_over_time_data <- function(year_range = c(2019, 2022),
                                   districts = "All Districts",
                                   diseases = c("Criptosporidiosis",
                                                "Zika")){
  epi_data %>% 
    select(disease_type, districts, year, week_num) %>% 
    filter(year >= year_range[1],
           year <= year_range[2]) %>% 
    filter(disease_type %in% diseases) %>% 
    select(-week_num) %>% 
    group_by(disease_type, year) %>% 
    summarize_all(sum)
}

# this creates the plot itself
disease_over_time_plot <- function(year_range = c(2019, 2022),
                                   district = "All Districts",
                                   diseases = c("Criptosporidiosis",
                                              "Zika")){
  
  # my_col_name <- ensym(district) # TODO: Check later why this approach worked/didn't work
  
  disease_over_time_data(year_range,
                         district,
                         diseases) %>% 
    rename_at(.vars = 3, ~"cases") %>%
    ggplot(aes(x = year, y = cases)) + 
    geom_col() + 
    facet_wrap(~disease_type, scales = "free_y") +
    ggtitle(paste0("Disease over time - year total - ", district)) + 
    xlab("Year") + 
    ylab("Cases")
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("MOH Epidemiology dashboard"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput("disease_type",
                      "Disease type",
                      choices = disease_type_list$disease_type,
                      selected = "Criptosporidiosis",
                      multiple = TRUE),
          selectInput("district_input",
                      "Select district",
                      choices = district_list,
                      selected = "All Districts"),
          sliderInput("year_range",
                      "Select years",
                      min = 2015,
                      max = 2022,
                      value = c(2019,2022),
                      sep = "")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("disease_over_time")
        )
    )
)

# Define server logic
server <- function(input, output) {
  
  debounced_disease <- reactive({
    input$disease_type
  }) %>% 
    debounce(1000)
  
  # This is the server logic for the plot
  output$disease_over_time <- renderPlot({
    
    disease_over_time_plot(year_range = input$year_range,
                           district = input$district_input,
                           diseases = debounced_disease())
    
  })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
