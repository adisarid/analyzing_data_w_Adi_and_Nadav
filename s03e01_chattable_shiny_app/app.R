library(shiny)
library(tidyverse)
library(bslib)
library(elmer)
library(promises)
library(future)

# Init required data -----------------------------------------------------
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
dish_choices <- unique(aroma$dish_type)

# Initialize chat
chat <- chat_openai(
  system_prompt = "You are an analyst that can explain chart to decision makers, and you also really like building lego."
)

ui <- page_sidebar(
  title = "Interactive chat about charts",
  sidebar = sidebar(
    title = "Controls",
    numericInput("bins", "Number of bins", 30),
    selectInput("dish_type", "Type of dish", 
                selected = "Salads",
                choices = dish_choices,
                multiple = TRUE),
    selectInput(
      "chart_type", 
      "Chart type",
      choices = c(Histogram = "hist", Scatterplot = "scat"),
      selected = "Histogram"),
    actionButton("retrieve_insights", label = "Insights")
  ),
  card(
    card_header("The chart"),
    plotOutput("aroma_chart")
  )
)

server <- function(input, output) {
  
  aroma_reactive <- reactive({
    aroma |> 
      filter(dish_type %in% input$dish_type)
  }) |> 
    debounce(500)
  
  aroma_reactive_plot <- reactive({
    
    if (input$chart_type == "hist"){
      plt <- ggplot(aroma_reactive(), aes(`אנרגיה (קלוריות)`)) + 
        geom_histogram(bins = input$bins)
    } else {
      plt <- ggplot(aroma_reactive(), 
                    aes(
                      x = `אנרגיה (קלוריות)`,
                      y = `חלבונים (גרם)`
                    )) + 
        geom_point(aes(color = dish_type))
    }
    
    plt + theme_bw()
    
  })
  
  output$aroma_chart <- renderPlot({
    
    aroma_reactive_plot()
    
  })
  
  observeEvent(input$retrieve_insights, {
    
    # First, save the plot using the plot reactive
    filename <- "temporary_files/temporary_aroma_plot.png"
    ggsave(
      filename = filename, 
      plot = aroma_reactive_plot(), 
      width = 768, height = 768, unit = "px"
    )
    
    the_plot_content <- content_image_file(filename)
    
    # Show the loading spinner
    # Show modal with a loading message while waiting for the result
    showModal(
      modalDialog(
        title = "Processing...",
        div(class = "spinner-border text-primary", role = "status",
            span(class = "sr-only")),
        p("ChatGPT is processing your request. Please wait..."),
        easyClose = FALSE
      )
    )
    
    # Call the chat_async function
    result_async <- chat$chat_async("
    Explain this plot in one paragraph, as suitable for decision makers. 
    You should briefly describe the plot type, the axes, and 2-5 major visual patterns.",
                                    the_plot_content)
    
    result_async %...>% {
      # Update the modal once the result is available
      removeModal()  # Remove the loading modal
      showModal(
        modalDialog(
          title = "Here is ChatGPT's take on your chart",
          markdown(.),
          easyClose = TRUE,
          footer = tagList(
            modalButton("Thanks!")
          )
        )
      ) 
    } %...!% {
      # Handle errors if any
      removeModal()
      showModal(modalDialog(
        title = "Error",
        "There was an error processing your request. Please try again.",
        easyClose = TRUE
      ))
    }
    })
  
  onStop(function(){
    filename <- "temporary_files/temporary_aroma_plot.png"
    if (file.exists(filename)){
      file.remove(filename)
    }
  })
  }

# Run the app
shinyApp(ui = ui, server = server)