library(shiny)
library(bslib)
library(elmer)
library(promises)
library(future)
library(shinyjs)

# Initialize chat
chat <- chat_openai(
  system_prompt = "You like chatting about star trek, mostly TNG and onwards (not TOS). Answers should be concise and star trek inspired."
)

# The user interface is simple
ui <- page_sidebar(
  title = "Interactive chat with async",
  sidebar = sidebar(
    title = "Controls",
    useShinyjs(),
    textInput("user_query", "Enter query:"),
    actionButton("ask_chat", label = "Ask the chat")
  ),
  card(
    card_header("The chat's response"),
    hidden(
      div("I'm thinking...", 
          div(class = "spinner-border text-primary", role = "status",
              span(class = "sr-only")),
          id = "loading")),
    uiOutput("chat_response")
  )
)

server <- function(input, output) {
  
  # Once the user requests chat gpt output:
  observeEvent(input$ask_chat, {
    
    # Do something to show you're thinking
    showElement("loading")
    
    # Call the chat_async function
    result_async <- chat$chat_async("Answer this question:",
                                    input$user_query)
    
    # Using the promises notation, this will run once the chat result comes through
    result_async %...>% {
      # Update the modal once the result is available
      hideElement("loading") # results are in - hide the "loading".
      output$chat_response <- renderUI({
        markdown(.) # The `.` is the chat result, markdown is just used to turn content into html.
      })
    } %...!% {
      # Error handling
      output$chat_response <- renderUI({
        p("Something went wrong... :(")
      })
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)