library(shiny)
library(bslib)
library(elmer)
library(promises)

ui <- page_sidebar(
  title = "Interactive chat with async",
  sidebar = sidebar(
    title = "Controls",
    textInput("user_query", "Enter query:"),
    input_task_button("ask_chat", label = "Ask the chat")
  ),
  card(
    card_header("The chat's response"),
    uiOutput("chat_response")
  )
)

server <- function(input, output) {
  output$chat_response <- renderUI({
    # Start the chat fresh each time, as the UI is not a multi-turn conversation
    chat <- chat_openai(
      system_prompt = "You like chatting about star trek, mostly TNG and onwards (not TOS). Answers should be concise and star trek inspired."
    )
    # Asynchronously get the (Markdown) results and render to HTML
    chat$chat_async("Answer this question:", input$user_query) %...>% markdown()
  }) |> bindEvent(input$ask_chat)
}

shinyApp(ui = ui, server = server)