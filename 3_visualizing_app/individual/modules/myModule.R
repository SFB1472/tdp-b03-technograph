myModuleUI <- function(id, label = "Input text: ") {
  ns <- NS(id)
  tagList(
    textInput(ns("txt"), label),
    textOutput(ns("result"))
  )
}

myModuleServer <- function(id, prefix = "") {
  moduleServer(
    id,
    function(input, output, session) {
      output$result <- renderText({
        paste0(prefix, toupper(input$txt))
      })
    }
  )
}