inputHashUI <- function(id) {
  ns <- NS(id)
  tagList(
    textInput(ns("selectHash"), label = "Please enter the number of the hash"),
    actionButton(ns("submitHash"), label = "submit")
  )
}

inputHashServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      react_data <- reactiveValues()
      
      observeEvent(
        input$submitHash,{
          req(input$selectHash)
          react_data$selectHash <- input$selectHash
        }
      )
      return(react_data)
      })
    }
  