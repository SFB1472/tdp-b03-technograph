selectSphereModUI <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(ns("selectSphere"), label = "Please choose which sphere should be loaded", inline = TRUE, choices = c("German", "World", "Dutch"), selected = "German")
  )
}

selectSphereModServer <- function(id, sphere_to_load) {
  moduleServer(
    id,
    function(input, output, session) {
      # print("calling selectSphereModServer")
      
      react_data <- reactiveValues()
      
      observeEvent(
        input$selectSphere,{
          req(input$selectSphere)
          react_data$selectSphere <- input$selectSphere
        }
      )
      return(react_data)

      })
    }
  