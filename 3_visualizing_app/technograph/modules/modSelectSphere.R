selectSphereModUI <- function(id) {
  # print(sphere)
  ns <- NS(id)
  tagList(
    radioButtons(ns("selectSphere"), label = "Please choose which sphere should be loaded", inline = TRUE, choices = c("German", "World", "Dutch"), selected = "German"),
    
    # tableOutput(ns("selectedSite"))
    # textOutput(ns("selectedSite"))
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
          # print("handling reactive data")
          # print(input$selectSite)
          req(input$selectSphere)
          # CURRENT_SPHERE <<- input$selectSphere
          react_data$selectSphere <- input$selectSphere
        }
      )
      
      return(react_data)
      # return(list(site_to_load = reactive(input$selectSite)))
      
      })
    }
  