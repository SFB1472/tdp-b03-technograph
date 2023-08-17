selectSiteModUI <- function(id, sphere) {
  # print(sphere)
  ns <- NS(id)
  tagList(
    selectInput(ns("selectSite"), label = "Please choose the site to be investigated from the menu below", choices = sphere),
    # tableOutput(ns("selectedSite"))
    # textOutput(ns("selectedSite"))
  )
}

selectSiteModServer <- function(id, site_to_load) {
  moduleServer(
    id,
    function(input, output, session) {
      # print("calling selectSiteModServer")
      
      react_data <- reactiveValues()
      
      observeEvent(
        input$selectSite,{
          # print("handling reactive data")
          # print(input$selectSite)
          req(input$selectSite)
          react_data$selectSite <- input$selectSite
        }
      )
      
      return(react_data)
      # return(list(site_to_load = reactive(input$selectSite)))
      
      })
    }
  