selectSiteModUI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("selectSite"), label = "Please choose the site to be investigated from the menu below", choices = SELECT_MENU_SITE[[{CURRENT_SPHERE}]])
  )
}

selectSiteModServer <- function(id, sphere_to_load) {
  moduleServer(
    id,
    function(input, output, session) {
      react_data <- reactiveValues()
      current_data <- reactiveValues()
      
      change_select_list <- function(i){
        updateSelectInput(inputId = "selectSite", label = "Please choose the site to be investigated from the menu below", choices = SELECT_MENU_SITE[[{i}]])
      }
      
      observeEvent(sphere_to_load(),{
        current_data$site_to_load = sphere_to_load()
        change_select_list(sphere_to_load())
      })
      
      observeEvent(
        input$selectSite,{
          req(input$selectSite)
          react_data$selectSite <- input$selectSite
        }
      )
      
      return(react_data)
      })
    }
  