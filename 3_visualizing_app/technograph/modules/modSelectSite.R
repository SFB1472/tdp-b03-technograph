selectSiteModUI <- function(id) {
  # print(paste0("id of input ", id))
  ns <- NS(id)
  tagList(
    selectInput(ns("selectSite"), label = "Please choose the site to be investigated from the menu below", choices = SELECT_MENU_SITE[[{CURRENT_SPHERE}]]),
    # tableOutput(ns("selectedSite"))
    # textOutput(ns("selectedSite"))
  )
}

selectSiteModServer <- function(id, sphere_to_load) {
  moduleServer(
    id,
    function(input, output, session) {
      # print("calling selectSiteModServer")
      # print(CURRENT_SPHERE)
      react_data <- reactiveValues()
      current_data <- reactiveValues()
      
      change_select_list <- function(i){
        # print(paste0("update select input ", i))
        # print(SELECT_MENU_SITE[[{i}]])
        updateSelectInput(inputId = "selectSite", label = "Please choose the site to be investigated from the menu below", choices = SELECT_MENU_SITE[[{i}]])
      }
      
      observeEvent(sphere_to_load(),{
        # print(paste0("change sphere to ", sphere_to_load()))
        current_data$site_to_load = sphere_to_load()
        change_select_list(sphere_to_load())
      })
      
      observeEvent(
        input$selectSite,{
          # print("handling reactive site")
          # print(reactive(input$selectSite))
          req(input$selectSite)
          react_data$selectSite <- input$selectSite
        }
      )
      
      return(react_data)
      # return(list(site_to_load = reactive(input$selectSite)))
      
      })
    }
  