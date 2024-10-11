selectSubdomainUI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("selectSubdomain"), label = "Please choose the subdomain the hash belongs to", choices = subdomain_list$abendblatt[[2]])
  )
}

selectSubdomainServer <- function(id, site_to_load) {
  moduleServer(
    id,
    function(input, output, session) {
      react_data <- reactiveValues()
      current_data <- reactiveValues()
      
      change_select_list <- function(i){
        choices_vec <- subdomain_list[[{i}]][[2]]
        print(paste0("choises vec ", choices_vec))
        updateSelectInput(inputId = "selectSubdomain", label = "Please choose the subdomain the hash belongs to", choices = choices_vec)
      }
      
      observeEvent(site_to_load(),{
        current_data$site_to_load = site_to_load()
        change_select_list(site_to_load())
      })
      
      observeEvent(
        input$selectSubdomain,{
          req(input$selectSubdomain)
          react_data$selectSubdomain <- input$selectSubdomain
        }
      )
      
      return(react_data)
      })
    }
  