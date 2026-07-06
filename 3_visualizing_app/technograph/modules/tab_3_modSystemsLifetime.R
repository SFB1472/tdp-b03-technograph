library(ggiraph)

tab_3_modSystemsLifetimeUI <- function(id) {
  ns <- NS(id)
  tagList(
    addGFontHtmlDependency(family = c("Roboto Mono")),
    imageOutput(ns("systemsLifetime"), inline = TRUE)
  )
}

tab_3_modSystemsLifetimeServer <- function(id) {

  moduleServer(
    id,
    function(input, output, session) {
      
       # print the visualization -----------------------------------------
      
      get_systems_lifetime <- function(){
        list(src = paste0("data/German/overview-systems-available.png"), width = "100%")
        
      }
      
      # reactive value handling ---------------------------------------------------
      
      # rebuild_df <- function(){
      #   current_data$counted_systems = get_counted_systems()
      #   current_data$system_lifetime = get_system_lifetime()
      #   current_data$height = get_height()
      # }
      # observeEvent(tab_(),{
      #   print(paste0("3 systems found tab loaded: ", tab_()))
      #   if(tab_() == "tab_3"){
      #     rebuild_df()
      #   }
      # })

      output$systemsLifetime <- renderImage({get_systems_lifetime()}, deleteFile=FALSE)
    
  })
  
}

