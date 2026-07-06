library(ggiraph, "MetBrewer")

tab_3_modVizSystemFoundUI <- function(id) {
  ns <- NS(id)
  tagList(
    addGFontHtmlDependency(family = c("Roboto Mono")),
    imageOutput(ns("systemsFound"), inline = TRUE)
  )
}

tab_3_modVizSystemFoundServer <- function(id, tab_, sphere) {

  moduleServer(
    id,
    function(input, output, session) {
      
      # print the visualization -----------------------------------------
      
      get_systems_over_time <- function(){
        list(src = paste0("data/",current_data$sphere_to_load,"/overview-systems-found.png"), width = "100%")
      }
      
      # reactive value handling ---------------------------------------------------
      
    current_data <- reactiveValues()
      
      observeEvent(sphere(),{
        # print("3 tab  sphere fired")
        current_data$sphere_to_load = sphere()
        
      })
      
      output$systemsFound <- renderImage({get_systems_over_time()}, deleteFile=FALSE)
    
  })
  
}

