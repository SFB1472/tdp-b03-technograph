library(ggiraph, "MetBrewer")

tab_1_overviewTracesFoundUI <- function(id) {
  ns <- NS(id)
  tagList(
    # addGFontHtmlDependency(family = c("Roboto Mono")),
    imageOutput(ns("overviewTracesFound"), inline = TRUE)
  )
}

tab_1_overviewTracesFoundServer <- function(id, tab_, sphere) {

  moduleServer(
    id,
    function(input, output, session) {
      
      # print the visualization -----------------------------------------
      
      plot_traces_found <- function(){
        # print("adding image")
        # print(paste0("image path: ", "/data/",current_data$sphere_to_load,"/overview-traces-found.png"))
        list(src = paste0("data/",current_data$sphere_to_load,"/overview-traces-found.png"), width = "100%")
      }
      
      # reactive value handling ---------------------------------------------------
      
      current_tab <- reactiveValues()
      
      observeEvent(tab_(),{
        # print(paste0("overviewTracesFound tab loaded: ", tab_()))
        current_tab$tab = tab_()
      })
      
      current_data <- reactiveValues()
      
      observeEvent(sphere(),{
        if(current_tab$tab == "tab_1"){
          # print(paste0("findingsVizServer ", site_to_load()))
          current_data$sphere_to_load = sphere()
       }
      })
      
      output$overviewTracesFound <- renderImage({plot_traces_found()}, deleteFile=FALSE)
  })
}
