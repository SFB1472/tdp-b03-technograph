library(ggiraph, "MetBrewer")

tab_1_archivedSitesUI <- function(id) {
  ns <- NS(id)
  tagList(
    addGFontHtmlDependency(family = c("Roboto Mono")),
    imageOutput(ns("archivedSites"), inline = TRUE)
  )
}

tab_1_archivedSitesServer <- function(id, tab_, sphere) {

  moduleServer(
    id,
    function(input, output, session) {
      
     # print the visualization -----------------------------------------
      
      get_sites_over_time <- function(){
        list(src = paste0("data/",current_data$sphere_to_load,"/histogram-archived-sites.png"), width = "100%")
      }
      
      # reactive value handling ---------------------------------------------------
      
      current_tab <- reactiveValues()
      
      observeEvent(tab_(),{
        # print(paste0("sites archived tab loaded: ", tab_()))
        current_tab$tab = tab_()
      })
      
      current_data <- reactiveValues()
      
      observeEvent(sphere(),{
        if(current_tab$tab == "tab_1"){
          current_data$sphere_to_load = sphere()
          # current_data$sites = get_sites()
          # current_data$snippet_data = get_data_snippets_found()
        }
      })
      
      output$archivedSites <- renderImage({get_sites_over_time()}, deleteFile=FALSE)
  })
}

