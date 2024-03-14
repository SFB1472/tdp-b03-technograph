library(ggiraph)

tab_3_modSystemsLifetimeUI <- function(id) {
  ns <- NS(id)
  tagList(
    addGFontHtmlDependency(family = c("Roboto Mono")),
    girafeOutput(ns("systemsLifetime"))
  )
}

tab_3_modSystemsLifetimeServer <- function(id) {

  moduleServer(
    id,
    function(input, output, session) {
      
       # print the visualization -----------------------------------------
      
      get_systems_lifetime <- function(){
        # current_systems_found <- current_data$counted_systems %>% select(system) %>% pull(.)
        load(paste0("data/systems/df_system_lifetime.RData"))
        plot <- df_system_lifetime %>% 
          mutate(cat = ifelse(is.na(present), NA, cat)) %>% 
          ggplot(., aes()) +
          geom_tile_interactive(aes(x = check_years, y = reorder(name, desc(sorting_lifetime)), fill = cat, tooltip = paste0("system: ", name, "\nyear: ", check_years)), color = NA) +
          scale_fill_manual(values = c("year" = "#333333", "year_unsecure" = "#c2c2c2"), na.value = NA) +
          scale_x_continuous( limits = c(1997, 2021)) +
          theme_b03_base + theme_b03_box_timeline + theme(legend.position = "none")
        
        girafe(ggobj = plot, options = list(opts_sizing(rescale = FALSE)),
               width_svg = 15,
               height_svg = 10)
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

      output$systemsLifetime <- renderGirafe({get_systems_lifetime()})
    
  })
  
}

