library(ggiraph, "MetBrewer")

tab_3_modVizSystemFoundUI <- function(id) {
  ns <- NS(id)
  tagList(
    addGFontHtmlDependency(family = c("Roboto Mono")),
    girafeOutput(ns("systemsFound"))
  )
}

tab_3_modVizSystemFoundServer <- function(id, tab_, sphere) {

  moduleServer(
    id,
    function(input, output, session) {
      
      initLoad_vizSystems <- TRUE
      
      get_system_lifetime <- function(){
        load(paste0("data/systems/df_system_lifetime.RData"))
        return(df_system_lifetime)
      }
      
     # get all those site with traces found, snippets and comments --------------------------
      
      get_counted_systems <- function(){
        # print("3 tab, 1 vis, systems counted")
        retranslate_snippets <- read_csv("data/helper/23-01-13-Commenting-system-detection-patterns.csv", show_col_types = FALSE) %>% 
          select("system" = `Commenting system`, "snippet" = Snippet) %>% 
          filter(!is.na(system)) %>% select(snippet, system)
        
        db_systems_counted <- tbl(pool, "sites") %>%
          select(sites_id, sha1, crawl_date, sphere, site, of_interest) %>% 
          inner_join(., tbl(pool, "snippets_2"), by = c("sha1" = "site")) %>% #collect()
          filter(sphere == !!current_data$sphere_to_load, of_interest == TRUE, detected == 1) %>% #collect()
          mutate(year = year(crawl_date)) %>% 
          select(year, snippet, site) %>% 
          distinct() %>% 
          collect() %>% #View()
          left_join(., retranslate_snippets, by = "snippet") %>% 
          select(year, system, site) %>% 
          distinct() %>%
          reframe(counted_domains = n(), .by = c(year, system))
          
        # View(db_systems_counted)
        return(db_systems_counted)
      }
      
      # calc height ------------------------------------------------------------
      
      get_height <- reactive({
        # print("calc height")
        nr_systems <- as.numeric(current_data$counted_systems %>% select(system) %>% distinct() %>% nrow())

        # facethöhe: hier fließt die überschrift und die x-achse als entscheidende größen ein. x-achse gibts für alle facets nur einmal 
        minheight_one_facet = 1
        # jede datenreihe braucht ein bisschen platz, aber keine neue überschrift
        minheight_one_row = 0.5
        
        sum_height <- minheight_one_facet + minheight_one_row + (minheight_one_row * nr_systems)
        print(sum_height)
        
        return(sum_height)
        
      })
      
       # print the visualization -----------------------------------------
      
      get_systems_over_time <- function(){
        current_systems_found <- current_data$counted_systems %>% select(system) %>% pull(.)
        
        df_system_lifetime_filtered <- current_data$system_lifetime %>% 
          filter(name %in% current_systems_found) %>% #View()
          group_by(name, check_years) %>% #View()
          summarise(counted_years = sum(present, na.rm = TRUE)) %>%
          mutate(counted_years = ifelse(counted_years == 1, as.character(counted_years), NA)) %>% 
          ungroup()
        
        plot <- ggplot() +
          geom_tile(data = df_system_lifetime_filtered, aes(x = check_years, y = name, color = counted_years), fill = "white", alpha = 0.7) +
          scale_color_manual(values = c("1" = "darkgreen"), na.value = NA) + 
          geom_tile(data = current_data$counted_systems, aes(x = year, y = system, fill = counted_domains)) +
          scale_fill_gradientn(colors = met.brewer("Hokusai2", type="continuous"), na.value = "grey90", name = "number of websites with system" ) +
          theme_b03_base + theme_b03_heatmap +
          guides(color = "none",
                 fill = guide_colorbar(title.position = "top", barwidth = unit(20, "lines"), barheight = unit(.5, "lines")))
        
        girafe(ggobj = plot,
               options = list(opts_sizing(rescale = FALSE)),
               width_svg = 15,
               height_svg = current_data$height)
      }
      
      # reactive value handling ---------------------------------------------------
      
      rebuild_df <- function(){
        current_data$counted_systems = get_counted_systems()
        current_data$system_lifetime = get_system_lifetime()
        current_data$height = get_height()
      }
      observeEvent(tab_(),{
        # print(paste0("3 systems found tab loaded: ", tab_()))
        if(tab_() == "tab_3"){
          rebuild_df()
        }
      })
      
      current_data <- reactiveValues()
      
      observeEvent(sphere(),{
        # print("3 tab  sphere fired")
        current_data$sphere_to_load = sphere()
        if(!initLoad_vizSystems){
          rebuild_df()
        }
        initLoad_vizSystems <<- FALSE
        
      })
      
      output$systemsFound <- renderGirafe({get_systems_over_time()})
    
  })
  
}

