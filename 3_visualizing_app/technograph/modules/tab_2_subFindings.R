library(ggiraph, "MetBrewer")

subFindingsVizUI <- function(id) {
  ns <- NS(id)
  tagList(
    addGFontHtmlDependency(family = c("Roboto Mono")),
    girafeOutput(ns("subFindings"))
  )
}

subFindingsVizServer <- function(id, tab_, site_to_load, sphere) {
  
  stopifnot(is.reactive(site_to_load))
  # print(paste0("xxxxxxxxxxx ", sphere))
  moduleServer(
    id,
    function(input, output, session) {
      initLoad <- TRUE
      # initLoad <- FALSE
      
      ## get the data ----------------------------------------------------------
      
      ### sub traces -----------------------------------------------------------
      
      get_data_sub_traces <- function(sphere_){
        
        return_df <- tbl(pool, "tbl_gfx_subfindings") |> 
          filter(site == !!current_data$site_to_load) |> 
          collect() 
        
        return(return_df)
        
      }
      
      # calc height ------------------------------------------------------------
      
      get_height <- reactive({
       # print("calc height")
       nr_site_subdomains <- as.numeric(current_data$sub_traces %>% select(site_subdomain) %>% distinct() %>% nrow())
       nr_traces <- as.numeric(current_data$sub_traces %>% select(trace) %>% distinct() %>% nrow())
       # has_annotation <- ifelse(current_data$annotation %>% nrow() > 0, 1, 0)
       has_site_subdomains <- ifelse(nr_site_subdomains > 0, 1, 0)
       has_traces <- ifelse(nr_traces > 0, 1, 0)
       
       nr_facets <- as.numeric(nr_site_subdomains)
       nr_rows <- as.numeric(nr_site_subdomains + nr_traces)
       
       # facethöhe: hier fließt die überschrift und die x-achse als entscheidende größen ein. x-achse gibts für alle facets nur einmal 
       minheight_one_facet = 0.8
       # jede datenreihe braucht ein bisschen platz, aber keine neue überschrift
       minheight_one_row = 0.1
       
       sum_height <- minheight_one_facet + (minheight_one_facet * nr_facets) + minheight_one_row + (minheight_one_row * nr_rows)
       # print(sum_height)
       
       return_value <- sum_height
       
     })

      # print the viz --------------------------------------------------------------
      
      print_sub_findings <- reactive({
        
        color_search_type <- c("snippet_list"= "black", "sub_traces" = "white")
        shape_search_type <- c("snippet_list"= 18, "sub_traces" = 19)
        
        plot <- ggplot() +
          geom_tile(data = current_data$sub_traces, aes(x = simple_date, y = trace, fill = counted_sites)) +
          geom_point(data = current_data$sub_traces, aes(x = simple_date, y = trace, size = counted_comments), color = "#31eccd", shape = 15) +
          geom_point(data = current_data$sub_traces, aes(x = simple_date, y = trace, size = counted_traces), color = "black") +
          facet_col(vars(site_category), scales = "free", space = "free") +
          scale_x_date(date_breaks = "2 years", date_labels = "%Y", limits = c(ymd("1997-01-01"), ymd("2021-01-01"))) +
          # scale_color_manual(values = color_search_type) +
          # scale_shape_manual(values = shape_search_type) +
          scale_fill_gradientn(colors = met.brewer("Hokusai2", type="continuous"), na.value = "grey90") +
          theme_b03_base + theme_b03_heatmap + theme_b03_facets + theme_b03_legend_discrete + theme_b03_panel_spacing +
          theme_b03_timeline_faceted +  theme(legend.position = "none", panel.grid.major.y = element_line(color = "grey70", linewidth = .2))
        
        girafe(ggobj = plot,
               options = list(opts_sizing(rescale = FALSE)),
               width_svg = 9,
               height_svg = current_data$height)
      })
      
      # observe Events ----------------------------------
      
      current_data <- reactiveValues()
      
      rebuild_df <- function(){
        current_data$site_to_load = site_to_load()
        current_data$sub_traces = get_data_sub_traces(sphere())
        current_data$height = get_height()
      }
      
      observeEvent(tab_(),{
        if(tab_() == "tab_2"){
         rebuild_df()
        }
      })
      
      observeEvent(site_to_load(),{
        current_data$site_to_load = site_to_load()
        if(!initLoad){
          rebuild_df()
        }
        initLoad <<- FALSE
      })
     
      output$subFindings <- renderGirafe({print_sub_findings()})
    })
  }
  