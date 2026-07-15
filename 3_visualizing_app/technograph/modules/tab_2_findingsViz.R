library(ggiraph, "MetBrewer")

findingsVizUI <- function(id) {
  ns <- NS(id)
  tagList(
    addGFontHtmlDependency(family = c("Roboto Mono")),
    girafeOutput(ns("selectedSite"))
  )
}

findingsVizServer <- function(id, tab_, site_to_load, sphere) {
  
  stopifnot(is.reactive(site_to_load))
  # print(paste0("xxxxxxxxxxx ", sphere))
  moduleServer(
    id,
    function(input, output, session) {
      initLoad <- TRUE
      # initLoad <- FALSE
      
      ## get the data ----------------------------------------------------------
      
      ### google doc data -------------------------------------------------------
      
      get_print_data <- function(){
        df_return <- tbl(pool, "tbl_gfx_all_findings") |> 
          filter(site == !!current_data$site_to_load) |> 
          collect()
        return(df_return)
      }
      
      # calc height ------------------------------------------------------------
      
      get_height <- reactive({
       
       nr_facets <- as.numeric(current_data$print_data |> select(type) |> distinct() |> nrow())
       nr_rows <- as.numeric(current_data$print_data |> select(site_subdomain) |> distinct() |> nrow())
       
       # facethöhe: hier fließt die überschrift und die x-achse als entscheidende größen ein. x-achse gibts für alle facets nur einmal 
       minheight_one_facet = 0.5
       # jede datenreihe braucht ein bisschen platz, aber keine neue überschrift
       minheight_one_row = 0.3
       
       sum_height <- minheight_one_facet + (minheight_one_facet * nr_facets) + minheight_one_row + (minheight_one_row * nr_rows)
       # print(nr_facets)
       # print(nr_rows)
       
       return_value <- sum_height
       
     })

      # print the viz --------------------------------------------------------------
      
      print_form_findings <- reactive({
        unique_colors <- c("")
        
        nr_tags <- current_data$print_data |> 
          select(tooltip_info) |> 
          filter(str_detect(tooltip_info, "tagindex")) |> 
          distinct() |> 
          nrow()
        
        print(nr_tags)   
        
        if (nr_tags > 0){
          unique_colors <- current_data$print_data |>  
            filter(!is.na(shape_color)) |> 
            distinct(shape_color) %>% nrow() %>% rainbow(.)
          }
        
        color_values <- current_data$print_data %>% 
          select(shape_color) %>% 
          filter(!is.na(shape_color)) |> 
          distinct() %>% 
          arrange(shape_color) %>% 
          bind_cols(unique_colors) %>% 
          add_row(shape_color = 9999999, "...2" = "#000000") %>% deframe()
        
        fill_values <- current_data$print_data %>% 
          select(shape_fill) %>% 
          filter(!is.na(shape_fill)) |> 
          distinct() %>% nrow()
        
        data_first_layer <- current_data$print_data |> 
          filter(type != "traces via snippets")
        
        data_sec_layer <- current_data$print_data |> 
          filter(type == "traces via snippets")
        
        # df_plot  <-  current_data$form_data %>% 
        #   # bind_rows(., current_data$snippet_data) %>% 
        #   bind_rows(., current_data$annotation) %>% 
        #   mutate(shape_nr = as.character(shape_nr))
        
        # shape_values <- current_data$print_data %>% 
        #   # bind_rows(., current_data$snippet_data) %>% 
        #   select(shape_nr) %>% 
        #   distinct() %>% 
        #   mutate(value = as.numeric(shape_nr)) %>% deframe()
        
        plot <- ggplot() +
          geom_point_interactive(data = data_first_layer, aes(x = crawl_date, y = site_subdomain, color =  as.character(shape_color), tooltip = paste0(tooltip_info, "\nDatum: ", crawl_date)), size = 3, alpha = 0.8, shape = 19) +
          geom_point_interactive(data = data_sec_layer, aes(x = crawl_date, y = site_subdomain, fill = as.character(shape_fill), tooltip = paste0(tooltip_info, "\nDatum: ", crawl_date)), shape = 23, size = 4, stroke = 0.1, color = "white") +
          scale_x_date(date_breaks = "2 years", date_labels = "%Y", limits = c(ymd("1997-01-01"), ymd("2021-01-01"))) +
          scale_color_manual(values = color_values) +
          scale_fill_manual(values = MetBrewer::met.brewer("Cross", fill_values)) +
          theme_b03_base + theme_b03_facets_individual + theme(legend.position = "none") + 
          facet_col(vars(type), scales = "free_y", space = "free") +
          # facet_grid(row = vars(type), scales = "free_y", space = "free_y", switch = "y") +
          # facet_wrap(~type, ncol = 1, scales = "free_y") +
          coord_cartesian(clip = "off")
        
        
        girafe(ggobj = plot,
               options = list(opts_sizing(rescale = FALSE)),
               width_svg = 9,
               height_svg = current_data$height)
      })
      
      # observe Events ----------------------------------
      
      current_data <- reactiveValues()
      
      rebuild_df <- function(){
        current_data$site_to_load = site_to_load()
        # current_data$annotation = get_gs_annoted_data(sphere())
        # current_data$snippet_data = get_snippet_traces_data()
        current_data$print_data = get_print_data()
        current_data$height = get_height()
      }
      
      observeEvent(tab_(),{
        # print(paste0("tab 2 visualization: tab loaded: ", tab_()))
        # current_tab$tab = tab_()
        if(tab_() == "tab_2"){
          # print(paste0("init load ", initLoad))
          rebuild_df()
        }
      })
      
      observeEvent(site_to_load(),{
        # print(paste0("tab 2 site to load fired ", site_to_load(), " first load? ", initLoad))
        current_data$site_to_load = site_to_load()
        if(!initLoad){
          rebuild_df()
        }
        initLoad <<- FALSE
      })
     
      output$selectedSite <- renderGirafe({print_form_findings()})
    })
  }
  