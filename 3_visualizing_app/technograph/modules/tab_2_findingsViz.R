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
      # initLoad <- TRUE
      initLoad <- FALSE
      
      ## get the data ----------------------------------------------------------
      
      ### google doc data -------------------------------------------------------
      
      ANNOTATION_IS_EMPTY <- TRUE
      
      check_gs_empty <- function(gs){
        ANNOTATION_IS_EMPTY <<- gs %>% nrow() == 0
      }
      
      get_gs_annoted_data <- function(sphere){
        
        # print(paste0("get_gs_annoted_data ", sphere, " current site to load ", current_data$site_to_load))
        # googlesheets4::gs4_deauth()
        # gs_annotation_raw <- googlesheets4::read_sheet(SPREADSHEET_ANNOTATION_DATA, sheet = SPREADSHEET_ANNOTATION[[{{sphere}}]]) %>% 
        gs_annotation_raw <- read_csv(paste0("data/", SPREADSHEET_ANNOTATION[[{sphere}]]), show_col_types = FALSE) %>% 
          filter(site == !!current_data$site_to_load)
        # print("sheet loaded")
        check_gs_empty(gs_annotation_raw)
        if(!ANNOTATION_IS_EMPTY) {
          gs_annotation <- gs_annotation_raw %>% 
            filter(site == !!current_data$site_to_load, tooltip_info != "") %>% 
            mutate(type = "manual research",
                   date = paste0(year, "-", ifelse(is.na(month), "1", month), "-", ifelse(is.na(day), "1", day)) %>% ymd(.),
                   # technology = ifelse(is.na(technology), "other", technology),
                   shape_color = 9999999,
                   shape_fill = NA,
                   shape_nr = 19
                   ) %>% 
            rename("crawl_date" = "date", "site_subdomain" = "Name") %>% 
            select(crawl_date, site_subdomain, type, shape_color, shape_fill, shape_nr, tooltip_info)
        }
        else{
          # print("no annotations")
          # print(gs_annotation_raw)
          gs_annotation <- gs_annotation_raw %>% 
            mutate(type = "manual research",
                   date = NA_Date_,
                   Name = NA_character_,
                   tooltip_info = NA_character_,
                   shape_color = NA,
                   shape_fill = NA,
                   shape_nr = NA) %>% 
            rename("crawl_date" = "date", "site_subdomain" = "Name") %>% 
            select(crawl_date, site_subdomain, type, shape_color, shape_fill, shape_nr, tooltip_info) 
        }
        return(gs_annotation)
      }
      
      ### snippets data --------------------------------------------------------
      
      get_snippet_traces_data <- function(){
        
        df_return <- tbl(pool, "sites") %>% 
          inner_join(., tbl(pool, "snippets_2"), by = c("sha1" = "site")) %>% 
          filter(site == !!current_data$site_to_load, detected == 1) %>% 
          collect() %>% #View()
          mutate(subdomain = urltools::domain(url) %>% suffix_extract(.) %>% select(subdomain) %>% pull(.),
                 site_subdomain = paste(site, subdomain, sep = "_") %>% as.character(.),
                 type = "traces via snippet detection",
                 shape_color = NA,
                 shape_nr = 23 %>% as.character(.),
                 tooltip_info = paste0("Snippet: ", snippet),
                 archive_url = paste0("http://web.archive.org/web/", str_remove_all(as.character(crawl_timestamp), "-|\\s|:"), "/", url)) %>% 
          mutate(shape_fill = cur_group_id(), .by = "snippet")

        return(df_return)
      }
      
      ### form data ------------------------------------------------------------
      
      get_form_finding_data <- function(){
        
        df_return <- tbl(pool, "sites") %>%
          inner_join(., tbl(pool, "findings_hashed_2")) %>%
          filter(site == !!current_data$site_to_load, iteration == 2) %>% 
          collect() %>% 
          mutate(subdomain = urltools::domain(url) %>% suffix_extract(.) %>% select(subdomain) %>% pull(.),
                 site_subdomain = paste(site, subdomain, sep = "_") %>% as.character(.),
                 type = "traces via form tags") %>%
          arrange(crawl_date) %>% 
          mutate(
            nr_unique_hashes = match(hashed_forms, unique(hashed_forms)), 
            # nr_unique_hashes = dense_rank(hashed_forms), .by = c(site, subdomain),
                 shape_color = nr_unique_hashes,
                 shape_fill = NA,
                 shape_nr = 19,
                 tooltip_info = paste0("formindex: ", nr_unique_hashes)) %>%  
          select(crawl_date, site_subdomain, type, shape_color, shape_fill, shape_nr, tooltip_info)
        # View(df_return)
        # print(head(df_return))
        return(df_return)
      }
      
      # calc height ------------------------------------------------------------
      
      get_height <- reactive({
       # print("calc height")
       nr_snippets <- as.numeric(current_data$snippet_data %>% select(site_subdomain) %>% distinct() %>% nrow())
       nr_forms <- as.numeric(current_data$form_data %>% select(site_subdomain) %>% distinct() %>% nrow())
       has_annotation <- ifelse(current_data$annotation %>% nrow() > 0, 1, 0)
       has_snippets <- ifelse(nr_snippets > 0, 1, 0)
       has_forms <- ifelse(nr_forms > 0, 1, 0)
       
       nr_facets <- as.numeric(has_annotation + has_snippets + has_forms)
       nr_rows <- as.numeric(has_annotation + nr_snippets + nr_forms)
       
       # facethöhe: hier fließt die überschrift und die x-achse als entscheidende größen ein. x-achse gibts für alle facets nur einmal 
       minheight_one_facet = 1
       # jede datenreihe braucht ein bisschen platz, aber keine neue überschrift
       minheight_one_row = 0.5
       
       sum_height <- minheight_one_facet + (minheight_one_facet * nr_facets) + minheight_one_row + (minheight_one_row * nr_rows)
       # print(sum_height)
       
       return_value <- sum_height
       
     })

      # print the viz --------------------------------------------------------------
      
      print_form_findings <- reactive({
        
        unique_colors <- c("")
        if (nrow(current_data$form_data) > 0 & !is.null(nrow(current_data$form_data))){
          unique_colors <- current_data$form_data %>% 
            # select() %>% 
            distinct(shape_color) %>% nrow() %>% rainbow(.)
          }
        
        color_values <- current_data$form_data %>% 
          select(shape_color) %>% 
          distinct() %>% 
          arrange(shape_color) %>% 
          bind_cols(unique_colors) %>% 
          add_row(shape_color = 9999999, "...2" = "#000000") %>% deframe()
        
        fill_values <- current_data$snippet_data %>% 
          select(shape_fill) %>% 
          distinct() %>% nrow()
        
        df_plot  <-  current_data$form_data %>% 
          # bind_rows(., current_data$snippet_data) %>% 
          bind_rows(., current_data$annotation) %>% 
          mutate(shape_nr = as.character(shape_nr))
        
        shape_values <- df_plot %>% 
          # bind_rows(., current_data$snippet_data) %>% 
          select(shape_nr) %>% 
          distinct() %>% 
          mutate(value = as.numeric(shape_nr)) %>% deframe()
        
        plot <- ggplot() +
          geom_point_interactive(data = df_plot, aes(x = crawl_date, y = site_subdomain, color =  as.character(shape_color), tooltip = paste0(tooltip_info, "\nDatum: ", crawl_date)), size = 3, alpha = 0.8, shape = 19) +
          geom_point_interactive(data = current_data$snippet_data, aes(x = crawl_date, y = site_subdomain, fill = as.character(shape_fill), tooltip = paste0(tooltip_info, "\nDatum: ", crawl_date)), shape = 23, size = 4, stroke = 0.1, color = "white") +
          scale_x_date(date_breaks = "year", date_labels = "%Y", limits = c(ymd("1997-01-01"), ymd("2021-01-01"))) +
          scale_color_manual(values = color_values) +
          scale_fill_manual(values = MetBrewer::met.brewer("Cross", fill_values)) +
          theme_b03_base + theme_b03_facets_individual + theme(legend.position = "none") + 
          facet_col(vars(type), scales = "free_y", space = "free") +
          # facet_grid(row = vars(type), scales = "free_y", space = "free_y", switch = "y") +
          # facet_wrap(~type, ncol = 1, scales = "free_y") +
          coord_cartesian(clip = "off")
        
        
        girafe(ggobj = plot,
               options = list(opts_sizing(rescale = FALSE)),
               width_svg = 15,
               height_svg = current_data$height)
      })
      
      # observe Events ----------------------------------
      
      current_data <- reactiveValues()
      
      rebuild_df <- function(){
        current_data$site_to_load = site_to_load()
        current_data$annotation = get_gs_annoted_data(sphere())
        current_data$snippet_data = get_snippet_traces_data()
        current_data$form_data = get_form_finding_data()
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
  