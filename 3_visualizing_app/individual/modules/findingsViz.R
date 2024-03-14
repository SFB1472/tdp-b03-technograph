library(ggiraph, googlesheets4, "MetBrewer")

findingsVizUI <- function(id) {
  ns <- NS(id)
  tagList(
    addGFontHtmlDependency(family = c("Roboto Mono")),
    girafeOutput(ns("selectedSite"))
  )
}

findingsVizServer <- function(id, site_to_load, sphere) {
  
  stopifnot(is.reactive(site_to_load))
  # print(paste0("xxxxxxxxxxx ", sphere))
  moduleServer(
    id,
    function(input, output, session) {
      
      ## get the data ----------------------------------------------------------
      
      ### google doc data -------------------------------------------------------
      
      ANNOTATION_IS_EMPTY <- TRUE
      
      check_gs_empty <- function(gs){
        ANNOTATION_IS_EMPTY <<- gs %>% nrow() == 0
      }
      # gs4_auth(cache=here::here(".secrets"), email = TRUE)
      # drive_auth(cache = ".secrets", email = TRUE)
      
      
      get_gs_annoted_data <- function(sphere){
        
        print(sphere)
        googlesheets4::gs4_deauth()
        gs_annotation_raw <- googlesheets4::read_sheet(SPREADSHEET_ANNOTATION_DATA, sheet = SPREADSHEET_ANNOTATION[[{{sphere}}]]) %>% 
          filter(site == !!current_data$site_to_load)
        print("sheet loaded")
        check_gs_empty(gs_annotation_raw)
        if(!ANNOTATION_IS_EMPTY) {
          gs_annotation <- gs_annotation_raw %>% 
            filter(site == !!current_data$site_to_load, tooltip_info != "") %>% 
            mutate(type = "manual research",
                   date = paste0(year, "-", ifelse(is.na(month), "1", month), "-", ifelse(is.na(day), "1", day)) %>% ymd(.),
                   # technology = ifelse(is.na(technology), "other", technology),
                   shape_color = 9999999,
                   shape_fill = NA_character_,
                   shape_nr = 19
                   ) %>% 
            rename("crawl_date" = "date", "site_subdomain" = "Name") %>% 
            select(crawl_date, site_subdomain, type, shape_color, shape_fill, shape_nr, tooltip_info)
        }
        else{
          print("no annotations")
          # print(gs_annotation_raw)
          gs_annotation <- gs_annotation_raw %>% 
            mutate(type = "manual research",
                   date = NA_Date_,
                   Name = NA_character_,
                   tooltip_info = NA_character_,
                   shape_color = NA,
                   shape_fill = NA_character_,
                   shape_nr = NA) %>% 
            rename("crawl_date" = "date", "site_subdomain" = "Name") %>% 
            select(crawl_date, site_subdomain, type, shape_color, shape_fill, shape_nr, tooltip_info) 
        }
        return(gs_annotation)
      }
      
      ### snippets data --------------------------------------------------------
      
      get_snippet_traces_data <- function(){
        # db_sites <- tbl(pool, "sites")
        # db_snippet_traces <- tbl(pool, "snippets_2")
        
        df_return <- tbl(pool, "sites") %>% 
          inner_join(., tbl(pool, "snippets_2"), by = c("sha1" = "site")) %>% 
          filter(site == !!current_data$site_to_load, detected == 1) %>% 
          collect() %>% #View()
          mutate(subdomain = urltools::domain(url) %>% suffix_extract(.) %>% select(subdomain) %>% pull(.),
                 site_subdomain = paste(site, subdomain, sep = "_") %>% as.character(.),
                 type = "traces via snippet detection",
                 shape_color = 8888888888,
                 shape_fill = NA_character_,
                 shape_nr = 18,
                 tooltip_info = paste0("Snippet: ", snippet)) #%>% 

        # View(df_return)
        return(df_return)
      }
      
      ### form data ------------------------------------------------------------
      
      get_form_finding_data <- function(){
        # db_sites <- tbl(pool, "sites")
        # db_findings_hashed <- tbl(pool, "findings_hashed_2")
        
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
                 shape_fill = NA_character_,
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
        # print(paste0("create plot ", current_data$site_to_load))
        # View(current_data$form_data)
        
        unique_colors <- current_data$form_data %>% 
          select(shape_color) %>% 
          distinct() %>% nrow() %>% rainbow(.)
        
        color_values <- current_data$form_data %>% 
          select(shape_color) %>% 
          distinct() %>% 
          arrange(shape_color) %>% 
          bind_cols(unique_colors) %>% 
          add_row(shape_color = 9999999, "...2" = "#000000") %>% deframe()
        
        # print(color_values)
        
        df_plot  <-  current_data$form_data %>% 
          bind_rows(., current_data$snippet_data) %>% 
          bind_rows(., current_data$annotation) %>% 
          mutate(shape_nr = as.character(shape_nr))
        
        shape_values <- df_plot %>% 
          select(shape_nr) %>% 
          distinct() %>% 
          mutate(value = as.numeric(shape_nr)) %>% deframe()
        
        plot <- ggplot() +
          geom_point_interactive(data = df_plot, aes(x = crawl_date, y = site_subdomain, color =  as.character(shape_color), shape = shape_nr, fill = shape_fill, tooltip = paste0(tooltip_info, "\nDatum: ", crawl_date)), size = 3, alpha = 0.8) +
          # geom_point_interactive(data = current_data$snippet_data, aes(x = crawl_date, y = site_subdomain, fill = snippet, tooltip = paste0("Snippet: ", snippet, "\nDatum: ", crawl_date)), shape = 21) +
          # geom_point_interactive(data = current_data$form_data, aes(x = crawl_date, y = site_subdomain, color = as.character(nr_unique_hashes), tooltip = paste0("formindex: ", nr_unique_hashes, "\nDatum: ", crawl_date))) +
          # geom_point_interactive(data = current_data$annotation, aes(x = crawl_date, y = site_subdomain, tooltip = tooltip_info), color = "#000000") +
          scale_x_date(date_breaks = "year", date_labels = "%Y", limits = c(ymd("1997-01-01"), ymd("2021-01-01"))) +
          # scale_x_continuous(breaks = year_breaks_for_plotting, labels = year_breaks_for_plotting,  expand = c(0, NA), name = "crawl year") +#, limits = year_breaks) +
          scale_shape_manual(values = shape_values) +
          scale_color_manual(values = color_values) +
          scale_fill_manual(values = MetBrewer::met.brewer("Hokusai2", type="discrete")) +
          theme_b03_base + theme_b03_facets_individual + theme(legend.position = "none") + 
          facet_wrap(~type, ncol = 1, scales = "free_y") +
          coord_cartesian(clip = "off")
        # plot
        
        girafe(ggobj = plot,
               options = list(opts_sizing(rescale = FALSE)),
               width_svg = 15,
               height_svg = current_data$height)

      })
      
      # current_sphere <- reactiveValues()
      
      # observeEvent(sphere(),{
      #   print(paste0("sphere to load: ", sphere()))
      #   current_sphere$sphere = sphere()
      # })
      
      current_data <- reactiveValues()
      
      observeEvent(site_to_load(),{
        # print(paste0("findingsVizServer ", site_to_load()))
        current_data$site_to_load = site_to_load()
        current_data$annotation = get_gs_annoted_data(sphere)
        current_data$snippet_data = get_snippet_traces_data()
        current_data$form_data = get_form_finding_data()
        current_data$height = get_height()
      })
      
      output$selectedSite <- renderGirafe({print_form_findings()})
    })
  }
  