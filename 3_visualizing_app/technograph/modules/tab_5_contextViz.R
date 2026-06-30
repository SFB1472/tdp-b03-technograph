library(ggiraph, "MetBrewer")

tab_5_contextVizUI <- function(id) {
  ns <- NS(id)
  tagList(
    addGFontHtmlDependency(family = c("Roboto Mono")),
    girafeOutput(ns("contextViz"))
  )
}

tab_5_contextVizServer <- function(id, tab_, site_to_load, sphere) {
  
  stopifnot(is.reactive(site_to_load))
  # print(paste0("xxxxxxxxxxx ", sphere))
  moduleServer(
    id,
    function(input, output, session) {
      initLoad <- TRUE
      # initLoad <- FALSE
      
      ## get the data ----------------------------------------------------------
      
      
      ### context data ------------------------------------------------------------
      
      
      get_context_data <- function(){
        
        # df_context_info <- tbl(pool, "sites") %>%
        #   inner_join(., tbl(pool, "tag_context")) %>% 
        #   filter(site == !!current_data$site_to_load) %>% 
        #   select(sha1, id_sha1_group, context_path_cleaned, context_path) %>%
        #   distinct() %>% 
        #   collect()
        df_context_info <- tbl(pool, "sites") %>%
          inner_join(., tbl(pool, "traces_context")) %>%
          filter(site == !!current_data$site_to_load) %>%
          window_order(crawl_timestamp) %>%
          mutate(
            group_size = n(), .by = id_sha1_group
            ) %>%
          # ungroup() %>%
          # filter(group_rows == 1) %>%
          select(sha1, id_sha1_group, context_path_cleaned, context_path, group_size) %>%
          distinct() %>% 
          collect() #%>%
        
        # View(df_context_info)
        
        df_return <- tbl(pool, "sites") %>%
          filter(site == !!current_data$site_to_load) %>% 
          inner_join(., tbl(pool, "context_hashed")) %>%
          filter(iteration == "cleaned_whitelist") %>% 
          select(sha1, site, url, crawl_date, agg_id, hashed_context) %>% 
          collect() %>% 
          left_join(., df_context_info) %>% 
          mutate(subdomain = urltools::domain(url) %>% suffix_extract(.) %>% select(subdomain) %>% pull(.),
                 site_subdomain = paste(site, subdomain, sep = "_") %>% as.character(.)) %>%
          arrange(crawl_date) %>% 
          mutate(
            nr_unique_hashes = match(hashed_context, unique(hashed_context)), 
            shape_color = nr_unique_hashes,
            tooltip_info = paste0("context: ", context_path, " hashnr: ", nr_unique_hashes, " umfang: " + group_size)
            ) %>%  
          select(crawl_date, site_subdomain, hashed_context, context_path, nr_unique_hashes, id_sha1_group, context_path_cleaned, group_size, tooltip_info)
          # select(crawl_date, site_subdomain, hashed_context, context_path, nr_unique_hashes, id_sha1_group, context_path_cleaned, tooltip_info)
        
        # View(df_return)
        return(df_return)
      }
      
      
      # get archivied sites ----------------------------------------------------
      
      get_sites <- function(){
        # print("1 tab, 2 vis, aggregating data for printing missing archived data")
        
        db_sites <- tbl(pool, "sites") %>% 
          filter(site == !!current_data$site_to_load) %>% 
          select(site, crawl_date, sha1, url) %>% 
          distinct() %>% 
          collect() %>% 
          mutate(subdomain = urltools::domain(url) %>% suffix_extract(.) %>% select(subdomain) %>% pull(.),
                 site_subdomain = paste(site, subdomain, sep = "_") %>% as.character(.)) %>% 
          select(crawl_date, sha1, site_subdomain) %>% distinct()

        # day <- df_timespan_day %>% mutate(year = year(value)) %>% select(year) %>% pull(.)
        # year <- df_timespan_year %>% select(value) %>% pull(.)
        site <- tbl(pool, "sites") %>%
          filter(site == !!current_data$site_to_load) %>%
          distinct(site, url) %>%
          collect() %>% #View()
          mutate(subdomain = urltools::domain(url) %>% suffix_extract(.) %>% select(subdomain) %>% pull(.),
                 site_subdomain = paste(site, subdomain, sep = "_") %>% as.character(.)) %>% 
          select(site_subdomain) %>% 
          distinct() 
        
        # str(db_sites)
        # str(site)
        
        df_na_sites <- tidyr::expand_grid(df_timespan_day, site) %>%
          left_join(., db_sites, by = c("value" = "crawl_date", "site_subdomain")) %>% #View()
          filter(is.na(sha1)) %>%
          mutate(sha1 = "no sites archived") %>% 
          rename(crawl_date = value)
        
        # View(df_na_sites)
        return(df_na_sites)
      }
      
      # calc height ------------------------------------------------------------
      
      get_height <- reactive({
       # print("calc height")
       # nr_snippets <- as.numeric(current_data$snippet_data %>% select(site_subdomain) %>% distinct() %>% nrow())
       nr_site_subdomain <- as.numeric(current_data$context_data %>% select(site_subdomain) %>% distinct() %>% nrow())
       nr_forms <- as.numeric(current_data$context_data %>% select(context_path_cleaned) %>% distinct() %>% nrow())
       has_sites_subdomains <- ifelse(nr_site_subdomain > 0, 1, 0)
       # has_snippets <- ifelse(nr_snippets > 0, 1, 0)
       
       has_forms <- ifelse(nr_forms > 0, 1, 0)
       
       nr_facets <- as.numeric(has_sites_subdomains + has_forms)
       nr_rows <- as.numeric(nr_site_subdomain + nr_forms)
       
       # facethöhe: hier fließt die überschrift und die x-achse als entscheidende größen ein. x-achse gibts für alle facets nur einmal 
       minheight_one_facet = 1
       # jede datenreihe braucht ein bisschen platz, aber keine neue überschrift
       minheight_one_row = 0.2
       
       # sum_height <- minheight_one_facet + (minheight_one_facet * nr_facets) + minheight_one_row + (minheight_one_row * nr_rows)
       sum_height <- minheight_one_facet + minheight_one_row + (minheight_one_row * nr_rows)
       # print(sum_height)
       
       return_value <- sum_height
       
     })

      # print the viz --------------------------------------------------------------
      
      print_context_findings <- reactive({
        print("viz aufgerufen")
        # View(current_data$context_data)
        if(!is.null(current_data$context_data)) {
          pathes <- current_data$context_data %>% select(context_path_cleaned) %>% distinct()
          
          df_na <- current_data$sites %>% 
            filter(site_subdomain %in% current_data$context_data$site_subdomain) %>% 
            expand_grid(., pathes) #%>% 
          
          # View(df_na)
          
          plot <- ggplot() +
          # plot <- ggplot(data = current_data$context_data, aes(x = crawl_date, y = context_path_cleaned, color = as.character(nr_unique_hashes), tooltip = paste0(tooltip_info, "\nDatum: ", crawl_date))) +
            geom_point(data = df_na, aes(x = crawl_date, y = context_path_cleaned), color = "grey90", size=.2, shape = 20) +
            geom_point_interactive(data = current_data$context_data, aes(x = crawl_date, y = context_path_cleaned, color = as.character(nr_unique_hashes), size = as.numeric(group_size), tooltip = paste0(tooltip_info, "\nDatum: ", crawl_date))) +
            scale_x_date(date_breaks = "year", date_labels = "%Y", limits = c(ymd("1997-01-01"), ymd("2021-01-01"))) +
            scale_size_area(max_size = 6) +
            theme_b03_base + theme_b03_facets_individual + theme(legend.position = "none", axis.text.y = element_blank()) + 
            facet_col(vars(site_subdomain), scales = "free_y", space = "free") +
            coord_cartesian(clip = "off")
          
          girafe(ggobj = plot,
                 options = list(opts_sizing(rescale = FALSE)),
                 width_svg = 15,
                 height_svg = current_data$height
                 # height_svg = 20
        )
        }
      })
      
      # observe Events ----------------------------------
      
      current_data <- reactiveValues()
      
      rebuild_df <- function(){
        current_data$site_to_load = site_to_load()
        current_data$sites = get_sites()
        # current_data$snippet_data = get_snippet_traces_data()
        current_data$context_data = get_context_data()
        current_data$height = get_height()
      }
      
      observeEvent(site_to_load(),{
        print(paste0("tab 5 site to load fired ", site_to_load(), " first load? ", initLoad))
        current_data$site_to_load = site_to_load()
        if(!initLoad){
          rebuild_df()
        }
        initLoad <<- FALSE
      })
      
      observeEvent(tab_(),{
        print(paste0("tab 5 visualization: tab loaded: ", tab_()))
        # current_tab$tab = tab_()
        if(tab_() == "tab_5"){
          # print(paste0("init load ", initLoad))
          rebuild_df()
        }
      })
      
      
     
      output$contextViz <- renderGirafe({print_context_findings()})
    })
  }
  