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
      
      ### context data ------------------------------------------------------------
      
      find_dup <- function(string, thres) {
        purrr::map_lgl(seq_along(string), function(i) {
          sim <- stringdist::stringsim(string[i], string[0:(i - 1)], method = "lcs") #  method = "dl" -> schlecht
          any(sim > thres)
        })
      }
      
      get_context_data <- function(){
        
        # df_context_info_view <- tbl(pool, "sites") %>%
        #   filter(site == !!current_data$site_to_load) %>%
        #   inner_join(., tbl(pool, "tag_context")) %>% 
        #   inner_join(., tbl(pool, "context_hashed")) %>% 
        #   collect()
        
        df_context_info <- tbl(pool, "sites") %>%
          inner_join(., tbl(pool, "tag_context")) %>% 
          filter(site == !!current_data$site_to_load) %>% 
          group_by(id_sha1_group) %>%
          # arrange(., .by_group = TRUE) %>% 
          window_order(tag, attr) %>% 
          mutate(group_rows = row_number()) %>%
          ungroup() %>%
          filter(group_rows == 1) %>%
          select(sha1, id_sha1_group, tag, attr, value_cleaned, context_path) %>% 
          collect()
        
        View(df_context_info)
        
        df_return <- tbl(pool, "sites") %>%
          filter(site == !!current_data$site_to_load) %>% 
          inner_join(., tbl(pool, "context_hashed")) %>%
          filter(iteration == "cleaned_whitelist") %>% 
          select(sha1, site, url, crawl_date, id_sha1_group, hashed_context) %>% 
          collect() %>% 
          left_join(., df_context_info) %>% 
          mutate(subdomain = urltools::domain(url) %>% suffix_extract(.) %>% select(subdomain) %>% pull(.),
                 site_subdomain = paste(site, subdomain, sep = "_") %>% as.character(.)) %>%
          arrange(crawl_date) %>% 
          mutate(
            nr_unique_hashes = match(hashed_context, unique(hashed_context)), 
            new_y = paste0(tag, "_", attr, "_", value_cleaned),
            # nr_unique_hashes = dense_rank(hashed_forms), .by = c(site, subdomain),
                 shape_color = nr_unique_hashes,
                 shape_fill = NA,
                 shape_nr = 19,
                 tooltip_info = paste0("tag: ", tag, " context: ", context_path, " hashnr: ", nr_unique_hashes)
            # context_path_to_hash = row_number(), .by = hashed_context
            ) %>%  
          select(crawl_date, site_subdomain, hashed_context, context_path, nr_unique_hashes,id_sha1_group,shape_color,new_y, shape_fill, shape_nr, tooltip_info)
        
        # df_context_pathes_counted <- df_return_ %>% 
        #   select(site_subdomain, hashed_context, context_path) %>% 
        #   distinct() %>% 
        #   reframe(counted_pathes = n(), .by = c(hashed_context, site_subdomain)) %>% #View()
        #   arrange(desc(counted_pathes)) %>% 
        #   mutate(hash_pathes_combi = paste0("context_path_", row_number()) )
        # 
        # df_context_pathes_counted_ <- df_return_ %>% 
        #   select(site_subdomain, hashed_context, context_path) %>% 
        #   distinct() %>%
        #   arrange(context_path) %>% 
        #   mutate(dup = find_dup(context_path, 0.87),
        #          group_ = ifelse(lead(dup) == TRUE & dup == FALSE, paste0("group_", row_number()), NA),
        #          group_ = ifelse(dup == FALSE & lag(dup) == TRUE, paste0("group_", row_number()), group_)#,
        #          
        #          # ifelse(dup == TRUE & lead(dup)==TRUE, lag(group_), NA))
        #   ) %>% #View()
        #   fill(group_) %>% 
        #   mutate(group_ = ifelse(is.na(group_), paste0("group_", row_number()), group_),
        #          group_ = ifelse(dup == FALSE & lag(dup) == FALSE, paste0("group_", row_number()), group_)) #%>% 
          # reframe(counted_pathes = n(), .by = group_)
        
        
        # hashes_context_path_combis <- df_return_ %>% 
        #   # select(hashed_context, context_path) %>% 
        #   # distinct() %>% 
        #   reframe(counted_combis = n(), .by = c(hashed_context, context_path))
        
        # df_return <- df_return_ %>% 
        #   left_join(., df_context_pathes_counted) %>%
        #   mutate(new_y = ifelse(counted_pathes > 1, hash_pathes_combi, context_path))
        
        # View(hashes_context_path_combis)
        # View(df_context_pathes_counted_)
        View(df_return)
        # print(head(df_return))
        return(df_return)
      }
      
      # calc height ------------------------------------------------------------
      
      get_height <- reactive({
       # print("calc height")
       # nr_snippets <- as.numeric(current_data$snippet_data %>% select(site_subdomain) %>% distinct() %>% nrow())
       nr_site_subdomain <- as.numeric(current_data$context_data %>% select(site_subdomain) %>% distinct() %>% nrow())
       nr_forms <- as.numeric(current_data$context_data %>% select(new_y) %>% distinct() %>% nrow())
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
          plot <- ggplot(data = current_data$context_data, aes(x = crawl_date, y = new_y, color = as.character(nr_unique_hashes), tooltip = paste0(tooltip_info, "\nDatum: ", crawl_date))) +
            geom_point_interactive() +
            scale_x_date(date_breaks = "year", date_labels = "%Y", limits = c(ymd("1997-01-01"), ymd("2021-01-01"))) +
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
        # current_data$annotation = get_gs_annoted_data(sphere())
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
  