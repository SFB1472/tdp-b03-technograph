library(ggiraph, "MetBrewer")

tab_5_compareHashesUI <- function(id) {
  ns <- NS(id)
  tagList(
    addGFontHtmlDependency(family = c("Roboto Mono")),
    DT::dataTableOutput(ns("comparingTable"))
  )
}

tab_5_compareHashesServer <- function(id, tab_, subdomain, hash_1, hash_2) {
  
  stopifnot(is.reactive(hash_1))
  # print(paste0("xxxxxxxxxxx ", sphere))
  moduleServer(
    id,
    function(input, output, session) {
      # initLoad <- TRUE
      # initLoad <- FALSE
      
      all_values_set <- function(){
        nr_values_needed <- 3
        values_counted <- 0
        values_counted <- ifelse(!is.null(current_data$subdomain), values_counted + 1, values_counted)
        values_counted <- ifelse(!is.null(current_data$hash_1), values_counted + 1, values_counted)
        values_counted <- ifelse(!is.null(current_data$hash_2), values_counted + 1, values_counted)
        
        return_value <- ifelse(values_counted == nr_values_needed, TRUE, FALSE)
        print(paste0("all values set? ", values_counted))
        return(return_value)
      }
      
      ## get the data ----------------------------------------------------------
      
      ### hash data ------------------------------------------------------------
      get_hash_data <- function(hash_){
        site_ = str_extract(current_data$subdomain, "^\\w{1,}_") %>% str_remove(., "_")
        print(paste0("site ", site_))
        
        if(!is.null(hash_)){
        
          id_to_filter <- tbl(pool, "sites") %>%
            filter(site == site_) %>% 
            inner_join(., tbl(pool, "context_hashed")) %>%
            filter(iteration == "most_pragmatic") %>% 
            select(sha1, site, url, crawl_date, id_sha1_group, hashed_context) %>% 
            collect() %>% 
            mutate(subdomain = urltools::domain(url) %>% suffix_extract(.) %>% select(subdomain) %>% pull(.),
                   site_subdomain = paste(site, subdomain, sep = "_") %>% as.character(.)) %>%
            arrange(crawl_date) %>% 
            mutate(
              nr_unique_hashes = match(hashed_context, unique(hashed_context))) %>% 
            filter(site_subdomain == !!current_data$subdomain, nr_unique_hashes == !!hash_) %>% 
            head(1) %>% select(id_sha1_group) %>% pull(.)
            # left_join(., df_context_info) #%>%
            
          df_hash_readable <- tbl(pool, "sites") %>%
            inner_join(., tbl(pool, "tag_context")) %>% 
            filter(id_sha1_group == id_to_filter) %>% 
            # group_by(id_sha1_group) %>% 
            # mutate(group_rows = row_number()) %>% 
            # filter(group_rows == 1) %>% 
            select(tag, attr, value, context_path) %>% 
            collect()
          
          View(df_hash_readable)
          return(df_hash_readable)
        } 
      }
      
      build_compare_table <- function(){
        
        df_table <- current_data$hash_1_table %>% 
          anti_join(., current_data$hash_2_table)
        
      }
      
      # observe Events ----------------------------------
      
      current_data <- reactiveValues()
      
      rebuild_df <- function(){
        current_data$subdomain = subdomain()
        current_data$hash_1 = hash_1()
        current_data$hash_2 = hash_2()
        current_data$hash_1_table = get_hash_data(current_data$hash_1)
        current_data$hash_2_table = get_hash_data(current_data$hash_2)
        current_data$compare_table = build_compare_table()
        # current_data$context_data = get_context_data()
        # current_data$height = get_height()
      }
      
      observeEvent(hash_1(),{
        print(paste0("tab 5 hash 1 fired ", hash_1()))
        current_data$hash_1 = hash_1()
        if(all_values_set()){
          rebuild_df()
        }
        # initLoad <<- FALSE
      })
      
      observeEvent(hash_2(),{
        print(paste0("tab 5 hash 2 fired ", hash_2()))
        current_data$hash_2 = hash_2()
        if(all_values_set()){
          rebuild_df()
        }
        # initLoad <<- FALSE
      })
      
      observeEvent(subdomain(),{
        print(paste0("tab 5 subdomain fired ", subdomain()))
        current_data$subdomain = subdomain()
        if(all_values_set()){
          rebuild_df()
        }
        # initLoad <<- FALSE
      })
      
      observeEvent(tab_(),{
        print(paste0("tab 5 compare hashes: tab loaded: ", tab_()))
        # current_tab$tab = tab_()
        if(tab_() == "tab_5"){
          # print(paste0("init load ", initLoad))
          # rebuild_df()
        }
      })
      
      
     
      output$comparingTable <- renderDataTable({
        # print(paste0("form findings table ", is.null((current_data$form_data)) ))
        if (!is.null(current_data$compare_table)){
          # if(nrow(current_data$form_data) > 0) {
            datatable(current_data$compare_table, rownames= FALSE, filter = "top", escape=F#, colnames=c("formindex", "name", "attr", "text", "archive link")
                      )
            # ) 
      # %>%
      #         formatStyle(columns = "nr_unique_hashes", 
      #                     target = "cell", 
      #                     backgroundColor = styleEqual(levels = current_data$colors["nr_unique_hashes"] %>% pull(.), values = current_data$colors["hex"] %>% pull(.))
      #         )
          # } else {
          #   datatable(current_data$form_data, rownames= FALSE, filter = "top", escape=F, colnames=c("formindex", "name", "attr", "text", "archive link"),
          #             options = list(order = list(0, 'asc'))
          #   )
          # }
        }
      })
    })
  }
  