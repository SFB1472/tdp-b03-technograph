library(DT)

findingsTableUI <- function(id) {
  ns <- NS(id)
  tagList(
    DT::dataTableOutput(ns("tableFindings"))
  )
}

findingsTableServer <- function(id, tab_, site_to_load) {
  
  stopifnot(is.reactive(site_to_load))
  
  moduleServer(
    id,
    function(input, output, session) {
      
      current_data <- reactiveValues()
      
      rebuilt_data <- function(){
        current_data$form_data = get_form_finding_data()
        current_data$colors = get_colors()
      }
      
      observeEvent(tab_(),{
        print(paste0("tab 2 tabledata: tab loaded: ", tab_()))
        if(tab_() == "tab_2"){
          rebuilt_data()
        }
      })
      
      observeEvent(site_to_load(),{
        print(paste0("findingsTableServer ", site_to_load()))
        current_data$site_to_load = site_to_load()
        if(tab_() == "tab_2"){
          rebuilt_data()
        }
      })
      
      ## get the data ----------------------------------------------------------
      
      get_form_finding_data <- function(){
        
        df_table <- tbl(pool, "sites") %>%
          inner_join(., tbl(pool, "findings_hashed_2")) %>%
          filter(site == !!current_data$site_to_load, iteration == 2) %>% 
          collect()
        
        df_sites <- tbl(pool, "sites") %>%
          # inner_join(., db_findings_hashed) %>%
          filter(site == !!current_data$site_to_load) %>% 
          collect() %>% 
          mutate(archive_url = paste0("http://web.archive.org/web/", str_remove_all(as.character(crawl_timestamp), "-|\\s|:"), "/", url),
                 empty = "") %>% 
          arrange(crawl_date)
        
        df_table_filled_ <- df_table %>% 
            mutate(subdomain = urltools::domain(url) %>% suffix_extract(.) %>% select(subdomain) %>% pull(.),
                   site_subdomain = paste(site, subdomain, sep = "_"),
                   type = "traces via form tags") %>%
            arrange(crawl_date) %>% 
            mutate(
              # nr_unique_hashes = dense_rank(hashed_forms), .by = c(site, subdomain)
              nr_unique_hashes = match(hashed_forms, unique(hashed_forms))
              ) 
          
          df_table_filled <- df_table_filled_ %>% 
            reframe(first_crawl_date = min(crawl_date), first_id_sha1_form_group = first(id_sha1_form_group), first_url = first(url), .by = c("site", "hashed_forms", "nr_unique_hashes")) #%>% View()
          
          ids_to_look_at <- df_table_filled %>% select(first_id_sha1_form_group) %>% pull(.)
          
          df_return_table <- tbl(pool, "tag_context_2") %>% 
            filter(id_sha1_form_group %in% ids_to_look_at) %>% 
            collect() %>% 
            left_join(., df_table_filled, by = c("id_sha1_form_group" = "first_id_sha1_form_group")) %>% 
            arrange(id_sha1_form_group, nr_unique_hashes) %>% 
            mutate(
              first_crawl_date = as.character(first_crawl_date) %>% str_remove_all(., "-"),
              archive_url = paste0('<a href= "http://web.archive.org/web/', first_crawl_date, '/', first_url,'"  target="_blank">Link ins Archive</a>')) %>% 
            select(nr_unique_hashes, name, attr, text, archive_url) # %>% 
            # rename("Kommentar- index" = "nr_unique_hashes")
          
        return(df_return_table)
      }
      
      # print the viz --------------------------------------------------------------
      
      get_colors <- reactive({
        # print("recalc colors")
        # print(current_data$site_to_load)    
        unique_colors <- current_data$form_data %>% 
          select(nr_unique_hashes) %>% 
          distinct() %>% nrow() %>% rainbow(.)
        
        colors <- current_data$form_data %>% 
          select(nr_unique_hashes) %>% 
          distinct() %>% 
          arrange(nr_unique_hashes) %>% 
          bind_cols(unique_colors) %>% #deframe()
          rename("hex" = `...2`)
        
        # print(colors)
        return(colors)
      })
      
      output$tableFindings <- renderDataTable({
        
        if (nrow(current_data$form_data) >0)
        {
          datatable(current_data$form_data, rownames= FALSE, filter = "top", escape=F, colnames=c("formindex", "name", "attr", "text", "archive link"),
                    options = list(order = list(0, 'asc'))
                    ) %>%
                          formatStyle(columns = "nr_unique_hashes", 
                                      target = "cell", 
                                      backgroundColor = styleEqual(levels = current_data$colors["nr_unique_hashes"] %>% pull(.), values = current_data$colors["hex"] %>% pull(.))
                          )
        } else {
          datatable(current_data$form_data, rownames= FALSE, filter = "top", escape=F, colnames=c("formindex", "name", "attr", "text", "archive link"),
                    options = list(order = list(0, 'asc'))
          )
        }
        })
    })
  }
  