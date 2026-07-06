library(DT)

formFindingsTableUI <- function(id) {
  ns <- NS(id)
  # tagList(reactableOutput(ns("tableFindings")))
  # DT::dataTableOutput(ns("tableFindings"))
  tagList(DT::dataTableOutput(ns("tableFindings")))
}

formFindingsTableServer <- function(id, tab_, site_to_load, sphere) {
  
  stopifnot(is.reactive(site_to_load))
  
  moduleServer(
    id,
    function(input, output, session) {
      initLoad <- TRUE
      ## get the data ----------------------------------------------------------
      
      
      get_traces_data <- function(sphere_){
      
        # print("building table data")
        
        df_ids <- tbl(pool, "context_hashed") %>%
          filter(iteration == "monthly_whitelist", sphere == sphere_) %>% 
          select(agg_id, hashed_context) |> 
          arrange(agg_id) |> 
          collect() |> 
          mutate(site_subdomain = str_remove(agg_id, "\\d{4,4}-\\d{2,2}-\\d{2,2}_"),
                 site = str_remove(site_subdomain, "_.*$")) |> 
          filter(site == current_data$site_to_load) |> 
          mutate(nr_unique_hashes = match(hashed_context, unique(hashed_context))) |> 
          reframe(agg_id = first(agg_id), .by = c(nr_unique_hashes, hashed_context, site)) |> 
          select(-hashed_context)
        
        # View(df_ids)
        
        df_archived_sites <- tbl(pool, "sites") %>%
          filter(site == !!current_data$site_to_load) %>%
          left_join(., tbl(pool, "traces_context")) %>%
          filter(id_date_site_subdomain %in% df_ids$agg_id) |> 
          # left_join(., tbl(pool, "context_hashed"), by = c("id_date_site_subdomain" = "agg_id")) |> 
          select(crawl_date, crawl_timestamp, site, url, id_date_site_subdomain) |> 
          distinct() |> 
          collect() %>%
          mutate(archive_url = paste0("http://web.archive.org/web/", str_remove_all(as.character(crawl_timestamp), "-|\\s|:"), "/", url)) %>%
          arrange(crawl_date) |>
          select(id_date_site_subdomain, archive_url) |> 
          reframe(archive_url = first(archive_url), .by = c("id_date_site_subdomain")) # |>
          
        
        df_table <- tbl(pool, "traces_context") %>%
          select(id_date_site_subdomain, tag, attr, value, value_cleaned_whitelist) |>
          filter(id_date_site_subdomain %in% df_ids$agg_id) |>
          distinct() |> 
          collect() %>% #View()
          mutate(site_subdomain = str_remove(id_date_site_subdomain, "\\d{4,4}-\\d{2,2}-\\d{2,2}_"),
                 site = str_remove(site_subdomain, "_.*$"),
                 crawl_date = str_extract(id_date_site_subdomain, "\\d{4,4}-\\d{2,2}-\\d{2,2}"),
                 crawl_date = ymd(crawl_date),
                 type = "traces via tag findings") %>%
          left_join(., df_ids, by = c("id_date_site_subdomain" = "agg_id")) |> 
          arrange(crawl_date) |>
          select(nr_unique_hashes, tag, attr, value_cleaned_whitelist, id_date_site_subdomain) |>
          distinct() |> 
          left_join(df_archived_sites) |> 
          mutate(archive_url = paste0('<a href= "', archive_url,'"  target="_blank">Link ins Archive</a>')) %>% 
          select(-id_date_site_subdomain)#, archive_url) # %>%
        
        # View(df_table)
        # print("finished building table data")
        
        return(df_table)
      }
      
      # build table --------------------------------------------------------------
      
      get_colors <- reactive({
        # print(paste0("changed ", structure(current_data$traces_data)))
        # print(current_data$site_to_load)    
        unique_colors <- current_data$traces_data %>% 
          select(nr_unique_hashes) %>% 
          distinct() %>% nrow() %>% rainbow(.)
        
        colors <- current_data$traces_data %>% 
          select(nr_unique_hashes) %>% 
          distinct() %>% 
          arrange(nr_unique_hashes) %>% 
          bind_cols(unique_colors) %>% #deframe()
          rename("hex" = `...2`)
        
        # print(colors)
        return(colors)
      })
      
      get_table <- reactive({
        if (!is.null(nrow(current_data$traces_data))){
            if(nrow(current_data$traces_data) > 0) {
              datatable(current_data$traces_data, rownames= FALSE, filter = "top", escape=F, colnames=c("tagindex", "tag", "attr", "value cleaned", "archive link (excample)"),
                      options = list(order = list(0, 'asc'))
                      ) %>%
                            formatStyle(columns = "nr_unique_hashes",
                                        target = "cell",
                                        backgroundColor = styleEqual(levels = current_data$colors["nr_unique_hashes"] %>% pull(.), values = current_data$colors["hex"] %>% pull(.))
                            )
          } else {
            DT::datatable(current_data$traces_data, rownames= FALSE, filter = "top", escape=F, colnames=c("tagindex", "tag", "attr", "value cleaned", "archive link  (excample)"),
          options = list(order = list(0, 'asc'))
            )
          }
            }
          })

      current_data <- reactiveValues()
      
      rebuilt_data <- function(){
        current_data$test = penguins
        current_data$traces_data = get_traces_data(sphere())
        current_data$colors = get_colors()
      }
      
      observeEvent(tab_(),{
        # print(paste0("tab 2 snippet table data: tab loaded: ", tab_()))
        if(tab_() == "tab_2"){
          rebuilt_data()
        }
      })
      
      observeEvent(site_to_load(),{
        # print(paste0("2 tab, 4 viz, snippet findingsTableServer ", site_to_load()))
        current_data$site_to_load = site_to_load()
        if(tab_() == "tab_2"){
          rebuilt_data()
          }
        })
      
      output$tableFindings <- DT::renderDataTable({get_table()})
    })
  }
  