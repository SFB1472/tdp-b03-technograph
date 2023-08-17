library(DT)

findingsTableUI <- function(id) {
  ns <- NS(id)
  tagList(
    DT::dataTableOutput(ns("tableFindings"))
  )
}

findingsTableServer <- function(id, site_to_load) {
  
  stopifnot(is.reactive(site_to_load))
  
  moduleServer(
    id,
    function(input, output, session) {
      
      current_data <- reactiveValues()
      
      observeEvent(site_to_load(),{
        # print("server findings viz")
        print(paste0("findingsTableServer ", site_to_load()))
        current_data$site_to_load = site_to_load()
        current_data$form_data = get_form_finding_data()
        current_data$colors = get_colors()
      })
      
      ## get the data ----------------------------------------------------------
      
      get_form_finding_data <- function(){
        db_sites <- tbl(pool, "sites")
        db_findings_hashed <- tbl(pool, "findings_hashed")
        db_tags_parsed <- tbl(pool, "tag_context")
        
        df_table <- db_sites %>%
          inner_join(., db_findings_hashed) %>%
          filter(site == !!current_data$site_to_load) %>% 
          collect()
        
        # if(df_table %>% nrow > 0){
          df_table_filled <- df_table %>% 
            mutate(subdomain = urltools::domain(url) %>% suffix_extract(.) %>% select(subdomain) %>% pull(.),
                   site_subdomain = paste(site, subdomain, sep = "_"),
                   type = "traces via form tags") %>%
            arrange(crawl_date) %>% 
            mutate(
              # nr_unique_hashes = dense_rank(hashed_forms), .by = c(site, subdomain)
              nr_unique_hashes = match(hashed_forms, unique(hashed_forms))
              ) %>% #View()
            reframe(first_crawl_date = min(crawl_date), first_id_sha1_form_group = first(id_sha1_form_group), first_url = first(url), .by = c("site", "hashed_forms", "nr_unique_hashes")) #%>% View()
           
          ids_to_look_at <- df_table_filled %>% select(first_id_sha1_form_group) %>% pull(.)
          
          df_return_table <- db_tags_parsed %>% 
            filter(id_sha1_form_group %in% ids_to_look_at) %>% 
            collect() %>% 
            left_join(., df_table_filled, by = c("id_sha1_form_group" = "first_id_sha1_form_group")) %>% #View()
            arrange(id_sha1_form_group, nr_unique_hashes) %>% 
            mutate(archive_url = paste0('<a href= "http://web.archive.org/web/', first_crawl_date, '/', first_url,'"  target="_blank">Link ins Archive</a>')) %>% 
            select(nr_unique_hashes, name, attr, value, text, archive_url) # %>% 
            # rename("Kommentar- index" = "nr_unique_hashes")
        # }
        # else{
        #   print("no formtags found")
        #   df_return_table <- tibble(nr_unique_hashes = numeric(1), name = character(1), attr = character(1), value = character(1), text = character(1), archive_url = character(1))
        #   df_return_table[["name"]][[1]] = "table is empty"
        #   print(df_return_table)
        # }
        # print(df_return_table %>% head())
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
      
      # gather_table_data <- reactive({
      #   print(paste0("create table ", current_data$site_to_load))
      #   
      #   # df_snippet_findings <- get_snippet_traces_data()
      #   df_form_findings <- current_data$form_data
      #   return(df_form_findings)
      # })
      # print(current_data$colors$nr_unique_hashes)
      output$tableFindings <- renderDataTable({
        
        if (nrow(current_data$form_data) >0)
        {
          # print(nrow(current_data$form_data))
        
          datatable(current_data$form_data, rownames= FALSE, filter = "top", escape=F, colnames=c("formindex", "name", "attr", "value", "text", "archive link"),
                    options = list(order = list(0, 'asc'))
                    ) %>%
                          formatStyle(columns = "nr_unique_hashes", 
                                      target = "cell", 
                                      backgroundColor = styleEqual(levels = current_data$colors["nr_unique_hashes"] %>% pull(.), values = current_data$colors["hex"] %>% pull(.))
                          )
        } else {
          datatable(current_data$form_data, rownames= FALSE, filter = "top", escape=F, colnames=c("formindex", "name", "attr", "value", "text", "archive link"),
                    options = list(order = list(0, 'asc'))
          )
        }
        })
    })
  }
  