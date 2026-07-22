library(DT)

snippetFindingsTableUI <- function(id) {
  ns <- NS(id)
  tagList(
    DT::dataTableOutput(ns("tableSnippetFindings"))
  )
}

snippetFindingsTableServer <- function(id, tab_, site_to_load) {
  
  stopifnot(is.reactive(site_to_load))
  
  moduleServer(
    id,
    function(input, output, session) {
      
      current_data <- reactiveValues()
      
      rebuilt_data <- function(){
        current_data$snippet_data = get_snippet_finding_data()
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
      
      ## get the data ----------------------------------------------------------
      
      get_snippet_finding_data <- function(){
        
        df_table <- tbl(pool, "tbl_snippet_findings") %>%
          filter(site == !!current_data$site_to_load) %>% 
          # inner_join(., tbl(pool, "traces"), by = c("sha1" = "site")) %>%
          # filter(search_type == "snippet_list") %>% 
          # select(site, crawl_date, trace, url, crawl_timestamp) %>% 
          collect() # %>%
          # arrange(crawl_date) %>% 
          # mutate(snippet_group = cur_group_id(),
          #        snippet_group_member = row_number(), .by = "trace") %>% 
          # filter(snippet_group_member < 6) %>% 
          # mutate(archive_url = paste0('<a href= "http://web.archive.org/web/', crawl_timestamp, '/', url,'"  target="_blank">Link ins Archive</a>')) %>% 
          # select(trace, crawl_date, url, archive_url)
        
        # View(df_table)
        return(df_table)
      }
      
      # print the viz --------------------------------------------------------------
      
      get_colors <- reactive({
        
        nr_fill_values <- current_data$snippet_data %>% 
          select(trace) %>% 
          distinct() %>% nrow()
        
        color_values = MetBrewer::met.brewer("Cross", nr_fill_values)
        
        colors <- current_data$snippet_data %>% 
          select(trace) %>% 
          distinct() %>% 
          # arrange(snippet) %>% 
          bind_cols(color_values) %>% #deframe()
          rename("hex" = `...2`)
        
        # print(colors)
        return(colors)
      })
      
      output$tableSnippetFindings <- DT::renderDataTable({
        
        if (!is.null(current_data$snippet_data)){
          if(nrow(current_data$snippet_data) > 0){
            DT::datatable(current_data$snippet_data, rownames= FALSE, filter = "top", escape=F, colnames=c("snippet found", "crawl date", "url", "archive link"),
                      options = list(order = list(0, 'asc'))
                      ) %>%
                            formatStyle(columns = "trace",
                                        target = "cell",
                                        backgroundColor = styleEqual(levels = current_data$colors["trace"] %>% pull(.), values = current_data$colors["hex"] %>% pull(.))
                            )
        } else {
          DT::datatable(current_data$snippet_data, rownames= FALSE, filter = "top", escape=F, colnames=c("snippet found", "crawl date", "url", "archive link"),
                    options = list(order = list(0, 'asc'))
          )
          }
        }
        })
    })
  }
  