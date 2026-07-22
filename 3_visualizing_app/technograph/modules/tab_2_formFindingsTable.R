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
        
        df_table <- tbl(pool, "tbl_context_hashed_tags") %>%
          filter(site == !!current_data$site_to_load) |> 
          select(nr_unique_hashes, tag, attr, value_cleaned_whitelist, archive_url) |> 
          distinct() |> 
          collect()
        
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
  