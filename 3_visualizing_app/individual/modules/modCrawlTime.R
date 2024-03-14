modCrawlTimeUI <- function(id) {
  ns <- NS(id)
  tagList(
    addGFontHtmlDependency(family = c("Roboto Mono")),
    plotOutput(ns("crawlTime"), height = 300, width = 1100)
  )
}

modCrawlTimeServer <- function(id, site_to_load) {
  moduleServer(
    id,
    function(input, output, session) {
      
      get_site_timestamps <- function(){
        db_sites <- tbl(pool, "sites")
        
        df <- db_sites %>% 
          filter(site == !!current_data$site_to_load) %>% 
          mutate(hour = hour(crawl_timestamp),
                 year = year(crawl_date)) %>% 
          collect() %>% 
          reframe(., counted = n(), .by = c(hour, year, site)) 

        return(df)
      }
      
      
      print_crawl_time <- reactive({
       
        get_site_timestamps() %>% 
          ggplot(., aes(x = year, y = hour, fill = counted)) +
          geom_tile() +
          scale_fill_gradientn(colors = met.brewer("Hokusai2", type="continuous"), na.value = "grey90", name = "number of websites available" ) +
          scale_x_continuous(expand = c(0, NA), limits = c(1996, 2021), breaks = c(1996:2021)) +
          scale_y_continuous(breaks = c(0, 6, 12, 18), labels = c("00:00", "06:00", "12:00", "18:00"), trans = "reverse") +
          coord_cartesian(clip = "off")+
          theme_b03_base + theme_b03_heatmap + 
          guides(fill = guide_colorbar(title.position = "top", barwidth = unit(20, "lines"), barheight = unit(.5, "lines")))
      })
      
      current_data <- reactiveValues()
      observeEvent(site_to_load(),{
        # print(paste0("findingsVizServer ", site_to_load()))
        current_data$site_to_load = site_to_load()
        current_data$sites = get_site_timestamps()
        # current_data$snippet_data = get_snippet_traces_data()
        # current_data$form_data = get_form_finding_data()
        # current_data$height = get_height()
      })
      
      output$crawlTime <- renderPlot({print_crawl_time()})
    }
  )
}