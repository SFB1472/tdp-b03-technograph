modCrawlTimeUI <- function(id) {
  ns <- NS(id)
  tagList(
    addGFontHtmlDependency(family = c("Roboto Mono")),
    plotOutput(ns("crawlTime"), height = 300, width = 1100)
  )
}

modCrawlTimeServer <- function(id, tab_, site_to_load) {
  moduleServer(
    id,
    function(input, output, session) {
      
      get_site_timestamps <- function(){
        df <- tbl(pool, "sites") %>% 
          filter(site == !!current_data$site_to_load) %>% 
          mutate(hour = hour(crawl_timestamp),
                 year = year(crawl_date)) %>% 
          group_by(hour, year, site) %>% 
          summarise(counted = n()) %>% 
          ungroup() %>% 
          collect() 

        return(df)
      }

      print_crawl_time <- reactive({
       print("tab 2 get data on crawl time")
        
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
      
      # current_tab <- reactiveValues()
      
      current_data <- reactiveValues()
      
      observeEvent(site_to_load(),{
        current_data$site_to_load = site_to_load()
      })
      
      output$crawlTime <- renderPlot({print_crawl_time()})
    }
  )
}