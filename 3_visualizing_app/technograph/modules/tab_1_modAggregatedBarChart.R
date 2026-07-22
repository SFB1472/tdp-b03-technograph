tab_1_aggregatedBarchartUI <- function(id) {
  ns <- NS(id)
  tagList(
    addGFontHtmlDependency(family = c("Roboto Mono")),
    plotOutput(ns("aggregatedBarChart"), height = "50px")
  )
}

tab_1_aggregatedBarchartServer <- function(id, tab_, sphere_) {

  moduleServer(
    id,
    function(input, output, session) {
      
      get_data <- function(){
        # print("1 tab, 1 vis, aggregating data")
        
        generell_findings <- tbl(pool, "sites_with_traces") %>%
          filter(sphere == !!current_data$sphere_to_load, !is.na(trace)) %>% 
          # inner_join(., tbl(pool, "traces"), by = c("sha1" = "site")) %>%
          distinct(site) %>%
          collect() 
        
        df_bar_data <- tbl(pool, "sites_with_traces") %>%
          filter(sphere == !!current_data$sphere_to_load) %>% 
          select(site) %>% 
          distinct() %>% 
          collect() %>%
          mutate(detected = ifelse(site %in% generell_findings$site, "system found", "no system found")) %>% # View()
          reframe(overview = n(), .by = detected) %>% 
          mutate(bar = "bar")
       
        # View(df_bar_data)
        return(df_bar_data)
    
      }

      get_sites_with_traces_agg <- reactive({
        # print("1 tab, 1 vis, printing")
        
        color_breaks <- c("#e5e5e5", "#098eb7")
        
        plot <- current_data$data %>%
        # plot <- get_data() %>% 
          ggplot(., aes(y = bar, x = overview, fill = detected, label = overview)) +
          geom_bar(stat = "identity") +
          geom_text(position = "stack", hjust = 1, color = "white", family = typo_sfb_mono_bold_remote) +
          scale_fill_manual(values = color_breaks) + 
          theme_void() + theme_b03_base + theme_b03_text + theme(axis.text = element_blank(), legend.title = element_blank())
        
        return(plot)
      })

      current_data <- reactiveValues()
      
      observeEvent(sphere_(),{
        # print("observe event")
        current_data$sphere_to_load = sphere_()
        current_data$data = get_data()
      })
      
      output$aggregatedBarChart <- renderPlot({get_sites_with_traces_agg()}, height = 50, width = 1000)
  })
}

