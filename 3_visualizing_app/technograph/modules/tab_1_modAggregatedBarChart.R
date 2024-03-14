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
        
        df_snippets_traces <- tbl(pool, "sites") %>%
          filter(sphere == !!current_data$sphere_to_load, of_interest == TRUE) %>% 
          inner_join(., tbl(pool, "snippets_2"), by = c("sha1" = "site")) %>%
          filter(detected == 1) %>%
          distinct(site) %>%
          collect()
        
        # print("1 tab snippets aggregated")
        
        df_form_tag_findings <- tbl(pool, "sites") %>% 
          filter(sphere == !!current_data$sphere_to_load, of_interest == TRUE) %>% 
          inner_join(., tbl(pool, "findings_hashed_2"), by = "sha1") %>%
          # filter(str_detect(attr, "comment|komment")) %>%
          distinct(site) %>% 
          collect() 
        
        # print("1 tab form tag findings aggregated")
        
        generell_findings <- df_snippets_traces %>% 
          bind_rows(., df_form_tag_findings) %>% 
          select(site) %>% 
          distinct() %>% 
          pull(.)
        
        # print("1 tab getting all sites")
        
        df_bar_data <- tbl(pool, "sites") %>%
          filter(sphere == !!current_data$sphere_to_load, of_interest == TRUE) %>% 
          select(site) %>% 
          distinct() %>% 
          collect() %>%
          mutate(detected = ifelse(site %in% generell_findings, "system found", "no system found")) %>% 
          reframe(overview = n(), .by = detected) %>% 
          mutate(bar = "bar")
        
        # print("1 tab chart data done")
        
        return(df_bar_data)
    
      }

      get_sites_with_traces_agg <- reactive({
        print("1 tab, 1 vis, printing")
        
        color_breaks <- c("#e5e5e5", "#098eb7")
        
        # plot <- current_data$data %>% 
        plot <- get_data() %>% 
          ggplot(., aes(y = bar, x = overview, fill = detected, label = overview)) +
          geom_bar(stat = "identity") +
          geom_text(position = "stack", hjust = 1, color = "white", family = typo_sfb_mono_bold_remote) +
          scale_fill_manual(values = color_breaks) + 
          theme_void() + theme_b03_base + theme_b03_text + theme(axis.text = element_blank(), legend.title = element_blank())
        
        return(plot)
      })

      # not needed for first tab, because event only fired when tab is active 
      # current_tab <- reactiveValues()
      # 
      # observeEvent(tab_(),{
      #   print(paste0("aggregatedBarChart: tab loaded: ", tab_()))
      #   current_tab$tab = tab_()
      # })

      current_data <- reactiveValues()
      
      observeEvent(sphere_(),{
        # print("observe event")
        current_data$sphere_to_load = sphere_()
        # current_data$data = get_data()
      })
      
      output$aggregatedBarChart <- renderPlot({get_sites_with_traces_agg()}, height = 50, width = 1000)
  })
}

