library(needs)
needs(shiny, tidyverse, ggiraph, MetBrewer)
source("config.R")

load(file = "data/df_raw_vis_data.RData")
load(file = "data/df_snippets_year.RData")
load(file = "data/df_sites_year.RData")

year_breaks <- df_sites_year %>% select(year) %>% distinct() %>% pull(.)


girafe_css(
  css ="fill:orange;stroke:gray;",
  text = "align:left",
  point = "stroke-width:3px")

df_raw_vis_data <- df_raw_vis_data %>% 
  filter(!is.na(name), name != "", !is.na(start_date)) %>% 
  arrange((time_span)) %>% 
  mutate(sort_life_span = row_number()) %>% 
  arrange(desc(name)) %>% 
  mutate(sort_name = row_number()) %>% 
  pivot_longer(., sort_life_span:sort_name, names_to = "filter_sort", values_to = "values")


get_systems_over_time_ggirafe <- function(sorting){
  print(sorting)
  
  df_plot <- df_raw_vis_data %>% 
    filter(filter_sort == sorting)
  
  plot <- df_plot %>% ggplot(., aes()) +
    geom_rect_interactive(aes(xmin = start_date, xmax = secure_start, ymin = values - 0.45, ymax = values + 0.45, tooltip = paste0(name, ", Laufzeit: ", time_span), fill = "timespan_unsafe")) +
    geom_rect_interactive( aes(xmin = secure_start, xmax = secure_end, ymin = values - 0.45, ymax = values + 0.45, tooltip = paste0(name, ", Laufzeit: ", time_span))) +
    geom_rect_interactive(aes(xmin = secure_end, xmax = end_date, ymin = values -0.45, ymax = values + 0.45, tooltip = paste0(name, ", Laufzeit: ", time_span), fill = "timespan_unsafe")) +
    scale_x_date(date_minor_breaks = "1 year") +
    scale_y_continuous(breaks = df_plot$values, labels = df_plot$name, expand = c(0,NA))+
    theme_b03_box_timeline
  
  girafe(ggobj = plot, options = list(opts_sizing(rescale = FALSE)),
         width_svg = 10,
         height_svg = 11
         )
}

get_snippets_over_time <- function(){
  # print(year_breaks)
  plot_snippets <- df_snippets_year %>% 
    right_join(., df_sites_year) %>% #View()
    ggplot(., aes()) +
    geom_jitter_interactive(aes(x = year, y = site, color = snippet, tooltip = paste0("year: ", year, "\nsite: ", site, "\nsnippet: ", snippet)), width = .2, height = 0, na.rm = TRUE) +
    # facet_wrap(~site, ncol = 1)+
    scale_color_manual(values = met.brewer("Troy", 6), na.value = NA, name = "snippets found") +
    scale_x_continuous(breaks = year_breaks,  expand = c(0, NA), name = "crawl_year") +#, limits = year_breaks) +
    theme_b03_dot_timeline
  
  girafe(ggobj = plot_snippets, options = list(opts_sizing(rescale = TRUE)),
         width_svg = 10,
         height_svg = 5.5
         )
}

get_sites_over_time <- function(){
  
  plot <- df_sites_year %>% 
    ggplot(aes(x = year, y = site, fill = color_breaks)) +
    geom_tile_interactive(aes(tooltip = paste0("year: ", year, "\nsite: ", site))) +
    scale_fill_manual(values = met.brewer("Hokusai2"), na.value = "grey90") +
    scale_x_continuous(breaks = year_breaks, expand = c(0, NA), name = "crawl_year") +
    scale_y_discrete(expand = c(0, NA)) +
    theme_b03_heatmap
  
  girafe(ggobj = plot, options = list(opts_sizing(rescale = TRUE)),
         width_svg = 10,
         height_svg = 5.5
  ) 
}

shinyServer(function(input, output) {
  ## Tab 1
  getSystemsOverTime <- reactive({get_systems_over_time_ggirafe(input$searchWord)})
  output$getSystemsOverTime <- renderGirafe({getSystemsOverTime()})
  
  ## Tab 2
  getSnippetsOverTime <- reactive({get_snippets_over_time()})
  output$getSnippetsOverTime <- renderGirafe({getSnippetsOverTime()})
  
  getSitesOverTime <- reactive({get_sites_over_time()})
  output$getSitesOverTime <- renderGirafe({getSitesOverTime()})

})
