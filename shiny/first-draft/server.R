library(shiny)
library(tidyverse)
library(ggiraph)

source("config.R")

load(file = "data/df_raw_vis_data.RData")

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
         height_svg = 11)
}

shinyServer(function(input, output) {

  getSystemsOverTime <- reactive({get_systems_over_time_ggirafe(input$searchWord)})
  output$getSystemsOverTime <- renderGirafe({getSystemsOverTime()})

})
