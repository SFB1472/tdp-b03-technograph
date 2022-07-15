library(needs)
needs(shiny, tidyverse, ggiraph, MetBrewer, googlesheets4)
source("config.R")

load(file = "data/df_raw_vis_data.RData")
load(file = "data/df_snippets_year.RData")
load(file = "data/df_sites_year.RData")

year_breaks <- df_sites_year %>% select(year) %>% distinct() %>% pull(.)

link_manuell_added_data_points_domains <- "https://docs.google.com/spreadsheets/d/1aEQAAh0UZlFImajCQ-E7cH-0nrlH8TGuxQt4PTbVCJE/"
gs4_auth(cache=".secrets")

ss <- gs4_get(link_manuell_added_data_points_domains)
gs_manual_domain_snippet <- read_sheet(ss)

gs4_deauth
# print(google_sheet)

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
  # print(sorting)
  
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


#### graphic: dot plot on which domains snippets had been found

get_snippets_over_time <- function(){
  # print(year_breaks)
  df_na_archived_domains <- df_sites_year %>% 
    filter(is.na(counted_sites)) %>% 
    mutate(counted_sites = "no sites archived")
  
  # print(head(gs_manual_domain_snippet))
  
  data_plot_snippets <- df_snippets_year %>% 
    right_join(., df_sites_year) %>% #View()
    mutate(type = "automated") #%>% 
  
  plot_snippets <- ggplot(data = data_plot_snippets, aes()) +
    geom_jitter_interactive(data = data_plot_snippets, aes(x = year, y = site, color = snippet, tooltip = paste0("year: ", year, "\nsite: ", site, "\nsnippet: ", snippet), shape = type), width = .2, height = 0, na.rm = TRUE) +
    geom_tile(data = df_na_archived_domains, aes(x = year, y = site), fill = "grey90") +
    # geom_tile_interactive(data = df_na_archived_domains, aes(x = year, y = site, tooltip = paste0("year: ", year, "\nsite: ", site, "\nno sites archived")), fill = "grey90") +
    geom_point_interactive(data = gs_manual_domain_snippet, aes(x = year, y = site, color = snippet, tooltip = tooltip_info, shape = type), size = 2) +
    scale_color_manual(values = met.brewer("Troy", 6), na.value = NA, name = "sort of snippets", guide = guide_legend(override.aes = list(shape = 15) )) +
    scale_shape_manual(values = c(16, 18), breaks = c("automated", "manual"), name = "sort of observation",
      guide = guide_legend(override.aes = list(color = "black") ) )+
    scale_x_continuous(breaks = year_breaks,  expand = c(0, NA), name = "crawl year") +#, limits = year_breaks) +
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
