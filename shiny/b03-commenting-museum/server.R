library(needs)
needs(shiny, tidyverse, ggiraph, MetBrewer, googlesheets4, ggtext, patchwork, lubridate)
source("config.R")

load(file = "data/df_raw_vis_data.RData")
load(file = "data/df_snippets_year.RData")
load(file = "data/df_sites_year.RData")
load(file = "data/df_snippets_per_month_domain.RData")

df_timespan <- seq(ymd("2007-01-01"), ymd("2021-06-01"), by = "month") %>% as_tibble()

year_breaks <- df_sites_year %>% select(year) %>% distinct() %>% pull(.)

link_manuell_added_data_points_domains <- "https://docs.google.com/spreadsheets/d/1aEQAAh0UZlFImajCQ-E7cH-0nrlH8TGuxQt4PTbVCJE/"
gs4_deauth()
# gs4_auth(cache=".secrets")

ss <- gs4_get(link_manuell_added_data_points_domains)
gs_manual_domain_snippet <- read_sheet(ss) %>% 
  mutate(type = "manual",
         date = paste0(year, "-", ifelse(is.na(month), "1", month), "-", ifelse(is.na(day), "1", day)) %>% ymd(.),
         technology = ifelse(is.na(technology), "other", technology))

gs4_deauth()


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

###################################
## currently first tab graphics 
###################################

#### graphic: dot plot on which domains snippets had been found

get_domains_over_time <- function(){
  # print(year_breaks)
  df_na_archived_domains <- df_sites_year %>% 
    filter(is.na(counted_sites)) %>% 
    mutate(counted_sites = "no sites archived")
  
  # print(head(gs_manual_domain_snippet))
  
  data_plot_snippets <- df_snippets_year %>% 
    right_join(., df_sites_year) %>% #View()
    mutate(type = ifelse(!is.na(system),"automated", NA)) %>% 
    group_by(year, site, system, type) %>% 
    summarise(snippet = paste0(snippet), counted_snippets = sum(counted_snippets)) %>%  #%>% View()
    ungroup()
  
  plot_snippets <- ggplot() +
    geom_tile(data = df_na_archived_domains, aes(x = year, y = site), fill = "grey90") +
    geom_point_interactive(data = data_plot_snippets, aes(x = year, y = site, fill = system, shape = type, tooltip = paste0("system: ", system, "\nsite: ", site, "\nyear: ", year)), width = .2, height = 0, na.rm = TRUE, size = 5, color = "black", alpha = .8, position = position_nudge(x = -.1)) +
    # geom_tile_interactive(data = df_na_archived_domains, aes(x = year, y = site, tooltip = paste0("year: ", year, "\nsite: ", site, "\nno sites archived")), fill = "grey90") +
    geom_point_interactive(data = gs_manual_domain_snippet, aes(x = year, y = site, fill = technology, tooltip = paste0("comment: ", tooltip_info, "\nsystem: ", technology, "\nsite: ", site, "\nyear: ", year), shape = type), size = 5, color = "black", alpha = .8, position = position_nudge(x = .1)) +
    scale_fill_manual(values = met.brewer("Klimt", 3), na.value = NA, name = "type of system") +
    scale_shape_manual(values = c(21, 23), breaks = c("automated", "manual"), name = "type of observation")+
    scale_x_continuous(breaks = year_breaks,  expand = c(0, NA), name = "crawl year") +#, limits = year_breaks) +
    theme_b03_dot_timeline +
    coord_cartesian(clip = "off") +
    guides(
      fill = guide_legend(title.position = "top", override.aes = list(shape = 22)),
      shape = guide_legend(title.position = "top", override.aes = list(color = "black"))
    )
  
  girafe(ggobj = plot_snippets, options = list(opts_sizing(rescale = TRUE)),
         width_svg = 10,
         height_svg = 6
         )
}

#### graphic: how often which domain has been crawled (only those with snippets found)

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


#### graphic on details: 

get_details_on <- function(domain_to_look_at){
  
  df_detail_data_automated <- df_snippets_per_month_domain %>% 
    mutate(date = paste0(year, "-", month, "-1") %>% ymd(.),
           type = "automated") %>% 
    filter(site == domain_to_look_at) %>% 
    right_join(., df_timespan, by = c("date" = "value")) %>% #View()
    mutate(verification = "automated",
           verification_code = case_when(
             is.na(type) ~ "0",
             counted_snippets > 1 ~ "1"
           ))
  
  df_detail_data_manuall <- gs_manual_domain_snippet %>% 
    filter(site == domain_to_look_at) %>% 
    right_join(., df_timespan, by = c("date" = "value")) %>% 
    group_by(date) %>% 
    mutate(verified_by_researcher = ifelse(verified_by_researcher == "x", "2", NA),
           verified_by_interview = ifelse(verified_by_interview == "x", "3", NA)) %>% #View()
    pivot_longer(., cols = "verified_by_researcher":"verified_by_interview", names_to = "verification", values_to = "verification_code") %>% 
    ungroup()
  
  df_detail_data <- df_detail_data_automated %>% 
    bind_rows(df_detail_data_manuall)
  
  current_snippets_found_a <- df_detail_data_automated %>% 
    filter(counted_snippets > 0) %>% #View()
    select(snippet) %>% 
    distinct()
  
  current_snippets_found_m <- df_detail_data_manuall %>% 
    select(snippet) %>% 
    filter(!is.na(snippet)) %>% 
    distinct()
  
  current_snippets_found <- current_snippets_found_a %>% 
    bind_rows(., current_snippets_found_m) %>% 
    distinct()
  
  detail_colors <- c("grey90", "#f5bb50", "#cba9be", "#b0799a")
  color_breaks <- c("0" = "no archive data available", "1" = "detected automatically", "2" = "by researcher", "3" = "interview findings")
  
  df_detail_graphic <- df_detail_data %>% 
    filter(snippet %in% current_snippets_found$snippet | is.na(snippet)) %>% #View()
    mutate(verification = factor(verification, levels = c("automated", "verified_by_researcher", "verified_by_interview")))
  
  df_label_colors <- df_detail_graphic %>% 
    select(date, verification_code) %>% 
    filter(!is.na(verification_code)) %>% 
    group_by(date) %>% 
    summarise(verification_code = max(verification_code))
  
  df_detail_annotations <- df_detail_graphic %>% 
    select(date, tooltip_info) %>% 
    distinct() %>% 
    arrange(desc(date)) %>% 
    mutate(dot = ifelse(is.na(tooltip_info), "0", "1")) %>% 
    left_join(., df_label_colors)

  plot_details <- ggplot() +
    geom_tile_interactive(data = df_detail_graphic, aes(y = date, x = verification, fill = verification_code, tooltip = tooltip_info), alpha = .8) +
   scale_fill_manual(values = detail_colors, labels = color_breaks, na.value = "white") +
    scale_x_discrete(position = "top") +
    scale_y_date(date_breaks = "years", minor_breaks = "month", date_labels = "%Y", expand = c(0, NA)) +
    labs(subtitle = "At the first column you can see the findings based on automated reseach.
<span style ='color:#bbbbbb;'>Grey fields</span> indicate, that there was no data in the dump from the internet archive.
<span style ='color:#f5bb50;'>Yellow</span> shows, that there was a system found by the automated search.\n
The the second and third column showing the qualitativ research. Any findings detected by researcher is colored <span style ='color:#cba9be;'>purple</span>.
In case those findings are verified by an interview partner, fields are colored in slightly <span style ='color:#b0799a;'>darker color</span>.") +
    theme_b03_heatmap_details +
    coord_cartesian(clip = 'off')
  
  
  plot_text <- ggplot() +
    geom_textbox(data = df_detail_annotations, aes(y = date, x = 0.05, label = tooltip_info, fill = verification_code, color = "white", orientation = "upright", hjust = 0), width = unit(.8, "npc")) +
    geom_point(aes(y = as.Date("2007-01-01"), x = 3), color = "#ffffff")+
    geom_point(data = df_detail_annotations, aes(y = date, x = 0, color = dot)) +
    scale_y_date(date_breaks = "years", minor_breaks = "month", date_labels = "%Y", expand = c(0, 0), position = "right") +
    scale_x_continuous(expand = c(0,0), limits = c(0, NA)) +
    scale_color_manual(values = c("#ffffff", "#000000", "#ffffff")) +
    scale_fill_manual(values = detail_colors) +
    labs(subtitle = "In case of any qualitative research findings, it's sometimes necessary to comment on the concrete finding. It's noted in this column of the graphic.")+
    theme_b03_heatmap_details + theme(axis.text.x = element_blank()) +
    coord_cartesian(clip = 'off')
  
  print("hi")
  
  plots <- plot_details + plot_text
  plots
}



###################################
## currently second tab graphics 
###################################

#####################################################
## heatmap-grafik zu wie oft kommen systems (nicht snippets) im zeitverlauf bisher vor?


get_snippets_over_time <- function(){
  df <- df_snippets_year %>% 
    select(year, system, site) %>% 
    distinct() %>% 
    group_by(year, system) %>% 
    summarise(counted_domains = n())# %>% View()
  
  plot <- df %>% ggplot(., aes()) +
    geom_tile(aes(x = year, y = system, fill=counted_domains)) +
    theme_b03_heatmap
  
  girafe(ggobj = plot, options = list(opts_sizing(rescale = TRUE)),
         width_svg = 10,
         height_svg = 5.5)
}

#####################################################
#### grafik zeitverlauf zu lebenszeit eines systems

get_systems_over_time_ggirafe <- function(){
  # print(sorting)
  sorting <- "sort_life_span"
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

shinyServer(function(input, output) {
  ## Tab 1
  getSystemsLifeTime <- reactive({get_systems_over_time_ggirafe()})
  output$getSystemsLifeTime <- renderGirafe({getSystemsLifeTime()})
  
  getSystemsOverTime <- reactive({get_snippets_over_time()})
  output$getSystemsOverTime <- renderGirafe({getSystemsOverTime()})
  
  getDetailOnDomain  <- reactive({get_details_on("ksta")})
  output$getDetailOnDomain <- renderPlot({getDetailOnDomain()})
  
  ## Tab 2
  getSnippetsOverTime <- reactive({get_domains_over_time()})
  output$getSnippetsOverTime <- renderGirafe({getSnippetsOverTime()})
  
  getSitesOverTime <- reactive({get_sites_over_time()})
  output$getSitesOverTime <- renderGirafe({getSitesOverTime()})

})
