library(needs)
needs(shiny, tidyverse, ggiraph, MetBrewer, googlesheets4, ggtext, patchwork, lubridate, ggforce)
source("config/config.R")
source("config/config-graphic.R")

# load(file = paste0("data/", CURRENT_SPHERE,"/df_systems_per_year.Rdata"))
# load(file = paste0("data/", CURRENT_SPHERE,"/df_sites_per_year.RData"))
load(file = "data/df_snippets_per_month_domain.RData")
# load(file = "data/df_heatmaps_availability.RData")
load(file = "data/df_system_lifetime.RData")

## https://stackoverflow.com/questions/62898726/how-to-refresh-rdata-objects-in-shiny-app

df_timespan_month <- seq(ymd("2007-01-01"), ymd("2021-06-01"), by = "month") %>% as_tibble()
df_timespan_year <- seq(ymd("2007-01-01"), ymd("2021-06-01"), by = "year") %>% as_tibble()

year_breaks_for_plotting <- df_timespan_year %>% 
  mutate(years = year(value)) %>% 
  select(years) %>% pull(.)

ANNOTATION_IS_EMPTY <- TRUE

check_gs_empty <- function(gs){
  ANNOTATION_IS_EMPTY <<- gs %>% nrow() == 0
}
gs4_auth(cache=here::here(".secrets"), email = TRUE)
# drive_auth(cache = ".secrets", email = TRUE)


get_gs_annoted_data <- function(sphere){
  # gs4_deauth()
  gs_annotation_raw <- read_sheet(SPREADSHEET_ANNOTATION_DATA, sheet = SPREADSHEET_ANNOTATION[[{{sphere}}]])
    
  check_gs_empty(gs_annotation_raw)
  if(!ANNOTATION_IS_EMPTY) {
    gs_annotation <- gs_annotation_raw %>% 
      mutate(type = "manual",
             date = paste0(year, "-", ifelse(is.na(month), "1", month), "-", ifelse(is.na(day), "1", day)) %>% ymd(.),
             technology = ifelse(is.na(technology), "other", technology)) 
  }
  else{
    gs_annotation <- gs_annotation_raw %>% 
      mutate(type = "manual",
             technology = ifelse(is.na(technology), "other", technology)) 
  }
  return(gs_annotation)
}

###################################
## currently first tab graphics 
###################################

### websites with systems

get_overview_sites_with_systems <- function(df_systems_per_year, df_sites_per_year){
  color_breaks <- c("#e5e5e5", "#098eb7")
  
  site_with_systems <- df_systems_per_year %>% 
    filter(!is.na(system)) %>% 
    select(site) %>% distinct() %>% pull(.)
  
  df_sites_per_year %>% 
    mutate(system_detected = ifelse(site %in% site_with_systems, "system found", "no system found")) %>% 
    select(site, system_detected) %>% #View()
    distinct() %>% #View()
    group_by(system_detected) %>% 
    summarise(overview = n()) %>% #View()
    mutate(bar = "bar") %>% 
    ggplot(., aes(y = bar, x = overview, fill = system_detected, label = overview)) +
    geom_bar(stat = "identity") +
    geom_text(position = "stack", hjust = 1, color = "white", family = typo_sfb_mono_bold_remote) +
    scale_fill_manual(values = color_breaks) + 
    theme_void() + theme_b03_text
  
}

#### graphic: dot plot on which domains snippets had been found

get_domains_over_time <- function(df_systems_per_year, df_sites_per_year, translate_sites, gs_annotation){
  print("building dot plot")
  df_na_archived_domains <- df_sites_per_year %>%
    filter(is.na(counted_sites)) %>%
    left_join(., translate_sites, by = "site") %>% 
    mutate(counted_sites = "no sites archived")
  
  nr_systems_found <- df_systems_per_year %>% select(system) %>% distinct() %>% nrow()
  
  data_plot_snippets <- df_systems_per_year %>%
    mutate(type = ifelse(!is.na(system), "automated", NA),
           year = as.numeric(year)) %>% 
    group_by(year, site) %>%
    mutate(year_printing = paste0(year, ".", nr_systems_per_site) %>% as.numeric()) %>% #View()
    ungroup()
  
  plot_snippets <- ggplot() +
    geom_tile(data = df_na_archived_domains, aes(x = year, y = Name), fill = "grey90") +
    geom_point_interactive(data = data_plot_snippets, aes(x = year_printing, y = Name, fill = system, shape = type, tooltip = paste0("site: ", site, "\nyear: ", year, "\nsystem: ", system)), width = .2, height = 0, na.rm = TRUE, size = 5, color = "black", alpha = .8) +
    # geom_tile_interactive(data = df_na_archived_domains, aes(x = year, y = site, tooltip = paste0("year: ", year, "\nsite: ", site, "\nno sites archived")), fill = "grey90") +
    geom_point_interactive(data = gs_annotation, aes(x = year, y = Name, fill = technology, tooltip = paste0("site: ", site, "\nyear: ", year, "\ncomment: ", tooltip_info, "\nsystem: ", technology), shape = type), size = 5, color = "black", alpha = .8, position = position_nudge(x = -.3)) +
    scale_fill_manual(values = met.brewer("Klimt", nr_systems_found), na.value = NA, name = "type of system") +
    scale_shape_manual(values = c(21, 23), breaks = c("automated", "manual"), name = "type of observation")+
    scale_x_continuous(breaks = year_breaks_for_plotting, labels = year_breaks_for_plotting,  expand = c(0, NA), name = "crawl year") +#, limits = year_breaks) +
    theme_b03_base + theme_b03_dot_timeline +  
    coord_cartesian(clip = "off") +
    guides(
      fill = guide_legend(title.position = "top", override.aes = list(shape = 22)),
      shape = guide_legend(title.position = "top", override.aes = list(color = "black"))
    )
  
  girafe(ggobj = plot_snippets, options = list(opts_sizing(rescale = TRUE)),
         width_svg = 16,
         height_svg = 10
         )
}

#### graphic: how often which domain has been crawled (only those with snippets found)

get_sites_over_time <- function(df_systems_per_year, df_sites_per_year, translate_sites){
  
  site_with_systems <- df_systems_per_year %>% 
    filter(!is.na(system)) %>% 
    select(site) %>% distinct() %>% pull(.)
  
  plot <- df_sites_per_year %>% 
    mutate(system_detected = ifelse(site %in% site_with_systems, "system found", "no system found")) %>% 
    left_join(., translate_sites, by = "site") %>% 
    ggplot(aes(x = year, y = Name, fill = counted_sites)) +
    geom_tile_interactive(aes(tooltip = paste0("year: ", year, "\nsite: ", site, "\nsites_counted: ", counted_sites))) +
    scale_fill_gradientn(colors = met.brewer("Hokusai2", type="continuous"), na.value = "grey90", name = "number of websites available" ) +
    scale_x_continuous(breaks = year_breaks_for_plotting, expand = c(0, NA), name = "crawl_year") +
    scale_y_discrete(expand = c(0, NA)) +
    ggforce::facet_col(facets = vars(system_detected), scales = "free_y", space = "free") +
    theme_b03_base + theme_b03_heatmap + theme_b03_facets  +
    guides(fill = guide_colorbar(title.position = "top", barwidth = unit(20, "lines"), barheight = unit(.5, "lines")))
  
  girafe(ggobj = plot, options = list(opts_sizing(rescale = TRUE)),
         width_svg = 16,
         height_svg = 10
  ) 
}

##################################
#### graphic on details: 

get_details_on <- function(domain_to_look_at, gs_annotation){
  print("building details graphic")
  df_detail_data_automated <- df_snippets_per_month_domain %>% 
    mutate(date = paste0(year, "-", month, "-1") %>% ymd(.),
           type = "automated") %>% 
    filter(site == domain_to_look_at) %>% 
    right_join(., df_timespan_month, by = c("date" = "value")) %>% #View()
    mutate(verification = "automated",
           verification_code = case_when(
             is.na(type) ~ "0",
             counted_snippets > 1 ~ "1"
           ))
  
  df_detail_data_manuall <- get_gs_annoted_data("German") %>% 
    filter(site == domain_to_look_at) %>% 
    right_join(., df_timespan_month, by = c("date" = "value")) %>% 
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
    theme_b03_base + theme_b03_heatmap_details + 
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
    theme_b03_base + theme_b03_heatmap_details + theme(axis.text.x = element_blank()) +
    coord_cartesian(clip = 'off')
  
  # print("hi")
  
  plots <- plot_details + plot_text
  plots
}


###################################
## currently second tab graphics 
###################################

#####################################################
## heatmap-grafik zu wie oft kommen systems (nicht snippets) im zeitverlauf bisher vor?


get_snippets_over_time <- function(df_systems_per_year){
  
  df_plot <- df_systems_per_year %>% 
    select(year, system, site) %>% 
    distinct() %>% 
    group_by(year, system) %>% 
    mutate(year = as.numeric(year)) %>% 
    summarise(counted_domains = n()) %>% #View()
    filter(!is.na(system)) %>% 
    ungroup()
  
  df_system_lifetime_filtered <- df_system_lifetime %>% 
    filter(name %in% df_plot$system) %>% #View()
    group_by(name, check_years) %>% #View()
    summarise(counted_years = sum(present, na.rm = TRUE)) %>%
    mutate(counted_years = ifelse(counted_years == 1, as.character(counted_years), NA)) %>% 
    ungroup()
  
  ggplot() +
    geom_tile(data = df_system_lifetime_filtered, aes(x = check_years, y = name, color = counted_years), fill = "white", alpha = 0.7) +
    scale_color_manual(values = c("1" = "darkgreen"), na.value = NA) + 
    geom_tile(data = df_plot, aes(x = year, y = system, fill = counted_domains)) +
    scale_fill_gradientn(colors = met.brewer("Hokusai2", type="continuous"), na.value = "grey90", name = "number of websites with system" ) +
    theme_b03_base + theme_b03_heatmap +
    guides(color = "none",
           fill = guide_colorbar(title.position = "top", barwidth = unit(20, "lines"), barheight = unit(.5, "lines")))
  
  # girafe(ggobj = plot, options = list(opts_sizing(rescale = TRUE)),
  #        width_svg = 10,
  #        height_svg = 5.5)
}

#####################################################
#### grafik zeitverlauf zu lebenszeit eines systems

get_systems_over_time_ggirafe <- function(){

  plot <- df_system_lifetime %>% 
    mutate(cat = ifelse(is.na(present), NA, cat)) %>% 
    ggplot(., aes()) +
    geom_tile_interactive(aes(x = check_years, y = reorder(name, desc(sorting_lifetime)), fill = cat, tooltip = paste0("system: ", name, "\nyear: ", check_years)), color = NA) +
    scale_fill_manual(values = c("year" = "#333333", "year_unsecure" = "#c2c2c2"), na.value = NA) +
    theme_b03_base + theme_b03_box_timeline + theme(legend.position = "none")
  
  girafe(ggobj = plot, options = list(opts_sizing(rescale = FALSE)),
         width_svg = 10,
         height_svg = 7
  )
}

###################################
## currently third tab graphics 
###################################

# get_histograms <- function(heatmap_for){
# 
#   df_heatmaps_availability %>%
#   filter(site == heatmap_for) %>%
#     ggplot() +
#     geom_tile(aes(x = month, y = year, fill = count)) +
#     scale_x_continuous(breaks = 1:12) +
#     scale_y_continuous(breaks = 2007:2021) +
#     guides(fill = guide_colorbar(title.position = "top", barwidth = unit(20, "lines"), barheight = unit(.5, "lines"))) +
#     theme_b03_base + theme_b03_heatmap + theme(axis.title = element_text(hjust = 0.5))
# }

##################################

shinyServer(function(input, output) {

  ## Tab 1
  
  react_data <- reactiveValues()

  observeEvent(
    input$selected_sphere,
    {
      print("handling reactive data")
      req(input$selected_sphere)
      load(paste0("data/", input$selected_sphere,"/df_systems_per_year.RData"))
      load(paste0("data/", input$selected_sphere,"/df_sites_per_year.RData"))
      load(paste0("data/",  input$selected_sphere,"/gs_domain_to_look.RData"))
      react_data$systems <- df_systems_per_year
      react_data$sites <- df_sites_per_year
      react_data$translate_sites <- gs_domain_to_look
      react_data$annotation <- get_gs_annoted_data(input$selected_sphere)
    })
  
  getOverviewSitesWithSystems <- reactive({get_overview_sites_with_systems(react_data$systems, react_data$sites)})
  output$getOverviewSitesWithSystems <- renderPlot({getOverviewSitesWithSystems()})

  getSnippetsOverTime <- reactive({get_domains_over_time(react_data$systems, react_data$sites, react_data$translate_sites, react_data$annotation)})
  output$getSnippetsOverTime <- renderGirafe({getSnippetsOverTime()})
  
  getSitesOverTime <- reactive({get_sites_over_time(react_data$systems, react_data$sites, react_data$translate_sites)})
  output$getSitesOverTime <- renderGirafe({getSitesOverTime()})

  getDetailOnDomain  <- reactive({get_details_on("ksta", react_data$annotation)})
  output$getDetailOnDomain <- renderPlot({getDetailOnDomain()})

  # ## Tab 2
  getSystemsOverTime <- reactive({get_snippets_over_time(react_data$systems)})
  output$getSystemsOverTime <- renderPlot({getSystemsOverTime()})
  # 
  # 
  getSystemsLifeTime <- reactive({get_systems_over_time_ggirafe()})
  output$getSystemsLifeTime <- renderGirafe({getSystemsLifeTime()})
  # 
  # ## Tab 3
  # getHistograms <- reactive({get_histograms(input$heatmap_for)})
  # output$getHistograms <- renderPlot({getHistograms()})
  
})
