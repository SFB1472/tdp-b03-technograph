library(ggiraph)

tab_4_historiogramUI <- function(id) {
  ns <- NS(id)
  tagList(
    addGFontHtmlDependency(family = c("Roboto Mono")),
    girafeOutput(ns("historiogram"))
  )
}

tab_4_historiogramServer <- function(id, tab_) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ## get the data ----------------------------------------------------------
      
      load_sheet <- function(){
        df_sheet <- read_csv("data/historiograms/visual analyses - traces of commenting options - Tabellenblatt1.csv", show_col_types = FALSE) %>% 
          filter(type %in% c("offsite", "onsite", "meta"))
        return(df_sheet)
      }
       
      make_segment_data <- function(){ 
        df_make_shades <- current_data$sheet %>% 
          select(-comment, -archive_link, -screenshot) %>% 
          filter(type != "meta") %>% 
          mutate(continuous_group = cur_group_id(), .by = c(site, type)) %>% 
          mutate(closed_groups = ifelse(lead(continuous_group) != continuous_group, "last", "same"),
                 closed_groups_start = ifelse(lag(continuous_group) != continuous_group, "first", "same"), .by = site) %>% 
          mutate(closed_groups = ifelse(is.na(closed_groups), "last", closed_groups),
                 closed_groups_start = ifelse(is.na(closed_groups_start), "first", closed_groups_start),
                 date_blurred = floor_date(date,unit ="month"),
                 change = ifelse(closed_groups == "same" & closed_groups_start == "first", T, F)) %>% 
          mutate(index = cumsum(change), .by = site) #%>% 
        
        df_plot_segments <- df_make_shades %>% 
          summarise(start_date = first(date), end_date = last(date), .by = c(site, index, type)) %>% 
          mutate(blurry_start = start_date - months(6),
                 blurry_end = end_date + months(6))
        
        return(df_plot_segments)
      }
      
      # print the viz --------------------------------------------------------------
      
      print_historiograms <- reactive({
        # View(current_data$segment_data)
        # View(current_data$sheet)
        plot <- ggplot() +
          geom_segment(data = current_data$segment_data, aes(x = blurry_start, y = type, xend = blurry_end, yend = type, color = type), alpha = .5, lineend = "round", linewidth = 2)+
          geom_segment(data = current_data$segment_data, aes(x = start_date, y = type, xend = end_date, yend = type, color = type), lineend = "round", linewidth = 2)+
          geom_point_interactive(data = current_data$sheet, aes(x = date, y = type, tooltip = paste0(comment))) +
          theme_b03_base + theme_b03_facets_individual + theme(legend.position = "none") +
          facet_col(vars(site), scales = "free_y", space = "free" )
        
        girafe(ggobj = plot,
               options = list(opts_sizing(rescale = FALSE)),
               width_svg = 15,
               height_svg = 8)
      })
      
      # observe Events ----------------------------------
      
      current_data <- list()
      
      # rebuild_df <- function(){
        current_data$sheet = load_sheet()
        current_data$segment_data = make_segment_data()
      # }
      
      # observeEvent(tab_(),{
      #   print(paste0("tab 4 visualization: tab loaded: ", tab_()))
        # current_tab$tab = tab_()
        # if(tab_() == "tab_4"){
          # rebuild_df()
        # }
      # })
      
      output$historiogram <- renderGirafe({print_historiograms()})
    })
  }
  