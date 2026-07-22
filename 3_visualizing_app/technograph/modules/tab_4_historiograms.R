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
        df_return <- tbl(pool, "tbl_historiogram_comments") |> 
          collect()
        
        # print(df_plot_segments)
        return(df_return)
      }
      
      make_segment_data <- function(){ 
        df_make_shades <- tbl(pool, "tbl_historiogram") |> 
          collect()
        
        # print(df_plot_segments)
        return(df_make_shades)
      }
      
      # print the viz --------------------------------------------------------------
      
      print_historiograms <- reactive({
       
        plot <- ggplot() +
          geom_segment(data = current_data$segment_data, aes(x = ymd(blurry_start), y = type, xend = ymd(blurry_end), yend = type, color = type), alpha = .5, lineend = "round", linewidth = 2)+
          geom_segment(data = current_data$segment_data, aes(x = ymd(start_date), y = type, xend = ymd(end_date), yend = type, color = type), lineend = "round", linewidth = 2)+
          geom_point_interactive(data = current_data$sheet, aes(x = ymd(date), y = type, tooltip = paste0(comment))) +
          scale_x_date(date_breaks = "2 years", date_labels = "%Y", limits = c(ymd("1997-01-01"), ymd("2021-01-01"))) +
          theme_b03_base + theme_b03_facets_individual + theme(legend.position = "none") +
          facet_col(vars(site), scales = "free_y", space = "free" )
        
        girafe(ggobj = plot,
               options = list(opts_sizing(rescale = FALSE)),
               width_svg = 10,
               height_svg = 6)
      })
      
      # observe Events ----------------------------------
      
      current_data <- reactiveValues()
      
      rebuild_df <- function(){
        current_data$sheet = load_sheet()
        current_data$segment_data = make_segment_data()
      }
      
      observeEvent(tab_(),{
        # print(paste0("tab 4 visualization: tab loaded: ", tab_()))
      # current_tab$tab = tab_()
      if(tab_() == "tab_4"){
        rebuild_df()
      }
      })

      output$historiogram <- renderGirafe({print_historiograms()})
    })
}

