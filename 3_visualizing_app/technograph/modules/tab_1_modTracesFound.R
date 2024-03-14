library(ggiraph, "MetBrewer")

tab_1_overviewTracesFoundUI <- function(id) {
  ns <- NS(id)
  tagList(
    addGFontHtmlDependency(family = c("Roboto Mono")),
    girafeOutput(ns("overviewTracesFound"))
  )
}

tab_1_overviewTracesFoundServer <- function(id, tab_, sphere) {

  moduleServer(
    id,
    function(input, output, session) {
      
      ANNOTATION_IS_EMPTY <- TRUE
      
      # helper function checking on if there are any annotations for current sphere ----------------------------------------
      check_gs_empty <- function(gs){
        ANNOTATION_IS_EMPTY <<- gs %>% nrow() == 0
      }
      
      # get spreadsheet for translating sites ---------------------------------------
      get_spreadsheet_sites <- function(){
        load(paste0("data/", current_data$sphere_to_load, "/gs_domain_to_look.RData"))
        return(gs_domain_to_look)
      }
      
      # get all the sites with no findings found, no snippets, no comments ---------------
      get_data_sites_na <- function(){
        print("1 tab, 2 vis, aggregating data for printing missing archived data")
        
        db_sites <- tbl(pool, "sites") %>% 
          filter(sphere == !!current_data$sphere_to_load, of_interest == TRUE) %>% 
          select(site, crawl_date, sha1) %>% 
          distinct() %>% 
          mutate(year = year(crawl_date)) %>% 
          group_by(year, site) %>% 
          summarise(counted_site = n()) %>% 
          ungroup() %>% 
          collect()
        
        # View(db_sites)
        
        year <- df_timespan_year %>% mutate(year = year(value)) %>% select(year) %>% pull(.)
        # year <- df_timespan_year %>% select(value) %>% pull(.)
        site <- tbl(pool, "sites") %>% 
          filter(sphere == !!current_data$sphere_to_load, of_interest == TRUE) %>% 
          distinct(site) %>% 
          collect() %>% #View()
          pull(site)
        
        df_na_sites <- tidyr::expand_grid(year, site) %>% 
          left_join(., db_sites) %>% 
          filter(is.na(counted_site)) %>% 
          mutate(counted_sites = "no sites archived") %>% 
          left_join(., current_data$sites_of_interest, by = "site")
        
        # View(df_na_sites)
        return(df_na_sites)
      }
      
      # get all those site with traces found, snippets and comments --------------------------
      
      get_data_automated_findings <- function(){
        print("1 tab, 2 vis, aggregating data for automated findings")
        # print(str(current_data$sites_of_interest))
        
        retranslate_snippets <- read_csv("data/helper/23-01-13-Commenting-system-detection-patterns.csv", show_col_types = FALSE) %>% 
          select("system" = `Commenting system`, "snippet" = Snippet) %>% 
          filter(!is.na(system)) %>% select(snippet, system)
        
        # print("retranslate snippets")
        
        db_snippet_findings <- tbl(pool, "sites") %>%
          select(sites_id, sha1, crawl_date, sphere, site, of_interest) %>% 
          inner_join(., tbl(pool, "snippets_2"), by = c("sha1" = "site")) %>% #collect()
          filter(sphere == !!current_data$sphere_to_load, of_interest == TRUE, detected == 1) %>% #collect()
          mutate(year = year(crawl_date)) %>% 
          select(site, year, snippet) %>%
          distinct() %>% 
          collect() %>% #View()
          left_join(., retranslate_snippets) %>% 
          select(site, year, system) %>% 
          distinct()
            # group_by(site, snippet, )
        
        db_form_finings <- tbl(pool, "sites") %>%
          select(sites_id, sha1, crawl_date, site, of_interest) %>%
          inner_join(., tbl(pool, "findings_hashed_2"), by = "sha1") %>%
          filter(sphere == !!current_data$sphere_to_load, of_interest == TRUE) %>%
          mutate(year = year(crawl_date),
                 system = "forms parsed") %>%
          select(site, year, system) %>% 
          distinct() %>% 
          collect()
        
        # print("retranslate sites")
        
        df_traces_found <- db_snippet_findings %>% 
          bind_rows(., db_form_finings) %>% 
          group_by(year, site) %>% ## these lines until the end needed for building the aes for plotting
          mutate(
            nr_systems_per_site = row_number() %>% max(.),
            duplicated = ifelse(is.na(system) & nr_systems_per_site > 1, "duplicated", NA)) %>% #View()
          filter(is.na(duplicated)) %>%
          mutate(
            nr_systems_per_site = row_number(),
            year_printing = paste0(year, ".", nr_systems_per_site) %>% as.numeric(),
            type = "automated") %>% #View()
          ungroup() %>% 
          left_join(., current_data$sites_of_interest, by = "site")
        # View(df_traces_found)
        return(df_traces_found)
      }
      
      # get manually added data points, google sheets are currently offline loaded into the app --------------------------
      get_annotated_data <- function(){
        annotated_data <- read_csv(paste0("data/", SPREADSHEET_ANNOTATION[[{current_data$sphere_to_load}]]), show_col_types = FALSE)
        
        check_gs_empty(annotated_data)
        if(!ANNOTATION_IS_EMPTY) {
          gs_annotation <- annotated_data %>%
            mutate(type = "manual",
                   date = paste0(year, "-", ifelse(is.na(month), "1", month), "-", ifelse(is.na(day), "1", day)) %>% ymd(.),
                   technology = ifelse(is.na(technology), "other", technology))
        }
        else{
          gs_annotation <- annotated_data %>%
            mutate(type = "manual",
                   technology = ifelse(is.na(technology), "other", technology))
        }
        
        return(gs_annotation)
      }
      

      # print the visualization -----------------------------------------
      
      plot_traces_found <- function(){
        
        systems_found <- current_data$snippet_data %>% select(system) %>% distinct() %>% pull(.)
        color_values <- df_systems_color_legend %>% filter(system %in% systems_found) %>% deframe()
        
        plot_snippets <- ggplot() +
          geom_tile(data = current_data$sites_na, aes(x = year, y = Name), fill = "grey90") +
          geom_point_interactive(data = current_data$snippet_data, aes(x = year_printing, y = Name, fill = system, shape = type, tooltip = paste0("site: ", site, "\nyear: ", year, "\nsystem: ", system)), width = .2, height = 0, na.rm = TRUE, size = 5, color = "black", alpha = .8) +
          geom_point_interactive(data = current_data$annotated_sites, aes(x = year, y = Name, fill = technology, tooltip = paste0("site: ", site, "\nyear: ", year, "\ncomment: ", tooltip_info, "\nsystem: ", technology), shape = type), size = 5, color = "black", alpha = .8, position = position_nudge(x = -.3)) +
          scale_fill_manual(values = color_values, na.value = NA, name = "type of system") +
          scale_shape_manual(values = c(21, 23), breaks = c("automated", "manual"), name = "type of observation")+
          scale_x_continuous(breaks = year_breaks_for_plotting, labels = year_breaks_for_plotting,  expand = c(0, NA), name = "crawl year") +#, limits = year_breaks) +
          theme_b03_base + theme_b03_dot_timeline +  
          coord_cartesian(clip = "off") +
          guides(
            fill = guide_legend(title.position = "top", override.aes = list(shape = 22)),
            shape = guide_legend(title.position = "top", override.aes = list(color = "black"))
          )
        
        girafe(ggobj = plot_snippets, fonts = list(mono = typo_sfb_mono_remote), options = list(opts_sizing(rescale = FALSE)),
               width_svg = 15,
               height_svg = 10
        )
        # girafe(ggobj = plot,
        #        options = list(opts_sizing(rescale = FALSE)),
        #        width_svg = 15,
        #        height_svg = current_data$height)
      }
      
      # reactive value handling ---------------------------------------------------
      
      current_tab <- reactiveValues()
      
      observeEvent(tab_(),{
        print(paste0("overviewTracesFound tab loaded: ", tab_()))
        current_tab$tab = tab_()
      })
      
      
      current_data <- reactiveValues()
      
      observeEvent(sphere(),{
        if(current_tab$tab == "tab_1"){
          # print(paste0("findingsVizServer ", site_to_load()))
          current_data$sphere_to_load = sphere()
          current_data$sites_of_interest <- get_spreadsheet_sites()
          current_data$sites_na = get_data_sites_na()
          current_data$snippet_data = get_data_automated_findings()
          current_data$annotated_sites = get_annotated_data()
          # current_data$form_data = get_form_finding_data()
          # current_data$height = get_height()
        }
        else{
          print("doing nothing")
        }
      })
      
      output$overviewTracesFound <- renderGirafe({plot_traces_found()})
    
  })
  
}

