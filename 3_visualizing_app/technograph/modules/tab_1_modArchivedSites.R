library(ggiraph, "MetBrewer")

tab_1_archivedSitesUI <- function(id) {
  ns <- NS(id)
  tagList(
    addGFontHtmlDependency(family = c("Roboto Mono")),
    girafeOutput(ns("archivedSites"))
  )
}

tab_1_archivedSitesServer <- function(id, tab_, sphere) {

  moduleServer(
    id,
    function(input, output, session) {
      
      # get all the sites with no findings found, no snippets, no comments ---------------
      get_sites <- function(){
        print("1 tab, 2 vis, aggregating data for printing missing archived data")
        
        db_sites <- tbl(pool, "sites") %>% 
          filter(sphere == !!current_data$sphere_to_load, of_interest == TRUE) %>% 
          select(site, crawl_date, sha1) %>% 
          mutate(year = year(crawl_date)) %>% 
          distinct() %>% 
          group_by(year, site) %>% 
          summarise(counted_sites = n()) %>% 
          ungroup() %>% 
          collect()
        
        return(db_sites)
      }
      
      # get all those site with traces found, snippets and comments --------------------------
      
      get_data_snippets_found <- function(){
        print("1 tab, 3 vis, sites archived")
        
        db_snippet_findings <- tbl(pool, "sites") %>%
          select(sites_id, sha1, crawl_date, sphere, site, of_interest) %>% 
          inner_join(., tbl(pool, "snippets_2"), by = c("sha1" = "site")) %>% #collect()
          filter(sphere == !!current_data$sphere_to_load, of_interest == TRUE, detected == 1) %>% #collect()
          distinct(site) %>% 
          collect() #%>% #View()
          
        db_form_finings <- tbl(pool, "sites") %>%
          select(sites_id, sha1, crawl_date, site, of_interest) %>%
          inner_join(., tbl(pool, "findings_hashed_2"), by = "sha1") %>%
          filter(sphere == !!current_data$sphere_to_load, of_interest == TRUE) %>%
          mutate(year = year(crawl_date),
                 system = "forms parsed") %>%
          # select(site) %>% 
          distinct(site) %>% 
          collect()
        
        df_traces_found <- db_snippet_findings %>% 
          bind_rows(., db_form_finings) #%>% 
          
        # View(df_traces_found)
        return(df_traces_found)
      }
      
      # print the visualization -----------------------------------------
      
      get_sites_over_time <- function(){
        load(paste0("data/", current_data$sphere_to_load, "/gs_domain_to_look.RData"))
        # load(paste0("3_visualizing_app/technograph/data/German/gs_domain_to_look.RData"))
        site_with_systems <- current_data$snippet_data %>% select(site) %>% pull(.)
        
        plot <- current_data$sites %>% 
          mutate(system_detected = ifelse(site %in% site_with_systems, "system found", "no system found")) %>% 
          left_join(., gs_domain_to_look, by = "site") %>%
          ggplot(aes(x = year, y = Name, fill = counted_sites)) +
          geom_tile_interactive(aes(tooltip = paste0("year: ", year, "\nsite: ", site, "\nsites_counted: ", counted_sites))) +
          scale_fill_gradientn(colors = met.brewer("Hokusai2", type="continuous"), na.value = "grey90", name = "number of websites available" ) +
          scale_x_continuous(breaks = year_breaks_for_plotting, expand = c(0, NA), name = "crawl_year") +
          scale_y_discrete(expand = c(0, NA)) +
          ggforce::facet_col(facets = vars(system_detected), scales = "free_y", space = "free") +
          theme_b03_base + theme_b03_heatmap + theme_b03_facets  +
          guides(fill = guide_colorbar(title.position = "top", barwidth = unit(20, "lines"), barheight = unit(.5, "lines")))
        
        girafe(ggobj = plot, options = list(opts_sizing(rescale = FALSE)),
               width_svg = 15,
               height_svg = 10
        ) 
      }
      
      # reactive value handling ---------------------------------------------------
      
      current_tab <- reactiveValues()
      
      observeEvent(tab_(),{
        print(paste0("sites archived tab loaded: ", tab_()))
        current_tab$tab = tab_()
      })
      
      
      current_data <- reactiveValues()
      
      observeEvent(sphere(),{
        if(current_tab$tab == "tab_1"){
          current_data$sphere_to_load = sphere()
          current_data$sites = get_sites()
          current_data$snippet_data = get_data_snippets_found()
        }
        else{
          print("doing nothing")
        }
      })
      
      output$archivedSites <- renderGirafe({get_sites_over_time()})
    
  })
  
}

