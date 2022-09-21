library(tidyverse)
library(lubridate)
library(googlesheets4)
library(BAMMtools)

path_to_shinydata <- "shiny/b03-commenting-museum/"
site_of_interest <- "ksta"

load(file = paste0(path_to_shinydata, "data/df_raw_vis_data.RData"))
load(file = paste0(path_to_shinydata, "data/df_snippets_year.RData"))
load(file = paste0(path_to_shinydata, "data/df_sites_year.RData"))

link_manuell_added_data_points_domains <- "https://docs.google.com/spreadsheets/d/1aEQAAh0UZlFImajCQ-E7cH-0nrlH8TGuxQt4PTbVCJE/"
# gs4_deauth()
# gs4_auth(cache=".secrets")

ss <- gs4_get(link_manuell_added_data_points_domains)
gs_manual_domain_snippet_special <- read_sheet(ss) %>% 
  select(-month) %>% 
  filter(site == site_of_interest) %>% 
  mutate(type = "manual",
         technology = ifelse(is.na(technology), "other", technology),
         month = "09",
         date = paste0(year, "-", month, "-01") %>% ymd(.)) %>% 
  distinct()

gs4_deauth()

########################################################################################
## VISUALISIERUNG nur ksta: Datensatz, für welche Jahre haben wir Daten? (wie viele?) ##
########################################################################################


df_counted_sites_year <- df_snippet_info %>% 
  # filter(site %in% site_of_interest) %>% 
  select(year, site, sha1) %>% 
  distinct() %>% 
  group_by(year, site) %>% 
  summarise(counted_sites = n()) %>% 
  ungroup()

df_counted_sites_year_special <- df_snippet_info %>% 
  filter(site %in% site_of_interest) %>% 
  select(year, site, sha1) %>% 
  distinct() %>% 
  group_by(year, site) %>% 
  summarise(counted_sites = n()) %>% 
  ungroup() 

jenks_color_breaks <- getJenksBreaks(df_counted_sites_year_special %>% select(counted_sites) %>% pull(.), 4)

df_years_to_show <- df_counted_sites_year %>% select(year) %>% distinct()

df_sites_year_special <- df_counted_sites_year_special %>% 
  pivot_wider(., id_cols = site, names_from = year, values_from = counted_sites) %>% #View()
  pivot_longer(., 2:last_col(), names_to = "year", values_to = "counted_sites") %>% 
  mutate(color_breaks = cut(counted_sites, 
                            breaks = jenks_color_breaks,
                            include.lowest = TRUE,
                            right = TRUE,
                            ordered_result = FALSE),
         year = as.numeric(year)
  ) %>% 
  right_join(., df_years_to_show) %>% 
  mutate(site = ifelse(is.na(site), site_of_interest, site),
         date = paste0(year,"-01-01") %>% ymd(.))
  



plot_1_how_much_data <- df_sites_year_special %>% 
  # filter(site == site_of_interest) %>% 
  ggplot(aes(x = year, y = site, fill = counted_sites)) +
  geom_tile_interactive(aes(tooltip = paste0("year: ", year, "\nsite: ", site)), color = "white", size = .3) +
  scale_fill_gradientn(colors = met.brewer("Hokusai2", type="continuous"), na.value = "grey90", name = "number of websites available" ) +
  scale_x_continuous(breaks = df_years_to_show$year, expand = c(0, NA)) +
  scale_y_discrete(expand = c(0, NA)) +
  theme_b03_heatmap_intro +
  guides(fill = guide_colorbar(title.position = "top", barwidth = unit(20, "lines"), barheight = unit(.5, "lines")))

plot_1_how_much_data

##################################################################################
## VISUALISIERUNG nur ksta, nur automatisiert (on top of erster Visualisierung) ##
##################################################################################

df_snippets_per_year_special <- df_snippets_year %>% 
  filter(site == site_of_interest) %>% 
  mutate(month = "03",
         date = paste0(year, "-", month, "-01") %>% ymd(.))

plot_2_automated_snippets_found <- ggplot()+
  geom_tile(data = df_sites_year_special, aes(x = year, y = site, fill = counted_sites)) +
  geom_point(data = df_snippets_per_year_special, aes(x = year, y = site), size = 7, color = "black", fill = "white", shape = 21, alpha = .8) +
  scale_fill_gradientn(colors = met.brewer("Hokusai2", type="continuous"), na.value = "grey90", name = "number of websites available" , guide = guide_legend(override.aes = list(alpha = .5))) +
  scale_x_continuous(breaks = df_years_to_show$year, expand = c(0, NA), name = "crawl_year") +
  scale_y_discrete(expand = c(0, NA)) +
  theme_b03_heatmap_intro +
  guides(fill = guide_colorbar(title.position = "top", barwidth = unit(20, "lines"), barheight = unit(.5, "lines")))

plot_2_automated_snippets_found


######################################################
## VISUALISIERUNG nur ksta, manuell + automatisiert ##
######################################################


plot_3_manual_verification <- ggplot()+
  geom_tile(data = df_sites_year_special, aes(x = year, y = site, fill = counted_sites), color = "white", size = .3) +
  geom_point(data = df_snippets_per_year_special, aes(x = year, y = site), position = position_nudge(x = - 0.1), size = 7, color = "black", fill = "white", shape = 21, alpha = .8) +
  geom_point(data = gs_manual_domain_snippet_special, aes(x = year, y = site), shape = 23, position = position_nudge(x = .1), size = 7, color = "black", fill = "white", alpha = .8) +
  scale_fill_gradientn(colors = met.brewer("Hokusai2", type="continuous"), na.value = "grey90", name = "number of websites available") +
  scale_x_continuous(breaks = df_years_to_show$year, expand = c(0, NA), name = "crawl_year") +
  # scale_x_date(date_labels = "%Y", date_breaks = "years") +
  scale_y_discrete(expand = c(0, NA)) +
  theme_b03_heatmap_intro +
  guides(
    fill = 
      guide_colorbar(title.position = "top", barwidth = unit(20, "lines"), barheight = unit(.5, "lines")),
      guide = guide_legend(override.aes = list(alpha = .5)) 
                # guide_legend(override.aes = list(alpha = .5))
         # fill = guide_legend(override.aes = list(alpha = .5))
    
)
 

plot_3_manual_verification

## https://github.com/tidyverse/ggplot2/issues/3103
