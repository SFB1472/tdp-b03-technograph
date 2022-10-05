library(tidyverse)
library(urltools)
library(BAMMtools)
library(lubridate)

source("config/config.R")

### reading in google-spreadsheet downloaded from: https://docs.google.com/spreadsheets/d/10Dy9nvovj1HCFVIok417d0JErk_caHQs/edit#gid=826641030

df_domains_to_analyse <- read_csv(file = "data/helper/22-09-21-Top News Websites [AU - public].xlsx - German news.csv") %>% 
  mutate(cleaned_urls = domain(URL) %>% suffix_extract(.) %>% select(domain) %>% pull(.),)

load("data/df_snippet_info.RData")


######################################################
## preparing data for dot plot graphic 
######################################################

df_counted_sites_year <- df_snippet_info %>% 
  filter(site %in% df_domains_to_analyse$cleaned_urls) %>% 
  select(year, site, sha1) %>% 
  distinct() %>% View()
  group_by(year, site) %>% 
  summarise(counted_sites = n()) %>% 
  ungroup()

jenks_color_breaks <- getJenksBreaks(df_counted_sites_year %>% select(counted_sites) %>% pull(.), 5)

df_sites_year <- df_counted_sites_year %>% 
  pivot_wider(., id_cols = site, names_from = year, values_from = counted_sites) %>% #View()
  pivot_longer(., 2:last_col(), names_to = "year", values_to = "counted_sites") %>% 
  mutate(color_breaks = cut(counted_sites, 
                            breaks = jenks_color_breaks,
                            include.lowest = TRUE,
                            right = TRUE,
                            ordered_result = FALSE),
         year = as.numeric(year)
  )

save(df_sites_year, file = paste0(path_to_shinydata,"df_sites_year.RData"))


#####################################################################################################################
## preparing data for printing heatmap about how many pages of a domain are in the dump from the internet archive 
#####################################################################################################################


df_snippet_mapping <- read_csv(file = "../data/helper/22-07-15-Commenting-system-detection-patterns.csv") %>% select(-Regex,-Target, "system" = "Commenting system")

df_snippets_year <- df_snippet_info %>% 
  ungroup() %>% 
  filter(site %in% df_domains_to_analyse$cleaned_urls) %>% #View()
  mutate(year = year(crawl_date)) %>% #View()
  group_by(year, site, snippet) %>% 
  summarise(counted_snippets = sum(detected, na.rm=TRUE)) %>% 
  ungroup() %>% 
  arrange(desc(counted_snippets)) %>% 
  filter(counted_snippets > 0) %>% 
  left_join(., df_snippet_mapping, by = c("snippet" = "Snipit"))

save(df_snippets_year, file = paste0(path_to_shinydata, "df_snippets_year.RData"))
