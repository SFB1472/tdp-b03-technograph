library(tidyverse)
library(RCurl)
library(httr)
library(lubridate)
library(yaml)
library(jsonlite)
library(urltools)
library(googlesheets4)

source("config/config.R")
source("config/config-secret.R")

req <- GET(PATH_DATA_CONFIG, authenticate(github_token, ""))
config <- content(req, as = "parsed", type = "application/json" )$content %>% base64_dec() %>% rawToChar() %>% yaml.load()


gs4_auth(cache=".secrets")
gs_domain_to_look <- read_sheet(SPREADSHEET_PATH_GENERELL, sheet = SPREADSHEET_PATH_DOMAINS[[{{CURRENT_SPHERE}}]]) %>% 
  select(Name, URL) %>% 
  mutate(site = domain(URL) %>% suffix_extract(.) %>% select(domain) %>% pull(.)) 

save(gs_domain_to_look, file = paste0(PATH_TO_SHINY_DATA, "/", CURRENT_SPHERE,"/gs_domain_to_look.RData"))

# create_table_available_sites <- function(path, domain){
#   df_archived_site <- readr::read_csv(file = paste0(path, "?token=",jupyter_token), col_select = c("crawl_date", "url", "md5")) %>% 
#     mutate(archive_url = paste0("http://web.archive.org/web/", crawl_date, "/", url)) %>% 
#     select(crawl_date, archive_url, md5)
#   write_csv(df_archived_site, file = paste0("data/index/",domain,"-analysed-sites.csv") )
# }

create_table_available_sites_counted <- function(sphere){
  df_sites_per_year <- readr::read_csv(file = paste0(config$server$url, sphere, DATAPATH_RAW, "?token=", jupyter_token), col_select = c("crawl_date", "url", "md5")) %>% 
    mutate(year = ymd(crawl_date) %>% year(),
           site = domain(url) %>% suffix_extract(.) %>% select(domain) %>% pull(.)
          ) %>% 
    filter(site %in% gs_domain_to_look$site) %>% 
    distinct() %>% 
    group_by(year, site) %>% 
    summarise(counted_sites = n()) %>% 
    ungroup() %>% 
    pivot_wider(., id_cols = site, names_from = year, values_from = counted_sites) %>% #View()
    pivot_longer(., 2:last_col(), names_to = "year", values_to = "counted_sites") %>% 
    mutate(year = as.numeric(year))
  save(df_sites_per_year, file = paste0(PATH_TO_SHINY_DATA, CURRENT_SPHERE, "/df_sites_per_year.RData"))
}


# create_table_available_sites(DATAPATH_RAW_DE, "de")
# create_table_available_sites(DATAPATH_RAW_WORLD, "world")
# create_table_available_sites(DATAPATH_RAW_NL, "nl")

load(file = paste0("data/", CURRENT_SPHERE, "/df_sites_per_year.RData"))

## world
create_table_available_sites_counted(config$server$datasets$spheres$World)

#dutch
create_table_available_sites_counted(config$server$datasets$spheres$Dutch)

#german
create_table_available_sites_counted(config$server$datasets$spheres$German)

print(paste0(config$server$url, config$server$datasets$spheres$German, DATAPATH_RAW, "?token=",jupyter_token))


create_table_wanted_sites <- function(sphere){
  df_archived_site <- readr::read_csv(file = paste0(config$server$url, sphere, DATAPATH_RAW, "?token=",jupyter_token), col_select = c("crawl_date", "url", "md5", "sha1")) %>%
    mutate(site = domain(url) %>% suffix_extract(.) %>% select(domain) %>% pull(.)) %>%
    select(crawl_date, site, md5, sha1) %>% 
    filter(site %in% gs_domain_to_look$site)
  write_csv(df_archived_site, file = paste0("data/", CURRENT_SPHERE,"-wanted-sites.csv") )
}

create_table_wanted_sites(config$server$datasets$spheres$German)


df_archived_site_test <- readr::read_csv(file = paste0(config$server$url, config_data$server$datasets$spheres[{{CURRENT_SPHERE}}], DATAPATH_RAW, "?token=", jupyter_token), col_select = c("crawl_date", "url", "md5"), n_max = 5000) %>%
  mutate(year = ymd(crawl_date) %>% year(),
         site = domain(url) %>% suffix_extract(.) %>% select(domain) %>% pull(.)
  ) %>%
  filter(site %in% gs_domain_to_look$site) %>% 
  group_by(year, site) %>% 
  summarise(counted_sites = n()) %>% 
  pivot_wider(., id_cols = site, names_from = year, values_from = counted_sites) %>% #View()
  pivot_longer(., 2:last_col(), names_to = "year", values_to = "counted_sites") %>% 
  mutate(year = as.numeric(year))
