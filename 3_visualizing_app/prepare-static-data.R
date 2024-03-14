library(tidyverse)
library(googlesheets4)

source("config/config.R")

gs4_auth(cache=".secrets")
get_sites_for_sphere <- function(sphere){
  gs_domain_to_look <- read_sheet(SPREADSHEET_PATH_GENERELL, sheet = SPREADSHEET_PATH_DOMAINS[[{{sphere}}]]) %>% 
  select(Name, URL) %>% 
  mutate(site = domain(URL) %>% suffix_extract(.) %>% select(domain) %>% pull(.)) 
}

## seiten für sphere abrufen und copy-paste in die config für shiny-app individual pasten
de <- get_sites_for_sphere("German") %>% select(site) %>% distinct()# %>% #pull(.) %>% shQuote(.) %>% toString(.) %>% clipr::write_clip()
nl <- get_sites_for_sphere("Dutch") %>% select(site) %>% distinct() #%>% #pull(.) %>% shQuote(.) %>% toString(.) %>% clipr::write_clip()
world <- get_sites_for_sphere("World") %>% select(site) %>% distinct()# %>% #pull(.) %>% shQuote(.) %>% toString(.) %>% clipr::write_clip()

get_sites_with_form_findings <- function(sphere){
  df_form_groups <- dbGetQuery(conn = con, paste0("SELECT DISTINCT s.site FROM tag_context_2 t INNER JOIN sites s ON t.site = s.sha1 WHERE s.sphere = '", sphere, "'"))
}


get_sites_with_snippet_findings <- function(sphere){
  df_form_groups <- dbGetQuery(conn = con, paste0("SELECT DISTINCT s.site FROM snippets_2 sp INNER JOIN sites s ON sp.site = s.sha1 WHERE s.sphere = '", sphere, "' AND sp.detected = 1"))
}

get_sites_with_form_findings("German") %>% 
  bind_rows(get_sites_with_snippet_findings("German")) %>% 
  filter(site %in% de$site) %>% 
  distinct() %>% 
  arrange(site) %>% pull(.) %>% shQuote(.) %>% toString(.) %>% clipr::write_clip()

get_sites_with_form_findings("Dutch") %>% 
  bind_rows(get_sites_with_snippet_findings("Dutch")) %>% 
  filter(site %in% nl$site) %>% 
  distinct() %>% pull(.) %>% shQuote(.) %>% toString(.) %>% clipr::write_clip()

get_sites_with_form_findings("World") %>% 
  bind_rows(get_sites_with_snippet_findings("World")) %>% 
  filter(site %in% world$site) %>% 
  distinct() %>% pull(.) %>% shQuote(.) %>% toString(.) %>% clipr::write_clip()
