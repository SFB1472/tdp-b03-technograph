
library(tidyverse)
library(lubridate)
library(urltools)
library(MetBrewer)
library(DBI)
library(RPostgres)
library(re2)
library(ggiraph)
library(cli)
library(BAMMtools)
library(scales)
library(digest)


SPHERE_FOR_SHEET <- "German"


con <- dbConnect(RPostgres::Postgres(), 
                 dbname = dsn_database,
                 host = dsn_hostname, 
                 port = dsn_port,
                 user = dsn_uid, 
                 password = dsn_pwd
)

# AND tc.site LIKE '00012777602a9a99e667f0e4b5416440a9d0f081' 

# creating a group_id for all form tags  ----------------------------------------------------------------------------------------------------------------------------------------------------------

df_form_groups <- dbGetQuery(conn = con, paste0("SELECT DISTINCT s.crawl_date, s.sphere,s.sha1, tc.group, tc.name FROM sites s INNER JOIN tags_context tc ON tc.site = s.sha1 WHERE s.sphere LIKE '", SPHERE_FOR_SHEET, "' ORDER BY s.sha1, tc.group")) %>% 
  group_by(sha1, name) %>% 
  mutate(is_form = ifelse(name == "form", 1, 0)) %>% 
  ungroup() %>% 
  group_by(sha1, is_form) %>% 
  mutate(form_group = row_number(),
         form_group = ifelse(is_form == 0, NA, form_group)) %>% 
  ungroup() %>%
  group_by(sha1) %>% 
  fill(form_group) %>% 
  mutate(id_sha1_form_group = paste0(sha1, "_", form_group))

# appling this id to all elements nested in the form tags no whether if there are traces of commenting or not ----------------------------------------------------------------------------------------------

df_all_context_form_tags <- dbGetQuery(conn = con, paste0("SELECT DISTINCT s.crawl_date, s.sphere, s.site, s.url, s.sha1, tc.parent_path_str, tc.group, tc.name, tc.value, tc.text FROM sites s INNER JOIN tags_context tc ON tc.site = s.sha1 WHERE s.sphere LIKE '", SPHERE_FOR_SHEET, "'  ORDER BY s.sha1, tc.group" )) %>% 
  mutate(archive_url = paste0("http://web.archive.org/web/", crawl_date, "/", url),
         path_form = str_extract(parent_path_str, "form;.*$")) %>% 
  left_join(., df_form_groups) %>% 
  group_by(sha1, form_group) %>% 
  fill(path_form, .direction = "up") 

# fetching all forms with comment traces and appling the form ids to it ------------------------------------------------------------------------------------------------------------------------

df_form_context <- dbGetQuery(conn = con, paste0("SELECT DISTINCT s.crawl_date, s.sphere, s.site, s.url, tc.site as sha1, tc.parent_path_str, tc.group, regexp_matches(tc.value, '", COMMENTS_IN_TAGS, "') as matches FROM sites s INNER JOIN tags_context tc ON tc.site = s.sha1 WHERE s.sphere LIKE '", SPHERE_FOR_SHEET, "'  ORDER BY sha1, tc.group" ))  %>% 
  left_join(., df_form_groups) 

# extracting all those ids with comment traces  ----------------------------------------------------------------------------------------------------------------------------------------------------------

ids_to_look_at <- df_form_context %>% select(id_sha1_form_group) %>% distinct() %>% pull(.)

# filter the big df down to only thoses forms with comment traces and all elements nestes  ------------------------------------------------------------------------------------------------------------------------

df_form_context_full <- df_all_context_form_tags %>% 
  filter(id_sha1_form_group %in% ids_to_look_at)
  
# hashing all form tags with nested elements for better checking on changes  ------------------------------------------------------------------------------------------------------------------------

df_hashed_forms <- df_form_context_full %>% 
  select(crawl_date, site, sha1, id_sha1_form_group, parent_path_str, group, name, value, text, form_group) %>% 
  # filter(sha1 == "000e2ca1dfa73ba2f57b64a8dd25701f3c68f49c") %>% 
  # group_by(id_site_form_group) %>% 
  mutate(hashed_forms = digest::sha1(c(parent_path_str, group, name, value, text, form_group), serialize = F),
         sphere = SPHERE_FOR_SHEET) %>% 
  ungroup() %>% 
  select(sha1, id_sha1_form_group, hashed_forms, sphere) %>% 
  distinct() #%>% View()

df_hashed_forms %>% select(sha1) %>% distinct() %>% nrow()

df_hashed_forms %>% select(hashed_forms) %>% distinct() %>% nrow()

# write new table of hashes to db ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dbCreateTable(conn = con, name = "findings_hashed", fields = df_hashed_forms)



  