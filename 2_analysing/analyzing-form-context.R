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
library(dbx)

source("config/config-secret.R")

SPHERE_FOR_SHEET <- "World"

con <- dbConnect(RPostgres::Postgres(), 
                 dbname = dsn_database,
                 host = dsn_hostname, 
                 port = dsn_port,
                 user = dsn_uid, 
                 password = dsn_pwd
)

# creating a group_id for all form tags  ----------------------------------------------------------------------------------------------------------------------------------------------------------

first_sites <- dbGetQuery(conn = con, paste0("SELECT DISTINCT tc.sphere, tc.site FROM tag_context tc WHERE tc.sphere = '", SPHERE_FOR_SHEET, "'")) %>% 
  select(site) %>% distinct() %>% pull(.)

df_form_groups <- dbGetQuery(conn = con, paste0("SELECT DISTINCT tc.sphere, tc.site, tc.group, tc.name FROM tag_context_2 tc WHERE tc.sphere = '", SPHERE_FOR_SHEET, "' ORDER BY tc.site, tc.group")) %>% 
  filter(!site %in% first_sites) %>% 
  mutate(is_form = ifelse(name == "form", 1, 0), .by = c(site, name)) %>% 
  mutate(form_group = row_number(),
         form_group = ifelse(is_form == 0, NA, form_group), .by = c(site, is_form)) %>% 
  group_by(site) %>% 
  fill(form_group) %>% 
  mutate(id_sha1_form_group = paste0(site, "_", form_group)) %>% 
  ungroup()

# applying this id to all elements nested in the form tags if there are traces of commenting or not ----------------------------------------------------------------------------------------------

db_form_groups <- df_form_groups %>% select(site, group, name, sphere, id_sha1_form_group) %>% distinct()

dbxUpdate(con, "tag_context_2", db_form_groups, where_cols = c("site", "group", "name", "sphere"))

# test <- dbGetQuery(conn = con, paste0("SELECT tc.id_sha1_form_group, tc.site, tc.group, tc.name FROM tag_context tc WHERE tc.sphere LIKE 'German'"))
dbGetQuery(conn = con, paste0("SELECT DISTINCT tc.sphere, tc.site, tc.group, tc.id_sha1_form_group FROM tag_context_2 tc INNER JOIN sites s ON tc.site = s.sha1  WHERE s.export = 2 AND tc.sphere = '", SPHERE_FOR_SHEET, "' ORDER BY tc.site, tc.group"))# %>% 
  # filter(!site %in% first_sites) %>% View()

# filter the big df down to only those forms with comment traces and all elements nested  ------------------------------------------------------------------------------------------------------------------------

df_form_context_full <- dbGetQuery(conn = con, paste0("SELECT DISTINCT s.site, s.sha1, s.crawl_date, tc.sphere, tc.id_sha1_form_group, tc.group, tc.name, tc.attr, tc.value, tc.text FROM sites s INNER JOIN tag_context_2 tc ON s.sha1 = tc.site WHERE tc.id_sha1_form_group IN (SELECT DISTINCT tc.id_sha1_form_group FROM tag_context_2 tc WHERE tc.sphere = '", SPHERE_FOR_SHEET, "' AND tc.name = 'form' AND tc.value ~ '", COMMENTS_IN_TAGS,"') ORDER BY s.sha1, tc.group")) %>% 
  filter(attr != "action")

df_form_context_full <- dbGetQuery(conn = con, paste0("SELECT DISTINCT s.site, s.sha1, s.crawl_date, tc.sphere, tc.id_sha1_form_group, tc.group, tc.name, tc.attr, tc.text FROM sites s INNER JOIN tag_context_2 tc ON s.sha1 = tc.site WHERE tc.id_sha1_form_group IN (SELECT DISTINCT tc.id_sha1_form_group FROM tag_context_2 tc WHERE tc.sphere = '", SPHERE_FOR_SHEET, "' AND tc.name = 'form' AND tc.value ~ '", COMMENTS_IN_TAGS,"') ORDER BY s.sha1, tc.group")) 

0072d1d32f14f2879f7d6361011afefda0094196_2
01545c628333a5b596d254e1b8cd540f6ed9ec51_2

df_form_context_full <- df_form_context_full %>% filter(!site %in% first_sites)

# df_form_context_full %>% select(id_sha1_form_group) %>% distinct() %>% nrow()

# hashing all form tags with nested elements for better checking on changes  ------------------------------------------------------------------------------------------------------------------------
# rm(df_hashed_forms)

df_hashed_forms <- df_form_context_full %>% 
  select(id_sha1_form_group, name, attr, text) %>% 
  reframe(hashed_forms = digest::sha1(c(name, attr, text), serialize = F), .by = id_sha1_form_group) %>% #,
  mutate(sphere = SPHERE_FOR_SHEET,
         sha1 = str_remove(id_sha1_form_group, "_\\d{1,}$"),
         iteration = 2)

# df_test_1 <- df_form_context_full %>% 
#   filter(site == "bild") %>% 
#   left_join(., df_hashed_forms) %>% 
#   arrange(crawl_date) %>% 
#   mutate(nr_unique_hashes = match(hashed_forms, unique(hashed_forms)))# %>% 
#   # filter(id_sha1_form_group == "133f83214d349e94be9ec1715fe2e4f484e23064_3")
#   # View()
# 
# df_test_2 <- df_form_context_full %>% 
#   filter(site == "bild") %>% 
#   left_join(., df_hashed_forms) %>% 
#   arrange(crawl_date) %>% 
#   mutate(nr_unique_hashes = match(hashed_forms, unique(hashed_forms))) %>% 
#   filter(id_sha1_form_group == "8cfc8474fde7a42d3bbe955953aebc4674e22048_3")


133f83214d349e94be9ec1715fe2e4f484e23064_3
8cfc8474fde7a42d3bbe955953aebc4674e22048_3

# write new table of hashes to db ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# dbCreateTable(conn = con, name = "findings_hashed", fields = df_hashed_forms)
dbWriteTable(conn = con, name = "findings_hashed_2", value = df_hashed_forms, append = TRUE)

# test <- dbGetQuery(conn = con, paste0("SELECT tc.id_sha1_form_group FROM findings_hashed_2 tc WHERE tc.iteration = 2"))

dbDisconnect(con)

  