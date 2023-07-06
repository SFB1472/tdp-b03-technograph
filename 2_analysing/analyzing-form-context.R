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

SPHERE_FOR_SHEET <- "Dutch"

con <- dbConnect(RPostgres::Postgres(), 
                 dbname = dsn_database,
                 host = dsn_hostname, 
                 port = dsn_port,
                 user = dsn_uid, 
                 password = dsn_pwd
)

# creating a group_id for all form tags  ----------------------------------------------------------------------------------------------------------------------------------------------------------

df_form_groups <- dbGetQuery(conn = con, paste0("SELECT DISTINCT tc.sphere, tc.site, tc.group, tc.name FROM tag_context tc WHERE tc.sphere LIKE '", SPHERE_FOR_SHEET, "' ORDER BY tc.site, tc.group")) %>% 
  mutate(is_form = ifelse(name == "form", 1, 0), .by = c(site, name)) %>% 
  mutate(form_group = row_number(),
         form_group = ifelse(is_form == 0, NA, form_group), .by = c(site, is_form)) %>% 
  group_by(site) %>% 
  fill(form_group) %>% 
  mutate(id_sha1_form_group = paste0(site, "_", form_group)) %>% 
  ungroup()

# applying this id to all elements nested in the form tags if there are traces of commenting or not ----------------------------------------------------------------------------------------------

db_form_groups <- df_form_groups %>% select(site, group, name, sphere, id_sha1_form_group) %>% distinct()

dbxUpdate(con, "tag_context", db_form_groups, where_cols = c("site", "group", "name", "sphere"))

# test <- dbGetQuery(conn = con, paste0("SELECT tc.id_sha1_form_group, tc.site, tc.group, tc.name FROM tag_context tc WHERE tc.sphere LIKE 'German'"))


# filter the big df down to only those forms with comment traces and all elements nestes  ------------------------------------------------------------------------------------------------------------------------

df_form_context_full <- dbGetQuery(conn = con, paste0("SELECT DISTINCT tc.sphere, tc.site, tc.id_sha1_form_group, tc.group, tc.name, tc.attr, tc.value, tc.text FROM tag_context tc WHERE tc.id_sha1_form_group IN (SELECT DISTINCT tc.id_sha1_form_group FROM tag_context tc WHERE tc.sphere LIKE '", SPHERE_FOR_SHEET, "' AND tc.name LIKE 'form' AND tc.value ~ '", COMMENTS_IN_TAGS,"') ORDER BY tc.site, tc.group")) %>% 
  filter(attr != "action")

# df_form_context_full %>% select(id_sha1_form_group) %>% distinct() %>% nrow()

# hashing all form tags with nested elements for better checking on changes  ------------------------------------------------------------------------------------------------------------------------

df_hashed_forms <- df_form_context_full %>% 
  select(id_sha1_form_group, name, attr, value, text) %>% 
  reframe(hashed_forms = digest::sha1(c(name, attr, value, text), serialize = F), .by = id_sha1_form_group) %>% #,
  mutate(sphere = SPHERE_FOR_SHEET,
         sha1 = str_remove(id_sha1_form_group, "_\\d{1,}$"))

# write new table of hashes to db ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# dbCreateTable(conn = con, name = "findings_hashed", fields = df_hashed_forms)
dbWriteTable(conn = con, name = "findings_hashed", value = df_hashed_forms, append = TRUE)

dbDisconnect(con)

  