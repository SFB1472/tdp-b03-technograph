library(tidyverse)
library(DBI)
library(RPostgres)
library(digest)

source("config/config-secret.R")

SPHERE_FOR_SHEET <- "German"

con <- dbConnect(RPostgres::Postgres(), 
                 dbname = dsn_database,
                 host = dsn_hostname, 
                 port = dsn_port,
                 user = dsn_uid, 
                 password = dsn_pwd
)

# creating a group_id for all form tags  ----------------------------------------------------------------------------------------------------------------------------------------------------------


#### Verbesserung im Vergleich zu den form tags: Es gibt jetzt schon eine Gruppenvariable aus dem Parsing
#### in der Datenbank gibt es eine Spalte of_interest in der sites tabelle, so fallen hier einige Schritte raus.

df_tags_context <- read_csv(file = "data/1-parsing/tags-context/German/context-all-traces.csv", show_col_types = FALSE) %>% 
  bind_rows(., read_csv(file = "data/1-parsing/tags-context/German/context-all-traces-2.csv", show_col_types = FALSE)) %>% 
  select(-`...1`) %>% 
  mutate(tag_group = cur_group_id(), .by = c(sha1, context_path),
         id_sha1_group = paste0(sha1, "_", tag_group)) 

df_tags_context_ <- df_tags_context %>%  mutate(tag_group = cur_group_id(), .by = c(sha1, context_path))

dbWriteTable(conn = con, name = "tag_context", value = df_tags_context, append = TRUE)

# applying this id to all elements nested in the form tags if there are traces of commenting or not ----------------------------------------------------------------------------------------------
# entfällt, weil die Tabelle schon mit der id aus dem parsing kam
  
# filter the big df down to only those forms with comment traces and all elements nested  ------------------------------------------------------------------------------------------------------------------------
# entfällt ebenfalls, weil die neue Tabelle sowieso nur solche context-tags enthält, deren Elternelemente commenting-traces enthalten. Und zwar auch solche Traces, die auf Kommentarsysteme hindeuten

# hashing all form tags with nested elements for better checking on changes  ------------------------------------------------------------------------------------------------------------------------
# rm(df_hashed_forms)
## iteration "most_pragmatic" steht für eine minimal Variante an verwendeten Spalten zum hashen. Überlegungen dazu in den Fieldnotes

df_hashed_forms <- df_tags_context %>% 
  select(id_sha1_group, tag, attr) %>% 
  reframe(hashed_forms = digest::sha1(c(tag, attr), serialize = F), .by = id_sha1_group) %>% #,
  mutate(sphere = SPHERE_FOR_SHEET,
         sha1 = str_remove(id_sha1_group, "_\\d{1,}$"),
         iteration = "most_pragmatic")

save(df_hashed_forms, file="data/1-parsing/tags-context/German/hashed-tags.RData")

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


# 133f83214d349e94be9ec1715fe2e4f484e23064_3
# 8cfc8474fde7a42d3bbe955953aebc4674e22048_3

# write new table of hashes to db ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

dbWriteTable(conn = con, name = "context_hashed", value = df_hashed_forms, append = TRUE)

dbDisconnect(con)

  