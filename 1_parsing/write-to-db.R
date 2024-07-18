library(tidyverse)
library(DBI)
library(RPostgres)

source("config/config-secret.R")

con <- dbConnect(RPostgres::Postgres(), 
                    dbname = dsn_database,
                    host = dsn_hostname, 
                    port = dsn_port,
                    user = dsn_uid, 
                    password = dsn_pwd
                 )

dbListTables(conn = con)

# writing tag table ------------------------------------------------------------

write_tags_to_db_v2 <- function(type_of_data, sphere_, db_name, export, appending){
  
  df <- read_csv(paste0("data/1-parsing/tags/", sphere_, "/", type_of_data ,"-raw-",export,".csv")) %>% 
    mutate(sphere = sphere_)
  dbWriteTable(conn = con, name = db_name, df, append = appending)
  
}

write_tags_to_db_v1 <- function(type_of_data, sphere_, db_name, export, appending){
  
  df <- read_csv(paste0("data/1-parsing/tags/", sphere_, "/", type_of_data ,"-checked-",export,".csv")) %>% 
    mutate(sphere = sphere_)
  dbWriteTable(conn = con, name = db_name, df, append = appending)
  
}

df <- read_csv(paste0("data/1-parsing/tags/German/form-checked-2.csv"), n_max = 10) 
dbWriteTable(conn = con, name = "tags_uuid_test", df, append = FALSE)


# writing form tags to tag table ------------------------------------------------------------

write_tags_to_db("form", "German", "tags_2", "1", FALSE)
dbGetQuery(conn = con, "SELECT COUNT(*) as counted_rows FROM tags_2")
write_tags_to_db("form", "German", "tags_2", "2", TRUE)

write_tags_to_db("form", "Dutch", "tags_2", "1", TRUE)
dbGetQuery(conn = con, "SELECT COUNT(*) as counted_rows FROM tags_2")
write_tags_to_db("form", "Dutch", "tags_2", "2", TRUE)
dbGetQuery(conn = con, "SELECT COUNT(*) as counted_rows FROM tags_2")

write_tags_to_db("form", "World", "tags_2", "1", TRUE)
dbGetQuery(conn = con, "SELECT COUNT(*) as counted_rows FROM tags_2")
write_tags_to_db("form", "World", "tags_2", "2", TRUE)
dbGetQuery(conn = con, "SELECT COUNT(*) as counted_rows FROM tags_2")

# writing script tags to tag table ------------------------------------------------------------


write_tags_to_db("script", "German", "tags_2", "1", TRUE)
dbGetQuery(conn = con, "SELECT * FROM tags_2")
write_tags_to_db("script", "German", "tags_2", "2", TRUE)
dbGetQuery(conn = con, "SELECT * FROM tags_2")

write_tags_to_db("script", "Dutch", "tags_2", "1", TRUE)
dbGetQuery(conn = con, "SELECT COUNT(*) as counted_rows FROM tags_2")
write_tags_to_db("script", "Dutch", "tags_2", "2", TRUE)
dbGetQuery(conn = con, "SELECT COUNT(*) as counted_rows FROM tags_2")

write_tags_to_db("script", "World", "tags_2", "1", TRUE)
dbGetQuery(conn = con, "SELECT COUNT(*) as counted_rows FROM tags_2")
write_tags_to_db("script", "World", "tags_2", "2", TRUE)
dbGetQuery(conn = con, "SELECT COUNT(*) as counted_rows FROM tags_2")


# writing doctype tags to tag table ------------------------------------------------------------

write_tags_to_db("doctype", "German", "tags_2", "1", TRUE)
dbGetQuery(conn = con, "SELECT COUNT(*) as counted_rows FROM tags")
write_tags_to_db("doctype", "German", "tags_2", "2", TRUE)
dbGetQuery(conn = con, "SELECT COUNT(*) as counted_rows FROM tags")

write_tags_to_db("doctype", "Dutch", "tags_2", "1", TRUE)
dbGetQuery(conn = con, "SELECT COUNT(*) as counted_rows FROM tags")
write_tags_to_db("doctype", "Dutch", "tags_2", "2", TRUE)
dbGetQuery(conn = con, "SELECT COUNT(*) as counted_rows FROM tags_2")

write_tags_to_db("doctype", "World", "tags_2", "1", TRUE)
dbGetQuery(conn = con, "SELECT COUNT(*) as counted_rows FROM tags")
write_tags_to_db("doctype", "World", "tags_2", "2", TRUE)
dbGetQuery(conn = con, "SELECT COUNT(*) as counted_rows FROM tags_2")

# writing iframe tags to tag table ----------------------------------------------------

write_tags_to_db("iframe", "German", "tags_2", "1", TRUE)
dbGetQuery(conn = con, "SELECT COUNT(*) as counted_rows FROM tags_2")
write_tags_to_db("iframe", "German", "tags_2", "2", TRUE)

write_tags_to_db("iframe", "Dutch", "tags_2", "1", TRUE)
dbGetQuery(conn = con, "SELECT COUNT(*) as counted_rows FROM tags_2")
write_tags_to_db("iframe", "Dutch", "tags_2", "2", TRUE)
dbGetQuery(conn = con, "SELECT COUNT(*) as counted_rows FROM tags_2")

write_tags_to_db("iframe", "World", "tags_2", "1", TRUE)
dbGetQuery(conn = con, "SELECT COUNT(*) as counted_rows FROM tags_2")
write_tags_to_db("iframe", "World", "tags_2", "2", TRUE)
dbGetQuery(conn = con, "SELECT COUNT(*) as counted_rows FROM tags_2")


# writing div tags to tag table ----------------------------------------------------

write_tags_to_db_v2("div", "German", "tags_2", "1", TRUE)
dbGetQuery(conn = con, "SELECT COUNT(*) as counted_rows FROM tags_2")
write_tags_to_db_v2("div", "German", "tags_2", "2", TRUE)

write_tags_to_db_v2("div", "Dutch", "tags_2", "1", TRUE)
dbGetQuery(conn = con, "SELECT COUNT(*) as counted_rows FROM tags_2")
write_tags_to_db_v2("div", "Dutch", "tags_2", "2", TRUE)
dbGetQuery(conn = con, "SELECT COUNT(*) as counted_rows FROM tags_2")

write_tags_to_db_v2("div", "World", "tags_2", "1", TRUE)
dbGetQuery(conn = con, "SELECT COUNT(*) as counted_rows FROM tags_2")
write_tags_to_db_v2("div", "World", "tags_2", "2", TRUE)
dbGetQuery(conn = con, "SELECT COUNT(*) as counted_rows FROM tags_2")

## writing div-1 of world into database, error at line 60820966, site feef440503566ce7b4b6fafe9953ef1d2f9c513a
## filter 60820926

# writing index table ------------------------------------------------------------

# dbRemoveTable(conn = con, name = "sites")

write_index_to_db <- function(sphere, appending){
  df <- read_csv(paste0("data/raw/", sphere, "/html-file-information.csv"), col_select = c("crawl_date", "url", "sha1", "md5")) %>% 
    mutate(sphere = sphere)
  dbWriteTable(conn = con, name =  "sites", value = df, append=appending)
}

write_index_to_db("German", FALSE)
dbGetQuery(conn = con, "SELECT COUNT(*) as counted_rows FROM sites")

write_index_to_db("Dutch", TRUE)

write_index_to_db("World", TRUE)

# writing snippets table ---------------------------------------------------------

write_snippets_to_db <- function(sphere, appending){
  df <- read_csv(paste0("data/1-parsing/snippet-detection/", sphere, "/snippets-2.csv"))
  dbWriteTable(conn = con, name = "snippets_2", value = df, append = appending)
}

write_snippets_to_db("German", TRUE)
dbGetQuery(conn = con, "SELECT COUNT(*) as counted_rows FROM snippets")

write_snippets_to_db("Dutch", TRUE)
dbGetQuery(conn = con, "SELECT COUNT(*) as counted_rows FROM snippets")

write_snippets_to_db("World", TRUE)
dbGetQuery(conn = con, "SELECT COUNT(*) as counted_rows FROM snippets")

# writing tag-context table --------------------------------------------------------
## decided to go that way, despide the python script is able to write directly to the db, because it takes much longer writing bites of information
## via wifi into the db, than parsing and storing locally and write it in a dump

write_tag_context_to_db <- function(sphere, appending){
  df <- read_csv(paste0("data/1-parsing/tags-context/", sphere, "/tags-context-2.csv")) %>% select(-index)
  # dbCreateTable(conn = con, name = "tag_context", fields = df)
  dbWriteTable(conn = con, name = "tag_context_2", value = df, append = appending)
  
}

dbGetQuery(conn = con, "SELECT COUNT(*) as counted_rows FROM tag_context_2")
write_tag_context_to_db("German", TRUE)
dbGetQuery(conn = con, "SELECT COUNT(*) as counted_rows FROM tag_context_2")
write_tag_context_to_db("Dutch", TRUE)
dbGetQuery(conn = con, "SELECT COUNT(*) as counted_rows FROM tag_context_2")

write_tag_context_to_db("World", TRUE)
dbGetQuery(conn = con, "SELECT COUNT(*) as counted_rows FROM tag_context")

# write all tag context to db ---------------------------------------------------

write_all_tag_context_to_db <- function(sphere, appending){
  df <- read_csv(file = "../data/1-parsing/tags-context/German/context-all-traces.csv") %>% 
    bind_rows(., read_csv(file = "../data/1-parsing/tags-context/German/context-all-traces-2.csv")) %>% 
    select(-`...1`) %>% 
    mutate(form_group = row_number(), .by = c(sha1, context_path),
           id_sha1_group = paste0(sha1, "_", form_group)) %>% 
    select(-form_group)
  
  # df <- read_csv(paste0("data/1-parsing/tags-context/", sphere, "/context-all-traces.csv")) #%>% select(-index)
  # dbCreateTable(conn = con, name = "tag_context", fields = df)
  dbWriteTable(conn = con, name = "tag_context", value = df, append = appending)
  
}


write_all_tag_context_to_db("German", TRUE)


### https://www.postgresqltutorial.com/postgresql-tutorial/postgresql-uuid/
### https://stackoverflow.com/questions/67293538/generate-a-uuid-in-postgres
### https://www.postgresql.org/docs/current/functions-uuid.html
### unique ids sql-statement via beekeeper 
# ##
# ALTER TABLE tags_2
# ADD COLUMN tags_id uuid DEFAULT gen_random_uuid();

# ALTER TABLE snippets_2 ADD PRIMARY KEY (snippets_id)




dbGetQuery(conn = con, "SELECT DISTINCT COUNT(id_sha1_group) as counted_rows FROM tag_context")

dbGetQuery(conn = con, "SELECT DISTINCT COUNT(tag_context_id) as counted_rows FROM tag_context")

dbGetQuery(conn = con, "SELECT * FROM tag_context LIMIT 30") %>% View()

##### in dem geparstem file zu den div-tags in der world-sphere gab es probleme
df_world_div_2 <- read_csv(paste0("data/1-parsing/tags/World/div-raw-2.csv")) 

df_error_page <- df_world_div_1 %>% 
  filter(site == "feef440503566ce7b4b6fafe9953ef1d2f9c513a") 

df_world_div_2_db <- df_world_div_2 %>% 
  filter(validUTF8(site)) %>% 
  mutate(helper = ifelse(nchar(site) == 40, 1, 0),
         sphere = "World") %>% 
  filter(helper == 1, validUTF8(attr)) %>% 
  select(-helper)# %>% 

# div_2_error <- df_world_div_2 %>% slice(14135545)
# 
# df_world_div_1_bytes <- df_world_div_1_db %>% 
#   filter(!validUTF8(attr)) 

dbWriteTable(conn = con, name = "tags_2", df_world_div_2_db, append = TRUE)   
  
df_world_div_1_no_db <- df_world_div_1 %>% 
  mutate(helper = ifelse(nchar(site) == 40, 1, 0),
         sphere = "World") %>% 
  filter(helper != 1)


