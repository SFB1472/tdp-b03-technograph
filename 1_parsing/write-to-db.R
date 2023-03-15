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

write_tags_to_db <- function(type_of_data, sphere, db_name, appending){
  
  df <- read_csv(paste0("data/1-parsing/tags/", sphere, "/", type_of_data ,"-checked.csv")) 
  dbWriteTable(conn = con, name = db_name, df, append = appending)
  
}

# writing form tags to tag table ------------------------------------------------------------

write_tags_to_db("form", "German", "tags", FALSE)

write_tags_to_db("form", "Dutch", "tags", TRUE)
dbGetQuery(conn = con, "SELECT COUNT(*) as counted_rows FROM tags")

write_tags_to_db("form", "World", "tags", TRUE)
dbGetQuery(conn = con, "SELECT COUNT(*) as counted_rows FROM tags")

# writing script tags to tag table ------------------------------------------------------------

write_tags_to_db("script", "Dutch", "tags", TRUE)
dbGetQuery(conn = con, "SELECT COUNT(*) as counted_rows FROM tags")

write_tags_to_db("script", "German", "tags", TRUE)
dbGetQuery(conn = con, "SELECT * FROM tags")

write_tags_to_db("script", "World", "tags", TRUE)
dbGetQuery(conn = con, "SELECT COUNT(*) as counted_rows FROM tags")


# writing doctype tags to tag table ------------------------------------------------------------

write_tags_to_db("doctype", "German", "tags", TRUE)
dbGetQuery(conn = con, "SELECT COUNT(*) as counted_rows FROM tags")

write_tags_to_db("doctype", "Dutch", "tags", TRUE)
dbGetQuery(conn = con, "SELECT COUNT(*) as counted_rows FROM tags")

write_tags_to_db("doctype", "World", "tags", TRUE)
dbGetQuery(conn = con, "SELECT COUNT(*) as counted_rows FROM tags")


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
  df <- read_csv(paste0("data/1-parsing/snippet-detection/", sphere, "/snippets.csv"))
  dbWriteTable(conn = con, name = "snippets", value = df, append = appending)
}

write_snippets_to_db("German", FALSE)
dbGetQuery(conn = con, "SELECT COUNT(*) as counted_rows FROM snippets")

write_snippets_to_db("Dutch", TRUE)
dbGetQuery(conn = con, "SELECT COUNT(*) as counted_rows FROM snippets")

write_snippets_to_db("World", TRUE)
dbGetQuery(conn = con, "SELECT COUNT(*) as counted_rows FROM snippets")



