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

write_tags_to_db <- function(type_of_data, sphere, db_name, export, appending){
  
  df <- read_csv(paste0("data/1-parsing/tags/", sphere, "/", type_of_data ,"-checked-",export,".csv")) 
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
## decided to go that way, despide the python script is able to write directly to the db, because it takes much longer writing bites for information
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

### https://www.postgresqltutorial.com/postgresql-tutorial/postgresql-uuid/
### https://stackoverflow.com/questions/67293538/generate-a-uuid-in-postgres
### https://www.postgresql.org/docs/current/functions-uuid.html
### unique ids sql-statement via beekeeper 
# ##
# ALTER TABLE tags_2
# ADD COLUMN tags_id uuid DEFAULT gen_random_uuid();

# ALTER TABLE snippets_2 ADD PRIMARY KEY (snippets_id)
