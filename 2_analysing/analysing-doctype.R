library(tidyverse)
library(lubridate)
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


df_doctype_legend <- dbGetQuery(conn = con, paste0("SELECT DISTINCT(name) FROM tags WHERE tag LIKE 'doctype'")) %>% 
  mutate(doctype_cleaned = str_extract(name, "-\\/\\/(W3C|IETF)?\\/\\/DTD\\sX?HTML\\s?(\\d{1}\\.\\d{1,})?\\s?(Transitional|Strict|Frameset|Final)?\\/\\/\\w{2}"),
         doctype_cleaned = ifelse(str_detect(name, "^html$|^HTML$"), str_extract(name, "^html$|^HTML$"), doctype_cleaned),
         doctype_cleaned = ifelse(str_detect(name,"html SYSTEM"), name, doctype_cleaned),
         doctype_cleaned = str_remove(doctype_cleaned, "-\\/\\/(W3C|IETF)?\\/\\/DTD\\s"),
         doctype_cluster = str_extract(doctype_cleaned, "X?(HTML|html)\\s?(\\d{1}\\.\\d{1,})?"),
         doctype_cluster = ifelse(str_detect(doctype_cluster,"html\\s{0,}"), trimws(str_to_upper(doctype_cluster)), doctype_cluster)) %>% 
  select(doctype_cleaned, doctype_cluster) %>% 
  distinct() %>% 
  group_by(doctype_cluster) %>% 
  mutate(group = row_number())


# df <- dbGetQuery(conn = con, paste0("SELECT t.site, t.tag, t.name, s.crawl_date, s.sha1, s.sphere FROM sites s INNER JOIN tags t ON t.site = s.sha1 WHERE s.sphere LIKE'German' AND t.tag LIKE 'doctype'")) 


get_doctype_info <- function(sphere, tag){
  df <- dbGetQuery(conn = con, paste0("SELECT t.site, t.tag, t.name, s.crawl_date, s.sha1, s.sphere FROM sites s INNER JOIN tags t ON t.site = s.sha1 WHERE s.sphere LIKE'", sphere, "' AND t.tag LIKE '", tag, "'")) %>% 
    mutate(doctype_cleaned = str_extract(name, "-\\/\\/(W3C|IETF)?\\/\\/DTD\\sX?HTML\\s?(\\d{1}\\.\\d{1,})?\\s?(Transitional|Strict|Frameset|Final)?\\/\\/\\w{2}"),
           doctype_cleaned = ifelse(str_detect(name, "^html$|^HTML$"), str_extract(name, "^html$|^HTML$"), doctype_cleaned),
           doctype_cleaned = ifelse(str_detect(name,"html SYSTEM"), name, doctype_cleaned),
           doctype_cleaned = str_remove(doctype_cleaned, "-\\/\\/(W3C|IETF)?\\/\\/DTD\\s"),
           doctype_cluster = str_extract(doctype_cleaned, "X?(HTML|html)\\s?(\\d{1}\\.\\d{1,})?"),
           doctype_cluster = ifelse(str_detect(doctype_cluster,"html\\s{0,}"), trimws(str_to_upper(doctype_cluster)), doctype_cluster))
}


df_doctype_de <- get_doctype_info("German", "doctype")

# df_doctype_de <- df

df_doctype_de %>% select(doctype_cleaned, doctype_cluster) %>% distinct() %>% View()

df_doctype_de %>% 
  mutate(doctype_cluster = ifelse(str_detect(doctype_cluster,"html\\s{0,}"), trimws(str_to_upper(doctype_cluster)), doctype_cluster),
         crawl_date = ymd(crawl_date)) %>% 
  group_by(crawl_date, doctype_cleaned, doctype_cluster) %>% 
  summarise(count = n()) %>% 
  ggplot(., aes(x = crawl_date, y = count, fill = doctype_cluster)) +
  geom_col()

df_doctype_nl <- get_doctype_info("Dutch", "doctype")
df_doctype_nl %>% select(doctype_cleaned, doctype_cluster) %>% distinct() %>% View()

df_doctype_world <- get_doctype_info("World", "doctype")

df_doctype_world %>% select(doctype_cleaned) %>% distinct() %>% View()

df_doctype_de %>% select(doctype_cleaned) %>% group_by(doctype_cleaned) %>% summarise(count = n()) %>% View()

df_index_file <- read_csv(file = paste0("data/raw/", CURRENT_SPHERE, "/html-file-information.csv"), col_select = c("crawl_date", "url", "sha1")) #%>% 

df_index_file <- df_index_file %>% distinct()

## die mehreren
df_doctype_de_ <- df_doctype_de %>% 
  right_join(., df_index_file, by = c("site" = "sha1"))

