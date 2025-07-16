library("tidyverse")
library(purrr)
library(rvest)
library(xml2)
library(urltools)
library(DBI)
library(RPostgres)
# library(googlesheets4)
library(furrr)
source("config/config.R")
source("config/config-secret.R")

future::plan(multisession)

con <- dbConnect(RPostgres::Postgres(), 
                 dbname = dsn_database,
                 host = dsn_hostname, 
                 port = dsn_port,
                 user = dsn_uid, 
                 password = dsn_pwd
)

CURRENT_SPHERE <- "Dutch"
tag <- "script"

PATH_TO_SAVE_PARSINGS <- paste0("data/1-parsing/scripts/",CURRENT_SPHERE,"/",tag,"-embedded/",tag,"-raw")

file_list <- dbGetQuery(conn = con, paste0("SELECT s.sha1 FROM sites s WHERE s.sphere = '", CURRENT_SPHERE, "' AND s.of_interest = TRUE")) %>% 
  distinct() %>% 
  # mutate(sha1 = paste0(sha1, ".html")) %>% 
  select(sha1) %>% pull()

# read_broken_files <- list.files("data/0-preprocessing/Dutch-fail/")

# file_list <- file_list[!file_list %in% broken_sites]

rm(tags_embedds)
rm(df_tags_embedds)
tags_embedds <- future_map(file_list, function(i){
# df_scripts <- map(file_list, function(i){
  # print(i)
  if(file.exists(paste0("data/0-preprocessing/", CURRENT_SPHERE, "-2/", i,".html"))){
    test_site <- xml2::read_xml(paste0("data/0-preprocessing/", CURRENT_SPHERE, "-2/", i, ".html"), as_html = TRUE, encoding = "UTF-16", options = c("RECOVER", "HUGE")) 
  }
  else {
    test_site <- xml2::read_xml(paste0("data/0-preprocessing/", CURRENT_SPHERE, "-1/", i, ".html"), as_html = TRUE, encoding = "UTF-16", options = c("RECOVER", "HUGE")) 
  }
    
    df_empty <- tibble(
      site = i %>% str_remove(".html"), 
      content = "NA", 
      # search_date = search_date,
      tag = "NA",
      attr  =  "NA",
      value =  "NA",
      group = "NA"
    )
  
  if(length(test_site) > 1){
    
    all_script_tags <- test_site %>% html_elements(tag)
    
    if(length(all_script_tags) != 0){
      # i <- "000080b9fc7f12c6c4c4255621e5a61c3e4c7179.html"
      df_inner <- tibble(
        site = i %>% str_remove(".html"), 
        content =  all_script_tags %>% xml_text(),# %>% str_replace_all(., ",", "\\\\,"),
        tag = tag,
        attrs  =  all_script_tags %>% html_attrs() %>% map_df(., function(j){
          if(length(j) > 0){
            df_attrs <- tibble(
              attr = names(j),
              vlaue = j
            ) %>% nest(data = everything()) 
          }
          else{
            df_attrs <- tibble(
              attr = "no attr",
              value = "no attr"
            ) %>% nest(data = everything()) 
          }
        }
        )
      ) %>%
        mutate(group = row_number() %>% as.character(.)) %>%
        unnest(attrs) %>%
        unnest(data)
      }
    else {
    df_inner <- tibble(
      site = i %>% str_remove(".html"), 
      content =  NA,
      tag = NA, 
      attr  =  NA,
      value = NA,
      group = NA
    )
    }}
    else {
      print("unvalid html site")
      df_empty$missing[[1]] = "unvalid html site"
      df_inner <- df_empty
    }
    pid = Sys.getpid()
    write_csv(df_inner, paste0(PATH_TO_SAVE_PARSINGS, "-", pid, ".csv"), append = TRUE)
  
}, .progress = TRUE)

### zwischenschritt wenn parsing fehlt schlägt --------------------------------
header <-  c("site", "embedded", "attr","value","group")
list_of_files <- list.files(path = paste0("data/1-parsing/scripts/",CURRENT_SPHERE,"/script-embedded/"), recursive = FALSE, pattern = "\\.csv$", full.names = TRUE)
already_parsed_sites <- read_csv(list_of_files, id = "file_name", col_names = header) %>% 
  select(site) %>%
  distinct() %>% 
  # mutate(site = paste0(site, ".html")) %>%
  pull()

file_list <- setdiff(file_list, already_parsed_sites)

###

# df_scripts_ <- df_scripts %>% enframe()

df_tags_embedds <- tags_embedds %>% 
  enframe() %>% 
  # select(value) %>% 
  unnest(value, names_sep = "_") %>% 
  as_tibble() %>% 
  unnest(value_missing, keep_empty = TRUE) %>%
  rename(id = name) %>% #, str_remove(., "value_"))
  rename_with(., ~gsub("value_", "", .x))

save(df_tags_embedds, file=paste0("data/1-parsing/scripts/",CURRENT_SPHERE,"/",tag,"-raw.RData"))


# head(df_scripts)
# 
# df_scripts_plain <- df_scripts %>% 
#   mutate(content = trimws(content)) %>% 
#   filter(content != "", !str_detect(content, "^<!-- "), !str_detect(content, "^//"))
# 
# 
# 
# write_csv(df_scripts_plain, "data/German/scripts-to-guess.csv")