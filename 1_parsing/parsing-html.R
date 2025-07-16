##################################################
### scripts parses html-files for a specified tag 
### and saves the findings in a local csv-file
##################################################

library(tidyverse)
library(purrr)
library(rvest)
library(xml2)
library(urltools)
library(googlesheets4)
library(testdat)
library(DBI)

library(RPostgres)
library(furrr)
source("config/config.R")
source("config/config-secret.R")

future::plan(multisession)

search_date <- lubridate::today()
html_element_to_search_for <- "script"



CURRENT_SPHERE <- "Dutch"

# delivery <- "1"

con <- dbConnect(RPostgres::Postgres(), 
                 dbname = dsn_database,
                 host = dsn_hostname, 
                 port = dsn_port,
                 user = dsn_uid, 
                 password = dsn_pwd
)

## risky lines ahead, cause they can destroy already parsed data
# df_empty <- tibble(site = character(), tag = character(), search_date = date(), name = character(), attr = character(), group = character(), missing = character())
# write_csv(df_empty, paste0("data/1-parsing/tags/", CURRENT_SPHERE, "/", html_element_to_search_for, "-raw.csv"))

file_list <- dbGetQuery(conn = con, paste0("SELECT s.sha1 FROM sites s WHERE s.sphere = '", CURRENT_SPHERE, "' AND s.of_interest = TRUE")) %>% 
  distinct() %>% 
  # mutate(sha1 = paste0(sha1, ".html")) %>% 
  select(sha1) %>% pull()

# file_list <- list.files(paste0("data/0-preprocessing/", CURRENT_SPHERE, "-", delivery, "/")) #%>% head(20)
# 
# already_parsed_sites <- list.files(paste0("data/0-preprocessing/", CURRENT_SPHERE, "-2/")) #%>% head(20)
# file_list <- setdiff(html_files_of_interest, already_parsed_sites)
broken_sites <- c("48ddbc87878a63872636bd7188adea4076215989", "da6a398b5265df86a1bc482d234d4833485bdcea", "ef8c8a4c25c01b1a35b73ee3466522e9fa1779d6", 
                  "798cfff45a6e563d3f93b2b70f4aa6f13815de19", "c62b0ce32e34fc1a1f0fd4d5d8075a96c2a13bac", "0f038852a28befd8d23e04439a175e12a95bcf4f", 
                  "af2eaa296d92bb92742095ac43a729fa4858de46", "8b56602a79bc7516e115f19497487dfbc0946130", "17426cebf9a8a68e2939302812bccabbbe577e81",
                  "e288b30d705423f91dcf26f83023a9569e698b28", "b5fd8fbb96a19685215a44e28725eaa73f40ce7d", "052e8349087ae241a77cdf0bfe72c6b2fae71bd0",
                  "3bdd3fb99c037131ff38aeee87a9c34bf9265d89", "366ccf96502c8a869e08cecd324ff1e257591dbf", "08d19979c9159a59870b954d4c0dd09663873ff4",
                  "fcd033c5916d2270fe8b0e83b4bd202f2506a08e", "e7ad8cdaacc4204caca82d50dfe6c88a8dc3755f"
                  )
file_list <- file_list[!file_list %in% broken_sites]

rm(tags)

# parsing tags ------------------------------------------------------------

tags <- future_map(file_list, function(i){
  # i <- "0480ddd7f304507a4f7fb18bf5ae39fe2e994a29.html"

  # cli::cli_inform("Parse file {i}")
  
  df_empty <- tibble(
    site = i %>% str_remove(".html"), 
    tag = html_element_to_search_for, 
    search_date = search_date,
    name  =  "NA",
    attr =  "NA",
    group = "NA",
    missing = "NA"
  )
  
  # site <- xml2::read_xml(paste0("data/0-preprocessing/", CURRENT_SPHERE, "-",delivery,"/", i), as_html = TRUE, encoding = "UTF-16", options = c("RECOVER", "HUGE")) #%>%
  
  if(file.exists(paste0("data/0-preprocessing/", CURRENT_SPHERE, "-1/", i, ".html"))){
    # print("file is da")
    site <- xml2::read_xml(paste0("data/0-preprocessing/", CURRENT_SPHERE, "-1/", i,".html"), as_html = TRUE, encoding = "UTF-16", options = c("RECOVER", "HUGE"))
  }else{
    # print("file is nich da")
    site <- xml2::read_xml(paste0("data/0-preprocessing/", CURRENT_SPHERE, "-2/", i,".html"), as_html = TRUE, encoding = "UTF-16", options = c("RECOVER", "HUGE"))
  }
  
  if(length(site) > 1){
    all_tags <- site %>% html_elements(html_element_to_search_for)
    
    if(length(all_tags) != 0){
     
      df_inner <- tibble(
        site = i %>% str_remove(".html"),
        tag = html_element_to_search_for,
        search_date = search_date,
        attrs  =  all_tags %>% html_attrs() %>% map_df(., function(j){
          if(length(j) > 0){
            df_attrs <- tibble(
              name = names(j),
              attr = j
            ) %>% nest(data = everything()) 
          }
          else{
            df_attrs <- tibble(
              name = "no attr",
              attr = "no attr"
            ) %>% nest(data = everything()) 
          }
          }
        )
      ) %>%
        mutate(group = row_number() %>% as.character(.)) %>%
        unnest(attrs) %>%
        unnest(data) %>%
        mutate(missing = "NA")

    } else{
      # print("no tags found")
      df_empty$missing[[1]] = "no tags found"
      df_inner <- df_empty
    }
  } else {
    # print("unvalid html site")
    df_empty$missing[[1]] = "unvalid html site"
    df_inner <- df_empty
  }
  pid = Sys.getpid()
  write_csv(df_inner, paste0("data/1-parsing/tags/", CURRENT_SPHERE, "/", html_element_to_search_for, "/", html_element_to_search_for, "-raw-", pid, ".csv"), append = TRUE)
 
  
}, .progress = TRUE)

# if parsing the many files fails, restart here ------------------------------------------------------------
# load(file="data/helper/file_list_db_de.RData")
header <- c("site","tag","search_date","attr","value","group","missing","sphere")
list_of_files <- list.files(path = paste0("data/1-parsing/tags/",CURRENT_SPHERE,"/", html_element_to_search_for,"/"), recursive = FALSE, pattern = "\\.csv$", full.names = TRUE)
already_parsed_sites <- read_csv(list_of_files, id = "file_name", col_names = header) %>% 
  select(site) %>%
  distinct() %>% 
  # mutate(site = paste0(site, ".html")) %>%
  pull()

file_list <- setdiff(file_list, already_parsed_sites)


# clipr::write_clip(file_list)
# if future map is finishing on its own, saving the local variable for checking on proper parsing ------------------
rm(df_tags__)
df_tags__ <- df_tags %>% 
  enframe() %>% 
  # select(value) %>% 
  unnest(value, names_sep = "_") %>% 
  as_tibble() %>% 
  unnest(value_missing, keep_empty = TRUE) %>%
  rename(id = name) %>% #, str_remove(., "value_"))
  rename_with(., ~gsub("value_", "", .x))

write_csv(df_tags__, paste0("data/1-parsing/tags/",CURRENT_SPHERE,"/",html_element_to_search_for,"/",html_element_to_search_for,"-raw-",delivery,".csv"))
