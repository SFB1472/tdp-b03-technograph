library(tidyverse)
library(lubridate)
# library(vroom)
library(furrr)
library(future)
source("config/config.R")
library(DBI)
library(RPostgres)
# source("config/config-secret.R")
source("config/config-secret-local.R")
CURRENT_SPHERE <- "German"

con <- dbConnect(RPostgres::Postgres(), 
                 dbname = dsn_database,
                 host = dsn_hostname, 
                 port = dsn_port,
                 user = dsn_uid, 
                 password = dsn_pwd
)
future::plan(multisession)

search_date <- lubridate::today()
# files to parse ---------------------------
# file_list <- list.files(paste0("data/0-preprocessing/", CURRENT_SPHERE, "-2/"))
df_sites <- dbGetQuery(conn = con, paste0("SELECT s.crawl_date, s.sha1, s.export FROM sites s WHERE s.of_interest = TRUE AND s.sphere ='German'")) #%>%
file_list <- df_sites %>% select(sha1) %>% pull()

# snippets to search for --------------------
# snippets_to_search_for <- read_csv("data/helper/23-01-13-Commenting-system-detection-patterns.csv") %>% 
#   select("system" = `Commenting system`, "snippet" = Snippet) %>% 
#   filter(!is.na(system)) %>% select(snippet) %>% pull(.)

# load(file="data/helper/tracker-domains.RData")
# load(file="data/helper/tracker-snippets/tracker-pattern.RData")
# load(file="data/helper/tracker-snippets/easylist-general-hide.RData")
load(file = "data/helper/tracker-snippets/all-snippets.RData")

df_snippet_detected <- future_walk(file_list, function(j){
  # print(j)
  # j = "349474870c5d6c02312506f702c5430c25f0e259.html"
  
  if(file.exists(paste0("data/0-preprocessing/", CURRENT_SPHERE, "-1/", j, ".html"))){
    # print("file is da")
    site <- read_file(paste0("data/0-preprocessing/", CURRENT_SPHERE, "-1/", j, ".html"))
  }else{
    # print("file is nich da")
    site <- read_file(paste0("data/0-preprocessing/", CURRENT_SPHERE, "-2/", j, ".html"))
  }
  
  snippets <- map_df(snippets_to_search_for, function(i){
  # snippets <- map_df(domains_to_search_for, function(i){
  # snippets <- map_df(patterns_to_search_for, function(i){
    # print(i)
    found = ifelse(str_detect(site, fixed(i)), 1, 0) ## findet nix!s
    if(found == 1){
      df_inner <- tibble(
        site = j %>% str_remove(".html"), 
        search_date = search_date,
        snippet = i,
        detected = found
      )
    }

  },.progress = TRUE)
  pid = Sys.getpid()
  write_csv(snippets, paste0("data/1-parsing/snippet-detection/", CURRENT_SPHERE,"/tracker-all-snippets-", pid, ".csv"), append = TRUE)
})

#2d83cf0072d3868f5aa4e725d734f24861393483

already_parsed_sites <- read_csv(paste0("data/1-parsing/snippet-detection/", CURRENT_SPHERE,"/snippets-2.csv"), col_select = c("site")) %>% #select(site) %>%
  distinct() %>% mutate(site = paste0(site, ".html")) %>%  pull()

file_list <- setdiff(file_list, already_parsed_sites)
