library(tidyverse)
library(lubridate)
# library(vroom)
library(furrr)
library(future)
source("config/config.R")

future::plan(multisession)

search_date <- lubridate::today()
file_list <- list.files(paste0("data/0-preprocessing/", CURRENT_SPHERE, "/"))

snippets_to_search_for <- read_csv("data/helper/23-01-13-Commenting-system-detection-patterns.csv") %>% 
  select("system" = `Commenting system`, "snippet" = Snippet) %>% 
  filter(!is.na(system)) %>% select(snippet) %>% pull(.)

df_snippet_detected <- future_walk(file_list, function(j){
  # print(j)
  # j = "349474870c5d6c02312506f702c5430c25f0e259.html"
  
  site <- read_file(paste0("data/0-preprocessing/", CURRENT_SPHERE, "/", j))
  
  snippets <- map_df(snippets_to_search_for, function(i){
    # print(i)
    found = ifelse(str_detect(site, fixed(i)), 1, 0) ## findet nix!s
    
    df_inner <- tibble(
      site = j %>% str_remove(".html"), 
      search_date = search_date,
      snippet = i,
      detected = found
    )

  },.progress = TRUE)

  write_csv(snippets, paste0("data/1-parsing/snippet-detection/", CURRENT_SPHERE,"/snippets.csv"), append = TRUE)
})

#2d83cf0072d3868f5aa4e725d734f24861393483

already_parsed_sites <- read_csv(paste0("data/1-parsing/snippet-detection/", CURRENT_SPHERE,"/snippets.csv"), col_select = c("site")) %>% #select(site) %>%
  distinct() %>% mutate(site = paste0(site, ".html")) %>%  pull()

file_list <- setdiff(file_list, already_parsed_sites)
