library(tidyverse)
# library(lubridate)
# library(vroom)

df_raw_archive <- read_csv("data/raw/html-file-information.csv")
# 
# save(df_raw_archive, file = "data/raw/df_raw_archive.Rdata")

df_testing_raw <- df_raw_archive %>% 
  select(crawl_date, sha1, content) %>% 
  distinct() %>% 
  mutate(content_length = nchar(content))

df_testing_raw %>% select(-content) %>% View()

df_ksta_ids <- df_snippet_info %>% 
  filter(site == "ksta") %>% 
  select(crawl_date, url, sha1) %>% 
  distinct()

df_ksta_sites <- read_csv("data/raw/html-file-information.csv") %>% 
  filter(sha1 %in% df_ksta_ids$sha1)


df_ksta_sites %>%
  pwalk(function(...) {
    current <- tibble(...) %>% 
      select(crawl_date, sha1, filename, content)
    
    save_name <- paste0("data/html-site/", crawl_date, "-",current$sha1, ".html")
    
    if(!file.exists(save_name)){
      write_file(current$content, save_name)
    }
  })


save_site <- function(x, pos){
  print(pos)
  x %>% 
    pwalk(function(...) {
    current <- tibble(...) %>% 
      select(sha1, filename, content)
    
    save_name <- paste0("data/html-site/", current$sha1, ".html")
    
    if(!file.exists(save_name)){
      write_file(current$content, save_name)
    }
    })
  }

read_csv_chunked("data/raw/html-file-information.csv", SideEffectChunkCallback$new(save_site), chunk_size = 100)
