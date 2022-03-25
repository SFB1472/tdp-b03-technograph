library(tidyverse)
# library(lubridate)
# library(vroom)

# df_raw_archive <- vroom("data/raw/html-file-information.csv")
# 
# save(df_raw_archive, file = "data/raw/df_raw_archive.Rdata")

snippits_to_search_for <- read_csv("data/helper/22-03-09-snipits-download.csv") %>% 
  select("system" = `Commenting system`, "snippit" = Snipit) %>% 
  filter(!is.na(system))

find_snippit <- function(x, pos){
  # print(str(x))
  df <- map_df(snippits_to_search_for$snippit, function(i){
    save_string <- str_replace_all(i, "/", "-")
    print(paste0(save_string," - ", pos))
    
    df <- x %>%
      select(crawl_date, url, filename, md5, sha1, content) %>%
      mutate(detected = ifelse(str_detect(content, fixed(i)), 1, 0),
             snippit = i) %>%
      select(-content)
    
    })
  write_csv(df, paste0("data/snippit-detection/snippit-detection-", pos,".csv"))
  }

read_csv_chunked("data/raw/html-file-information.csv", SideEffectChunkCallback$new(find_snippit), chunk_size = 100, skip = 149801) 
