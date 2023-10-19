################################################################################################################
## Data from the Internet Archive comes in one csv-file per websphere. 
## For better handling of this huge files, the html-files are getting saved as single html-files.
################################################################################################################

library(tidyverse)
source("config/config.R")

save_site <- function(x, pos){
  print(pos)
  x %>% 
    pwalk(function(...) {
    current <- tibble(...) %>% 
      select(sha1, filename, content)
    
    save_name <- paste0("data/0-preprocessing/", CURRENT_SPHERE,"-2/", current$sha1, ".html")
    
    if(!file.exists(save_name)){
      write_file(current$content, save_name)
    }
    })
  }

read_csv_chunked(paste0("data/raw/",CURRENT_SPHERE,"/html-file-information.csv"), SideEffectChunkCallback$new(save_site), chunk_size = 100)
