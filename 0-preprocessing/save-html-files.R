################################################################################################################
## Data from the Internet Archive comes in one csv-file per websphere. 
## For better handling of this huge files, the html-files are getting saved as single html-files.
################################################################################################################

library(tidyverse)

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
