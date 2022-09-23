library(tidyverse)
library(RCurl)
source("config/config.R")
source("config/config-secret.R")


create_table_available_sites <- function(path, domain){
  df_archived_site <- readr::read_csv(file = paste0(path, jupyter_token), col_select = c("crawl_date", "url", "md5")) %>% 
    mutate(archive_url = paste0("http://web.archive.org/web/", crawl_date, "/", url)) %>% 
    select(crawl_date, archive_url, md5)
  write_csv(df_archived_site, file = paste0("data/analysed-sites-",domain,".csv") )
}

create_table_available_sites(DATAPATH_RAW_DE, "de")
create_table_available_sites(DATAPATH_RAW_WORLD, "world")
create_table_available_sites(DATAPATH_RAW_NL, "nl")

