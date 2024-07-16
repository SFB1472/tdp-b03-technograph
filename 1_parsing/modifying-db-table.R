library(tidyverse)
source("config/config-secret.R")

con <- dbConnect(RPostgres::Postgres(), 
                 dbname = dsn_database,
                 host = dsn_hostname, 
                 port = dsn_port,
                 user = dsn_uid, 
                 password = dsn_pwd
)

SPHERE_TO_WORK <- "German"

load(paste0("3_visualizing_app/technograph/data/", SPHERE_TO_WORK, "/gs_domain_to_look.RData"))
sites_of_interest <- gs_domain_to_look$site

sites <- tbl(con, "sites")

german_sites <- tbl(con, "sites") %>%
  # inner_join(., tbl(pool, "snippets_2"), by = c("sha1" = "site")) %>%
  filter(sphere == SPHERE_TO_WORK) %>%
  mutate(of_interest = ifelse(site %in% sites_of_interest, TRUE, of_interest))# %>% 

rows_update(x = sites, y = german_sites, by = c("sites_id"), in_place = TRUE, unmatched = "ignore")

df_site_check <- dbGetQuery(conn = con, paste0("SELECT DISTINCT * FROM sites s WHERE s.site ='sueddeutsche' LIMIT 10"))
