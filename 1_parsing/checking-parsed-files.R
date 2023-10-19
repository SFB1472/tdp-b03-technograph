
###########################################################
### this script checks on the parsed csv-files, 
### created of parsing different tags from the html-files 
### some smaller to dos regarding the data are noted below
###########################################################

library(tidyverse)
library(lubridate)
source("config/config.R")


# prepare form and script tag data for db ----------------------------------------------

clean_form_script_tag_data <-  function(tag, sphere, export){
  df_tags <- read_csv(paste0("data/1-parsing/tags/", sphere, "/", tag ,"-raw-", export,".csv")) %>% distinct()
  df_tags_checked <- df_tags %>% 
    filter(is.na(missing) | missing =="no tags found" | missing == "unvalid html site") %>% 
    mutate(sphere = sphere,
           length_sha1 = nchar(site),
           unwanted_chars = str_detect(site, "[\\\\.\\\\;\\\\-\\\\/\\\\_\\\\=\\\\-]")) %>% 
    filter(length_sha1 == 40 | !unwanted_chars) %>% 
    select(-length_sha1, -unwanted_chars) #%>% 
    # distinct()
    
  write_csv(df_tags_checked, paste0("data/1-parsing/tags/", sphere, "/", tag,"-checked-", export,".csv"))
}

clean_form_script_tag_data("form", "German","2")
# df_form_de %>%
#   filter(length_sha1 != 40 | unwanted_chars) %>% View()

# df_script_de_2 <- 
clean_form_script_tag_data("script", "German", "2")
# df_script_de_2 %>% filter(length_sha1 != 40 | unwanted_chars) %>% View()
clean_form_script_tag_data("form", "Dutch", "2")
clean_form_script_tag_data("script", "Dutch", "2")
clean_form_script_tag_data("form", "World", "2")
clean_form_script_tag_data("script", "World", "2")

clean_form_script_tag_data("form", "German","1")
clean_form_script_tag_data("script", "German", "1")
clean_form_script_tag_data("form", "Dutch", "1")
clean_form_script_tag_data("script", "Dutch", "1")
clean_form_script_tag_data("form", "World", "1")
clean_form_script_tag_data("script", "World", "1")
df_script_world %>% filter(length_sha1 != 40 | unwanted_chars) %>% View()

nchar("4691bb4474f8c4c41e71e19e9e8b5592183b77609fe")

### keine probleme bei script-tags in german
### script dutch ca 580 fehlgeparste
### script world ca 1100 #### hier klappt etwas nicht (websphere: world): bei 1099 seiten werden die daten seltsam über die spalten verstreut. vermutung: das unnesten macht mal wieder schwierigkeiten
### keine probleme bei form-tag in german and dutch
### form world 106

# prepare doctype data for db -------------------------------------------

clean_doctype_data <- function(sphere, export){

  df <- read_csv(paste0("data/1-parsing/doctype/", sphere, "/doctype-", export, ".csv")) %>% distinct()
  df_cleaned <- df %>% 
    select(-`...1`) %>% 
    rename("name" = "doctype") %>% 
    mutate(tag = "doctype",
           attr = NA,
           search_date = "2023-03-02",
           group = NA,
           missing = NA,
           sphere = sphere) %>% 
    select(site, tag, search_date, name, attr, group, missing, sphere)
  write_csv(df_cleaned, paste0("data/1-parsing/tags/", sphere, "/doctype-checked-", export, ".csv"))
}

clean_doctype_data("German", "1")
clean_doctype_data("Dutch", "1")
clean_doctype_data("World", "1")

clean_doctype_data("German", "2")
clean_doctype_data("Dutch", "2")
clean_doctype_data("World", "2")


#### following rows only as reminder for maybe coming back to the early thoughts on embedded scripts in websites

# df_index <- read_csv("data/German/html-file-information.csv", col_select = c("crawl_date", "sha1", "url"))

df_guessed_scripts <- read_csv("data/German/scripts_guessed.csv", col_select = c("site", "language"))

df_scripts_over_time <- df_scripts %>% 
  left_join(., df_index, by = c("site" = "sha1")) %>% 
  mutate(crawl_date = ymd(crawl_date)) #%>%
  

df_scripts_over_time %>% 
  group_by(crawl_date, site) %>% 
  summarise(n_scripts = n()) %>% 
  ungroup() %>% 
  group_by(crawl_date) %>% 
  summarise(nr_sites = n(), nr_scripts = sum(n_scripts)) %>% 
  mutate(scripts_per_site = round(nr_scripts/nr_sites, digits = 1)) %>% 
  ggplot(., aes(x = crawl_date, y = scripts_per_site)) +
  geom_bar(stat = "identity")
