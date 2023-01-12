library("tidyverse")
library(purrr)
library(rvest)
library(xml2)
library(urltools)
library(googlesheets4)
library(testdat)
source("config/config.R")

gs4_auth(cache=".secrets")

gs_domain_to_look <- read_sheet(SPREADSHEET_PATH_GENERELL, sheet = SPREADSHEET_PATH_DOMAINS[[{{CURRENT_SPHERE}}]]) %>% 
  select(Name, URL) %>% 
  mutate(site = domain(URL) %>% suffix_extract(.) %>% select(domain) %>% pull(.)) 

df_sites_to_parse <- read_csv("data/German-wanted-sites.csv")

# test_site <- xml2::read_xml("data/html-site/0a0a0ac23706bc4d3235d9b124fc17af6be9a8c4.html", as_html = TRUE) #%>% html_elements("script")
# test_site %>% length()
# test_site %>% xml_structure()
# # test_scripts <- 
#   
# test_site %>% html_elements("script")
# 
# test_site %>% html_attrs()
# test_site %>% xml_text()

file_list <- list.files("data/0-preprocessing/German/") #%>% head(20)

# df_scripts_empty <- tibble(site = character(), attrs = character(), content = character())
# write_csv(df_scripts_empty, "data/1-parsing/scripts/German/scripts-raw.csv")


### zwischenschritt, wenn parsing fehlt schlägt, mit den nächsten beiden zeilen herausfinden, wo das script gebrochen ist und dort weitermachen
already_parsed_sites <- read_csv("data/1-parsing/scripts/German/scripts-raw.csv", col_select = c("site")) %>% #select(site) %>%
  distinct() %>% mutate(site = paste0(site, ".html")) %>%  pull()

file_list <- setdiff(file_list, already_parsed_sites)

df_scripts <- map_df(file_list, function(i){
  print(i)
  i <- "000080b9fc7f12c6c4c4255621e5a61c3e4c7179.html"
  test_site <- xml2::read_xml(paste0("data/0-preprocessing/German/", i), as_html = TRUE, options = "RECOVER") #%>% 
  
  if(length(test_site) > 1){
    
    all_script_tags <- test_site %>% html_elements("script")
    
      df_inner <- tibble(
        site = i %>% str_remove(".html"), 
        attrs  =  all_script_tags %>% html_attrs(),
        content =  all_script_tags %>%  xml_text()
      )
  } else {
    df_inner <- tibble(
      site = i %>% str_remove(".html"), 
      attrs  =  NA,
      content =  NA
    )
  }
  write_csv(df_inner, "data/1-parsing/scripts/German/scripts-raw.csv", append = TRUE)
    
})

# 
df_forms_empty <- tibble(site = character(), attrs = character(), action = character())
write_csv(df_scripts_empty, "data/1-parsing/scripts/German/form-raw.csv")



df_form_actions <- map_df(file_list, function(i){
  
  
  i <- "ce9196596a502815242c6e79b990a19e82e0a7bb.html"
  
  cli::cli_inform("Parse file {i}")
  
  df_empty <- tibble(
    site = i %>% str_remove(".html"), 
    name  =  NA,
    attr =  NA,
    group = NA
  )
  
  test_site <- xml2::read_xml(paste0("data/0-preprocessing/German/", i), as_html = TRUE, options = "RECOVER") #%>% 
  
  if(length(test_site) > 1){
    
    all_tags <- test_site %>% html_elements("form")
    
    if(length(all_tags) != 0){
    
      df_inner <- tibble(
        site = i %>% str_remove(".html"),
        attrs  =  all_tags %>% html_attrs() %>% map_df(., function(j){
          df_attrs <- tibble(
            name = names(j),
            attr = j
          ) %>% nest(data = everything()) }
        )
      ) %>% 
        mutate(group = row_number()) %>% 
        unnest(attrs) %>%
        unnest(data)
      
      test_that(
        desc = "df_inner: no duplicates",
        expect_unique(everything(), data = df_inner)
      )
    } else{
      df_empty
    }
  } else {
    df_empty
  }
  
  write_csv(df_inner, "data/1-parsing/scripts/German/form-action-raw.csv", append = TRUE)
  
})


head(df_scripts)

df_scripts_plain <- df_scripts %>% 
  mutate(content = trimws(content)) %>% 
  filter(content != "", !str_detect(content, "^<!-- "), !str_detect(content, "^//"))



write_csv(df_scripts_plain, "data/German/scripts-to-guess.csv")
