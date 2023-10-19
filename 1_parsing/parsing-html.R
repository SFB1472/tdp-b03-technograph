##################################################
### scripts parses html-files for a specified tag 
### and saves the findings in a local csv-file
##################################################

library(tidyverse)
library(purrr)
library(rvest)
library(xml2)
library(urltools)
library(googlesheets4)
library(testdat)
library(furrr)
source("config/config.R")

future::plan(multisession)

search_date <- lubridate::today()
html_element_to_search_for <- "iframe"

## risky lines ahead, cause they can destroy already parsed data
# df_empty <- tibble(site = character(), tag = character(), search_date = date(), name = character(), attr = character(), group = character(), missing = character())
# write_csv(df_empty, paste0("data/1-parsing/tags/", CURRENT_SPHERE, "/", html_element_to_search_for, "-raw.csv"))

file_list <- list.files(paste0("data/0-preprocessing/", CURRENT_SPHERE, "-2/")) #%>% head(20)

# parsing tags ------------------------------------------------------------

df_tags <- future_map(file_list, function(i){
# df_tags <- map(file_list, function(i){
  
  # i <- "0480ddd7f304507a4f7fb18bf5ae39fe2e994a29.html"

  # cli::cli_inform("Parse file {i}")
  
  df_empty <- tibble(
    site = i %>% str_remove(".html"), 
    tag = html_element_to_search_for, 
    search_date = search_date,
    name  =  "NA",
    attr =  "NA",
    group = "NA",
    missing = "NA"
  )
  
  site <- xml2::read_xml(paste0("data/0-preprocessing/", CURRENT_SPHERE, "-2/", i), as_html = TRUE, encoding = "UTF-16", options = c("RECOVER", "HUGE")) #%>%
  # site <- xml2::read_xml(paste0("data/0-preprocessing/", CURRENT_SPHERE, "/", i), as_html = TRUE, encoding = "UTF-16", options = c("RECOVER", "HUGE")) #%>%
  
  
  if(length(site) > 1){
    all_tags <- site %>% html_elements(html_element_to_search_for)
    
    if(length(all_tags) != 0){
     
      df_inner <- tibble(
        site = i %>% str_remove(".html"),
        tag = html_element_to_search_for,
        search_date = search_date,
        attrs  =  all_tags %>% html_attrs() %>% map_df(., function(j){
          if(length(j) > 0){
            df_attrs <- tibble(
              name = names(j),
              attr = j
            ) %>% nest(data = everything()) 
          }
          else{
            df_attrs <- tibble(
              name = "no attr",
              attr = "no attr"
            ) %>% nest(data = everything()) 
          }
          }
        )
      ) %>%
        mutate(group = row_number() %>% as.character(.)) %>%
        unnest(attrs) %>%
        unnest(data) %>%
        mutate(missing = "NA")
      
      # print(df_inner)
      
      test_that(
        desc = "df_inner: no duplicates",
        expect_unique(everything(), data = df_inner)
      )
    } else{
      print("no tags found")
      df_empty$missing[[1]] = "no tags found"
      df_inner <- df_empty
    }
  } else {
    print("unvalid html site")
    df_empty$missing[[1]] = "unvalid html site"
    df_inner <- df_empty
  }
  # print(df_inner)
  # print(paste0("before saving: ", "data/1-parsing/tags/", CURRENT_SPHERE, "/", html_element_to_search_for, "-raw-2-2.csv"))
  write_csv(df_inner, paste0("data/1-parsing/tags/", CURRENT_SPHERE, "/", html_element_to_search_for, "-raw-2.csv"), append = TRUE)
  
}, .progress = TRUE)

# if parsing the many files fail, restart here ------------------------------------------------------------

already_parsed_sites <- read_csv(paste0("data/1-parsing/tags/", CURRENT_SPHERE, "/", html_element_to_search_for,"-raw-2.csv"), col_select = c("site")) %>% #select(site) %>%
  distinct() %>% mutate(site = paste0(site, ".html")) %>%  pull()

file_list <- setdiff(file_list, already_parsed_sites)


