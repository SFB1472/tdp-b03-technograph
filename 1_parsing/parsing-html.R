library(tidyverse)
library(purrr)
library(rvest)
library(xml2)
library(urltools)
library(googlesheets4)
library(testdat)
source("config/config.R")

search_date <- lubridate::today()
html_element_to_search_for <- "form"

df_empty <- tibble(site = character(), tag = character(), search_date = date(), name = character(), attr = character(), group = character(), missing = character())
write_csv(df_empty, paste0("data/1-parsing/tags/", CURRENT_SPHERE, "/", html_element_to_search_for, "-raw.csv"))

file_list <- list.files(paste0("data/0-preprocessing/", CURRENT_SPHERE, "/"))

# if parsing the many files fail, restart here ------------------------------------------------------------

already_parsed_sites <- read_csv("data/1-parsing/scripts/German/scripts-raw.csv", col_select = c("site")) %>% #select(site) %>%
  distinct() %>% mutate(site = paste0(site, ".html")) %>%  pull()

file_list <- setdiff(file_list, already_parsed_sites)

# parsing tags ------------------------------------------------------------

df_tags <- map_df(file_list, function(i){
  
  # i <- "0010d9b0459a8ab77877b9e8f6f56fc6f810c270.html"
  
  cli::cli_inform("Parse file {i}")
  
  df_empty <- tibble(
    site = i %>% str_remove(".html"), 
    tag = html_element_to_search_for, 
    search_date = search_date,
    name  =  NA,
    attr =  NA,
    group = NA,
    missing = NA
  )
  
  site <- xml2::read_xml(paste0("data/0-preprocessing/", CURRENT_SPHERE, "/", i), as_html = TRUE, options = "RECOVER") #%>% 
  
  if(length(site) > 1){
    all_tags <- site %>% html_elements(html_element_to_search_for)
    
    if(length(all_tags) != 0){
      # print("innen")
    
      df_inner <- tibble(
        site = i %>% str_remove(".html"),
        tag = html_element_to_search_for,
        search_date = search_date,
        attrs  =  all_tags %>% html_attrs() %>% map_df(., function(j){
          df_attrs <- tibble(
            name = names(j),
            attr = j
          ) %>% nest(data = everything()) }
        )
      ) %>% 
        mutate(group = row_number()) %>% 
        unnest(attrs) %>%
        unnest(data) %>% 
        mutate(missing = NA) 
      
      test_that(
        desc = "df_inner: no duplicates",
        expect_unique(everything(), data = df_inner)
      )
    } else{
      # print("no tags found")
      df_empty$missing[[1]] = "no tags found"
      df_inner <- df_empty
    }
  } else {
    # print("unvalid html site")
    df_empty$missing[[1]] = "unvalid html site"
    df_inner <- df_empty
  }
  
  write_csv(df_inner, paste0("data/1-parsing/tags/", CURRENT_SPHERE, "/", html_element_to_search_for, "-raw.csv"), append = TRUE)
  
})





#### saving subset of data of found script tags for experimenting with library for guessing which script languages are embedded
#### -> turns out, works  not so well

head(df_scripts)

df_scripts_plain <- df_scripts %>% 
  mutate(content = trimws(content)) %>% 
  filter(content != "", !str_detect(content, "^<!-- "), !str_detect(content, "^//"))



write_csv(df_scripts_plain, "data/German/scripts-to-guess.csv")
