library(tidyverse)
library(googlesheets4)
library(urltools)
source("config/config.R")

link_predefined_domains <- SPREADSHEET_PATH_DOMAINS[[{{CURRENT_SPHERE}}]]
gs4_auth(cache=".secrets", scopes = "https://www.googleapis.com/auth/spreadsheets.readonly")

gs_domain_to_look <- read_sheet(SPREADSHEET_PATH_GENERELL, sheet = SPREADSHEET_PATH_DOMAINS[[{{CURRENT_SPHERE}}]]) %>% 
  select(Name, URL) %>% 
  mutate(site = domain(URL) %>% suffix_extract(.) %>% select(domain) %>% pull(.)) 

link_to_summary <- paste0(config_data$server$url, config_data$server$datasets$spheres[{{CURRENT_SPHERE}}], config_data$server$datasets$results$`system-year-overview`, "?token=", jupyter_token)

df_raw_systems_per_year <-  readr::read_csv(file = link_to_summary) %>% 
  rename("site" = `...1`) %>% #View()
  mutate(site = trimws(site),
         site = domain(site) %>% suffix_extract(.) %>% select(domain) %>% pull(.)) %>% #View()
  left_join(., gs_domain_to_look) %>% #View()
  select(site, Name, URL, everything()) %>% #View()
  pivot_longer(4:ncol(.), names_to = "year", values_to = "systems") #%>% 
  filter(Name != "Times of India") ## schlimmer hack, Times of India haben wir nicht in den Daten. Durch Roberts preprocessing, auf das ich keinen Einfluss habe, passiert beim rück übersetzen zum vollständigen Seitennamen ein Fehler und die Daten für India Times werden gedoppelt. 
    # distinct()

# config_data$server$datasets$spheres[{{CURRENT_SPHERE}}]


# anti_join(gs_domain_to_look, df_raw_systems_per_year, by = "site")


nr_systems_found <- df_raw_systems_per_year %>% 
  mutate(find_systems = strsplit(systems, ";") %>% lengths()) %>% 
  summarise(counted = max(find_systems))

df_systems_per_year <- df_raw_systems_per_year  %>% 
  # right_join(., gs_domain_to_look) %>% View()
  separate(systems, into = paste0("system_", 1:nr_systems_found$counted), sep = ";") %>% #View()
  pivot_longer(5:ncol(.), names_to = "names", values_to = "system") %>% 
  select(-names) %>% 
  distinct() %>%
  group_by(year, site) %>%
  mutate(nr_systems_per_site = row_number(),
         na_from_pivot_longer = ifelse(is.na(system) & nr_systems_per_site > 1, 1, 0)) %>% 
  ungroup() %>% #View()
  filter(na_from_pivot_longer == 0) %>% 
  mutate(system = trimws(system),
         system = ifelse(system == "Data unavailable", NA, system)) %>% 
  distinct() %>% #View()
  select(-na_from_pivot_longer)
  # filter(system != "Data unavailable")

save(df_systems_per_year, file = paste0(PATH_TO_SHINY_DATA,"/", CURRENT_SPHERE, "/df_systems_per_year.RData"))

