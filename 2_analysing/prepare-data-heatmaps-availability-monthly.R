library(urltools)
source("config/config.R")


df_domains_to_analyse <- read_csv(file = "data/helper/22-09-21-Top News Websites [AU - public].xlsx - German news.csv") %>% 
  mutate(cleaned_urls = domain(URL) %>% suffix_extract(.) %>% select(domain) %>% pull(.),)

df_heatmaps_availability <- df_snippet_info %>% 
  select(crawl_date, detected, site, sha1) %>% 
  distinct() %>% 
  filter(site %in% df_domains_to_analyse$cleaned_urls) %>% 
  mutate(year = year(crawl_date),
         month = month(crawl_date)) %>% 
  group_by(year, month, site) %>% 
  summarise(count = n()) %>% 
  ungroup()

save(df_heatmaps_availability, file = paste0(path_to_shinydata, "df_heatmaps_availability.RData"))

  filter(site == "bild") %>% 
  ggplot() +
  geom_tile(aes(x = month, y = year, fill = count))



testing <- read_csv(file = "data/raw/html-file-information.csv", n_max = 10)
### liste aller links ins archive erstellen

heatmap_for <- df_heatmaps_availability %>% 
  select(site) %>% distinct() %>% pull(.)

save(heatmap_for, file = paste0(path_to_shinydata, "heatmap_for.RData"))


## checking on domain that got filtered out

all_domains_found <- df_snippet_info %>% 
  select(site) %>% 
  distinct()

domains_to_keep <- df_snippet_info %>% 
  select(site) %>% 
  distinct() %>% 
  filter(site %in% df_domains_to_analyse$cleaned_urls) 


df_domains_to_analyse %>% 
  select(cleaned_urls) %>% 
  anti_join(., domains_to_keep, by= c("cleaned_urls" = "site"))

# setdiff(domains_to_keep, $cleaned_urls )

domains_to_skip <- df_snippet_info %>% 
  select(site) %>% 
  distinct() %>% 
  filter(!site %in% df_domains_to_analyse$cleaned_urls) 

find_similar_domains_matrix <- map_df(df_domains_to_analyse$cleaned_urls, function(i){
  print(i)
  # print(domains_to_skip)
  domains_to_skip %>% 
    mutate("{i}" := str_detect(site, i))
  
})

find_similar_domains <- find_similar_domains_matrix %>% 
  pivot_longer(2:ncol(.), names_to = "detected", values_to = "value") %>% 
  filter(value == TRUE)
