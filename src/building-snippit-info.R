library(tidyverse)
library(lubridate)
library(urltools)

path <-  "data/snippit-detection/"

all_snippit_info <- list.files(path = path)
# all_snippit_info <- "snippit-detection-1.csv"


df_snippit_info <- map_df(all_snippit_info, function(i){
  df <- read_csv(file = paste0(path, i)) %>% 
    select(-filename) %>% 
    mutate(crawl_date = ymd(crawl_date),
           site = domain(url) %>% suffix_extract(.) %>% select(domain) %>% pull(.))
  
})

save(df_snippit_info, file = "data/df_snippit_info.RData")

df_snippit_info %>% 
  filter(is.na(site)) %>% View()
  

df_snippit_info %>% 
  mutate(site = domain(url) %>% suffix_extract(.) %>% select(domain) %>% pull(.),
         year = year(crawl_date)) %>% 
  filter(site == "wiwo", year == 2017) %>% View()

## was fällt auf? 
## teilweise horrend hohe zahlen für manche domains: funktioniert das package richtig? 
## Warum sind die Seiten so ungleich vertreten, wenn die Zahlen stimmen sollten?
## Wie Anne schon sagte, es sind viele Domains dabei, die keine newssites sind. Wie filtern wir die raus? wie viele sinds denn überhaupt unique?
## im summarise-statement schreib ich ein na.rm mit rein, denn manche zeilen haben bei detected tatsächlich ein NA. 
## Warum? Ist dort kein html in der content-spalte?

test <- df_snippit_info %>% 
  # filter(detected == 1) %>% 
  mutate(site = domain(url) %>% suffix_extract(.) %>% select(domain) %>% pull(.),
         year = year(crawl_date)) %>% 
  group_by(year, site) %>% 
  summarise(counted_snippits = sum(detected, na.rm=TRUE), is_present = n())

nr_domains <- df_snippit_info %>% group_by(site) %>% summarise(count = n()) #%>% nrow()

