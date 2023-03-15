library(tidyverse)
library(lubridate)
library(DBI)
library(RPostgres)

source("config/config-secret.R")

con <- dbConnect(RPostgres::Postgres(), 
                 dbname = dsn_database,
                 host = dsn_hostname, 
                 port = dsn_port,
                 user = dsn_uid, 
                 password = dsn_pwd
)
dbDisconnect(con)

### , sha1 != "da39a3ee5e6b4b0d3255bfef95601890afd80709"

df_sites_per_day <- dbGetQuery(conn = con, paste0("SELECT s.crawl_date, COUNT(s.sha1) as sites_per_day FROM sites s GROUP BY s.crawl_date"))

dbListTables(conn = con)

#### allgemeine zeitliche übersicht

get_form_info <- function(sphere, tag){
  
  df <- dbGetQuery(conn = con, paste0("SELECT DISTINCT t.site, t.tag, t.group, s.crawl_date, s.sha1, s.sphere FROM sites s INNER JOIN tags t ON t.site = s.sha1 WHERE s.sphere LIKE'", sphere, "' AND t.tag LIKE '", tag, "'")) 
  
}

df_form_de <- get_form_info("German", "form") %>% 
  group_by(crawl_date) %>% 
  summarise(nr_tags = n()) %>% 
  left_join(., df_sites_per_day) %>% 
  mutate(normalized = nr_tags/sites_per_day)

df_form_de %>% 
  ggplot(., aes(x=crawl_date, y = normalized)) +
  geom_col()


#### welche scripte wurden eingesetzt?

get_form_attr_info <- function(sphere, tag, attr){
  df <- dbGetQuery(conn = con, paste0("SELECT t.site, t.tag, t.name, s.crawl_date, s.sphere, t.attr, t.group FROM sites s INNER JOIN tags t ON t.site = s.sha1 WHERE s.sphere LIKE'", sphere, "' AND t.tag LIKE '", tag, "' AND t.name LIKE '", attr, "'")) 
    
}

df_form_de_actions <- get_form_attr_info("German", "form", "action") %>% 
  mutate(script_detected = re2_match(attr, SCRIPT_ENDINGS_TO_SEARCH_FOR, simplify = TRUE)[,1]) %>% 
  arrange(crawl_date) %>% 
  group_by(crawl_date, script_detected) %>% 
  summarise(scripts_counted = n()) %>% 
  left_join(., df_sites_per_day) %>% 
  mutate(normalised = scripts_counted/sites_per_day)



df_form_de_actions %>% 
  ggplot(., aes(x = crawl_date, y = script_detected, fill = normalised)) +
  geom_tile()


df_all_form_colums <- dbGetQuery(conn = con, paste0("SELECT DISTINCT t.site, t.tag, t.group, s.crawl_date, t.name, t.attr, s.sphere FROM sites s INNER JOIN tags t ON t.site = s.sha1 WHERE s.sphere LIKE 'German' AND t.tag LIKE 'form'")) 

df_all_form_colums %>% 
  filter(!is.na(group)) %>% 
  group_by(site, group) %>% 
  mutate(found_action = ifelse(name == "action", 1, NA)) %>% #View()
  arrange(found_action) %>% 
  fill(found_action) %>% 
  filter(is.na(found_action)) %>% View()


df_form_tags_without_action <- df_all_form_colums %>% 
  left_join(., get_form_attr_info("German", "form", "action")) %>% 
  filter(is.na(name), !is.na(group))

# was bedeutet das ;iso am schluss? http://wissen.spiegel.de/wissen/resultset.html;iso  

# df_test <- get_form_info("German", "form")
# df_test %>% nrow() - df_form_de_actions %>% nrow() = 31945
# 31945 form tags haben kein action attribut, was bedeutet das? was sind das für formulare?
# falsche schlussfolgerung: in df_test sind 14059 seiten aufgeführt, die überhaupt keine formuluare enthalten
# bleiben immer noch 17886 ohne action-attribut


#### Wo stecken womöglich kommentare drin?

get_form_with_comment <- function(sphere_, tag){
  # print(sphere_)
  df <- dbGetQuery(conn = con, paste0("SELECT s.crawl_date, t.site as sha1, s.sphere, t.tag, s.site, t.name, t.attr, t.group, regexp_matches(t.attr, '", COMMENTS_IN_TAGS, "') as matches FROM sites s INNER JOIN tags t ON t.site = s.sha1 WHERE s.sphere LIKE '", sphere_, "' AND t.tag LIKE '", tag, "'"))%>% 
    mutate(year = year(crawl_date)) %>% 
    group_by(year, site) %>% 
    summarise(counted = n()) %>% #View()
    left_join(., df_domains_to_analyse) %>% #View()
    filter(!is.na(Name), sphere == sphere_) %>% #View()
    mutate(system = "forms parsed",
           type = "automated") %>% 
    select(-counted)
}

df_form_comments <- get_form_with_comment("German", "form") 
save(df_form_comments, file = "3_visualizing_app/data/German/df_form_comments.RData")

df_form_comments <- get_form_with_comment("Dutch", "form") 
save(df_form_comments, file = "3_visualizing_app/data/Dutch/df_form_comments.RData")

df_form_comments <- get_form_with_comment("World", "form") 
save(df_form_comments, file = "3_visualizing_app/data/World/df_form_comments.RData")


df_form_comments %>% 
  ggplot(., aes(x = year, y = Name)) +
  geom_point()+
  theme_b03_base + theme_b03_dot_timeline + theme(plot.margin = margin(0,0.5,0,0, "cm"))+
  scale_x_continuous(breaks = year_breaks_for_plotting, labels = year_breaks_for_plotting,  expand = c(0, NA), name = "crawl year") +
  coord_cartesian(clip = "off")
