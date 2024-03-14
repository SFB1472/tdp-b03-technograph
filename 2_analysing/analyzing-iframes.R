library(tidyverse)
library(urltools)
source("config/config-secret.R")

con <- dbConnect(RPostgres::Postgres(), 
                 dbname = dsn_database,
                 host = dsn_hostname, 
                 port = dsn_port,
                 user = dsn_uid, 
                 password = dsn_pwd
)

SPHERE_FOR_SHEET <- "World"

df_domains_to_analyse <- read_csv(file = "data/helper/22-09-21-Top News Websites [AU - public].xlsx - German news.csv") %>% 
  mutate(cleaned_urls = domain(URL) %>% suffix_extract(.) %>% select(domain) %>% pull(.),)

#### Opinary-module werden über diese url https://compass.pressekompass.net/compasses/ geladen

# SELECT DISTINCT s.site, s.sha1, s.crawl_date, tc.sphere, tc.id_sha1_form_group, tc.group, tc.name, tc.attr, tc.text FROM sites s INNER JOIN tag_context_2 tc ON s.sha1 = tc.site

df_world_test <- dbGetQuery(conn = con, paste0("SELECT COUNT(t.tags_id) as saved_sites FROM tags_2 t WHERE t.tag ='div' AND t.sphere = 'World'"))



df_opinary <- dbGetQuery(conn = con, paste0("SELECT DISTINCT s.site, s.sha1, s.crawl_date, t.tag, t.name, t.attr, t.group, t.sphere FROM sites s INNER JOIN tags_2 t ON s.sha1 = t.site WHERE t.tag ='iframe' AND t.sphere = '", SPHERE_FOR_SHEET, "' AND t.attr ~ 'pressekompass'"))

df_civey <- dbGetQuery(conn = con, paste0("SELECT DISTINCT s.site, s.sha1, s.crawl_date, t.tag, t.name, t.attr, t.group, t.sphere FROM sites s INNER JOIN tags_2 t ON s.sha1 = t.site WHERE t.tag ='iframe' AND t.sphere = '", SPHERE_FOR_SHEET, "' AND t.attr ~ 'civey'"))

df_rivva <- dbGetQuery(conn = con, paste0("SELECT DISTINCT s.site, s.sha1, s.crawl_date, t.tag, t.name, t.attr, t.group, t.sphere FROM sites s INNER JOIN tags_2 t ON s.sha1 = t.site WHERE t.tag ='iframe' AND t.sphere = '", SPHERE_FOR_SHEET, "' AND t.attr ~ 'rivva'"))

df_iframe <- dbGetQuery(conn = con, paste0("SELECT DISTINCT s.site, s.sha1, s.crawl_date, t.tag, t.name, t.attr, t.group, t.sphere FROM sites s INNER JOIN tags_2 t ON s.sha1 = t.site WHERE t.tag ='iframe' AND t.sphere = '", SPHERE_FOR_SHEET, "'"))

# df_iframe_invis <- dbGetQuery(conn = con, paste0("SELECT DISTINCT s.site, s.sha1, s.crawl_date, t.tag, t.name, t.attr, t.group, t.sphere FROM sites s INNER JOIN tags_2 t ON s.sha1 = t.site WHERE t.tag ='iframe' AND t.sphere = '", SPHERE_FOR_SHEET, "' AND t.name = 'height' AND t.attr = '0' GROUP BY t.group, s.site, s.sha1, s.crawl_date, t.tag, t.sphere, t.name, t.attr"))

df_wanted_sites <- dbGetQuery(conn = con, paste0("SELECT DISTINCT s.crawl_date, s.site, COUNT(s.sha1) as sites_per_day FROM sites s GROUP BY (s.crawl_date, s.site)")) %>% 
  filter(site %in% df_domains_to_analyse$cleaned_urls) %>% 
  mutate(year_month = paste0(year(crawl_date), "-", month(crawl_date), "-01")) %>% #View()
  reframe(counted_sites = sum(sites_per_day), .by = c("year_month", "site"))


df_iframe %>% 
  filter(site %in% df_domains_to_analyse$cleaned_urls, str_detect(attr, pattern = "facebook")) %>% 
  mutate(year_month = paste0(year(crawl_date), "-", month(crawl_date), "-01")) %>% #View()
  reframe(counted = n(), .by=c("year_month", "site")) %>% 
  left_join(., df_wanted_sites) %>% 
  mutate(normalized = counted/counted_sites) %>% 
  ggplot(., aes(x = year_month, y = normalized)) +
  geom_col() +
  facet_wrap(~ site) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

df_iframe %>% 
  filter(site %in% df_domains_to_analyse$cleaned_urls, str_detect(attr, pattern = "twitter")) %>% 
  mutate(year_month = paste0(year(crawl_date), "-", month(crawl_date), "-01")) %>% #View()
  reframe(counted = n(), .by=c("year_month", "site")) %>% 
  left_join(., df_wanted_sites) %>% 
  mutate(normalized = counted/counted_sites) %>% 
  ggplot(., aes(x = year_month, y = normalized)) +
  geom_col() +
  facet_wrap(~ site) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

df_iframe_invis <- df_iframe %>% 
  # group_by(group, site, sha1, crawl_date, sphere, tag) %>% 
  mutate(helper = ifelse(name == "height" & attr == "0", 1, NA)) %>% #View()
  group_by(group, sha1) %>% 
  fill(., helper) %>% #View()
  filter(helper == 1)

df_opinary %>% 
  mutate(year_month = paste0(year(crawl_date), "-", month(crawl_date), "-01")) %>% 
  ggplot(., aes(x = year_month)) +
  geom_bar() +
  facet_wrap(~ site) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


df_iframe_sz <- dbGetQuery(conn = con, paste0("SELECT DISTINCT s.site, s.sha1, s.crawl_date, t.tag, t.name, t.attr, t.group, t.sphere FROM sites s INNER JOIN tags_2 t ON s.sha1 = t.site WHERE t.tag ='iframe' AND s.site ='sueddeutsche'"))
df_script_sz <- dbGetQuery(conn = con, paste0("SELECT DISTINCT s.site, s.sha1, s.crawl_date, t.tag, t.name, t.attr, t.group, t.sphere FROM sites s INNER JOIN tags_2 t ON s.sha1 = t.site WHERE t.tag ='script' AND s.site ='sueddeutsche'"))

df_googletag <- dbGetQuery(conn = con, paste0("SELECT DISTINCT s.site, s.sha1, s.crawl_date, t.tag, t.name, t.attr, t.group, t.sphere FROM sites s INNER JOIN tags_2 t ON s.sha1 = t.site WHERE t.attr ~ 'googletag' AND t.sphere ='", SPHERE_FOR_SHEET,"'"))

df_civey <- dbGetQuery(conn = con, paste0("SELECT DISTINCT s.site, s.sha1, s.crawl_date, t.tag, t.name, t.attr, t.group, t.sphere FROM sites s INNER JOIN tags_2 t ON s.sha1 = t.site WHERE t.attr ~ 'civey' AND t.sphere ='", SPHERE_FOR_SHEET,"'"))

df_opinary <- dbGetQuery(conn = con, paste0("SELECT DISTINCT s.site, s.sha1, s.crawl_date, t.tag, t.name, t.attr, t.group, t.sphere FROM sites s INNER JOIN tags_2 t ON s.sha1 = t.site WHERE t.attr ~ 'pressekompass|opinary' AND t.sphere ='", SPHERE_FOR_SHEET,"'"))



df_sz <- dbGetQuery(conn = con, paste0("SELECT DISTINCT s.crawl_date, COUNT(s.sha1) as sites_per_day FROM sites s WHERE s.site ='sueddeutsche' GROUP BY s.crawl_date"))
df_sites_per_day <- dbGetQuery(conn = con, paste0("SELECT s.crawl_date, COUNT(s.sha1) as sites_per_day FROM sites s GROUP BY s.crawl_date"))

df_sz_counted <- df_sz %>% 
  mutate(year_month = paste0(year(crawl_date), "-", month(crawl_date), "-01")) %>% 
  reframe(sites_per_month = sum(sites_per_day), .by = "year_month")

df_iframe_sz %>% 
  filter(name == "src") %>% 
  filter(str_detect(attr, "googletag", negate= TRUE), str_detect(attr, "finanzen", negate= TRUE) ) %>% View()

df_sz %>% 
  mutate(year_month = paste0(year(crawl_date), "-", month(crawl_date), "-01")) %>% 
  ggplot(., aes(x = year_month)) +
  geom_bar()


df_iframe_sz %>% 
  select(site, sha1, crawl_date, group) %>% 
  distinct() %>% 
  mutate(year_month = paste0(year(crawl_date), "-", month(crawl_date), "-01")) %>% 
  reframe(counted_iframes = n(), .by = year_month) %>% #View()
  left_join(., df_sz_counted) %>% 
  mutate(normalized = counted_iframes/sites_per_month) %>% 
  ggplot(., aes(x = year_month, y = normalized)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

df_opinary %>% 
  filter(site %in% df_domains_to_analyse$cleaned_urls) %>% 
  mutate(year_month = paste0(year(crawl_date), "-", month(crawl_date), "-01") %>% ymd()) %>% 
  reframe(counted = n(), .by = c(year_month, site, tag)) %>% #View()
  # mutate(normalized = counted_iframes/sites_per_month) %>% 
  ggplot(., aes(x = year_month, y = counted, fill = tag)) +
  geom_col() +
  facet_wrap(~ site) +
  scale_x_date(date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
