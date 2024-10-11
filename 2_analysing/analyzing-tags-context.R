library(tidyverse)
library(DBI)
library(RPostgres)
library(digest)
library(scales)

source("config/config-secret.R")

SPHERE_FOR_SHEET <- "German"

con <- dbConnect(RPostgres::Postgres(), 
                 dbname = dsn_database,
                 host = dsn_hostname, 
                 port = dsn_port,
                 user = dsn_uid, 
                 password = dsn_pwd
)

# creating a group_id for all form tags  ----------------------------------------------------------------------------------------------------------------------------------------------------------


#### Verbesserung im Vergleich zu den form tags: Es gibt jetzt schon eine Gruppenvariable aus dem Parsing
#### in der Datenbank gibt es eine Spalte of_interest in der sites tabelle, so fallen hier einige Schritte raus.

df_tags_context_ <- read_csv(file = "data/1-parsing/tags-context/German/context-all-traces-2.csv", show_col_types = FALSE) %>% 
  # bind_rows(., read_csv(file = "data/1-parsing/tags-context/German/context-all-traces-2.csv", show_col_types = FALSE)) %>% 
  select(-`...1`) %>% 
  mutate(tag_group = cur_group_id(), .by = c(sha1, context_path),
         id_sha1_group = paste0(sha1, "_", tag_group)) 

# df_tags_context_ <- df_tags_context %>%  mutate(tag_group = cur_group_id(), .by = c(sha1, context_path))
# rm(df_tags_context_)
dbWriteTable(conn = con, name = "tag_context", value = df_tags_context, append = TRUE)

# applying this id to all elements nested in the form tags if there are traces of commenting or not ----------------------------------------------------------------------------------------------
# entfällt, weil die Tabelle schon mit der id aus dem parsing kam
  
# filter the big df down to only those forms with comment traces and all elements nested  ------------------------------------------------------------------------------------------------------------------------
# entfällt ebenfalls, weil die neue Tabelle sowieso nur solche context-tags enthält, deren Elternelemente commenting-traces enthalten. Und zwar auch solche Traces, die auf Kommentarsysteme hindeuten

# hashing all form tags with nested elements for better checking on changes  ------------------------------------------------------------------------------------------------------------------------
# rm(df_hashed_forms)
## iteration "most_pragmatic" steht für eine minimal Variante an verwendeten Spalten zum hashen. Überlegungen dazu in den Fieldnotes

df_hashed_context <- df_tags_context %>% 
  select(id_sha1_group, tag, attr) %>% 
  reframe(hashed_context = digest::sha1(c(tag, attr), serialize = F), .by = id_sha1_group) %>% #,
  mutate(sphere = SPHERE_FOR_SHEET,
         sha1 = str_remove(id_sha1_group, "_\\d{1,}$"),
         iteration = "pragmatic")

save(df_hashed_forms, file="data/1-parsing/tags-context/German/hashed-tags.RData")

# df_test_1 <- df_form_context_full %>% 
#   filter(site == "bild") %>% 
#   left_join(., df_hashed_forms) %>% 
#   arrange(crawl_date) %>% 
#   mutate(nr_unique_hashes = match(hashed_forms, unique(hashed_forms)))# %>% 
#   # filter(id_sha1_form_group == "133f83214d349e94be9ec1715fe2e4f484e23064_3")
#   # View()
# 
# df_test_2 <- df_form_context_full %>% 
#   filter(site == "bild") %>% 
#   left_join(., df_hashed_forms) %>% 
#   arrange(crawl_date) %>% 
#   mutate(nr_unique_hashes = match(hashed_forms, unique(hashed_forms))) %>% 
#   filter(id_sha1_form_group == "8cfc8474fde7a42d3bbe955953aebc4674e22048_3")


# 133f83214d349e94be9ec1715fe2e4f484e23064_3
# 8cfc8474fde7a42d3bbe955953aebc4674e22048_3

# write new table of hashes to db ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

dbWriteTable(conn = con, name = "context_hashed", value = df_hashed_context, append = TRUE)

dbDisconnect(con)


# trying some cleaning ----------------------------------


attrs_to_empty_values <- c("href", "title","data-video-poster", "alt", "data-video-src", "data-video-title", "data-video-teaser", "datetime", "action", "src", "name", "srcset")


## Welche Kombinationen von Attributen und Werten kommen wie häufig vor?
# interessant werden hier vor allem die einstelligen Einträge
duplicate_attr_values <- df_tags_context %>%
  reframe(counted = n(), .by = c(attr, value))

## auf einen Blick: welche Werte haben die einmal vorkommenden Attribute
individual_attrs <- duplicate_attr_values %>%
  filter(counted < 100, !is.na(value)) #%>% View()

individual_attrs  %>% filter(is.na(value)) %>% View()

duplicate_attr_values %>% 
  reframe(histogram = n(), .by = counted) %>% View()

## und welche Attribute haben die meisten unterschiedlichen Werte, denn ab denen würde ich mit dem Filtern ansetzen.
most_individual_attrs <- duplicate_attr_values %>%
  filter(counted < 100, !is.na(value)) %>% #View()
  reframe(counted = n(), .by = attr) %>% #View()
  arrange(desc(counted))

# most_individual_attrs
# rm(cleaning_all_content)
cleaning_all_content <- individual_attrs %>%
  filter(attr %in% attrs_to_empty_values) %>% 
  mutate(value_cleaned = "value_replaced")

cleaning_class <- individual_attrs %>%
  filter(attr %in% c("class", "data-in_reply_to", "data-bookmark", "aria-controls")) %>%
  mutate(
    value_cleaned = ifelse(str_detect(value, "\\d{1,}"), str_replace_all(value, "\\d", "0"), NA),
    value_cleaned = ifelse(str_detect(value, "[/]{2,}"), "replaced_url", value_cleaned),
    value_cleaned = ifelse(str_detect(value, "comment byuser comment-author-"), "comment byuser comment-author-shortend", value_cleaned),
    value_cleaned = ifelse(str_detect(value_cleaned, "ee-post post-000000 post type-post status-publish format-standard has-post-thumbnail hentry category"), "ee-post post-000000 post type-post status-publish format-standard has-post-thumbnail hentry category-shortend", value_cleaned),
    value_cleaned = ifelse(str_detect(value_cleaned, "elementor elementor-000000 elementor-location-single post-000000 post type-post status-publish format-standard has-post-thumbnail hentry category"), "elementor elementor-000000 elementor-location-single post-000000 post type-post status-publish format-standard has-post-thumbnail hentry category shortend", value_cleaned),
    value_cleaned = ifelse(str_detect(value_cleaned, "post-000000 post type-post status-publish format-standard has-post-thumbnail category"), "post-000000 post type-post status-publish format-standard has-post-thumbnail category shortend", value_cleaned)
         )

cleaning_class %>% reframe(counted= n(), .by = c(attr, value_cleaned)) %>% View()

cleaning_onclick <- individual_attrs %>%
  filter(attr == "onclick") %>% #View()
  # head(3) %>%
  mutate(
    url_detected = ifelse(str_detect(value, "[\\/]{1,}"), "replaced_url", NA),
    function_replaced = ifelse (str_detect(value, "\\(.*$"), str_replace(value, "\\(.*$", "_deleted"), NA),
    value_cleaned = ifelse(!is.na(function_replaced), function_replaced, url_detected),
    # value_cleaned = ifelse(is.na(value_cleaned), value, value_cleaned),
    value_cleaned = ifelse(str_detect(value, "return rating"), str_replace_all(value_cleaned, "\\d", "0"), value_cleaned),
    value_cleaned = ifelse(str_detect(value, "follow_18"), "follow_18_deleted", value_cleaned)
  ) %>% select(-url_detected, -function_replaced)

cleaning_onclick%>% reframe(counted= n(), .by = c(attr, value_cleaned)) %>% View()

cleaning_id_attrs <- individual_attrs %>%
  filter(str_detect(attr, "id|value"), attr != "width", !attr %in% attrs_to_empty_values) %>%
  mutate(
    # id_found = ifelse(str_detect(value, "(?<=-|_)(?=.*?\\d)[a-z\\d]+$"), 1, NA),
         finding = ifelse(str_detect(value, "(?<=-|_)(?=.*?\\d)[a-zA-Z\\d-_]{1,}$"), str_extract(value, "(?<=-|_)(?=.*?\\d)[a-zA-Z\\d-_]{1,}$"), NA),
         check_on_word = ifelse(str_detect(finding, "^[a-z]{1,}-"), str_remove(finding, "^[a-z]{1,}-"), NA),
         hash_found = ifelse(str_detect(value, "(?=.*?\\d)[a-zA-Z\\d]{1,}$"), str_extract(value, "(?=.*?\\d)[a-zA-Z\\d]{1,}$"), 0),
         test = nchar(hash_found),
         clean_id_from = case_when(
           !is.na(check_on_word) ~ check_on_word,
           !is.na(finding) ~ finding,
           test > 16 ~ hash_found
         ),
         check_hash = nchar(value),
         value_cleaned = ifelse(!is.na(clean_id_from), str_replace(value, clean_id_from, "id_replaced"), NA),
         value_cleaned = ifelse(is.na(value_cleaned) & str_detect(value, "\\d{1,}"), str_replace_all(value, "\\d", "0"), value_cleaned),
         value_cleaned = ifelse(attr == "data-comments-remoteid" & check_hash == 36, "id_replaced", value_cleaned),
         value_cleaned = ifelse(attr == "data-userid", "id_replaced", value_cleaned),
         value_cleaned = ifelse(str_detect(value_cleaned, "bookmark"), str_remove_all(value_cleaned, "\\d"), value_cleaned),
         value_cleaned = ifelse(str_detect(value_cleaned, "[0a-fA-F]{10,10}") & check_hash == 10, "id_replaced", value_cleaned),
         value_cleaned = ifelse(attr == "data-id-mconf", "replaced", value_cleaned),
         value_cleaned = ifelse(str_detect(value, "^/"), "url_cleaned", value_cleaned),
         value_cleaned = ifelse(str_detect(value, "^http"), "url_cleaned", value_cleaned),
         value_cleaned = ifelse(attr == "value" & str_detect(value, "^[a-zA-Z\\d-]{1,}$"), "id_replaced", value_cleaned),
         value_cleaned = ifelse(str_detect(value, "Schreiben Sie hier Ihren Kommentar zum Artikel|Video"), "Schreiben Sie hier Ihren Kommentar zum Artikel/Video shortend", value_cleaned),
         value_cleaned = ifelse(check_hash == 44 & attr == "value", "id_replaced", value_cleaned),
         value_cleaned = ifelse(str_detect(value, "^re\\:\\s"), "re_replaced", value_cleaned),
         value_cleaned = ifelse(str_detect(value, "^(Thread|thread)\\:\\s"), "thread_replaced", value_cleaned),
         value_cleaned = ifelse(attr == "data-id", "id_replaced", value_cleaned)
         ) %>% 
  select(-finding, -check_on_word, -hash_found, -test, -clean_id_from, -check_hash)

# cleaning_id_attrs %>% 
#   filter(str_detect(value_cleaned, "[0a-f]{10,10}"), check_hash == 10) %>% View()

check_on_clean_id_attrs <- cleaning_id_attrs %>%
  reframe(counted = n(), .by = c(attr, value_cleaned))

cleaning_rel <- individual_attrs %>% 
  filter(attr == "rel") %>% 
  mutate(
    value_cleaned = ifelse(str_detect(value, "\\d{1,}"), str_replace_all(value, "\\d", "0"), NA),
    value_cleaned = ifelse(str_detect(value, "TFCMS1_CMS"), "TFCMS1_CMS_deleted", value_cleaned),
    value_cleaned = ifelse(str_detect(value, "TFVIDEO_VIDEO"), "TFVIDEO_VIDEO_deleted", value_cleaned)
         )

cleaning_rel %>% reframe(counted= n(), .by = c(attr, value_cleaned)) %>% View()


### für alle zahlen vereinheitlichen (data-in_reply_to, data-bookmark,aria-controls), schwieriger fall: data-userid

cleaning_datanocache <- individual_attrs %>% 
  filter(attr %in% c("data-nocache", "data-cache")) %>% 
  mutate(value_cleaned = ifelse(str_detect(value, "^.*\\?"), paste0(str_extract(value, "^.*\\?"),"_deleted"), NA)
         )
cleaning_datanocache %>% reframe(counted= n(), .by = c(attr, value_cleaned)) %>% View()


cleaning_dataparam <- individual_attrs %>% 
  filter(attr %in% c("data-param", "data:param", "data-bind")) %>% 
  mutate(value_cleaned = ifelse(str_detect(value, "\\d{1,}"), str_replace_all(value, "\\d", "0"), NA),
    value_cleaned = ifelse(str_detect(value, "^.*\\?"), paste0(str_extract(value, "^.*\\?"),"_deleted"), value_cleaned),
         value_cleaned = ifelse(attr== "data:param" & str_detect(value, '[{\\"]url'), "url: cleaned", value_cleaned),
         value_cleaned = ifelse(attr== "data:param" & str_detect(value, '[{\\"]title'), "title: cleaned", value_cleaned),
    value_cleaned = ifelse(attr== "data:param" & str_detect(value, '[{\\"]prefix'), "prefix: cleaned", value_cleaned),
         value_cleaned = ifelse(str_detect(value, "^.*\\jsessionid"), paste0(str_extract(value, "^.*\\jsessionid")), value_cleaned),
    value_cleaned = ifelse(str_detect(value, "^/") & !str_detect(value, "^/ajaxentry"), "url_cleaned", value_cleaned),
    value_cleaned = ifelse(str_detect(value, "^http"), "url_cleaned", value_cleaned),
    value_cleaned = ifelse(attr == "data-bind" & str_detect(value, "widget.Comments|widget.Disqus"), paste0(str_extract(value, "widget.Comments|widget.Disqus"), "_data_replaced"), value_cleaned)
         )

cleaning_dataparam %>% reframe(counted= n(), .by = c(attr, value_cleaned)) %>% View()

cleaning_style <- duplicate_attr_values %>% 
  filter(attr == "style") %>% 
  mutate(
         value_cleaned = ifelse(str_detect(value, "url"), paste0(str_extract(value, "^.*url"), "_replaced"), NA),
         value_cleaned = ifelse(is.na(value_cleaned),
                                str_replace_all(value, "[0-9\\.]{1,}px","px_cleaned"), 
                                str_replace_all(value_cleaned, "[0-9\\.]{1,}px","px_cleaned")),
         value_cleaned = ifelse(is.na(value_cleaned),
                                str_replace_all(value, "[0-9\\.]{1,}%", "size_cleaned"), 
                                str_replace_all(value_cleaned, "[0-9\\.]{1,}%", "size_cleaned")),
         value_cleaned = ifelse(is.na(value_cleaned), 
                                str_replace_all(value, "\\#[0-9a-fA-F]{6,6}", "color_cleaned"), 
                                str_replace_all(value_cleaned, "\\#[0-9a-fA-F]{6,6}", "color_cleaned")
                                )
         )

cleaned_attrs <- cleaning_class %>% 
  bind_rows(., cleaning_datanocache) %>% 
  bind_rows(., cleaning_dataparam) %>% 
  bind_rows(., cleaning_id_attrs) %>% 
  bind_rows(., cleaning_onclick) %>% 
  bind_rows(., cleaning_rel) %>% 
  bind_rows(., cleaning_style) %>%
  bind_rows(., cleaning_all_content)

# rm(cleaned_all_attrs)

duplicate_attr_values_cleaned <- duplicate_attr_values %>% 
  filter(attr != "style") %>% 
  full_join(., cleaned_attrs) %>% 
  mutate(value_cleaned = ifelse(is.na(value_cleaned), value, value_cleaned)) %>% 
  select(-counted)

duplicate_attr_values_cleaned_aggr <- duplicate_attr_values_cleaned %>% 
  reframe(counted = n(), .by = c(attr, value_cleaned))


df_tags_context_cleaned <- df_tags_context %>% 
  left_join(., duplicate_attr_values_cleaned) %>% #View()
  filter(!is.na(value)) %>% 
  mutate(value_cleaned = ifelse(str_detect(value_cleaned, "\\d{1,}"), str_replace_all(value_cleaned, "\\d", "0"), value_cleaned))

test_1 <- df_tags_context_cleaned %>% filter(id_sha1_group == "0544116585f73a36910a01fb2213694ce14c3199_5190") #%>% View()
test_2 <- df_tags_context_cleaned %>% filter(id_sha1_group == "0544116585f73a36910a01fb2213694ce14c3199_5195") #%>% View()

# hashing cleaned data ---------------------

df_hashed_context_cleaned <- df_tags_context_cleaned %>% 
  select(id_sha1_group, tag, attr, value_cleaned) %>% 
  reframe(hashed_context = digest::sha1(c(tag, attr, value_cleaned), serialize = F), .by = id_sha1_group) %>% #,
  mutate(sphere = SPHERE_FOR_SHEET,
         sha1 = str_remove(id_sha1_group, "_\\d{1,}$"),
         iteration = "cleaned_counted")

# df_hashed_forms_cleaned <- df_hashed_forms_cleaned %>% rename("hashed_context" = "hashed_forms")
dbWriteTable(conn = con, name = "context_hashed", value = df_hashed_context_cleaned, append = TRUE)



df_hashes_context_site <- dbGetQuery(conn = con, paste0("SELECT s.site, s.crawl_date, t.tag, t.attr, t.value, t.context_path, ch.hashed_context, ch.id_sha1_group, ch.iteration FROM sites s INNER JOIN tag_context t ON s.sha1 = t.sha1 INNER JOIN context_hashed ch ON t.id_sha1_group = ch.id_sha1_group WHERE s.site ='handelsblatt'"))


# second cleaning attemt ------------------------------------------------------------------

attrs_to_be_cleaned <- c("id", "class")

cleaned_attrs <- duplicate_attr_values %>% filter(attr %in% attrs_to_be_cleaned) %>% #View()
  mutate(id_check = str_detect(value, "[a-fA-F0-9-_]{1,}$"),
         detected = str_extract(value, "[a-fA-F0-9-_]{1,}$"),
         digits_found = str_detect(detected, "[\\d]{1,}"),
         value_cleaned = ifelse(id_check & digits_found, str_replace(value, detected, "id_replaced"), NA),
         value_cleaned = ifelse(str_detect(value_cleaned, "comment byuser comment-author"), str_extract(value_cleaned, "comment byuser comment-author"), value_cleaned),
         value_cleaned = ifelse(is.na(value_cleaned), value, value_cleaned),
         value_cleaned = ifelse(str_detect(value_cleaned, "[\\d]{1,}"), str_replace_all(value_cleaned, "[\\d]{1,}", "0"), value_cleaned)
         ) #%>% View()

duplicate_attr_values_cleaned_2 <- duplicate_attr_values %>% 
  filter(!attr %in% attrs_to_be_cleaned) %>% 
  full_join(., cleaned_attrs) %>% 
  filter(!is.na(value)) %>% 
  select(-counted, -id_check, -digits_found) 

df_tags_context_cleaned_2 <- df_tags_context %>% 
  filter(!is.na(value)) %>% 
  left_join(., duplicate_attr_values_cleaned_2) %>% 
  select(-detected)

df_tags_context_cleaned_2_for_hashing <- df_tags_context_cleaned_2 %>% 
  distinct(tag, attr, value_cleaned, id_sha1_group, .keep_all = TRUE)

dbDisconnect(con)

tag_context <- tbl(con, "tag_context")
rows_update(x = tag_context, y = df_tags_context_cleaned_2, by = c("sha1", "id_sha1_group", "tag", "attr", "value", "text", "sphere", "tag_group"), in_place = TRUE, unmatched = "ignore", copy = TRUE)

df_hashed_context_cleaned_2 <- df_tags_context_cleaned_2_for_hashing %>%
  select(id_sha1_group, tag, attr, value_cleaned) %>%
  group_by(id_sha1_group) %>% 
  arrange(., .by_group = TRUE) %>% 
  ungroup() %>% # View()
  reframe(hashed_context = digest::sha1(c(tag, attr, value_cleaned), serialize = F), .by = id_sha1_group) %>% #,
  mutate(sphere = SPHERE_FOR_SHEET,
         sha1 = str_remove(id_sha1_group, "_\\d{1,}$"),
         iteration = "cleaned_whitelist")

dbWriteTable(conn = con, name = "context_hashed", value = df_hashed_context_cleaned_2, append = TRUE)

# checking on hashes and cleaned values ----------------------------------------------------

df_tags_context_cleaned_mini <- df_tags_context_cleaned_2 %>% 
  filter(context_path == "{'class': ['clearfix'], 'id': 'comments'}") %>% 
  distinct(tag, attr, value_cleaned, id_sha1_group, .keep_all = TRUE)

df_hashed_context_cleaned_mini <- df_tags_context_cleaned_mini %>% 
  select(id_sha1_group, tag, attr, value_cleaned) %>% 
  group_by(id_sha1_group) %>% 
  arrange(., .by_group = TRUE) %>% 
  ungroup() %>% 
  reframe(hashed_context = digest::sha1(c(tag, attr, value_cleaned), serialize = F), .by = id_sha1_group) %>% #,
  mutate(sphere = SPHERE_FOR_SHEET,
         sha1 = str_remove(id_sha1_group, "_\\d{1,}$"),
         iteration = "cleaned")

df_hashed_context_cleaned_mini %>% select(hashed_context) %>% distinct() %>% nrow()

df_hashed_context_cleaned_mini %>% 
  left_join(., df_tags_context_cleaned_mini) %>% 
  select(context_path, hashed_context, id_sha1_group) %>% #distinct() %>% 
  reframe(counted_rows = n(), .by = c(context_path, hashed_context,id_sha1_group)) %>% View()
  reframe(shortest = min(counted_rows), longest = max(counted_rows), .by = c(context_path)) %>% View()



data-teasertracking
data-content
alt


93ff3321b61a885bb807da69cd19e0f4e5d01d9e
7c6b79e64f7dd700668a66d615a5f1f056c89576

df_hashes_context_site %>% filter(iteration == "most_pragmatic", str_starts(context_path, "\\{'class': \\['ajaxify'\\], 'data:command': 'getComments', 'data:param': '")) %>% #{\"url":"/ajaxentry/cache/numberofcomments?commentRootId=9695352\",\"key\":\"cid\",\"className\":\"showCommentNumbers\",\"idPrefix\":\"commentDiv_\"}', 'id': 'hcf-content-wrapper'}") %>% 
  select(hashed_context) %>% distinct() %>% nrow()

df_hashes_context_site %>% filter(iteration == "most_pragmatic") %>%
  select(hashed_context) %>% distinct() %>% nrow()

df_hashes_context_site %>% filter(iteration == "most_pragmatic", !str_starts(context_path, "\\{'class': \\['ajaxify'\\], 'data:command': 'getComments', 'data:param': '")) %>%
  View()

df_tags_context_ %>% select(id_sha1_group) %>% distinct() %>% nrow()
