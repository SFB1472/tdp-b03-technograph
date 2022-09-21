library(tidyverse)
library(lubridate)
library(googlesheets4)
library(ggiraph)
library(patchwork)

df_timespan <- seq(ymd("2007-01-01"), ymd("2021-06-01"), by = "month") %>% as_tibble()
df_fill_year <- function(year){
  vec_year <- seq(ymd(paste0(year, "-01-01")), ymd(paste0(year, "-12-01")), by = "month")
}

load("data/df_snippet_info.RData")
domain_to_look_at <- "ksta"

link_manuell_added_data_points_domains <- "https://docs.google.com/spreadsheets/d/1aEQAAh0UZlFImajCQ-E7cH-0nrlH8TGuxQt4PTbVCJE/"
gs4_deauth()
# gs4_auth(cache=".secrets")

ss <- gs4_get(link_manuell_added_data_points_domains)
gs_manual_domain_snippet <- read_sheet(ss) %>% 
  # group_by(year, site) %>% 
  # complete(year, month, fill = list(site = "0")) %>% 
  mutate(date = paste0(year, "-", ifelse(is.na(month), "1", month), "-", ifelse(is.na(day), "1", day)) %>% ymd(.),
         type = "manual")# %>% 
  # group_by(year) %>% 
  # fill()

gs4_deauth()

df_snippets_per_month_domain <- df_snippet_info %>% 
  mutate(
    year = year(crawl_date),
    month = month(crawl_date) %>% as.character(.)
  ) %>%
  group_by(year, month,  site, snippet) %>% 
  summarise(counted_snippets = sum(detected, na.rm=TRUE), counted_sites = n()) %>% 
  mutate(counted_sites = round(counted_sites/nr_of_snippets, digits = 0)) %>% 
  ungroup() %>% 
  left_join(., df_snippet_mapping, by = c("snippet" = "Snipit"))

save(df_snippets_per_month_domain, file = "shiny/b03-commenting-museum/data/df_snippets_per_month_domain.RData")

df_detail_data_automated <- df_snippets_per_month_domain %>% 
  mutate(date = paste0(year, "-", month, "-1") %>% ymd(.),
         type = "automated") %>% 
  # bind_rows(., gs_manual_domain_snippet) %>% 
  filter(site == domain_to_look_at) %>% 
  right_join(., df_timespan, by = c("date" = "value")) %>% #View()
  mutate(verification = "automated",
    verification_code = case_when(
      is.na(type) ~ "0",
      counted_snippets > 1 ~ "1"
  ))
  # mutate(a_no_archive_data = ifelse(is.na(type), "0", NA),
  #        b_automation = ifelse(counted_snippets > 1, "1", NA)) %>% 
  # pivot_longer(. , cols = "a_no_archive_data":"b_automation", names_to = "verification", values_to = "verification_code")
  
df_detail_data_manuall <- gs_manual_domain_snippet %>% 
  filter(site == domain_to_look_at) %>% 
  right_join(., df_timespan, by = c("date" = "value")) %>% 
  group_by(date) %>% 
  mutate(verified_by_researcher = ifelse(verified_by_researcher == "x", "2", NA),
         verified_by_interview = ifelse(verified_by_interview == "x", "3", NA)) %>% #View()
  pivot_longer(., cols = "verified_by_researcher":"verified_by_interview", names_to = "verification", values_to = "verification_code") %>% 
    ungroup()

df_detail_data <- df_detail_data_automated %>% 
  bind_rows(df_detail_data_manuall)

current_snippets_found_a <- df_detail_data_automated %>% 
  filter(counted_snippets > 0) %>% #View()
  select(snippet) %>% 
  distinct()
  
current_snippets_found_m <- df_detail_data_manuall %>% 
  select(snippet) %>% 
  filter(!is.na(snippet)) %>% 
  distinct()

current_snippets_found <- current_snippets_found_a %>% 
  bind_rows(., current_snippets_found_m) %>% 
  distinct()

detail_colors <- c("grey90", "#f5bb50", "#cba9be", "#b0799a")
color_breaks <- c("0" = "no archive data available", "1" = "detected automatically", "2" = "by researcher", "3" = "interview findings")

# df_detail_graphic %>% 
#   ggplot(data = , aes(y = value, x = 1, fill = verification_depth)) +
#   geom_tile() +
#   scale_fill_manual(values = detail_colors, labels = color_breaks)

## next steps: text aus dem sheet daneben schreiben, 
## wie viel details zu den gefunden snippets?
## oder anders: tabelle machen mit eingefärbten kacheln ? weil dinge nebeneinander existieren können?
## daten umformatieren. erst die daten, dann zeitleiste left-joinen, oder?



df_detail_graphic <- df_detail_data %>% 
  filter(snippet %in% current_snippets_found$snippet | is.na(snippet)) %>% #View()
  mutate(verification = factor(verification, levels = c("automated", "verified_by_researcher", "verified_by_interview")))

df_label_colors <- df_detail_graphic %>% 
  select(date, verification_code) %>% 
  filter(!is.na(verification_code)) %>% 
  group_by(date) %>% 
  summarise(verification_code = max(verification_code))

df_detail_annotations <- df_detail_graphic %>% 
  select(date, tooltip_info) %>% 
  # filter(!is.na(verification_code)) %>%
  distinct() %>% 
  arrange(desc(date)) %>% 
  # group_by(date, tooltip_info) %>% 
  # summarise(verification_code = max(verification_code)) %>% 
  mutate(dot = ifelse(is.na(tooltip_info), "0", "1")) %>% 
  left_join(., df_label_colors)
  # mutate(group = row_number()) #%>% 
  # top_n(., n = 1, group)
  

plot_details <- ggplot() +
  geom_tile_interactive(data = df_detail_graphic, aes(y = date, x = verification, fill = verification_code, tooltip = tooltip_info), alpha = .8) +
  # geom_text(data = df_detail_annotations, aes(y = date, x = 5, label = tooltip_info), hjust = 0)+
  # annotate("text", y = df_detail_annotations$date, label = df_detail_annotations$tooltip_info, x = 3, hjust = 0)+
  scale_fill_manual(values = detail_colors, labels = color_breaks, na.value = "white") +
  scale_x_discrete(position = "top") +
  scale_y_date(date_breaks = "years", minor_breaks = "month", date_labels = "%Y", expand = c(0, NA)) +
  labs(subtitle = "At the first column you can see the findings based on automated reseach.
<span style ='color:#bbbbbb;'>Grey fields</span> indicate, that there was no data in the dump from the internet archive.
<span style ='color:#f5bb50;'>Yellow</span> shows, that there was a system found by the automated search.\n
The the second and third column showing the qualitativ research. Any findings detected by researcher is colored <span style ='color:#cba9be;'>purple</span>.
In case those findings are verified by an interview partner, fields are colored in slightly <span style ='color:#b0799a;'>darker color</span>.") +
  theme_b03_heatmap_details +
  coord_cartesian(clip = 'off')


plot_text <- ggplot() +
  geom_textbox(data = df_detail_annotations, aes(y = date, x = 0.05, label = tooltip_info, fill = verification_code, color = "white", orientation = "upright", hjust = 0), width = unit(.8, "npc")) +
  geom_point(aes(y = as.Date("2007-01-01"), x = 3), color = "#ffffff")+
  geom_point(data = df_detail_annotations, aes(y = date, x = 0, color = dot)) +
  scale_y_date(date_breaks = "years", minor_breaks = "month", date_labels = "%Y", expand = c(0, 0), position = "right") +
  scale_x_continuous(expand = c(0,0), limits = c(0, NA)) +
  scale_color_manual(values = c("#ffffff", "#000000", "#ffffff")) +
  scale_fill_manual(values = detail_colors) +
  labs(subtitle = "In case of any qualitative research findings, it's sometimes necessary to comment on the concrete finding. It's noted in this column of the graphic.")+
  theme_b03_heatmap_details + theme(axis.text.x = element_blank()) +
  coord_cartesian(clip = 'off')

plots <- plot_details + plot_text
plots

plots + plot_annotation(theme(plot.title = element_markdown(), plot.subtitle = element_markdown()))

df_detail_annotations$dot %>% unique()

# girafe(ggobj = plots)
