library(tidyverse)
library(ggforce)
library(ggiraph)
source("config/config-graphic.R")
df_sheet <- read_csv("3_visualizing_app/technograph/data/historiograms/visual analyses - traces of commenting options - Tabellenblatt1.csv")

df_sheet_plot <- df_sheet %>% 
  filter(type %in% c("offsite", "onsite", "meta"))

df_make_shades <- df_sheet_plot %>% 
  select(-comment, -archive_link, -screenshot) %>% 
  filter(type != "meta") %>% 
  mutate(continuous_group = cur_group_id(), .by = c(site, type)) %>% 
  mutate(closed_groups = ifelse(lead(continuous_group) != continuous_group, "last", "same"),
         closed_groups_start = ifelse(lag(continuous_group) != continuous_group, "first", "same"), .by = site) %>% 
  mutate(closed_groups = ifelse(is.na(closed_groups), "last", closed_groups),
         closed_groups_start = ifelse(is.na(closed_groups_start), "first", closed_groups_start),
         date_blurred = floor_date(date,unit ="month"),
         change = ifelse(closed_groups == "same" & closed_groups_start == "first", T, F)) %>% 
  mutate(index = cumsum(change), .by = site) #%>% 

df_plot_segments <- df_make_shades %>% 
  summarise(start_date = first(date), end_date = last(date), .by = c(site, index, type)) %>% 
  mutate(blurry_start = start_date - months(6),
         blurry_end = end_date + months(6))


# dates_min_max <- df_make_shades %>% select(date_blurred) %>% reframe(first_date = (min(date_blurred)), last_date = max(date_blurred))
# dates <- seq(dates_min_max$first_date[[1]], dates_min_max$last_date[[1]], by = "month") %>% as_tibble()
# sites <- df_make_shades %>% select(site) %>% distinct() %>% pull(.)
# df_dates_sites <- tidyr::expand_grid(dates, sites) %>% rename("date_blurred" = "value", "site" =)
# 
# 
# df_plot_data_tiles <- df_dates_sites %>% 
#   left_join(., df_make_shades, by = c("value" = "date_blurred", "sites" = "site")) %>% 
#   group_by(site) %>% 
#   fill(continuous_group, .direction = "down")
# 
# register_gfont("Roboto Mono")
# register_gfont("roboto")
# systemfonts::register_font(plain = "GT America Mono Rg")

plot <- ggplot() +
  geom_segment(data = df_plot_segments, aes(x = blurry_start, y = type, xend = blurry_end, yend = type, color = type), alpha = .5, lineend = "round", linewidth = 2)+
  geom_segment(data = df_plot_segments, aes(x = start_date, y = type, xend = end_date, yend = type, color = type), lineend = "round", linewidth = 2)+
  geom_point_interactive(data = df_sheet_plot, aes(x = date, y = type, tooltip = paste0(comment))) +
  theme_b03_base + theme_b03_facets_individual + theme(legend.position = "none") +
  facet_col(vars(site), scales = "free_y", space = "free" )

girafe(ggobj = plot)

plot
