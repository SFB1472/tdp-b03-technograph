
library(ggtext)

typo_sfb_serif <- "Nantes-Regular"
typo_sfb_mono <- "GTAmericaMonoLC-Rg"

register_gfont("Roboto Mono")

typo_sfb_mono_remote <- "Roboto Mono"
typo_sfb_mono_bold_remote <- "Roboto Mono"

# systemfonts::register_font(typo_sfb_mono_remote)
# systemfonts::register_font(typo_sfb_mono_bold_remote)

# df_timespan_year <- seq(ymd("2007-01-01"), ymd("2021-06-01"), by = "year") %>% as_tibble()

# year_breaks_for_plotting <- df_timespan_year %>% 
#   mutate(years = year(value)) %>% 
#   select(years) %>% pull(.)

sfb_black <- "#000000"
sfb_grey <- "#E5E5E5"

theme_b03_base <- theme(
  plot.title.position = "plot",
  axis.title = element_blank(),
  axis.text = element_text(family = typo_sfb_mono_remote, size = 12, hjust = 0.5),
  axis.ticks = element_blank(),
  axis.ticks.length = unit(0,"in"),
  panel.background = element_blank(),
  panel.border = element_blank(),
  panel.spacing = unit(0, "in"),
  panel.grid = element_blank(),
  strip.background = element_blank(),
  strip.text = element_blank(),
  legend.justification = "center",
  legend.position = "top",
  legend.direction = "horizontal",
  legend.title = element_text(family = typo_sfb_mono_remote, size = 12, hjust = 0.5),
  legend.margin = margin(0, .2, 0, .2, "in"),
  legend.key = element_blank(),
  text = element_text(family = typo_sfb_mono_remote, size = 12, hjust = 0)
)

theme_b03_dot_timeline <- theme(
  panel.grid.major.y = element_line(color = "#dddddd", size = .2),
  panel.grid.minor = element_blank(),
  panel.grid.minor.x = element_line(color = "#dddddd", size = .2),
  panel.grid.major.x = element_blank()
)

theme_b03_heatmap <- theme(
  panel.grid.major.y = element_line(color = "#c2c2c2", size = .2),
  panel.grid.minor.y = element_line(color = "#e2e2e2", size = .1),
  plot.title = element_markdown(),
  plot.subtitle = element_markdown()
)

theme_b03_heatmap_details <- theme(
  panel.grid.major.y = element_line(color = "#c2c2c2", size = .2),
  panel.grid.minor.y = element_line(color = "#e2e2e2", size = .1),
  legend.position = "none",
  plot.subtitle = element_textbox_simple(size = 10, lineheight = 1, padding = margin(5, 0, 20, 0))
)

theme_b03_facets <- theme(
  strip.text = element_text(family = typo_sfb_mono_bold_remote, size = 13, hjust = 0, angle=0)#,
  
)

theme_b03_facets_individual <- theme(
  strip.background = element_rect(fill = "#fbfbfb", color = NA),
  strip.text = element_text(family = typo_sfb_mono_remote, size = 13, hjust = 0, angle=0),
  panel.grid.major.x = element_line(color = "#e2e2e2", linewidth = .1),
  strip.placement = "outside"#,
  # strip.switch.pad.grid = unit(2, "in")
)

theme_b03_text <- theme(
  text = element_text(family = typo_sfb_mono_bold_remote, size = 12, hjust = 0),
  panel.spacing = unit(0, "in")
  
)

theme_b03_box_timeline <- theme(
  axis.text.y = element_text(size = 8),
  panel.grid.major.x = element_line(color = "#c2c2c2", size = .2),
  panel.grid.minor.x = element_line(color = "#e2e2e2", size = .2)
)