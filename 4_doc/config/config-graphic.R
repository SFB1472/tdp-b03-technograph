
library(ggtext)
library(gdtools)
# library(extrafont)
# extrafont::loadfonts(quiet = TRUE)
gdtools::register_gfont("Roboto Mono")

typo_sfb_mono <- "GT America Mono LC Rg"
# typo_sfb_mono_bold <- "Roboto Mono"

# systemfonts::register_font(typo_sfb_mono)
# typo_sfb_mono_remote <- "GT America Mono LC Rg"
# typo_sfb_mono_bold_remote <- "GT America Mono Rg"

sfb_black <- "#000000"
sfb_grey <- "#E5E5E5"
    
theme_b03_base <- theme(
    plot.title.position = "plot",
    plot.title = element_text(family = "Nantes-Regular", size = 18),
    axis.title = element_blank(),
    axis.text = element_text(family = typo_sfb_mono, size = 10, hjust = 0),
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
    legend.title = element_text(family = typo_sfb_mono, size = 12, hjust = 0.5),
    legend.margin = margin(0, .2, 0, .2, "in"),
    legend.key = element_blank(),
    text = element_text(family = typo_sfb_mono, size = 12, hjust = 0)
  )
  
theme_b03_dot_timeline <- theme(
    panel.grid.major.y = element_line(color = "#dddddd", linewidth = .2),
    panel.grid.minor = element_blank(),
    panel.grid.minor.x = element_line(color = "#dddddd", linewidth = .2),
    panel.grid.major.x = element_blank()
  )
  
theme_b03_heatmap <- theme(
    panel.grid.major.y = element_line(color = "#c2c2c2", linewidth = .2),
    panel.grid.minor.y = element_line(color = "#e2e2e2", linewidth = .1),
    plot.title = element_markdown(),
    plot.subtitle = element_markdown()
  )
  
theme_b03_heatmap_details <- theme(
    panel.grid.major.y = element_line(color = "#c2c2c2", linewidth = .2),
    panel.grid.minor.y = element_line(color = "#e2e2e2", linewidth = .1),
    legend.position = "none",
    plot.subtitle = element_textbox_simple(size = 10, lineheight = 1, padding = margin(5, 0, 20, 0))
  )
  
theme_b03_facets <- theme(
    strip.text = element_text(family = typo_sfb_mono, size = 11, hjust = 0, angle = 0),
    panel.grid.major.y = element_blank(),
    panel.background = element_rect(fill = "#f1f1f1")
  )

theme_b03_facets_individual <- theme(
  strip.background = element_rect(fill = "#fbfbfb", color = NA),
  strip.text = element_text(family = typo_sfb_mono, size = 13, hjust = 0, angle=0),
  panel.grid.major.y = element_line(color = "#e2e2e2", linewidth = .2),
  strip.placement = "outside"#,
  # strip.switch.pad.grid = unit(2, "in")
)

theme_b03_panel_spacing <- theme(
  panel.spacing.y = unit(.1, "in"),
  panel.spacing.x = unit(.2, "in")
)  

theme_b03_text <- theme(
    text = element_text(family = typo_sfb_mono, size = 12, hjust = 0),
    panel.spacing = unit(0, "in")
    
  )

theme_b03_legend_discrete <- theme(
  legend.justification = "center",
  legend.position = "top",
  legend.direction = "horizontal",
  legend.title = element_blank(),
  legend.margin = margin(.3, .2, 0, .15, "in"),
  legend.key = element_blank(),
  legend.key.height = unit(.5, "lines"),
  legend.key.width = unit(1, "lines")
)
  
theme_b03_box_timeline <- theme(
    axis.text.y = element_text(size = 8),
    panel.grid.major.x = element_line(color = "#c2c2c2", linewidth = .2),
    panel.grid.minor.x = element_line(color = "#e2e2e2", linewidth = .2)
  )
  
theme_b03_timeline_faceted <- theme(
  strip.text = element_text(margin = margin(t = 10, r = 0, b = 3, l = 0, unit = "pt")),
  axis.text.x = element_text(family = typo_sfb_mono, size = 10, hjust = 0.5),
  panel.grid.major.y = element_line(color = "#c2c2c2", linewidth = .1),
  panel.grid.minor.y = element_blank(),
  panel.grid.major.x = element_line(color = "#ffffff", linewidth = .2),
  panel.grid.minor.x = element_blank()
)
