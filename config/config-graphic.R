### the themes in this file are not the finals. 
### the final themes are composed in the config-files placed with the app code

library(ggtext)

typo_sfb_serif <- "Nantes-Regular"
typo_sfb_mono <- "GTAmericaMonoLC-Rg"

typo_sfb_mono_remote <- "GT America Mono LC Rg"

sfb_black <- "#000000"
sfb_grey <- "#E5E5E5"

theme_b03_dot_timeline <- theme(
  strip.background = element_blank(),
  axis.text.y = element_text(family = typo_sfb_mono, size = 8, hjust = 0),
  axis.ticks = element_blank(),
  panel.background = element_blank(),
  panel.border = element_blank(),
  panel.grid.major.y = element_line(color = "#c2c2c2", size = .2),
  panel.grid.major.x = element_line(color = "#c2c2c2", size = .2),
  legend.justification = "left",
  legend.position = "top",
  legend.direction = "horizontal",
  strip.text = element_blank(),
  panel.spacing = unit(0, "in")
  # plot.margin = unit(c(0, 0.1, 0, 0.1), "in")
)



theme_b03_heatmap <- theme(
  strip.background = element_blank(),
  axis.title = element_blank(),
  axis.text = element_text(family = typo_sfb_mono_remote, size = 8, hjust = 0.5),
  axis.ticks = element_blank(),
  text = element_text(hjust = 0),
  panel.grid = element_blank(),
  axis.ticks.length = unit(0,"in"),
  panel.background = element_blank(),
  panel.border = element_blank(),
  panel.grid.major.y = element_line(color = "#c2c2c2", size = .2),
  panel.grid.minor.y = element_line(color = "#e2e2e2", size = .1),
  legend.justification = "left",
  legend.position = "top",
  legend.direction = "horizontal",
  plot.title = element_markdown(),
  plot.subtitle = element_markdown(),
  plot.title.position = "plot",
  strip.text = element_blank(),
  panel.spacing = unit(0, "in")
)


theme_b03_heatmap_intro  <- theme(
  strip.background = element_blank(),
  axis.title = element_blank(),
  axis.text = element_blank(),
  axis.text.x = element_text(family = typo_sfb_mono, size = 10, hjust = 0.5, margin = unit(c(.1, 0, 0, 0), 'in')),
  axis.ticks.length = unit(0,"in"),
  axis.ticks = element_blank(),
  panel.grid = element_blank(),
  panel.background = element_blank(),
  panel.border = element_blank(),
  legend.justification = "center",
  legend.title = element_text(hjust = 0.5),
  legend.position = "top",
  legend.direction = "horizontal",
  text = element_text(family = typo_sfb_mono, color = sfb_black),
  plot.title = element_markdown(),
  plot.subtitle = element_markdown(),
  plot.title.position = "plot",
  strip.text = element_blank(),
  panel.spacing = unit(0, "in")
)

theme_b03_heatmap_details <- theme(
  strip.background = element_blank(),
  axis.title = element_blank(),
  axis.text = element_text(size = 8, hjust = 0.5),
  axis.ticks = element_blank(),
  text = element_text(size = 8, hjust = 0),
  panel.grid = element_blank(),
  axis.ticks.length = unit(0,"in"),
  panel.background = element_blank(),
  panel.border = element_blank(),
  panel.grid.major.y = element_line(color = "#c2c2c2", size = .2),
  panel.grid.minor.y = element_line(color = "#e2e2e2", size = .1),
  legend.justification = "left",
  legend.position = "none",
  legend.direction = "horizontal",
  plot.subtitle = element_textbox_simple(size = 10, lineheight = 1, padding = margin(5, 0, 20, 0)),
  plot.title.position = "plot",
  strip.text = element_blank(),
  panel.spacing = unit(0, "in")
)


