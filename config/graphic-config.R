

theme_b03_dot_timeline <- theme(
  strip.background = element_blank(),
  axis.text.y = element_text(size = 8),
  axis.ticks = element_blank(),
  # axis.ticks.length = unit(0,"in"),
  # axis.title = element_blank(),
  # panel.background = element_rect(fill = "#f2f2f2"),
  panel.background = element_blank(),
  panel.border = element_blank(),
  # panel.spacing = unit(c(0,0,0,0), "lines"),
  panel.grid.major.y = element_line(color = "#c2c2c2", size = .2),
  panel.grid.major.x = element_line(color = "#c2c2c2", size = .2),
  # panel.grid.minor.x = element_line(color = "#e2e2e2", size = .2),
  # legend.background = element_blank(),
  # legend.title = element_blank(),
  legend.justification = "left",
  legend.position = "top",
  legend.direction = "horizontal",
  # legend.margin = margin(0, 1, 0.2, 0, "in"),
  # legend.key = element_rect(fill = "#ffffff"),
  # legend.key.size = unit(0.2, "in"),
  # text = element_text(family = sz_font, color = sz_black),
  # plot.background = element_rect(fill = "#f2f2f2", color = NA),
  # plot.title = element_text(family = sz_font, color = sz_black),
  # strip.background = element_blank(),
  strip.text = element_blank(),
  panel.spacing = unit(0, "in")
  # plot.margin = unit(c(0, 0.1, 0, 0.1), "in")
)

theme_b03_heatmap <- theme(
  strip.background = element_blank(),
  axis.text.y = element_text(size = 8),
  axis.ticks = element_blank(),
  panel.grid.major.y = element_line(color = "#dddddd", size = .2),
  panel.grid.minor.y = element_line(color = "#dddddd", size = .2),
  # axis.ticks.length = unit(0,"in"),
  # axis.title = element_blank(),
  # panel.background = element_rect(fill = "#f2f2f2"),
  panel.background = element_blank(),
  panel.border = element_blank(),
  # panel.spacing = unit(c(0,0,0,0), "lines"),
  panel.grid.major.x = element_line(color = "#c2c2c2", size = .2),
  panel.grid.minor.x = element_line(color = "#e2e2e2", size = .2),
  # legend.background = element_blank(),
  # legend.title = element_blank(),
  legend.justification = "left",
  legend.position = "top",
  legend.direction = "horizontal",
  # legend.margin = margin(0, 1, 0.2, 0, "in"),
  # legend.key = element_rect(fill = "#ffffff"),
  # legend.key.size = unit(0.2, "in"),
  # text = element_text(family = sz_font, color = sz_black),
  # plot.background = element_rect(fill = "#f2f2f2", color = NA),
  # plot.title = element_text(family = sz_font, color = sz_black),
  # strip.background = element_blank(),
  strip.text = element_blank(),
  panel.spacing = unit(0, "in")
  # plot.margin = unit(c(0, 0.1, 0, 0.1), "in")
)

# sztheme_heatmap <- theme(
#   strip.background = element_blank(),
#   axis.text.x = element_text(family = "SZoSansCond-Light", size = 14, hjust = 1, vjust = 0.5, angle = 90),
#   axis.text.y = element_text(family = "SZoSansCond-Light", size = 14, hjust = 1),
#   axis.ticks = element_blank(),
#   axis.title = element_blank(),
#   panel.background = element_blank(),
#   panel.border = element_blank(),
#   # panel.grid.minor.x = element_line(color = "#ff0000", size = 10),
#   # panel.grid.minor.y = element_line(color = "#ff0000", size = 1),
#   panel.grid.major = element_blank(),
#   legend.background = element_blank(),
#   legend.title = element_blank(),
#   legend.justification = "left",
#   legend.position = "top",
#   legend.direction = "horizontal",
#   legend.key = element_blank(),
#   legend.key.size = unit(0.15, "in"),
#   text = element_text(size = 22.5, family = "SZoSansCond-Light"),
#   plot.background = element_blank()
# )
# 
# sztheme_lines <- theme(
#   strip.background = element_blank(),
#   axis.text.x = element_text(family = "SZoSansCond-Light", size = 14, hjust = 1, vjust = 0.5, angle = 90),
#   axis.text.y = element_text(family = "SZoSansCond-Light", size = 14, hjust = 1),
#   axis.ticks = element_blank(),
#   axis.title = element_blank(),
#   panel.background = element_rect(fill = "#f2f2f2"),
#   panel.border = element_blank(),
#   # panel.grid.minor.x = element_line(color = "#ff0000", size = 10),
#   # panel.grid.minor.y = element_line(color = "#ff0000", size = 1),
#   panel.grid.major = element_blank(),
#   legend.background = element_blank(),
#   legend.title = element_blank(),
#   legend.justification = "left",
#   legend.position = "top",
#   legend.direction = "horizontal",
#   legend.key = element_blank(),
#   legend.key.size = unit(0.15, "in"),
#   text = element_text(size = 22.5, family = "SZoSansCond-Light"),
#   plot.background = element_blank()
# )
# 
# sztheme_hate_bars <- theme(
#   strip.background = element_blank(),
#   axis.text.x = element_text(family = "SZoSansCond-Light", size = 14, hjust = 1, vjust = 1, angle = 45),
#   axis.text.y = element_text(family = "SZoSansCond-Light", size = 14, hjust = 1),
#   axis.ticks = element_blank(),
#   axis.title = element_blank(),
#   panel.background = element_rect(fill = "#f2f2f2"),
#   panel.border = element_blank(),
#   # panel.grid.minor.x = element_line(color = "#ff0000", size = 10),
#   # panel.grid.minor.y = element_line(color = "#ff0000", size = 1),
#   panel.grid.major = element_blank(),
#   # legend.background = element_blank(),
#   # legend.title = element_blank(),
#   # legend.justification = "left",
#   legend.position = "none",
#   # legend.direction = "horizontal",
#   # legend.key = element_blank(),
#   # legend.key.size = unit(0.15, "in"),
#   text = element_text(size = 22.5, family = "SZoSansCond-Light"),
#   plot.background = element_blank()
# )
# 
# sztheme_multiple <- theme(
#   strip.background = element_blank(),
#   # axis.text.y = element_blank(),
#   axis.text = element_text(family = "SZoSansCond-Light", size = 14),
#   # axis.title.x = element_blank(),
#   # axis.title.y = element_blank(),
#   # axis.ticks = element_blank(),
#   panel.background = element_rect(fill = "#f2f2f2"),
#   # panel.spacing.x = unit(3, "lines"),
#   # panel.spacing.y = unit(2, "lines"),
#   panel.border = element_blank(),
#   panel.grid.major = element_line(color = "#333333", size = .1),
#   panel.grid.minor = element_blank(),
#   legend.background = element_blank(),
#   legend.title = element_blank(),
#   legend.justification = "left",
#   legend.position = "top",
#   legend.direction = "horizontal",
#   # legend.key = element_blank(),
#   legend.key.size = unit(0.25, "in"),
#   legend.spacing.x = unit(0.25, "in"),
#   text = element_text(size = 22.5, family = "SZoSansCond-Light"),
#   plot.background = element_blank()
# )
# 
# sztheme_radar <- theme(
#   strip.background = element_blank(),
#   axis.text.x = element_text(family = "SZoSansCond-Light", size = 18, hjust = 0),
#   axis.text.y = element_text(family = "SZoSansCond-Light", size = 18, hjust = 1),
#   axis.ticks = element_blank(),
#   axis.title = element_blank(),
#   panel.background = element_blank(),
#   panel.border = element_blank(),
#   # panel.grid.minor.x = element_line(color = "#ff0000", size = 10),
#   # panel.grid.minor.y = element_line(color = "#ff0000", size = 1),
#   panel.grid.major = element_blank(),
#   legend.background = element_blank(),
#   legend.title = element_blank(),
#   legend.justification = "left",
#   legend.position = "top",
#   legend.direction = "horizontal",
#   legend.key = element_blank(),
#   legend.key.size = unit(0.15, "in"),
#   text = element_text(size = 22.5, family = "SZoSansCond-Light"),
#   plot.background = element_blank()
# )

# 
# research_colors = c("beifall"="#B2182B","heiterkeit"="#F4A582","sonstiges"="#f2f2f2","widerspruch"="#2166AC","zuruf"="#92C5DE")
# bucket_colors <- c("1" = "#ffffff", "2" = "#E0E0E3", "3" = "#C0C1C6", "4" = "#71737E", "5" = "#29293A")
# abs_colors = c("afd"="#00B0A9","union"="#222222","spd"="#333333","gruene"="#383838","linke"="#454545","fdp"="#555555", "fraktionslos"="#666666" )
# stacked_colors = c("AfD"="#4093B2","andere Fraktionen"="#29293A")
# stacked_share = c("AfD"="#00B0A9","andere Fraktionen"="#29293A")
# sz_greens = c("#00B0A9", "#66C0BA", "#97D0CC", "#BEE0DD")
# party_colors = c(
#   "AfD" = "#4093B2",
#   "Union" = "#333333",
#   "FDP" = "#F6CE5E",
#   "Die Grünen" = "#60A961",
#   "Die Linke" = "#B53360",
#   "SPD" = "#E53E44",
#   "Sonstige" = "#999999"
# )

# #### Plot Styles
# 
# desktop_width = 8.889
# mobile_width = 5.0
# 
# # desktop artikelbreite im longread
# lr_article_width = 9.0
# 
# # Heatmaps
# desktop_height <- 4.5
# mobile_height <- 4.5
# 
# sz_font = "SZoSans-Regular"
# sz_black = "#29293A"
# sz_font_size_geom_text = 6.35
# sz_font_size = 18

