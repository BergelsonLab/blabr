library(viridis)
library(extrafont)
font_install(paths = "fonts/")
font_import(paths = "fonts/", prompt = F)
if(Sys.info()['sysname'][[1]] == "Windows") {loadfonts(device = "win")}
loadfonts(device = "pdf")
library(tidyverse)

theme_blab <- function () {
  theme_bw(base_size=18, base_family="Franklin Gothic Medium") %+replace%
    theme(
      panel.background  = element_rect(fill = "white"),
      panel.border = element_rect(fill = NA, colour = "black"),
      plot.background = element_rect(fill="transparent", colour=NA),
      legend.background = element_rect(fill = "white"),
      legend.box.background = element_rect(colour = "black"),
      legend.key.size = unit(1.75, units = "picas"),
      legend.text = element_text(margin = margin(l = 0)),
      legend.key = element_rect(fill="transparent", colour=NA),
      axis.text = element_text(colour = "black"),
      axis.title = element_text(size = 24),
      panel.grid.major = element_line(colour = "grey85", size = .3),
      panel.grid.minor = element_line(colour = "grey92", size = .3),
      strip.background = element_rect(fill = "#440154"),
      strip.text = element_text(colour = "white", size = 18, margin = margin(t = 5, 0, b = 5, 0))
    )
}

theme_spooky <- function() {
  theme_bw(base_size=32, base_family="serif") %+replace%
    theme(
      panel.background = element_rect(fill = "#FFFFFF"),
      panel.border = element_rect(fill = "transparent", colour = "grey50"),
      legend.text = element_text(margin = margin(l = 0), colour = "white"),
      axis.text = element_text(colour = "white"),
      axis.title = element_text(colour = "white"),
      plot.background = element_rect(fill = "#0f0838", colour = "#0f0838"),
      legend.background = element_rect(fill = "#0f0838", colour = "#0f0838"),
      legend.key = element_blank(),
      strip.background = element_rect(fill = "#102b52", colour = "grey50"),
      strip.text = element_text(colour = "white", size = 24, margin = margin(t = 5, 0, b = 5, 0))
      )
}

theme_AMERICA <- function() {
  theme_bw(base_size=18, base_family="sans") %+replace%
    theme(
      panel.background = element_rect(fill = "#FFFFFF"),
      panel.border = element_rect(fill = "transparent", colour = "grey50"),
      legend.text = element_text(margin = margin(l = 0), colour = "white"),
      axis.text = element_text(colour = "white"),
      axis.title = element_text(colour = "white"),
      panel.grid.major = element_line(colour = "#BF0A30", size = 8),
      panel.grid.minor = element_line(colour = "#BF0A30", size = 8),
      panel.grid.major.x = element_line(colour = "transparent", size = .3),
      panel.grid.minor.x = element_line(colour = "transparent", size = .3),
      plot.background = element_rect(fill = "#002868", colour = "#002868"),
      legend.background = element_rect(fill = "#002868", colour = "#002868"),
      legend.key = element_blank(),
      strip.background = element_rect(fill = "white", colour = "grey50"),
      strip.text = element_text(colour = "#002868", size = 18, margin = margin(t = 5, 0, b = 5, 0))
    )
}
