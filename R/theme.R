# library(extrafont)
# font_import(paths = "fonts/", prompt = F)
# if(Sys.info()['sysname'][[1]] == "Windows") {loadfonts(device = "win")}
# loadfonts(device = "pdf")
# loadfonts(device = "postscript")
# fonts()

#' Theme "blab" for ggplot2
#'
#' @return a ggplot2::theme object
#' @export
theme_blab <- function () {
  ggplot2::theme_bw(base_size = 18, base_family = "") %+replace%
    ggplot2::theme(
      panel.background  = ggplot2::element_rect(fill = "white"),
      panel.border = ggplot2::element_rect(fill = NA, colour = "black"),
      plot.background = ggplot2::element_rect(fill = "transparent",
                                              colour = NA),
      legend.background = ggplot2::element_rect(fill = "white"),
      legend.box.background = ggplot2::element_rect(colour = "black"),
      legend.key.size = ggplot2::unit(1.75, units = "picas"),
      legend.text = ggplot2::element_text(margin = ggplot2::margin(l = 3)),
      legend.key = ggplot2::element_rect(fill = "transparent",
                                         colour = NA),
      axis.text = ggplot2::element_text(colour = "black"),
      axis.title = ggplot2::element_text(size = 24),
      panel.grid.major = ggplot2::element_line(colour = "grey85",
                                               linewidth = .3),
      panel.grid.minor = ggplot2::element_line(colour = "grey92",
                                               linewidth = .3),
      strip.background = ggplot2::element_rect(fill = "#440154"),
      strip.text = ggplot2::element_text(
        colour = "white",
        size = 18,
        margin = ggplot2::margin(t = 5, 0, b = 5, 0)
      )
    )
}

#' Theme "spooky" for ggplot2
#'
#' @return a ggplot2::theme object
#' @export
theme_spooky <- function() {
  ggplot2::theme_bw(base_size = 32, base_family = "serif") %+replace%
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "#FFFFFF"),
      panel.border = ggplot2::element_rect(fill = "transparent",
                                           colour = "grey50"),
      legend.text = ggplot2::element_text(margin = ggplot2::margin(l = 0),
                                          colour = "white"),
      axis.text = ggplot2::element_text(colour = "white"),
      axis.title = ggplot2::element_text(colour = "white"),
      plot.background = ggplot2::element_rect(fill = "#0f0838",
                                              colour = "#0f0838"),
      legend.background = ggplot2::element_rect(fill = "#0f0838",
                                                colour = "#0f0838"),
      legend.key = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "#102b52",
                                               colour = "grey50"),
      strip.text = ggplot2::element_text(
        colour = "white",
        size = 24,
        margin = ggplot2::margin(t = 5, 0, b = 5, 0)
      )
    )
}

#' Theme "AMERICA" for ggplot2
#'
#' @return a ggplot2::theme object
#' @export
theme_AMERICA <- function() {
  ggplot2::theme_bw(base_size = 18, base_family = "sans") %+replace%
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "#FFFFFF"),
      panel.border = ggplot2::element_rect(fill = "transparent",
                                           colour = "grey50"),
      legend.text = ggplot2::element_text(margin = ggplot2::margin(l = 0),
                                          colour = "white"),
      axis.text = ggplot2::element_text(colour = "white"),
      axis.title = ggplot2::element_text(colour = "white"),
      panel.grid.major = ggplot2::element_line(colour = "#BF0A30",
                                               linewidth = 8),
      panel.grid.minor = ggplot2::element_line(colour = "#BF0A30",
                                               linewidth = 8),
      panel.grid.major.x = ggplot2::element_line(colour = "transparent",
                                                 linewidth = .3),
      panel.grid.minor.x = ggplot2::element_line(colour = "transparent",
                                                 linewidth = .3),
      plot.background = ggplot2::element_rect(fill = "#002868",
                                              colour = "#002868"),
      legend.background = ggplot2::element_rect(fill = "#002868",
                                                colour = "#002868"),
      legend.key = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "white",
                                               colour = "grey50"),
      strip.text = ggplot2::element_text(
        colour = "#002868",
        size = 18,
        margin = ggplot2::margin(t = 5, 0, b = 5, 0)
      )
    )
}
