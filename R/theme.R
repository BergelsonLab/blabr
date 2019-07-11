library(viridis)
install.packages("extrafontdb")
install.packages("extrafont")
library(extrafont)
font_import(paths = "fonts/", prompt = F)
fonts() #get a list of fonts
ifelse(Sys.info()['sysname'][[1]] == "Windows",
       extrafont::loadfonts(device = "win"), NULL)
extrafont::loadfonts(device = "pdf")
library(tidyverse)

df <- tibble(x = c("a", "a", "a", "a", "b", "b", "b", "b"),
            y = c(1,2,3,4,3,4,5,6),
            z = as_factor(c("This", "is", "the", "Best", "Plot", "you'll", "Ever", "see")))

theme_blab <- function () {
  theme_bw(base_size=18, base_family="Franklin Gothic Medium") %+replace%
    theme(
      panel.background  = element_rect(fill = "white"),
      panel.border = element_rect(fill = NA, colour = "black"),
      plot.background = element_rect(fill="transparent", colour=NA),
      legend.background = element_blank(),
      legend.box.background = element_rect(colour = "black"),
      legend.key = element_rect(fill="transparent", colour=NA),
      axis.text = element_text(colour = "black"),
      panel.grid.major = element_line(colour = "grey90", size = .2),
      panel.grid.minor = element_line(colour = "grey95", size = .1),
      strip.background = element_rect(fill = "#440154"),
      strip.text = element_text(colour = "white", size = 14, margin = margin(t = 5, 0, b = 5, 0))
    )
}

plot <- ggplot(df, aes(x = x, y = y, fill = z)) +
  stat_summary(geom = "bar", position = "dodge") +
  ggtitle("I think we've arrived") +
  scale_fill_viridis(discrete = T) +
  facet_grid(~x) +
  theme_blab()
plot
ggsave(plot, filename = "plot.png", bg ="transparent")



printfigs <- "Y" # Set to anything but "Y" to stop the figures from printing
BW <- "Y" # Set to anything but "Y" for color figures
OL.thresh <- 3 # SD*N threshold for excluding outliers
corpuscolors <- c("firebrick1", "gold1", "forestgreen", "dodgerblue1")
corpuslabels <- c("Bergelson", "McDivitt", "VanDam", "Warlaumont")
agdcolors <- c("maroon", "mediumblue")
agdcolors.bu <- c("chartreuse3", "blue2")
agdlabels <- c("Woman  ", "Man  ")
agecutlabels <- c("<12mo.", ">12mo.")
#medcolors <- c("gray70", "gray50", "gray20")
medcolors <- c("#E495A5", "#ABB065", "#39BEB1")
medlabels <- c("No BA  ", "BA  ", "AD")
cgdcolors <- c("purple", "yellow")
cgdlabels <- c("girl", "boy")
