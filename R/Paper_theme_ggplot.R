#Create a custom Theme for ggplot2 for talks (typically I use a black background for my slides)
theme_paper <- function() {
  theme_light(base_size = 25, base_family = "") %+replace%
    theme(panel.background = element_rect(fill = "transparent", colour = NA),
          plot.background = element_rect(fill = "transparent", colour = NA),
          legend.background = element_blank(),
          panel.grid = element_blank(),
          title = element_blank(),
          legend.position = "none",
          axis.text = element_text(size = 6)
          )
}
