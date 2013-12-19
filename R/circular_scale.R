# doc:
# insert a plot in a plot: http://learnr.wordpress.com/2009/05/08/ggplot2-plot-inside-a-plot/

library(ggplot2)

circular_scale <- function(from = 0, to = 2 * pi, steps = 8, by = to / steps, offset = -pi * by / to, vertical = 3, colours = c("#0088FF", "#ffffff", "#F51D1D", "#400038", "#0088FF")) {
  pp = data.frame(x = seq(from, to, by = by), y = vertical - 1)
  ggplot(pp, aes(x=x, y=y)) + 
    geom_tile(aes(fill = x)) + 
  list(scale_y_continuous(limits = c(0, vertical)),
       scale_x_continuous(breaks = seq(from, to, by = by)),
       coord_polar(start = offset) ,
       scale_fill_gradientn(colours = colours, limits = c(from, to))
  )
}

circular_scale(0, 360, steps = 40)
circular_scale(steps = 40) +
theme_grey() %+replace%
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.length = grid::unit(0, "lines"),
    axis.ticks.margin = grid::unit(0, "lines"),
    legend.position = "none",
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.margin = grid::unit(0, "lines"),
    plot.background = element_blank(),
    strip.background = element_rect(colour = "#ffffff", fill="#eeeeee")
  )