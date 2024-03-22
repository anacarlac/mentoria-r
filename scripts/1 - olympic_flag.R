library(ggplot2)
library(ggforce)
library(ggtext)
library(htmltools)

olympic_colors <- c("#0085C7", "#F4C300", "#000000", "#009F3D", "#DF0024")

circles <- tibble::tibble(
  # circle positions
  x = c(1, 3, 5, 2, 4),
  y = c(2, 2, 2, 1, 1),
  r = .88,
  # add some arcs to make circles appear like a chain
  start = c(0.0, 0.5, 0.0, 0.0, 0.0) * pi,
  end =   c(0.7, 0.7, 0.1, 0.1, 0.1) * pi
)

olympic_flag <- ggplot(circles) +
  aes(x0 = x, y0 = y, r = r, colour = factor(x)) +
  # create olympic circles
  geom_circle(size = 6) +
  # add arcs to make the circles appear like a chain
  geom_arc(aes(start = start, end = end), size = 6) +
  # add colors
  scale_colour_manual(values = olympic_colors) +
  coord_equal() +
  theme_void(17) +
  # using {htmltools} to write in bold
  labs(
    title = p(
      "Olympic flag using",
      strong("#rstats"),
      "and",
      strong("{ggplot2}"), br()
    ), 
    caption = p(
      strong("Author: "), "Julio Trecenti", br(),
      strong("Code: "), "bit.ly/olympic-ggplot2"
    )
  ) +
  theme(
    plot.title = element_markdown(hjust = .5),
    plot.caption = element_markdown(size = 12),
    legend.position = "none"
  )

olympic_flag

ggsave(
  "olympic_flag.png",  
  width = 7.76, height = 5.34,
  olympic_flag, bg = "white"
)
