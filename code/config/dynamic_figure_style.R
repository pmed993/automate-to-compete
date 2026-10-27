# Shared typography and layout for the main DiD and HHI dynamic figures.
# Keeping these values in one place guarantees identical character sizes.

DYNAMIC_FIGURE_TEXT <- list(
  base = 10.5,
  axis_title = 10.5,
  axis_text = 9,
  strip_x = 11,
  strip_y = 9,
  title = 13,
  subtitle = 10.5,
  caption = 9,
  legend = 9
)

dynamic_figure_theme <- function(legend_position = "none") {
  ggplot2::theme_minimal(base_size = DYNAMIC_FIGURE_TEXT$base) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.spacing = grid::unit(0.75, "lines"),
      strip.text.x = ggplot2::element_text(
        face = "bold",
        size = DYNAMIC_FIGURE_TEXT$strip_x
      ),
      strip.text.y = ggplot2::element_text(
        face = "bold",
        size = DYNAMIC_FIGURE_TEXT$strip_y
      ),
      strip.background = ggplot2::element_rect(
        fill = "grey95",
        colour = "grey80"
      ),
      axis.text = ggplot2::element_text(size = DYNAMIC_FIGURE_TEXT$axis_text),
      axis.title.x = ggplot2::element_text(
        size = DYNAMIC_FIGURE_TEXT$axis_title,
        margin = ggplot2::margin(t = 8)
      ),
      axis.title.y = ggplot2::element_text(
        size = DYNAMIC_FIGURE_TEXT$axis_title,
        margin = ggplot2::margin(r = 8)
      ),
      plot.title = ggplot2::element_text(
        face = "bold",
        size = DYNAMIC_FIGURE_TEXT$title
      ),
      plot.subtitle = ggplot2::element_text(
        colour = "grey30",
        size = DYNAMIC_FIGURE_TEXT$subtitle
      ),
      plot.caption = ggplot2::element_text(
        colour = "grey35",
        hjust = 0,
        size = DYNAMIC_FIGURE_TEXT$caption
      ),
      legend.position = legend_position,
      legend.text = ggplot2::element_text(size = DYNAMIC_FIGURE_TEXT$legend),
      legend.title = ggplot2::element_text(size = DYNAMIC_FIGURE_TEXT$legend),
      plot.margin = ggplot2::margin(8, 10, 8, 8)
    )
}
