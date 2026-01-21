#' Theme function for BC defaults
#' Automatically sets BC color palette.
#'
#' @param base_size Can be set to adjust default font sizes
#' @param base_family Can be set to change default font (R isn't great at fonts, so be careful)
#'
#' @export
#'
theme_bc <- function(base_size = 12, base_family = "", ...) {
  theme_bw(base_size = base_size, base_family = base_family, ...) %+replace%
    theme(
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.border = element_rect(fill = NA, colour = "#43525a"),
      panel.grid = element_line(colour = "#f1f2f2"),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "#332a86", colour = "#43525a"),
      strip.text = element_text(
        colour = "white", size = rel(0.9),
        margin = margin(0.8 * base_size / 2, 0.8 * base_size / 2, 0.8 * base_size / 2, 0.8 * base_size / 2)
      ),
      strip.text.y = element_text(angle = 90),
      legend.key = element_rect(fill = "transparent", colour = NA),
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.box.background = element_rect(fill = "transparent", color = NA),
      axis.title.y.right = element_text(angle = 90, margin = margin(l = base_size / 4), vjust = 0, size = base_size),
      axis.text = element_text(size = rel(0.9))
    )
}

