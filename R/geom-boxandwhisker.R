# Manually pull functions from ggplot2
data_frame0 <- function(...) tibble(..., .name_repair = "minimal")

# Name ggplot grid object
# Convenience function to name grid objects
#
# @keyword internal
ggname <- function(prefix, grob) {
  grob$name <- grid::grobName(grob, prefix)
  grob
}


#' A box and whisker quantile plot
#'
#' The boxplot compactly displays the distribution of a continuous variable.
#' It visualises five summary statistics (the median, two hinges
#' and two whiskers), and all "outlying" points individually.
#'
#' @section Summary statistics:
#' By default, the box is the same as `ggplot2::geom_boxplot`, but the whiskers are based on quantiles instead of IQR.
#' Calculations use the 5 quantiles provided, with whiskers extending to the first and last quantiles, the hinges
#' corresponding to the second and fourth, and middle line for the third.
#' By default, it also includes a middle point at the mean and displays the count below the plot.
#'
#' In a notched box plot, the notches extend `1.58 * IQR / sqrt(n)`.
#' This gives a roughly 95% confidence interval for comparing medians.
#' See McGill et al. (1978) for more details.
#'
#'
#' @param geom,stat Use to override the default connection between
#'   `geom_boxplot()` and `stat_boxplot()`. For more information about
#'   overriding these connections, see how the [stat][layer_stats] and
#'   [geom][layer_geoms] arguments work.
#' @param outliers Whether to display (`TRUE`) or discard (`FALSE`) outliers
#'   from the plot. Hiding or discarding outliers can be useful when, for
#'   example, raw data points need to be displayed on top of the boxplot.
#'   By discarding outliers, the axis limits will adapt to the box and whiskers
#'   only, not the full data range. If outliers need to be hidden and the axes
#'   needs to show the full data range, please use `outlier.shape = NA` instead.
#' @param count Whether to display (`TRUE`) or hide (`FALSE`) the count below the plot.
#' @param middlepoint Can be equal to "mean" (default) or "90th" to select location, can be set to FALSE to remove.
#' @param staplewidth The relative width of staples to the width of the box.Defaults to 0.9.
#'   Staples mark the ends of the whiskers with a line.
#' @param qs Can be used to change the quantiles displayed in the plot. Defaults to `c(.05,.25,.50,.75,.95)`
#' @param outlier.colour,outlier.color,outlier.fill,outlier.shape,outlier.size,outlier.stroke,outlier.alpha
#'   Default aesthetics for outliers. Set to `NULL` to inherit from the
#'   data's aesthetics.
#' @param count.colour,count.color,count.size
#'   Default aesthetics for outliers. Set to `NULL` to inherit from the
#'   data's aesthetics.
#' @param count.label Whether to display the string "n=" before the count. Defaults to `FALSE`.
#' @param midpoint.colour,midpoint.color,midpoint.fill,midpoint.shape,midpoint.size,midpoint.stroke,midpoint.alpha
#'   Default aesthetics for middlepoint. Set to `NULL` to use plot defaults.
#' @param whisker.colour,whisker.color,whisker.linetype,whisker.linewidth
#'   Default aesthetics for the whiskers. Set to `NULL` to inherit from the
#'   data's aesthetics.
#' @param median.colour,median.color,median.linetype,median.linewidth
#'   Default aesthetics for the median line. Set to `NULL` to inherit from the
#'   data's aesthetics. **** Not yet implemented, waiting on next ggplot2 release.
#' @param staple.colour,staple.color,staple.linetype,staple.linewidth
#'   Default aesthetics for the staples. Set to `NULL` to inherit from the
#'   data's aesthetics. Note that staples don't appear unless the `staplewidth`
#'   argument is set to a non-zero size.
#' @param box.colour,box.color,box.linetype,box.linewidth
#'   Default aesthetics for the boxes. Set to `NULL` to inherit from the
#'   data's aesthetics.**** Not yet implemented, waiting on next ggplot2 release.
#' @param notch If `FALSE` (default) make a standard box plot. If
#'   `TRUE`, make a notched box plot. Notches are used to compare groups;
#'   if the notches of two boxes do not overlap, this suggests that the medians
#'   are significantly different.
#' @param notchwidth For a notched box plot, width of the notch relative to
#'   the body (defaults to `notchwidth = 0.5`).
#' @param varwidth If `FALSE` (default) make a standard box plot. If
#'   `TRUE`, boxes are drawn with widths proportional to the
#'   square-roots of the number of observations in the groups (possibly
#'   weighted, using the `weight` aesthetic).
#'
#' @export
#'
#' @import ggplot2
geom_boxandwhisker <- function(
    mapping = NULL, data = NULL, stat = "boxandwhisker", position = "dodge2",
    quantiles = c(.05, .25, .5, .75, .95), count = TRUE,
    middlepoint = "mean", whiskerbar = FALSE,
    outliers = TRUE, ...,
    outlier.colour = NULL, outlier.color = NULL, outlier.fill = NULL, outlier.shape = NULL,
    outlier.size = NULL, outlier.stroke = 0.5, outlier.alpha = NULL,
    whisker.colour = NULL, whisker.color = NULL, whisker.linetype = NULL, whisker.linewidth = NULL,
    staple.colour = NULL, staple.color = NULL, staple.linetype = NULL, staple.linewidth = NULL,
    median.colour = NULL, median.color = NULL, median.linetype = NULL, median.linewidth = NULL,
    box.colour = NULL, box.color = NULL, box.linetype = NULL, box.linewidth = NULL,
    count.colour = NULL, count.color = NULL, count.size = NULL, count.label = FALSE,
    midpoint.colour = NULL, midpoint.color = NULL, midpoint.fill = NULL,
    midpoint.shape = NULL, midpoint.size = NULL, midpoint.stroke = NULL, midpoint.alpha = NULL,
    notch = FALSE,
    notchwidth = 0.5,
    staplewidth = 0.9,
    varwidth = FALSE,
    na.rm = FALSE, orientation = NA, show.legend = NA, inherit.aes = TRUE) {
  if (is.character(position)) {
    if (varwidth == TRUE) {
      position <- position_dodge2(preserve = "single")
    }
  } else {
    if (identical(position$preserve, "total") & varwidth ==
        TRUE) {
      cli::cli_warn("Can't preserve total widths when {.code varwidth = TRUE}.")
      position$preserve <- "single"
    }
  }

  outlier_gp <- list(
    colour = outlier.color %||% outlier.colour,
    fill   = outlier.fill,
    shape  = outlier.shape,
    size   = outlier.size,
    stroke = outlier.stroke,
    alpha  = outlier.alpha
  )

  midpoint_gp <- list(
    colour = midpoint.color %||% midpoint.colour,
    fill   = midpoint.fill,
    shape  = midpoint.shape,
    size   = midpoint.size,
    stroke = midpoint.stroke,
    alpha  = midpoint.alpha
  )

  whisker_gp <- list(
    colour    = whisker.color %||% whisker.colour,
    linetype  = whisker.linetype,
    linewidth = whisker.linewidth
  )

  staple_gp <- list(
    colour    = staple.color %||% staple.colour,
    linetype  = staple.linetype,
    linewidth = staple.linewidth
  )

  median_gp <- list(
    colour    = median.color %||% median.colour,
    linetype  = median.linetype,
    linewidth = median.linewidth
  )

  box_gp <- list(
    colour    = box.color %||% box.colour,
    linetype  = box.linetype,
    linewidth = box.linewidth
  )

  count_gp <- list(
    colour = count.color %||% count.colour,
    size = count.size,
    countlab = count.label
  )

  rlang:::check_number_decimal(staplewidth)
  rlang:::check_bool(outliers)
  layer(
    data = data, mapping = mapping, stat = stat, geom = GeomBoxandwhisker,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = rlang::list2(
      outliers = outliers,
      count = count, middlepoint = middlepoint,
      outlier_gp = outlier_gp, midpoint_gp = midpoint_gp, whisker_gp = whisker_gp,
      staple_gp = staple_gp, median_gp = median_gp,
      count_gp = count_gp,
      box_gp = box_gp,
      notch = notch,
      notchwidth = notchwidth,
      staplewidth = staplewidth,
      varwidth = varwidth,
      na.rm = na.rm,
      orientation = orientation,
      ...
    )
  )
}

# Underlying Geom ----

#' @format NULL
#' @usage NULL
#' @export
GeomBoxandwhisker <- ggproto(
  "GeomBoxandwhisker", GeomBoxplot,
  draw_group = function(self, data, panel_params, coord, lineend = "butt",
                        count = TRUE, middlepoint = "mean", fontsize = 9,
                        linejoin = "mitre", fatten = 2, outlier_gp = NULL, midpoint_gp = NULL,
                        whisker_gp = NULL, staple_gp = NULL, median_gp = NULL,
                        count_gp = NULL,
                        box_gp = NULL, notch = FALSE, notchwidth = 0.5,
                        staplewidth = 0, varwidth = FALSE, flipped_aes = FALSE, ...) {
    # data <- fix_linewidth(data, snake_class(self))
    data <- flip_data(data, flipped_aes)

    # this may occur when using geom_boxplot(stat = "identity")
    if (nrow(data) != 1) {
      cli::cli_abort(c(
        "Can only draw one boxplot per group.",
        "i" = "Did you forget {.code aes(group = ...)}?"
      ))
    }

    common <- list(fill = fill_alpha(data$fill, data$alpha), group = data$group)

    whiskers <- data_frame0(
      x = c(data$x, data$x),
      xend = c(data$x, data$x),
      y = c(data$upper, data$lower),
      yend = c(data$ymax, data$ymin),
      colour = rep(whisker_gp$colour %||% data$colour, 2),
      linetype = rep(whisker_gp$linetype %||% data$linetype, 2),
      linewidth = rep(whisker_gp$linewidth %||% data$linewidth, 2),
      alpha = c(NA_real_, NA_real_),
      !!!common,
      .size = 2
    )
    whiskers <- flip_data(whiskers, flipped_aes)

    box <- transform(
      data,
      y = middle,
      ymax = upper,
      ymin = lower,
      ynotchlower = ifelse(notch, notchlower, NA),
      ynotchupper = ifelse(notch, notchupper, NA),
      notchwidth = notchwidth
    )
    box <- flip_data(box, flipped_aes)

    if (!is.null(data$outliers) && length(data$outliers[[1]]) >= 1) {
      outliers <- data_frame0(
        y = data$outliers[[1]],
        x = data$x[1],
        colour = outlier_gp$colour %||% data$colour[1],
        fill = outlier_gp$fill %||% data$fill[1],
        shape = outlier_gp$shape %||% data$shape[1] %||% 19,
        size = outlier_gp$size %||% data$size[1] %||% 1.5,
        stroke = outlier_gp$stroke %||% data$stroke[1] %||% 0.5,
        alpha = outlier_gp$alpha %||% data$alpha[1],
        .size = length(data$outliers[[1]])
      )
      outliers <- flip_data(outliers, flipped_aes)

      outliers_grob <- GeomPoint$draw_panel(outliers, panel_params, coord)
    } else {
      outliers_grob <- NULL
    }


    if (middlepoint %in% c("mean", "90th")) {
      midpoint <- data_frame0(
        y = ifelse(middlepoint == "90th", data$p90, data$mean),
        x = data$x,
        colour = midpoint_gp$colour %||% "black",
        fill = midpoint_gp$fill %||% "black",
        shape = midpoint_gp$shape %||% 8,
        size = midpoint_gp$size %||% 1.5,
        stroke = midpoint_gp$stroke %||% 0.5,
        alpha = midpoint_gp$alpha %||% 1
      )
      midpoint <- flip_data(midpoint, flipped_aes)

      midpoint_grob <- GeomPoint$draw_panel(midpoint, panel_params, coord)
    } else {
      midpoint_grob <- NULL
    }



    if (staplewidth != 0) {
      staples <- data_frame0(
        x = rep((data$xmin - data$x) * staplewidth + data$x, 2),
        xend = rep((data$xmax - data$x) * staplewidth + data$x, 2),
        y = c(data$ymax, data$ymin),
        yend = c(data$ymax, data$ymin),
        linetype = rep(staple_gp$linetype %||% data$linetype, 2),
        linewidth = rep(staple_gp$linewidth %||% data$linewidth, 2),
        colour = rep(staple_gp$colour %||% data$colour, 2),
        alpha = c(NA_real_, NA_real_),
        !!!common,
        .size = 2
      )
      staples <- flip_data(staples, flipped_aes)
      staple_grob <- GeomSegment$draw_panel(
        staples, panel_params, coord,
        lineend = lineend
      )
    } else {
      staple_grob <- NULL
    }

    if (count) {
      count <- data_frame0(
        x = data$x,
        y = ifelse(!is.null(data$outliers), data$allmin, data$ymin),
        label = ifelse(count_gp$countlab, paste0("n=", data$count), paste0(data$count)),
        colour = count_gp$colour %||% "black",
        size = count_gp$size %||% fontsize,
        alpha = 1,
        family = data$family,
        fontface = data$fontface,
        lineheight = data$lineheight,
        hjust = ifelse(flipped_aes, 1.5, .5),
        vjust = ifelse(flipped_aes, 0.5, 1.5),
        angle = 0
      )

      count <- flip_data(count, flipped_aes)

      count_grob <- GeomText$draw_panel(
        count, panel_params, coord, size.unit = "pt"
      )

    } else {
      count_grob <- NULL
    }

    ggname("geom_boxplot", grid::grobTree(
      outliers_grob,
      staple_grob,
      GeomSegment$draw_panel(whiskers, panel_params, coord, lineend = lineend),
      GeomCrossbar$draw_panel(
        box,
        fatten = fatten,
        panel_params,
        coord,
        lineend = lineend,
        linejoin = linejoin,
        flipped_aes = flipped_aes
        # These are in the dev version of ggplot2. Can re-add later.
        # middle_gp = median_gp,
        # box_gp = box_gp
      ),
      count_grob,
      midpoint_grob
    ))
  },
  default_aes = aes(weight = 1, colour = "grey20", fill = "white", size = NULL,
                    alpha = .8, shape = 19, linetype = "solid", linewidth = 0.5,
                    family = "", fontface = "plain", lineheight = 1.0
  ),
)
