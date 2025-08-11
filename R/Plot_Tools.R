# This script contains code for plot themes and the BC color palette

########################################################################################################################*

# BC COLOR LIST ----
#'
#' @export
#'
# These are all the colors from the brand guide.
bc_colors <- c(purple = "#332a86",
               blue = "#009ed1",
               teal = "#27bdbe",
               green = "#76b043",
               cool_dark_gray = "#43525a",
               warm_gray = "#8f9e96",
               cool_gray = "#c0cac7",
               navy = "#0a3049",
               dark_blue = "#2e83b7",
               cyan = "#6dcff6",
               light_blue = "#c7eafb",
               dark_green = "#478a57",
               light_green = "#bed73b",
               warm_light_gray = "#f1f2f2",
               light_yellow = "#fffcd5",
               bright_red = "#ef4123",
               red = "#b84626",
               orange = "#f58220",
               yellow = "#ffc233",
               brown = "#472a14",
               light_brown = "#b38f6b",
               charcoal = "#444d3e",
               # These are other tints of some colors for use in plots. (To find tints: https://www.colorhexa.com/)
               light_purple = "#aba5e3",
               lighter_green = "#A8D085",
               light_red = "#de7d62")

########################################################################################################################*

# Custom color palette code from: https://www.r-bloggers.com/2018/02/creating-corporate-colour-palettes-for-ggplot2/
# Function to pull the hex codes from the list for use below
extract_hex <- function(...) {
  cols <- c(...)
  if (is.null(cols))
    return (bc_colors)
  bc_colors[cols]
}

########################################################################################################################*

# PALETTE SELECTION ----
# Additional color palettes can be added here using a combo of the color names above. Try to ensure all palettes are
# color blind friendly using this website:
# https://projects.susielu.com/viz-palette?colors=[%22#332a86%22,%22#ffc233%22]&backgroundColor=%22white%22&fontColor=%22black%22&mode=%22normal%22
bc_color_palettes <- list(primary = extract_hex("purple", "orange", "dark_green", "teal",
                                                        "light_purple", "yellow", "lighter_green", "dark_blue",
                                                        "light_red", "cool_gray"),
                          fourcolor = extract_hex("purple", "orange", "dark_green", "teal"),
                          rainbow = extract_hex("bright_red", "orange", "yellow", "dark_green", "teal", "purple"),
                          bigrainbow = extract_hex("light_red", "bright_red", "orange", "yellow", "lighter_green",
                                                   "dark_green", "teal", "dark_blue", "light_purple", "purple", "cool_gray"))

########################################################################################################################*

# Use code from stack overflow to make a color palette that can interpolate additional colors but maintain the order of
# the color palette: https://stackoverflow.com/questions/61674217/custom-discrete-color-scale-in-ggplot-does-not-respect-order
# See answer from linog (https://stackoverflow.com/users/9197726/linog)
# Need to add ",drop=FALSE" to the line "if (n<nrow(colors)) colors <- colors[1:n,]" for it to work for one color

colorRamp_d <- function (colors, n,
                         bias = 1,
                         space = c("rgb", "Lab"),
                         interpolate = c("linear",
                                         "spline"),
                         alpha = FALSE){

  # PRELIMINARY STEPS
  if (bias <= 0)
    stop("'bias' must be positive")
  if (!missing(space) && alpha)
    stop("'alpha' must be false if 'space' is specified")
  colors <- t(col2rgb(colors, alpha = alpha)/255)
  space <- match.arg(space)
  interpolate <- match.arg(interpolate)

  # CUT THE COLOR VECTOR

  if (space == "Lab")
    colors <- convertColor(colors, from = "sRGB", to = "Lab")
  interpolate <- switch(interpolate, linear = stats::approxfun,
                        spline = stats::splinefun)

  # RESPECT ORDER IF NCLASSES<NCOLORS
  if (n<nrow(colors)) colors <- colors[1:n,,drop=FALSE]

  if ((nc <- nrow(colors)) == 1L) {
    colors <- colors[c(1L, 1L), ]
    nc <- 2L
  }
  x <- seq.int(0, 1, length.out = nc)^bias
  palette <- c(interpolate(x, colors[, 1L]), interpolate(x,
                                                         colors[, 2L]), interpolate(x, colors[, 3L]), if (alpha) interpolate(x,
                                                                                                                             colors[, 4L]))
  roundcolor <- function(rgb) pmax(pmin(rgb, 1), 0)
  if (space == "Lab")
    function(x) roundcolor(convertColor(cbind(palette[[1L]](x),
                                              palette[[2L]](x), palette[[3L]](x), if (alpha)
                                                palette[[4L]](x)), from = "Lab", to = "sRGB")) *
    255
  else function(x) roundcolor(cbind(palette[[1L]](x), palette[[2L]](x),
                                    palette[[3L]](x), if (alpha)
                                      palette[[4L]](x))) * 255
}


colorRampPalette_d <- function (colors, ...){
  # n: number of classes
  function(n) {
    ramp <- colorRamp_d(colors, n, ...)
    x <- ramp(seq.int(0, 1, length.out = n))
    if (ncol(x) == 4L)
      rgb(x[, 1L], x[, 2L], x[, 3L], x[, 4L], maxColorValue = 255)
    else rgb(x[, 1L], x[, 2L], x[, 3L], maxColorValue = 255)
  }
}

# continued from the Rbloggers link, but use the new function to fix the color ramp

bc_pal <- function(palette = "primary", reverse = FALSE, ...) {
  pal <- bc_color_palettes[[palette]]
  if (reverse) pal <- rev(pal)
  colorRampPalette_d(pal, ...)
}

########################################################################################################################*
# GGPLOT COLOR FUNCTIONS ----
#' BC Color Palette with scale_color
#'
#' @param palette Name of color palette to use. Current options: "primary", "rainbow"
#' @param discrete Set equal to false if gradient is desired
#' @param reverse Set equal to TRUE if you want the colors in the opposite order
#'
#' @export
#'
scale_color_bc <- function(palette = "primary", discrete = TRUE, reverse = FALSE, ...) {
  pal <- bc_pal(palette = palette, reverse = reverse)
  if (discrete) {
    # the paste0 arguement is for error messages
    discrete_scale("colour", paste0("bc_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

########################################################################################################################*
#' BC Color Palette with scale_fill
#'
#' @param palette Name of color palette to use. Current options: "primary", "rainbow"
#' @param discrete Set equal to false if gradient is desired
#' @param reverse Set equal to TRUE if you want the colors in the opposite order
#'
#' @export
#'
scale_fill_bc <- function(palette = "primary", discrete = TRUE, reverse = FALSE, ...) {
  pal <- bc_pal(palette = palette, reverse = reverse)
  if (discrete) {
    discrete_scale("fill", paste0("bc_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}


########################################################################################################################*
########################################################################################################################*
########################################################################################################################*

# TO DO: Underlying Geom ----

data_frame0 <- function(...) data_frame(..., .name_repair = "minimal")

#' @format NULL
#' @usage NULL
#' @export
GeomBoxplotq <- ggproto(
  "GeomBoxplotq", GeomBoxplot,

  draw_group = function(self, data, panel_params, coord, lineend = "butt",
                        linejoin = "mitre", fatten = 2, outlier_gp = NULL,
                        whisker_gp = NULL, staple_gp = NULL, median_gp = NULL,
                        box_gp = NULL, notch = FALSE, notchwidth = 0.5,
                        staplewidth = 0, varwidth = FALSE, flipped_aes = FALSE,
                        middlepoint = "mean", count = TRUE, countlabel = FALSE
                        ) {
    # data <- fix_linewidth(data, snake_class(self))
    data <- flip_data(data, flipped_aes)
    # this may occur when using geom_boxplot(stat = "identity")
    if (nrow(data) != 1) {
      cli::cli_abort(c(
        "Can only draw one boxplot per group.",
        "i"= "Did you forget {.code aes(group = ...)}?"
      ))
    }

    common <- list(fill = fill_alpha(data$fill, data$alpha), group = data$group)

    whiskers <- data_frame0(
      x = c(data$x, data$x),
      xend = c(data$x, data$x),
      y = c(data$upper, data$lower),
      yend = c(data$ymax, data$ymin),
      colour    = rep(whisker_gp$colour    %||% data$colour,    2),
      linetype  = rep(whisker_gp$linetype  %||% data$linetype,  2),
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
        fill   = outlier_gp$fill   %||% data$fill[1],
        shape  = outlier_gp$shape  %||% data$shape[1]  %||% 19,
        size   = outlier_gp$size   %||% data$size[1]   %||% 1.5,
        stroke = outlier_gp$stroke %||% data$stroke[1] %||% 0.5,
        fill = NA,
        alpha = outlier_gp$alpha %||% data$alpha[1],
        .size = length(data$outliers[[1]])
      )
      outliers <- flip_data(outliers, flipped_aes)

      outliers_grob <- GeomPoint$draw_panel(outliers, panel_params, coord)
    } else {
      outliers_grob <- NULL
    }

    if (staplewidth != 0) {
      staples <- data_frame0(
        x    = rep((data$xmin - data$x) * staplewidth + data$x, 2),
        xend = rep((data$xmax - data$x) * staplewidth + data$x, 2),
        y    = c(data$ymax, data$ymin),
        yend = c(data$ymax, data$ymin),
        linetype  = rep(staple_gp$linetype  %||% data$linetype, 2),
        linewidth = rep(staple_gp$linewidth %||% data$linewidth, 2),
        colour    = rep(staple_gp$colour    %||% data$colour, 2),
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

    # if (count){
    #   count <- data_frame0(
    #     x = data$x,
    #     y = ifelse(outliers, data$min, data$ymin),
    #     label = ifelse(countlab, paste0("n=", data$count), paste0(data$count))
    #   )
    #   count <- flip_data(count, flipped_aes)
    #   count_grob <- GeomText$draw_panel(
    #     count, panel_params, coord
    #   )
    #
    # } else {
    #   count_grob <- NULL
    # }


    ggname("geom_boxplotq", grobTree(
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
        flipped_aes = flipped_aes,
        middle_gp = median_gp,
        box_gp = box_gp
      )
    ))
  }
)

# Underlying Stat ----

#' @format NULL
#' @usage NULL
#' @export
StatBoxplotq <- ggproto(
  "StatBoxplotq", StatBoxplot,

  compute_group = function(data, scales, width = NULL, na.rm = FALSE, coef = 1.5,
                           flipped_aes = FALSE, qs = c(.05,.25,.50,.75,.95)) {
    data <- flip_data(data, flipped_aes)

    if (!is.null(data$weight)) {
      mod <- quantreg::rq(y ~ 1, weights = weight, data = data, tau = qs)
      stats <- as.numeric(stats::coef(mod))
    } else {
      stats <- as.numeric(stats::quantile(data$y, qs))
    }
    names(stats) <- c("ymin", "lower", "middle", "upper", "ymax")
    iqr <- diff(stats[c(2, 4)])

    outliers <- data$y < stats[1] | data$y > stats[5]

    if (vctrs::vec_unique_count(data$x) > 1)
      width <- diff(range(data$x)) * 0.9

    df <- data_frame0(!!!as.list(stats))
    df$outliers <- list(data$y[outliers])
    df$count <- length(data$y)
    df$mean <- mean(data$y)
    df$p90 <- as.numeric(stats::quantile(data$y, .9))
    df$allmin <- min(data$y)

    if (is.null(data$weight)) {
      n <- sum(!is.na(data$y))
    } else {
      # Sum up weights for non-NA positions of y and weight
      n <- sum(data$weight[!is.na(data$y) & !is.na(data$weight)])
    }

    df$notchupper <- df$middle + 1.58 * iqr / sqrt(n)
    df$notchlower <- df$middle - 1.58 * iqr / sqrt(n)

    df$x <- if (is.factor(data$x)) data$x[1] else mean(range(data$x))
    df$width <- width
    df$relvarwidth <- sqrt(n)
    df$flipped_aes <- flipped_aes
    flip_data(df, flipped_aes)
  }
)

#' A box and whiskers quantile plot
#'
#' The boxplot compactly displays the distribution of a continuous variable.
#' It visualises five summary statistics (the median, two hinges
#' and two whiskers), and all "outlying" points individually.
#'
#'
#' @section Summary statistics:
#' The lower and upper hinges correspond to the first and third quartiles
#' (the 25th and 75th percentiles). Whiskers extend to the specified quantiles, unlike `ggplot2::geom_boxplot`, which
#' calculates outliers.
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
#' @param outlier.colour,outlier.color,outlier.fill,outlier.shape,outlier.size,outlier.stroke,outlier.alpha
#'   Default aesthetics for outliers. Set to `NULL` to inherit from the
#'   data's aesthetics.
#' @param whisker.colour,whisker.color,whisker.linetype,whisker.linewidth
#'   Default aesthetics for the whiskers. Set to `NULL` to inherit from the
#'   data's aesthetics.
#' @param median.colour,median.color,median.linetype,median.linewidth
#'   Default aesthetics for the median line. Set to `NULL` to inherit from the
#'   data's aesthetics.
#' @param staple.colour,staple.color,staple.linetype,staple.linewidth
#'   Default aesthetics for the staples. Set to `NULL` to inherit from the
#'   data's aesthetics. Note that staples don't appear unless the `staplewidth`
#'   argument is set to a non-zero size.
#' @param box.colour,box.color,box.linetype,box.linewidth
#'   Default aesthetics for the boxes. Set to `NULL` to inherit from the
#'   data's aesthetics.
#' @param notch If `FALSE` (default) make a standard box plot. If
#'   `TRUE`, make a notched box plot. Notches are used to compare groups;
#'   if the notches of two boxes do not overlap, this suggests that the medians
#'   are significantly different.
#' @param notchwidth For a notched box plot, width of the notch relative to
#'   the body (defaults to `notchwidth = 0.5`).
#' @param staplewidth The relative width of staples to the width of the box.
#'   Staples mark the ends of the whiskers with a line.
#' @param varwidth If `FALSE` (default) make a standard box plot. If
#'   `TRUE`, boxes are drawn with widths proportional to the
#'   square-roots of the number of observations in the groups (possibly
#'   weighted, using the `weight` aesthetic).
#'
#' @export
#'
#' @import ggplot2
geom_boxplot_q <- function (mapping = NULL, data = NULL, stat = "boxplotq", position = "dodge2",
                            quantiles = c(.05,.25,.5,.75,.95), count = TRUE,
                            middlepoint = "mean", whiskerbar = FALSE,
                                 ..., outliers = TRUE, outlier.colour = NULL, outlier.color = NULL,
                                 outlier.fill = NULL, outlier.shape = 19, outlier.size = 1.5,
                                 outlier.stroke = 0.5, outlier.alpha = NULL, notch = FALSE,
                                 notchwidth = 0.5, staplewidth = 0.9, varwidth = FALSE, na.rm = FALSE,
                                 orientation = NA, show.legend = NA, inherit.aes = TRUE)
{
  if (is.character(position)) {
    if (varwidth == TRUE)
      position <- position_dodge2(preserve = "single")
  }
  else {
    if (identical(position$preserve, "total") & varwidth ==
        TRUE) {
      cli::cli_warn("Can't preserve total widths when {.code varwidth = TRUE}.")
      position$preserve <- "single"
    }
  }
  check_number_decimal(staplewidth)
  check_bool(outliers)
  layer(data = data, mapping = mapping, stat = stat, geom = GeomBoxplot,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = rlang::list2(outliers = outliers, outlier.colour = outlier.color %||%
                         outlier.colour, outlier.fill = outlier.fill, outlier.shape = outlier.shape,
                       outlier.size = outlier.size, outlier.stroke = outlier.stroke,
                       outlier.alpha = outlier.alpha, notch = notch, notchwidth = notchwidth,
                       staplewidth = staplewidth, varwidth = varwidth,
                       na.rm = na.rm, orientation = orientation, ...))



}

# iris2 <- iris %>%
#   mutate(binpetal = as.factor(round(Petal.Width/.5)*.5))
# ggplot(iris2, aes(x = Species, y = Sepal.Width, fill = binpetal)) +
#   geom_boxplot_q()




# BOX AND WHISKER ----

#' Box and Whisker Plot function that uses 25th and 75th percentile for the box and 5th and 95th percentiles for whiskers.
#' The middle point can be at the mean or 90th percentile.
#' Outlier points can be included or excluded.  To set fill to a single color, include the argument fill =.
#'
#'
#' @param outlier Can be set to FALSE to remove points <5th percentile and >95th percentile.
#' @param count Can be set to FALSE to remove count from below the whisker.
#' @param middlepoint Can be equal to "mean" (default) or "90th" to select location, can be set to FALSE to remove.
#' @param whiskerbar Can be set to FALSE to remove horizontal bars on the ends of the whiskers.
#' @param alpha Can be set to a different transparency or NA if an alpha scale is needed (not recommended).
#' @param width Can be set to a number between 0 and 1, with lower numbers narrowing boxes.
#' @param fontsize Can be used to adjust the size of the count. Use a number equivalent to a standard size in points.
#' @param whiskerloc Can be used to adjust the percentile that the whiskers extend to, use the low value, high will be calculated.
#' @param countlabel Can be set to TRUE if you want the count to appear as n=#
#' @param preserve Can be set to "single" or "total". See position_dodge documentation for the difference.
#' @param padding Sets the spacing between boxes. Works best between 0.1 and 0.5
#'
#' @export
#'
geom_boxandwhisker <- function (outlier = TRUE, count = TRUE, middlepoint = "mean", whiskerbar = TRUE,
                                alpha = .8, width = .9, fontsize = 9, whiskerloc = .05, countlabel = FALSE,
                                preserve = "single", padding = .1,
                                whiskerlabel = FALSE, boxedgelabel = FALSE, medianlabel = FALSE, ...) { # haven't added this functionality yet

  # Box and Whiskers - these functions set up for the stat_summary functions
  lowwhisker <- whiskerloc
  hiwhisker <- 1 - whiskerloc

  # Quantiles for the boxplot function
  boxplot_info <- function(x) {
    r <- quantile(x, probs = c(lowwhisker, 0.25, 0.5, 0.75, hiwhisker))
    names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
    r
  }

  # For the horizontal bars on the ends of the whiskers (errorbar)
  low_bar <- function(x) {
    range <- quantile(x, probs = c(lowwhisker, 0.25))
    names(range) <- c("ymin", "ymax")
    range
  }
  high_bar <- function(x) {
    range <- quantile(x, probs = c(0.75, hiwhisker))
    names(range) <- c("ymin", "ymax")
    range
  }

  # For the outlier points (point)
  outlier_points <- function(x) {
    outliers <- subset(x, x < quantile(x,lowwhisker) | quantile(x,hiwhisker) < x)
    fillpoint <- unname(quantile(x, .5))
    if(length(outliers) == 0){
      data.frame(y = fillpoint, colour = NA)
    } else {
      data.frame(y = outliers, colour = rep("black", length(outliers)))
    }
  }


  # For the count (text). Location changes depending on whether outlier points are included.
  if (outlier == TRUE){
    ncount <- function(x){
      if (countlabel == TRUE) {
        return(data.frame(y = min(x), label = paste0("n=",length(x))))
      } else {
        return(data.frame(y = min(x), label = length(x)))
      }
    }
  } else if (outlier == FALSE){
    ncount <- function(x){
      if (countlabel == TRUE) {
        return(data.frame(y = as.numeric(quantile(x, lowwhisker)), label = paste0("n=",length(x))))
      } else {
        return(data.frame(y = as.numeric(quantile(x, lowwhisker)), label = length(x)))
      }
    }
  }




  # Returns a list of stat_summary for the full plot
  list(
    # This makes the actual box and whisker
    if(!is.na(alpha))
      ggplot2::stat_summary(fun.data = boxplot_info, geom = "boxplot",
                            position = position_dodge2(width = width,preserve=preserve, padding = padding),
                            alpha = alpha, width = width, ...),
    if(is.na(alpha))
      ggplot2::stat_summary(fun.data = boxplot_info, geom = "boxplot",
                            position = position_dodge2(width = width,preserve=preserve, padding = padding),
                            width = width, ...),

    # This adds the outlier points
    if (outlier)
      ggplot2::stat_summary(fun.data = outlier_points, geom= "point", position = position_dodge(width), ...),

    # This adds the count (vjust spaces it away from the plot)
    if (count)
      ggplot2::stat_summary(fun.data = ncount, geom = "text", vjust = 1.5, position = position_dodge2(width = width, preserve=preserve),
                            size = fontsize / ggplot2::.pt, ...),

    # This makes the point at the mean or 90th percentile
    if (middlepoint == "mean")
      ggplot2::stat_summary(fun = mean, geom = "point", shape = 8, colour = "black",
                            position = position_dodge2(width = width, preserve=preserve), ...),
    if (middlepoint == "90th")
      ggplot2::stat_summary(fun = quantile, fun.args = list(probs = 0.9),  geom = "point", shape = 8, colour = "black",
                            position = position_dodge2(width = width, preserve=preserve), ...),

    # This adds the horizontal bars on the whiskers
    if (whiskerbar)
      ggplot2::stat_summary(fun.data = low_bar, geom = "errorbar",
                            position = position_dodge2(width = width, preserve=preserve, padding = padding+.2),
                            width = width),
    if (whiskerbar)
      ggplot2::stat_summary(fun.data = high_bar, geom = "errorbar",
                            position = position_dodge2(width = width, preserve=preserve, padding = padding+.2),
                            width = width)
  )

}

########################################################################################################################*
########################################################################################################################*
########################################################################################################################*

#' Theme function for BC defaults
#' Automatically sets BC color palette.
#'
#' @param base_size Can be set to adjust default font sizes
#' @param base_family Can be set to change default font (R isn't great at fonts, so be careful)
#'
#' @export
#'
theme_bc <- function (base_size = 12, base_family = "", ...) {
  theme_bw(base_size = base_size, base_family = base_family, ...) %+replace%
    theme(plot.background = element_rect(fill = "transparent", color = NA),
          panel.border = element_rect(fill = NA, colour = "#43525a"),
          panel.grid = element_line(colour = "#f1f2f2"),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = "#332a86", colour = "#43525a"),
          strip.text = element_text(colour = "white", size = rel(0.9),
                       margin = margin(0.8 * base_size/2, 0.8 * base_size/2, 0.8 * base_size/2, 0.8 * base_size/2)),
          strip.text.y = element_text(angle = 90),
          legend.key = element_rect(fill = "transparent", colour = NA),
          legend.background = element_rect(fill = "transparent", color = NA),
          legend.box.background = element_rect(fill = "transparent", color = NA),
          axis.title.y.right = element_text(angle = 90, margin = margin(l = base_size/4), vjust = 0, size = base_size),
          axis.text = element_text(size = rel(0.9)))
}


# Test code
# iris <- iris %>%
#   mutate(binpetal = as.factor(round(Petal.Width/.5)*.5))
# ggplot(iris, aes(x = Species, y = Sepal.Width, fill = binpetal)) +
#   geom_boxandwhisker(width = .5) +
#   theme_bc() +
#   scale_fill_bc()
#
# ggplot(iris, aes(x = Sepal.Width, y = Petal.Width, color = Petal.Length)) +
#   geom_point() +
#   facet_wrap(~Species) +
#   theme_bc() +
#   scale_color_bc(palette = "rainbow", discrete = FALSE)

########################################################################################################################*

# BOX AND WHISKER LEGEND ----
#' Box and Whisker Plot Legend for geom_boxandwhisker
#' Accepts the same arguments as geom_boxandwhisker with some additional for fixing legend appearance
#' You will have to manually add this to your B+W plots using cowplot or gridextra functions
#'
#'
#' @param fill Default setting is BC purple, can specify other hex codes or ggplot colors.
#' @param widthscale Legend spacing is hard, you can mess with this (approx range 0.5-1.5) to try to fit things better.
#' @param outlier Can be set to FALSE to remove points <5th percentile and >95th percentile.
#' @param count Can be set to FALSE to remove count from below the whisker.
#' @param middlepoint Can be equal to "mean" (default) or "90th" to select location, can be set to FALSE to remove.
#' @param whiskerbar Can be set to FALSE to remove horizontal bars on the ends of the whiskers.
#' @param alpha Can be set to a different transparency or NA if an alpha scale is needed (not recommended).
#' @param width Can be set to a number between 0 and 1, with lower numbers increasing space between boxes.
#' @param fontsize Can be used to adjust the size of the count. Use a number equivalent to a standard size in points.
#' @param whiskerloc Can be used to adjust the percentile that the whiskers extend to, use the low value, high will be calculated.
#' @param countlabel Can be set to TRUE if you want the count to appear as n=#
#'
#' @examples
#' legend_only <- boxwhisker_legend()
#' legend_plot <- ggdraw(your_plot) + draw_plot(legend_only, x = .65, y = .6, width = .35, height = .4)
#'
#' @export
#'
boxwhisker_legend <- function(fill = "#332a86", widthscale = 1, meaninside = TRUE,
                              outlier = TRUE, count = TRUE, middlepoint = "mean", whiskerbar = TRUE,
                              alpha = .8, fontsize = 9, whiskerloc = .05, countlabel = FALSE, ...) {
  require(magrittr)

  # xlocations
  nearx = ifelse(meaninside, 1.1, 1.5)
  midx = 1.2
  farx = 1.5
  adjustx <- ifelse(whiskerbar, farx, midx)

  # Whisker locations
  whiskerlow <- whiskerloc
  whiskerhigh <- 1- whiskerloc

  # Legend data
  set.seed(307)
  legend_data <- data.frame(yvalues = c(rnorm(48, mean = 50, sd = 10), 50,50,50,50,50,50,20,21.5,24,30,30,30))

  legend_labels <- legend_data %>%
    dplyr::summarise(P05 = quantile(yvalues, whiskerlow),
                     P25 = quantile(yvalues, .25),
                     P50 = quantile(yvalues, .5),
                     P75 = quantile(yvalues, .75),
                     P95 = quantile(yvalues, whiskerhigh),
                     low = quantile(yvalues, whiskerlow) - 5,
                     high = quantile(yvalues, whiskerhigh) + 10,
                     count = min(yvalues),
                     mean = mean(yvalues),
                     P90 = quantile(yvalues, .9)) %>%
    tidyr::pivot_longer(c(P05, P25, P50, P75, P95, low, high, count, mean, P90), names_to = "ID", values_to = "yloc") %>%
    dplyr::mutate(Label = dplyr::case_when(ID == "P05" ~ paste0(whiskerlow*100, "th percentile"),
                                           ID == "P25" ~ "25th percentile",
                                           ID == "P50" ~ "Median",
                                           ID == "P75" ~ "75th percentile",
                                           ID == "P95" ~ paste0(whiskerhigh*100, "th percentile"),
                                           ID == "low" ~ paste0("<", whiskerlow*100, "th percentile"),
                                           ID == "high" ~ paste0(">", whiskerhigh*100, "th percentile"),
                                           ID == "mean" ~ "Mean",
                                           ID == "P90" ~ "90th percentile",
                                           ID == "count" ~ "Number of values"),
                  xloc = dplyr::case_when(ID == "P25" | ID == "P50" | ID == "P75" ~ farx,
                                          ID == "low" | ID == "high" | ID == "count" | ID == "P90" ~ midx,
                                          ID == "mean" ~ nearx,
                                          ID == "P05" | ID == "P95" ~ adjustx))

  count_lab <-  dplyr::filter(legend_labels, ID == "count")

  legend_labels <- dplyr::filter(legend_labels, ID != "count")

  if(!outlier) {
    legend_labels <- dplyr::filter(legend_labels, ID != "low" & ID != "high")
    count_lab$yloc <- legend_labels$yloc[legend_labels$ID == "P05"]
  }

  if(middlepoint == "mean") {
    legend_labels <- dplyr::filter(legend_labels, ID != "P90")
  } else if (middlepoint == "90th") {
    legend_labels <- dplyr::filter(legend_labels, ID != "mean")
  } else {
    legend_labels <- dplyr::filter(legend_labels, ID != "P90" & ID != "mean")
  }

  ggplot2::ggplot(legend_data, ggplot2::aes(x = 1, y = yvalues)) +
    geom_boxandwhisker(outlier = outlier, count = count, middlepoint = middlepoint, whiskerbar = whiskerbar,
                       alpha = alpha, fontsize = fontsize, whiskerloc = whiskerloc, countlabel = countlabel,
                       whiskerlabel = FALSE, boxedgelabel = FALSE, medianlabel = FALSE, fill = fill, ...) +
    ggplot2::geom_text(data = legend_labels, ggplot2::aes(x = xloc, y = yloc, label = Label), size = fontsize/ggplot2::.pt, vjust = .5, hjust = 0) +
    theme_bc() +
    ggplot2::theme(axis.text = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank()) +
    ggplot2::coord_cartesian(xlim = c(.5,2.5 / widthscale)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expand_scale(mult = c(.12,.05))) +
    if(count & !countlabel)
      ggplot2::geom_text(data = count_lab, ggplot2::aes(x = xloc, y = yloc, label = Label), size = fontsize/ggplot2::.pt, vjust = 1.5, hjust = 0)


}

# boxwhisker_legend(widthscale = .5)

########################################################################################################################*
########################################################################################################################*
########################################################################################################################*
#' Creates a png file of plots with automatic scaling for adding to word documents
#'
#' @param plot Plot to be exported
#' @param filename Desired file name in quotes without file extension.
#' @param vscale Default height is 4", based on typical plot ratios to copy into a word doc.  Increase scale to multiply size.
#' @param hwidth Default width is 6.5", only change if plot is being added to a different page width. Specify in inches.
#'
#' @export
#'
export_plot <- function(plot, filename = "DRAFT R Plot", vscale = 1, hwidth = 6.5){

  ggplot2::ggsave(plot = plot, filename = paste(filename, ".png", sep = ""),
                  width = hwidth, height = 4*vscale, units = "in", dpi = 300, bg = "transparent")

}

