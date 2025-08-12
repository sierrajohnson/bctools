#' @format NULL
#' @usage NULL
#' @export
StatBoxandwhisker <- ggproto(
  "StatBoxandwhisker", StatBoxplot,
  compute_group = function(data, scales, width = NULL, na.rm = FALSE, coef = 1.5,
                           flipped_aes = FALSE, qs = c(.05, .25, .50, .75, .95)) {
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

    if (vctrs::vec_unique_count(data$x) > 1) {
      width <- diff(range(data$x)) * 0.9
    }

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
