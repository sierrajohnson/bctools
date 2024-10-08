% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Plot_Tools.R
\name{boxwhisker_legend}
\alias{boxwhisker_legend}
\title{Box and Whisker Plot Legend for geom_boxandwhisker
Accepts the same arguments as geom_boxandwhisker with some additional for fixing legend appearance
You will have to manually add this to your B+W plots using cowplot or gridextra functions}
\usage{
boxwhisker_legend(
  fill = "#332a86",
  widthscale = 1,
  meaninside = TRUE,
  outlier = TRUE,
  count = TRUE,
  middlepoint = "mean",
  whiskerbar = TRUE,
  alpha = 0.8,
  fontsize = 9,
  whiskerloc = 0.05,
  countlabel = FALSE,
  ...
)
}
\arguments{
\item{fill}{Default setting is BC purple, can specify other hex codes or ggplot colors.}

\item{widthscale}{Legend spacing is hard, you can mess with this (approx range 0.5-1.5) to try to fit things better.}

\item{outlier}{Can be set to FALSE to remove points <5th percentile and >95th percentile.}

\item{count}{Can be set to FALSE to remove count from below the whisker.}

\item{middlepoint}{Can be equal to "mean" (default) or "90th" to select location, can be set to FALSE to remove.}

\item{whiskerbar}{Can be set to FALSE to remove horizontal bars on the ends of the whiskers.}

\item{alpha}{Can be set to a different transparency or NA if an alpha scale is needed (not recommended).}

\item{fontsize}{Can be used to adjust the size of the count. Use a number equivalent to a standard size in points.}

\item{whiskerloc}{Can be used to adjust the percentile that the whiskers extend to, use the low value, high will be calculated.}

\item{countlabel}{Can be set to TRUE if you want the count to appear as n=#}

\item{width}{Can be set to a number between 0 and 1, with lower numbers increasing space between boxes.}
}
\description{
Box and Whisker Plot Legend for geom_boxandwhisker
Accepts the same arguments as geom_boxandwhisker with some additional for fixing legend appearance
You will have to manually add this to your B+W plots using cowplot or gridextra functions
}
\examples{
legend_only <- boxwhisker_legend()
legend_plot <- ggdraw(your_plot) + draw_plot(legend_only, x = .65, y = .6, width = .35, height = .4)

}
