#' @importFrom ggplot2 geom_smooth theme_void geom_ribbon
#' @importFrom rlang .data

#' @title Create a scatter plot with density
#'
#' @param x x variable.
#' @param y y variable.
#' @param group optional grouping variable.
#' @param xName name on x-axis.
#' @param yName name on y-axis.
#' @param addSmooth should a smoothed regression line be drawn?
#' @param addSmoothCI should a confidence interval be added to the smoothed regression line?
#' @param smoothCIValue a numeric in (0, 1) indicating the confidence interval.
#' @param forceLinearSmooth should the regression line be linear?
#' @param plotAbove type of plot above the scatter plot.
#' @param plotRight type of plot right of the scatter plot.
#' @param colorAreaUnderDensity Logical, should the area under the density be colored?
#' @param alphaAreaUnderDensity Real in [0, 1], transparancy for area under density.
#' @param showLegend Should the legend be shown?
#' @param legendTitle A string for the title of the legend. \code{NULL} implies the legend is not shown.
#' @param emulateGgMarginal Should the result be as similar as possible to \code{\link[ggExtra]{ggMarginal}}? Overwrites other parmeters.
#' @param ... passed to \code{\link{themeJaspRaw}}.
#'
#' @details The only change added when \code{emulateGgMarginal = TRUE} is that \code{ggplot2::theme(plot.margin = unit(c(0, 0, 0.25, 0.25), "cm"))}
#' is added to the main plot
#'
#' @example inst/examples/ex-JASPScatterPlot.R
#'
#' @export
JASPScatterPlot <- function(x, y, group = NULL, xName = NULL, yName = NULL,
                            addSmooth = TRUE, addSmoothCI = TRUE,
                            smoothCIValue = 0.95, forceLinearSmooth = FALSE,
                            plotAbove = c("density", "histogram", "none"), 
                            plotRight = c("density", "histogram", "none"),
                            colorAreaUnderDensity = TRUE,
                            alphaAreaUnderDensity = .5,
                            showLegend = !is.null(group),
                            legendTitle = NULL,
                            emulateGgMarginal = FALSE,
                            ...) {
  
  # TODO: make actual error messages
  stopifnot(
    is.numeric(x),
    is.numeric(y),
    is.null(group) || is.numeric(group)   || is.factor(group),
    is.null(xName) || is.character(xName) || is.expression(xName),
    is.null(yName) || is.character(yName) || is.expression(yName),
    is.logical(addSmooth),
    is.logical(emulateGgMarginal),
    is.logical(showLegend),
    length(x) == length(y) && (is.null(group) || length(x) == length(group))
  )
  plotAbove <- match.arg(plotAbove)
  plotRight <- match.arg(plotRight)

  # can't make a legend without group
  showLegend <- showLegend && !is.null(group)
  
  if (emulateGgMarginal)
    colorAreaUnderDensity <- FALSE

  if (is.null(group)) {
    df <- data.frame(x = x, y = y)
    mapping <- aes(x = .data$x, y = .data$y)
  } else {
    group <- factor(group)
    df <- data.frame(x = x, y = y, g = group)
    mapping <- aes(x = x, y = y, group = .data$g, color = .data$g, fill = .data$g)
  }

  geomSmooth <- if (addSmooth)
    geom_smooth(formula = y ~ x, se = addSmoothCI, level = smoothCIValue,
                method = if (forceLinearSmooth) "lm" else "loess")
  else NULL

  dots <- list(...)
  if (showLegend)
    dots <- setDefaults(dots, legend.position = "right")

  mainPlot <- ggplot(df, mapping) + 
    geom_point() +
    geomSmooth + 
    ggplot2::labs(x = xName, y = yName, color = legendTitle, fill = legendTitle) +
    geom_rangeframe() +
    do.call(themeJaspRaw, dots)

  if (!is.null(group))
    mainPlot <- mainPlot + scale_JASPcolor_discrete() + scale_JASPfill_discrete()

  if (emulateGgMarginal)
    mainPlot <- mainPlot + ggplot2::theme(plot.margin = unit(c(0, 0, 0.25, 0.25), "cm"))

  gb <- ggplot2::ggplot_build(mainPlot)
  scales <- gb$layout$get_scales(1L)
  x.range <- scales$x$get_limits()
  y.range <- scales$y$get_limits()

  topPlot   <- JASPScatterSubPlot(x, group, plotAbove, x.range, colorAreaUnderDensity, alphaAreaUnderDensity)
  rightPlot <- JASPScatterSubPlot(y, group, plotRight, y.range, colorAreaUnderDensity, alphaAreaUnderDensity, flip = TRUE)
  
  plotList <- list(mainPlot = mainPlot, topPlot = topPlot, rightPlot = rightPlot)
  plotList <- plotList[lengths(plotList) > 0L]
  
  plot <- JASPgraphsPlot$new(
    subplots     = plotList,
    plotFunction = reDrawAlignedPlot,
    size         = 5,
    showLegend   = showLegend
  )
  return(plot)
}

JASPScatterSubPlot <- function(x, group = NULL, type = c("density", "histogram", "none"), range, 
                               colorAreaUnderDensity = TRUE, alpha = 0.5, flip = FALSE) {
  
  if (type == "none")
    return()

  groupIsNull <- is.null(group) 
  if (groupIsNull) {
    mapping <- aes(x = .data$x, y = .data$y)
    group <- rep(1L, length(x))
  } else {
    mapping <- aes(x = .data$x, y = .data$y, group = .data$g, color = .data$g, fill = .data$g)
  }
  
  if (type == "density") {
    foo <- function(x, ...) as.data.frame(stats::density(x, from = range[1L], to = range[2L])[c("x", "y")])
    geom <- geom_line(size = 0.5, show.legend = FALSE)
    geom2 <- if (colorAreaUnderDensity) {
      geom_ribbon(aes(ymin = 0, ymax = .data$y), alpha = alpha)
    } else {
      NULL
    }

    ans <- tapply(x, group, foo, simplify = FALSE)
    df <- do.call(rbind, ans)
    df[["g"]] <- factor(rep(names(ans), vapply(ans, nrow, 1L)))

  } else {
    df <- data.frame(x = x, g = group)
    geom <- ggplot2::geom_histogram(alpha = alpha, position = "identity", show.legend = FALSE)
    geom2 <- NULL
    mapping$y <- ggplot2::quo(.data$..density..)
  }

  plot <- ggplot(df, mapping) +
    geom + geom2 +
    scale_JASPcolor_discrete() + scale_JASPfill_discrete() +
    scale_x_continuous(limits = range, oob = scales::squish) + theme_void()
  if (flip)
    plot <- plot + coord_flip()
  return(plot)
  
}
