#' @importFrom ggplot2 geom_smooth theme_void geom_area

#' @title Create a scatter plot with density
#' 
#' @param x x variable.
#' @param y y variable.
#' @param group optional grouping variable.
#' @param xName name on x-axis.
#' @param yName name on y-axis.
#' @param addSmooth should a smoothed regression line be drawn?
#' @param plotAbove type of plot above the scatter plot.
#' @param plotRight type of plot right of the scatter plot.
#' @param emulateGgMarginal Should the result be as similar as possible to \code{\link[ggExtra]{ggMarginal}}?
#' @param ... 
#'
#' @details The only change added when \code{emulateGgMarginal = TRUE} is that \code{ggplot2::theme(plot.margin = unit(c(0, 0, 0.25, 0.25), "cm"))}
#' is added to the main plot
#'
#' @example inst/examples/ex-JASPScatterPlot.R
#'
#' @export
JASPScatterPlot <- function(x, y, group = NULL, xName = NULL, yName = NULL, addSmooth = TRUE,
                            plotAbove = c("density", "histogram", "none"), 
                            plotRight = c("density", "histogram", "none"),
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
    length(x) == length(y) && (is.null(group) || length(x) == length(group))
  )
  plotAbove <- match.arg(plotAbove)
  plotRight <- match.arg(plotRight)
  
  if (is.null(group)) {
    df <- data.frame(x = x, y = y)
    mapping <- aes(x, y)
  } else {
    group <- factor(group)
    df <- data.frame(x = x, y = y, g = group)
    mapping <- aes(x = x, y = y, group = g, color = g, fill = g)
  }
  
  geomSmooth <- if (addSmooth) geom_smooth(formula = y ~ x, method = "loess") else NULL
  
  mainPlot <- ggplot(df, mapping) + 
    geom_point() + 
    geomSmooth + 
    ggplot2::labs(x = xName, y = yName) +
    geom_rangeframe() +
    themeJaspRaw()
  
  if (emulateGgMarginal)
    mainPlot <- mainPlot + ggplot2::theme(plot.margin = unit(c(0, 0, 0.25, 0.25), "cm"))
  
  gb <- ggplot2::ggplot_build(mainPlot)
  scales <- gb$layout$get_scales(1L)
  x.range <- scales$x$get_limits()
  y.range <- scales$y$get_limits()
  
  topPlot   <- JASPScatterSubPlot(x, group, plotAbove, x.range)
  rightPlot <- JASPScatterSubPlot(y, group, plotRight, y.range, flip = TRUE)
  
  plotList <- list(mainPlot = mainPlot, topPlot = topPlot, rightPlot = rightPlot)
  
  plot <- JASPgraphsPlot$new(
    subplots     = plotList[lengths(plotList) > 0L],
    plotFunction = reDrawAlignedPlot,
    size         = 5
  )
  return(plot)
}

JASPScatterSubPlot <- function(x, group = NULL, type = c("density", "histogram", "none"), range, 
                               densityRange = range, flip = FALSE) {
  
  if (type == "none")
    return()

  groupIsNull <- is.null(group) 
  if (groupIsNull) {
    mapping <- aes(x, y)
    group <- rep(1L, length(x))
  } else {
    mapping <- aes(x = x, y = y, group = g, color = g, fill = g)
  }
  
  if (type == "density") {
    foo <- function(x) as.data.frame(density(x, from = densityRange[1L], to = densityRange[2L])[c("x", "y")])
    geom <- geom_line(size = 0.5, show.legend = FALSE)
    geom2 <- geom_area()
  } else {
    foo <- function(x) {
      h <- hist(x, plot = FALSE)
      return(data.frame(x = h[["mids"]], y = h[["density"]], w = diff(h[["breaks"]])[1L]))
    }
    mapping$width <- ggplot2::quo(w)
    # width <- .5 * diff(range(range))
    geom <- if (groupIsNull) {
      ggplot2::geom_col(position = ggplot2::position_identity(), fill = "transparent", color = "black")
    } else {
      ggplot2::geom_col(position = ggplot2::position_identity(), show.legend = FALSE)
    }
    geom2 <- NULL
  }
  ans <- tapply(x, group, foo)
  df <- do.call(rbind, ans)
  df[["g"]] <- factor(rep(names(ans), vapply(ans, nrow, 1L)))
  
  plot <- ggplot(df, mapping) + geom + geom2 + scale_x_continuous(limits = range, oob = scales::squish) + theme_void()
  if (flip)
    plot <- plot + coord_flip()
  return(plot)
  
}