#' @importFrom ggplot2 is.ggplot

JASPgraphsPlot <- R6::R6Class(
  classname = "JASPgraphsPlot",
  public = list(
    initialize = function(subplots, plotFunction = reDrawJASPgraphsPlot, ...) {

      if (!all(vapply(subplots, is.ggplot, TRUE)))
        stop("all subplots should be of class ggplot!")
      if (!is.function(plotFunction))
        stop("plotFunction should be a function!")
      plotArgs <- list(...)
      if (!length(names(plotArgs)) == length(plotArgs))
        stop("... should be a named list")
      if (is.null(plotArgs[["names"]]) && identical(plotFunction, reDrawJASPgraphsPlot))
        plotArgs[["names"]] <- paste0("plot", seq_along(subplots))

      self$subplots     <- subplots
      self$plotFunction <- plotFunction
      self$plotArgs     <- plotArgs
    },
    plot = function(...) self$plotFunction(self$subplots, self$plotArgs, ...),
    plotFunction = NULL,
    plotArgs     = NULL,
    subplots     = NULL
  )
)

reDrawJASPgraphsPlot <- function(subplots, args, grob = FALSE, ...) {
  # redraws plots from PlotPriorAndPosterior, PlotRobustnessSequential, and ggMatrixplot
  g <- gridExtra::arrangeGrob(
    grobs         = subplots,
    heights       = args[["heights"]],
    layout_matrix = args[["layout"]],
    widths        = args[["widths"]],
    names         = args[["names"]]
  )
  if (grob)
    return(g)
  else
    return(gridExtra::grid.arrange(g, ...))
}
