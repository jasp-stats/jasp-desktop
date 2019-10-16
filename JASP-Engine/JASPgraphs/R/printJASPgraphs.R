#' @method print JASPgraphs
#' @export
print.JASPgraphs <- function(x, ...) {

  if (ggplot2::is.ggplot(x)) {
    # do not call ggplot2:::print.ggplot() to please R CMD check
    NextMethod()
  } else if (inherits(x, c("gtable", "gTree", "grob", "gDesc"))) {
    gridExtra::grid.arrange(x, ...)
  } else if (length(class(x)) > 1L) {
    NextMethod()
  } else {
    stop(sprintf(
      "unsupported plot object of class: %s",
      paste0(class(x), sep = ", ")
    ))
  }
  return(invisible(TRUE))
}

#' @method plot JASPgraphs
#' @export
plot.JASPgraphs <- function(x, ...) print.JASPgraphs(x, ...)

#' @method print JASPgraphsPlot
#' @export
print.JASPgraphsPlot <- function(x, ...) {

  x$plot(...)
  return(invisible(TRUE))
}

#' @method plot JASPgraphsPlot
#' @export
plot.JASPgraphsPlot <- function(x, ...) print.JASPgraphsPlot(x, ...)

