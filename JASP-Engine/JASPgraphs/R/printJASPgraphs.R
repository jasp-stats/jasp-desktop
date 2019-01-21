# print.JASPgraphs <- function(x, newpage = is.null(vp), vp = NULL, ...) {
#
#   tmp <- mget(x = c("x_custom", "y_custom", "element_grob.element_custom_x", "element_grob.element_custom_y"),
#               envir = .GlobalEnv, ifnotfound = rep(list(NULL), 4))
#
#   assign("x_custom", JASPgraphs:::x_custom, envir = .GlobalEnv)
#   assign("y_custom", JASPgraphs:::y_custom, envir = .GlobalEnv)
#   assign("element_grob.element_custom_x", JASPgraphs:::element_grob.element_custom_x, envir = .GlobalEnv)
#   assign("element_grob.element_custom_y", JASPgraphs:::element_grob.element_custom_y, envir = .GlobalEnv)
#
#   x <- ggplot2:::print.ggplot(x, newpage = newpage, vp = vp, ...)
#
#   assign("x_custom", tmp[[1L]], envir = .GlobalEnv)
#   assign("y_custom", tmp[[2L]], envir = .GlobalEnv)
#   assign("element_grob.element_custom_x", tmp[[3L]], envir = .GlobalEnv)
#   assign("element_grob.element_custom_y", tmp[[4L]], envir = .GlobalEnv)
#
#   invisible(x)
#
# }

#' @method print JASPgraphs
#' @export
print.JASPgraphs <- function(x, ...) {

  if (ggplot2::is.ggplot(x)) {
    ggplot2:::print.ggplot(x, ...)
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
plot.JASPgraphs <- function(x, ...) print.JASPgraphs(x)