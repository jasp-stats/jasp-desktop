print.JASPgraphs <- function(x, newpage = is.null(vp), vp = NULL, ...) {

  tmp <- mget(x = c("x_custom", "y_custom", "element_grob.element_custom_x", "element_grob.element_custom_y"),
              envir = .GlobalEnv, ifnotfound = rep(list(NULL), 4))

  assign("x_custom", JASPgraphs:::x_custom, envir = .GlobalEnv)
  assign("y_custom", JASPgraphs:::y_custom, envir = .GlobalEnv)
  assign("element_grob.element_custom_x", JASPgraphs:::element_grob.element_custom_x, envir = .GlobalEnv)
  assign("element_grob.element_custom_y", JASPgraphs:::element_grob.element_custom_y, envir = .GlobalEnv)

  x <- ggplot2:::print.ggplot(x, newpage = newpage, vp = vp, ...)

  assign("x_custom", tmp[[1L]], envir = .GlobalEnv)
  assign("y_custom", tmp[[2L]], envir = .GlobalEnv)
  assign("element_grob.element_custom_x", tmp[[3L]], envir = .GlobalEnv)
  assign("element_grob.element_custom_y", tmp[[4L]], envir = .GlobalEnv)

  invisible(x)

}


