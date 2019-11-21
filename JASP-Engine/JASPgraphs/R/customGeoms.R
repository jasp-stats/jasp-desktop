
setDefaults <- function(lst, ...) {

  defaults <- list(...)
  nms2change <- setdiff(names(defaults), names(lst))
  lst[nms2change] <- defaults[nms2change]
  return(lst)

}

# #' @export
# geom_point <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity",
#     ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
#
#   dots <- list(...)
#   dots <- setDefaults(dots, size = 3, shape = 21, fill = "gray")
#
#   ggplot2::layer(data = data, mapping = mapping, stat = stat, geom = ggplot2::GeomPoint,
#       position = position, show.legend = show.legend, inherit.aes = inherit.aes,
#       params = c(list(na.rm = na.rm), dots))
# }

jaspGeomPoint <- ggplot2::ggproto(
	`_class`    = "jaspGeomPoint",
	`_inherit`  = ggplot2::GeomPoint,
	default_aes = aes(size = 3, shape = 21, colour = "black", fill = "grey", alpha = NA, stroke = 0.5)
)

#' @title Custom geoms
#' @param mapping see details
#' @param data see details
#' @param stat see details
#' @param position see details
#' @param ... see details
#' @param na.rm see details
#' @param show.legend see details
#' @param inherit.aes see details
#' @details These functions are virtually identical to \code{\link[ggplot2]{geom_point}} and \code{\link[ggplot2]{geom_line}}
#' except that their underlying Geoms have different default values.
#'
#' @rdname geom_point
#' @export
geom_point <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity",
    ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {

  layer(data = data, mapping = mapping, stat = stat, geom = jaspGeomPoint,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...))
}

jaspGeomLine <- ggplot2::ggproto(
  `_class`    = "jaspGeomLine",
  `_inherit`  = ggplot2::GeomLine,
  default_aes = aes(size = 1.00, colour = "black", linetype = 1, alpha = NA)
)

#' @rdname geom_point
#' @export
geom_line <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity",
    ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {

  layer(data = data, mapping = mapping, stat = stat, geom = jaspGeomLine,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...))
}
