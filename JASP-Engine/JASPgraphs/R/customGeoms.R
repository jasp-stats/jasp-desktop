
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

#' @export
jaspGeomPoint <- ggplot2::ggproto(
	`_class`    = "jaspGeomPoint",
	`_inherit`  = ggplot2::GeomPoint,
	default_aes = aes(size = 3, shape = 21, colour = "black", fill = "grey", alpha = NA, stroke = 0.5)
)

#' @export
geom_point <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity",
    ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {

	layer(data = data, mapping = mapping, stat = stat, geom = jaspGeomPoint,
				position = position, show.legend = show.legend, inherit.aes = inherit.aes,
				params = list(na.rm = na.rm, ...))
}

