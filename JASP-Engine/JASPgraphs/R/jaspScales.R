# #' @export
# scale_x_continuous <- function(name = waiver(), breaks = axesBreaks, minor_breaks = waiver(),
#                                labels = axesLabeller, limits = jaspLimits, expand = waiver(), oob = censor,
#                                na.value = NA_real_, trans = "identity", position = "bottom",
#                                sec.axis = waiver()) {
#
#   sc <- continuous_scale(c("x", "xmin", "xmax", "xend", "xintercept",
#                            "xmin_final", "xmax_final", "xlower", "xmiddle", "xupper"),
#                          "position_c", identity, name = name, breaks = breaks,
#                          minor_breaks = minor_breaks, labels = labels, limits = limits,
#                          expand = expand, oob = oob, na.value = na.value, trans = trans,
#                          guide = "none", position = position, super = ScaleContinuousPosition)
#   if (identical(limits, "JASP"))
#     sc$get_limits <- jaspLimits
#
#   if (!is.waive(sec.axis)) {
#     if (is.formula(sec.axis))
#       sec.axis <- sec_axis(sec.axis)
#     if (!is.sec_axis(sec.axis))
#       stop("Secondary axes must be specified using 'sec_axis()'")
#     sc$secondary.axis <- sec.axis
#   }
#   sc
# }
#
# #' @export
# scale_y_continuous <- function(name = waiver(), breaks = axesBreaks, minor_breaks = waiver(),
#                                labels = axesLabeller, limits = jaspLimits, expand = waiver(), oob = censor,
#                                na.value = NA_real_, trans = "identity", position = "left",
#                                sec.axis = waiver()) {
#
#   sc <- continuous_scale(c("y", "ymin", "ymax", "yend", "yintercept",
#                            "ymin_final", "ymax_final", "lower", "middle", "upper"),
#                          "position_c", identity, name = name, breaks = breaks,
#                          minor_breaks = minor_breaks, labels = labels, limits = limits,
#                          expand = expand, oob = oob, na.value = na.value, trans = trans,
#                          guide = "none", position = position, super = ScaleContinuousPosition)
#
#   if (identical(limits, "JASP"))
#     sc$get_limits <- jaspLimits
#
#   if (!is.waive(sec.axis)) {
#     if (is.formula(sec.axis))
#       sec.axis <- sec_axis(sec.axis)
#     if (!is.sec_axis(sec.axis))
#       stop("Secondary axes must be specified using 'sec_axis()'")
#     sc$secondary.axis <- sec.axis
#   }
#   sc
# }


jaspLimits <- function(..., self = self) {
  # this function is basically identical to what is normally is in sc$get_limits,
  # except for everything inside if (identical(self$limits, "JASP"))
  if (self$is_empty())
    return(c(0, 1))
  if (identical(self$limits, "JASP")) {
    # ensures that outer breakpoints are always included in plot
    range(axesBreaks(self$range$range))
  } else if (!is.null(self$limits)) {
    ifelse(!is.na(self$limits), self$limits, self$range$range)
  } else {
    self$range$range
  }

}