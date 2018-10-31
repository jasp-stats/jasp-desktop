# this file exists because ggplot has a habit of changing standards over time
# it also imports some functions from ggplot

# cat(paste0(sort(strsplit(..., " ")[[1]]), collapse = " "))
#' @importFrom ggplot2 ggplot theme element_blank element_rect element_text element_line unit
#' @importFrom ggplot2 aes continuous_scale element_blank ggproto layer ScaleContinuousPosition sec_axis
#' @importFrom ggplot2 waiver xlab ylab
#' @importFrom grDevices dev.off png rgb
#' @importFrom scales censor
#' @importFrom gridExtra arrangeGrob grid.arrange

#' @export
getAxisBreaks <- function(x) {
  UseMethod("getAxisBreaks", x)
}

#' @export
getAxisBreaks.gg <- function(x) getAxisBreaks.ggplot_built(ggplot2::ggplot_build(x))

#' @export
getAxisBreaks.ggplot <- function(x) getAxisBreaks.ggplot_built(ggplot2::ggplot_build(x))

#' @export
getAxisBreaks.list <- function(x) getAxisBreaks.ggplot_built(x)

#' @export
getAxisBreaks.ggplot_built <- function(x) {

  if (graphOptions("ggVersion") <= 2.21) {
    return(list(
      x = x$layout$panel_scales$x[[1]]$break_positions(),
      y = x$layout$panel_scales$y[[1]]$break_positions()
      # x = x$layout$panel_ranges[[1]]$x.major_source,
      # y = x$layout$panel_ranges[[1]]$y.major_source
    ))
  } else {
    return(list(
      x = x$layout$panel_scales_x[[1]]$breaks,
      y = x$layout$panel_scales_y[[1]]$breaks
    ))
  }
}

#' @export
getMajorSource <- function(x) {
  UseMethod("getMajorSource", x)
}

getMajorSource.gg     <- function(x) getMajorSource.ggplot_built(ggplot2::ggplot_build(x))
getMajorSource.ggplot <- function(x) getMajorSource.ggplot_built(ggplot2::ggplot_build(x))
getMajorSource.list   <- function(x) getMajorSource.ggplot_built(x)

#' @export
getMajorSource.ggplot_built <- function(x) {

  if (graphOptions("ggVersion") <= 2.21) {
    return(list(
      x = x$layout$panel_ranges[[1]]$x.major_source,
      y = x$layout$panel_ranges[[1]]$y.major_source
    ))
  } else {
    return(list(
      x = x$layout$panel_params[[1]]$x.major_source,
      y = x$layout$panel_params[[1]]$y.major_source
    ))
  }
}

#' @export
getRanges <- function(x) {
  UseMethod("getMajorSouce", x)
}

getRanges.gg     <- function(x) getRanges.ggplot_built(ggplot2::ggplot_build(x))
getRanges.ggplot <- function(x) getRanges.ggplot_built(ggplot2::ggplot_build(x))
getRanges.list   <- function(x) getRanges.ggplot_built(x)

getRanges.ggplot_built <- function(x) {

  if (graphOptions("ggVersion") <= 2.21) {
    return(list(
      x = x$layout$panel_ranges[[1]]$x.range,
      y = x$layout$panel_ranges[[1]]$y.range
    ))
  } else {
    return(list(
      x = x$layout$panel_params[[1]]$x.range,
      y = x$layout$panel_params[[1]]$y.range
    ))
  }
}

getAxesScales <- function(gb) {
  return(
    switch(graphOptions("ggVersion"),
           "2.2.1" = gb$scales$scales,
           gb$scales$scales
    )
  )
}

isContinuousScale <- function(x) inherits(x, c("ScaleContinuousPosition", "ScaleContinuous"))



is.waive    <- function(x) inherits(x, "waiver")
is.sec_axis <- function(x) inherits(x, "AxisSecondary")
is.formula  <- function(x) inherits(x, "formula")