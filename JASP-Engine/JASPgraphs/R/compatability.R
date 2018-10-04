# this file exists because ggplot has a habit of changing standards over time
# it also imports some functions from ggplot

#' @importFrom ggplot2 waiver aes xlab ylab element_blank continuous_scale ScaleContinuousPosition sec_axis



getMajorSource <- function(gb) {

  # gb: output from ggplot2::ggbuild(ggplot)
  return(
    switch(graphOptions("ggVersion"),
           "2.2.1" = list(
             x = gb$layout$panel_ranges[[1]]$x.major_source,
             y = gb$layout$panel_ranges[[1]]$y.major_source
           ),
           list(
             x = gb$layout$panel_params[[1]]$x.major_source,
             y = gb$layout$panel_params[[1]]$y.major_source
           )
    )
  )
}

getRanges <- function(gb) {

  return(
    switch(graphOptions("ggVersion"),
           "2.2.1" = list(
             x = gb$layout$panel_ranges[[1]]$x.range,
             y = gb$layout$panel_ranges[[1]]$y.range
           ),
           list(
             x = gb$layout$panel_params[[1]]$x.range,
             y = gb$layout$panel_params[[1]]$y.range
           )
    )
  )
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



is.waive <- function(x) inherits(x, "waiver")
is.sec_axis <- function(x) inherits(x, "AxisSecondary")
