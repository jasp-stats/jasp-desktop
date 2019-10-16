# the code here is largely taken from/ inspired by ggExtra::ggMarginal

#' @importFrom ggplot2 ggplotGrob
#' @importFrom grid    unit
#' @importFrom gtable  gtable_add_padding gtable_add_grob gtable_add_rows gtable_add_cols

getPanelPos <- function(gtableGrob) {
  layDF <- gtableGrob$layout
  return(layDF[layDF$name == "panel", c("t", "l", "b", "r")])
}

getMargGrob <- function(margPlot, pattern = "panel") {
  margG <- ggplot2::ggplotGrob(margPlot)
  gtable::gtable_filter(margG, pattern = pattern)
}

makeGrobAlignedPlots <- function(mainplot, abovePlot = NULL, rightPlot = NULL, showLegend = FALSE, size = 5) {

  noTopPlot   <- is.null(abovePlot)
  noRightPlot <- is.null(rightPlot)
  if (noTopPlot && noRightPlot)
    return(ggplotGrob(mainplot))

  if (showLegend) {
    # extract the legend as grob
    legendGrob <- gtable::gtable_filter(ggplotGrob(mainplot), "guide-box")
    # remove the legend from the main plot
    pGrob <- ggplotGrob(mainplot + theme(legend.position = "none"))
  } else {
    pGrob <- ggplotGrob(mainplot)
  }

  # from ggExtra::ggMarginal
  if (!(noTopPlot || noRightPlot)) {
    ggxtraTmp   <- addTopMargPlot(pGrob, abovePlot, size)
    ggxtraNoTtl <- addRightMargPlot(ggxtraTmp, rightPlot, size)
  } else if (noRightPlot) {
    ggxtraTmp   <- gtable_add_padding(pGrob, unit(c(0, 0.5, 0, 0), "lines"))
    ggxtraNoTtl <- addTopMargPlot(ggxtraTmp, abovePlot, size)
  } else if (noTopPlot) {
    ggxtraTmp   <- gtable_add_padding(pGrob, unit(c(0.5, 0, 0, 0), "lines"))
    ggxtraNoTtl <- addRightMargPlot(ggxtraTmp, rightPlot, size)
  }
  # add the legend to the plot
  if (showLegend)
    ggxtraNoTtl <- addLegendMargPlot(ggxtraNoTtl, legendGrob, noTopPlot, noRightPlot, size)
  
  return(ggxtraNoTtl)
  
}

addTopMargPlot <- function(ggMargGrob, top, size) {
  panelPos <- getPanelPos(ggMargGrob)
  topMargG <- getMargGrob(top)
  gt <- gtable::gtable_add_rows(x = ggMargGrob, heights = unit(1/size, "null"), pos = 0)
  gtable::gtable_add_grob(x = gt, grobs = topMargG, t = 1, 
                          b = 1, l = panelPos[["l"]], r = panelPos[["r"]], z = Inf, 
                          clip = "on", name = "topMargPlot")
}

addRightMargPlot <- function(ggMargGrob, right, size) {
  panelPos <- getPanelPos(ggMargGrob)
  rightMargG <- getMargGrob(right)
  gt <- gtable::gtable_add_cols(x = ggMargGrob, widths = grid::unit(1/size, "null"), pos = -1)
  gtable::gtable_add_grob(x = gt, grobs = rightMargG, t = panelPos[["t"]], 
                          b = panelPos[["b"]], r = ncol(gt), l = ncol(gt), z = Inf, 
                          clip = "on", name = "rightMargPlot")
}

addLegendMargPlot <- function(ggMargGrob, legendGrob, noTopPlot, noRightPlot, size) {

  if (!noRightPlot) {
    if (noTopPlot) {
      gt <- gtable::gtable_add_cols(x = ggMargGrob, widths = grid::unit(1 / (1.5 * size), "null"), pos = -1)
    } else {
      gt <- ggMargGrob
    }
    return(
      gtable::gtable_add_grob(
        x = gt, grobs = legendGrob,
        t = 1 + noTopPlot, b = 1 + noTopPlot, r = ncol(gt), l = ncol(gt), z = Inf, clip = "on", name = "legendMargPlot")
    )
  } else {
    gt <- gtable::gtable_add_cols(x = ggMargGrob, widths = grid::unit(1 / (1.5 * size), "null"), pos = -1)
    return(
      gtable::gtable_add_grob(
        x = gt, grobs = legendGrob,
        t = 9, # TODO: this could be derived from the plot
        l = ncol(ggMargGrob),
        z = Inf, clip = "on", name = "legendMargPlot")
    )
  }
}
