# the code here is largely taken from/ inspired by ggExtra::ggMarginal

#' @importFrom ggplot2 ggplotGrob
#' @importFrom grid    unit
#' @importFrom gtable  gtable_add_padding gtable_add_grob gtable_add_rows gtable_add_cols

getPanelPos <- function(gtableGrob) {
  layDF <- gtableGrob$layout
  return(layDF[layDF$name == "panel", c("t", "l", "b", "r")])
}

getMargGrob <- function(margPlot) {
  margG <- ggplot2::ggplotGrob(margPlot)
  gtable::gtable_filter(margG, pattern = "panel")
}

makeGrobAlignedPlots <- function(mainplot, abovePlot = NULL, rightPlot = NULL, size = 5) {
  
  pGrob <- ggplotGrob(mainplot)
  if (is.null(abovePlot) && is.null(rightPlot)) {
    return(pGrob)
  # from ggExtra::ggMarginal
  } else if (!is.null(abovePlot) && !is.null(rightPlot)) {
    ggxtraTmp   <- addTopMargPlot(pGrob, abovePlot, size)
    ggxtraNoTtl <- addRightMargPlot(ggxtraTmp, rightPlot, size)
  } else if (!is.null(abovePlot)) {
    ggxtraTmp   <- gtable_add_padding(pGrob, unit(c(0, 0.5, 0, 0), "lines"))
    ggxtraNoTtl <- addTopMargPlot(ggxtraTmp, abovePlot, size)
  } else if (!is.null(rightPlot)) {
    ggxtraTmp   <- gtable_add_padding(pGrob, unit(c(0.5, 0, 0, 0), "lines"))
    ggxtraNoTtl <- addRightMargPlot(ggxtraTmp, rightPlot, size)
  }
  
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
