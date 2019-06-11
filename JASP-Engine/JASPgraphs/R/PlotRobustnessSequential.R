#' @title Create a robustness or sequential plot
#'
#' @param dfLines A dataframe with \code{$x}, \code{$y}, and optionally \code{$g}.
#' @param dfPoints A dataframe with \code{$x}, \code{$y}, and optionally \code{$g}.
#' @param BF01 Numeric, with value of Bayes factor.
#' @param hasRightAxis Logical, should there be a right axis displaying evidence?
#' @param xName String or expression, displayed on the x-axis.
#' @param yName String or expression, displayed on the y-axis.
#' @param addEvidenceArrowText Logical, should arrows indicating "Evidence for H0/H1" be drawn?
#' @param drawPizzaTxt Logical, should there be text above and below the pizza plot?
#' @param evidenceLeveltxt Logical, should "Evidence for H0: extreme" be drawn?
#' Ignored if \code{!is.null(dfLines$g) && linesLegend}.
#' @param pointLegend Logical, should a legend of \code{dfPoints$g} be shown?
#' @param linesLegend Logical, should a legend of \code{dfLines$g} be shown?
#' @param bfSubscripts String, to be parsed as expression. Indicates what BF type to display.
#' @param pizzaTxt String vector of length 2, text to be drawn above and below pizza plot.
#' @param pointColors String vector, colors for points if \code{dfPoints$g} is not \code{NULL}.
#' @param lineColors String vector, colors for lines if \code{dfLines$g} is not \code{NULL}.
#' @param lineTypes String vector, line types if \code{dfLines$g} is not \code{NULL}.
#' @param addLineAtOne Logical, should a black line be draw at BF = 1?
#' @param bty List of three elements. Type specifies the box type, ldwX the width of the x-axis, lwdY the width of the y-axis.
#' @param ... Unused.
#'
#' @export
PlotRobustnessSequential <- function(
  dfLines, dfPoints = NULL, BF01 = NULL, hasRightAxis = TRUE, xName = NULL, yName = bfSubscripts,
  addEvidenceArrowText = TRUE, drawPizzaTxt = !is.null(BF01), evidenceLeveltxt = !is.null(BF01),
  pointLegend = !is.null(dfPoints), linesLegend = !is.null(dfLines$g), bfSubscripts = "BF[0][1]",
  pizzaTxt = pizzaTxtFromBF(bfSubscripts), pointColors  = c("grey", "white", "black", "red"),
  lineColors = c("black", "grey", "black"), lineTypes = c("dotted", "solid", "solid"),
  addLineAtOne = TRUE, bty = list(type = "n", ldwX = .5, lwdY = .5), ...) {

  errCheckPlots(dfLines = dfLines, dfPoints = dfPoints, BF01 = BF01)
  if (!is.null(dfPoints) && !is.null(BF01)) {
    stop("Cannot provide both a BF pizzaplot and a points legend!")
  }

  yRange <- range(dfLines$y)

  if (all(abs(yRange) <= log(100))) {

    # steps from 1, 3, 10, 30
    yBreaksL  <- log(c(1 / 100, 1 / 30, 1 / 10, 1 / 3, 1, 3, 10, 30, 100))
    yLabelsL <- c("1 / 100", "1 / 30", "1 / 10", "1 / 3", "1", "3", "10", "30", "100")
    # yLabelsL <- c(paste0("frac(1,", c(100, 30, 10, 3), ")"), "1", "3", "10", "30", "100")

    # find the interval bounds, raw index corresponds to left bound of the interval
    # 1.05 allows the data to cross the upper gridlines
    # e.g., so that a yRange of c(..., 1.02) means an upper bound of 1 rather than 3.
    idx <- findInterval(yRange, 1.05 * yBreaksL, all.inside = TRUE)
    if (idx[2L] == 5L && abs(yRange[2L]) <= 0.05493061) # near zero exception, since 1.05 * 0 = 0
      idx[2L] <- 4L

    if (addEvidenceArrowText) { # if we have arrows, add space for them
      # -1 extra room for lb, +2, because findInterval returns LEFT bound of interval and we need one extra for ub.
      idx <- max(1L, idx[1L] - 1L):min(length(yBreaksL), idx[2L] + 2L)
    } else {
      idx <- idx[1L]:idx[2L]
    }
    yBreaksL <- yBreaksL[idx]
    yLabelsL <- yLabelsL[idx]

  } else {
    # adaptive steps
    hasRightAxis <- FALSE # make no sense to display this anymore

    yBreaksL <- getPrettyAxisBreaks(x = yRange)
    yLabelsL <- parse(text = paste0("10^", yBreaksL))

  }

  if (hasRightAxis) {

    yBreaksR  <- log(c(1 / 100, 1 / 30, 1 / 10, 1 / 3, 1, 3, 10, 30, 100))
    allYlabelR <- c("Anecdotal", "Moderate", "Strong", "Very Strong", "Extreme")
    allYlabelR <- c(rev(allYlabelR), allYlabelR)

    nr <- 2*length(yBreaksL) - 1
    yBreaksR <- numeric(nr)
    yLabelsR <- character(nr)
    colsRight <- character(nr)
    idxOdd  <- seq(1, nr, 2)
    idxEven <- seq(2, nr, 2)

    yBreaksR[idxOdd]   <- yBreaksL
    yBreaksR[idxEven]  <- (yBreaksL[-1] + yBreaksL[-length(yBreaksL)]) / 2
    yLabelsR[idxEven]  <- allYlabelR[idx][-1L]
    colsRight[idxOdd]  <- "black"
    colsRight[idxEven] <- NA_character_

    sexAcis <- ggplot2::sec_axis(
      trans  = ~.,
      name   = "Evidence",
      breaks = yBreaksR,
      labels = yLabelsR
    )

    dfRightAxisLines <- data.frame(
      x    = Inf,
      xend = Inf,
      y    = yBreaksR[1L],
      yend = yBreaksR[length(yBreaksR)]
    )
    rightAxisLine <- ggplot2::geom_segment(
      data = dfRightAxisLines, mapping = ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
      lwd = getGraphOption("bty")[["lwdY"]],
      position = ggplot2::PositionIdentity, stat = ggplot2::StatIdentity, inherit.aes = FALSE
    )

  } else {
    colsRight <- NA
    sexAcis <- waiver()
    rightAxisLine <- NULL
  }

  xBreaks <- getPrettyAxisBreaks(dfLines$x)

  if (is.null(dfLines$g)) {
    mapping <- aes(x = x, y = y)
    scaleCol <- scaleLty <- NULL
  } else {
    if (length(unique(dfLines$g)) != length(lineColors) || length(lineColors) != length(lineTypes))
      stop("lineColors and lineTypes must have the same length as the number of groups in dfLines.")
    mapping  <- aes(x = x, y = y, group = g, linetype = g, color = g)
    scaleCol <- ggplot2::scale_color_manual(values = lineColors)
    scaleLty <- ggplot2::scale_linetype_manual(values = lineTypes)
  }

  nYbreaksL <- length(yBreaksL)
  gridCols <- rep("gray", nYbreaksL)
  gridLtys <- rep("dashed", nYbreaksL)
  if (addLineAtOne) { # color line at 1 differently
    i <- which(abs(yBreaksL) <= .Machine$double.eps)
    gridCols[i] <- "black"
    gridLtys[i] <- "solid"
  }

  gridLines <- makeGridLines(
    x        = xBreaks[1L], #rep(xBreaks[c(1, length(xBreaks))], nYbreaksL),
    y        = yBreaksL, #rep(yBreaksL, each = 2),
    xend     = xBreaks[length(xBreaks)],
    colour   = gridCols,
    linetype = gridLtys
  )

  g <- ggplot(data = dfLines, mapping = mapping) +
    gridLines +
    geom_line() +
    scale_y_continuous(
      name     = parse(text = yName),
      breaks   = yBreaksL,
      labels   = parse(text = yLabelsL),
      limits   = range(yBreaksL),
      sec.axis = sexAcis
    ) +
    scale_x_continuous(
      name   = xName,
      breaks = xBreaks
    ) + scaleCol + scaleLty

  legendPlot <- list()
  if (!is.null(dfPoints)) {
    mapping <- if (ncol(dfPoints) == 2L) aes(x = x, y = y) else aes(x = x, y = y, fill = g)
    g <- g + geom_point(data = dfPoints, mapping = mapping) +
      ggplot2::scale_fill_manual(values = c("red", "gray", "black", "white"))

    if (pointLegend) {

      legendPlot <- makeLegendPlot(dfPoints$g, fill = pointColors, type = "point")

    }
  }

  if (!is.null(BF01)) {
    tmp <- makeBFwheelAndText(BF01, bfSubscripts, pizzaTxt, drawPizzaTxt)
    gTextBF <- tmp$gTextBF
    gWheel <- tmp$gWheel
  }

  linesLegendPlot <- gTextEvidence <- NULL
  if (linesLegend && !is.null(dfLines$g)) {
    evidenceLeveltxt <- FALSE
    linesLegendPlot <- makeLegendPlot(dfLines$g, colors = lineColors, linetypes = lineTypes, type = "line")
  } else if (evidenceLeveltxt) {

    val <- BF01
    if (val < 1)
      val <- 1 / val

    # returns 1 if val in [1, 3], 2 if val in [3, 10], ...
    idx <- findInterval(val, c(1, 3, 10, 30, 100), rightmost.closed = FALSE)
    evidenceLevel <- c("Anecdotal", "Moderate", "Strong", "Very Strong", "Extreme")[idx]

    evidenceTxt <- parseThis(c(evidenceLevel, "paste('Evidence for ', H[0], ':')"))
    gTextEvidence <- draw2Lines(evidenceTxt, x = 0.75, align = "right")
  }

  if (addEvidenceArrowText) {

    n <- length(yBreaksL) - 1L
    # distance from one gridline to the next
    d1 <- yBreaksL[1L]     - yBreaksL[2L]
    d2 <- yBreaksL[n + 1L] - yBreaksL[n]

    # start at 10% of x-range
    xlocation <- (xBreaks[length(xBreaks)] - xBreaks[1L]) * 0.1

    dfArrow <- data.frame(
      x    = xlocation,
      xend = xlocation,
      y    = c(yBreaksL[2L] + 0.25 * d1, yBreaksL[n] + 0.25 * d2),
      yend = c(yBreaksL[2L] + 0.75 * d1, yBreaksL[n] + 0.75 * d2)
    )

    dfArrowTxt <- data.frame(
      y = (dfArrow$y + dfArrow$yend) / 2,
      x = 1.5 * xlocation, # 15% of x-range
      # additional '' around for are necessary because otherwise it's parsed as a for loop
      # label = c("Evidence", "'for'", "H[0]", "Evidence", "'for'", "H[1]")
      label = c("Evidence~'for'~H[0]", "Evidence~'for'~H[1]")
    )
    g <- g + ggplot2::geom_segment(
      data    = dfArrow, aes(x = x, y = y, xend = xend, yend = yend),
      lineend = "round", linejoin = "bevel",
      arrow   = grid::arrow(length = grid::unit(0.4, "cm")),
      size    = 1,
      inherit.aes = FALSE
    ) +
      ggplot2::geom_text(
        data        = dfArrowTxt,
        mapping     = aes(x = x, y = y, label = label),
        parse       = TRUE,
        size        = .40 * getGraphOption("fontsize"),
        inherit.aes = FALSE,
        hjust       = 0.0
      )
  }

  # if (addLineAtOne) {
  #   idx <- 1L + (yBreaksL == 0)
  #   gridLines <- element_line(colour = c("gray", "black")[idx], linetype = c("dashed", "solid")[idx])
  # } else {
  #   gridLines <- element_line(colour = "grey", linetype = "dashed")
  # }
  thm <- theme(
    # panel.grid.major.y = gridLines,
    axis.ticks.y.right = element_line(colour = colsRight),
    axis.text.y.right  = element_text(margin = ggplot2::margin(r = 5))
  )
  g <- themeJasp(g, bty = bty) + rightAxisLine + thm

  if (pointLegend && !is.null(dfPoints)) {
    f <- tempfile()
    grDevices::png(f)
    plot <- gridExtra::arrangeGrob(grobs = list(legendPlot, g), nrow = 2L, ncol = 1L, heights = c(.2, .8))
    grDevices::dev.off()
    if (file.exists(f))
      file.remove(f)

  } else if (!is.null(BF01)) {

    if (!is.null(linesLegendPlot)) {
      topPlotList <- list(gTextBF, gWheel, linesLegendPlot, g)
    } else {
      topPlotList <- list(gTextBF, gWheel, gTextEvidence, g)
    }
    idx <- lengths(topPlotList[1:3]) == 0L
    layout <- matrix(1:3, 1, 3)
    layout[idx] <- NA_integer_
    layout <- rbind(layout, 4)

    f <- tempfile()
    png(f)
    plot <- arrangeGrob(
      grobs         = topPlotList,
      heights       = c(.2, .8),
      layout_matrix = layout,
      widths        = c(.4, .2, .4)
    )
    # topplot <- gridExtra::arrangeGrob(grobs = topPlotList[!idx], layout_matrix = layout,
    #                                   padding = grid::unit(0.0, "line"))
    # plot    <- gridExtra::arrangeGrob(grobs = list(topplot, g), nrow = 2L, ncol = 1L, heights = c(.2, .8),
    #                                   padding = grid::unit(0.0, "line"))
    dev.off()
    if (file.exists(f)) file.remove(f)

  } else {
    plot <- g
  }

  class(plot) <- c("JASPgraphs", class(plot))
  return(plot)
}



#' custom Gridlines for ggplot objects
#'
#' @param x Left bound of gridline.
#' @param xend Right bound of gridline.
#' @param y height of gridline.
#' @param ... Further arguments to \code{\link[ggplot2:geom_segment]{geom_segment}}, e.g., colour.
#' @param linetypes solid, dashed, dotted, etc.
#' @param size size of the line.
#'
#' @details This function exists only when gridlines need to exist at specific locations, for example from x1 to x2 but
#' don't extend further than x2. Otherwise, use the build in functionality inside \code{\link[ggplot2:theme]{theme}}.
#' This function is a wrapper around \code{\link[ggplot2:geom_segment]{geom_segment}}.
#' @return a ggproto object.
#'
#' @export
makeGridLines <- function(x, xend, y, size = 0.85, ...) {

  return(
    ggplot2::geom_segment(
      data        = data.frame(x = x, y = y, xend = xend),
      mapping     =        aes(x = x, y = y, xend = xend, yend = y),
      inherit.aes = FALSE,
      size        = size,
      ...,
    )
  )
}
