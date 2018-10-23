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
#' @param ... Unused.
#'
#' @export
PlotRobustnessSequential <- function(
  dfLines, dfPoints = NULL, BF01 = NULL, hasRightAxis = TRUE, xName = NULL, yName = bfSubscripts,
  addEvidenceArrowText = TRUE, drawPizzaTxt = !is.null(BF01), evidenceLeveltxt = !is.null(BF01),
  pointLegend = !is.null(dfPoints), linesLegend = !is.null(dfLines$g), bfSubscripts = "BF[0][1]",
  pizzaTxt = pizzaTxtFromBF(bfSubscripts), pointColors  = c("grey", "white", "black", "red"),
  lineColors = c("black", "grey", "black"), lineTypes = c("solid", "dashed", "dotted"), ...) {

  errCheckPlots(dfLines = dfLines, dfPoints = dfPoints, BF01 = BF01)
  if (!is.null(dfPoints) && !is.null(BF01)) {
    stop("Cannot provide both a BF pizzaplot and a points legend!")
  }

  yRange <- range(dfLines$y)

  if (all(abs(yRange) <= log(100))) {

    # steps from 1, 3, 10, 30
    yBreaksL  <- log(c(1 / 100, 1 / 30, 1 / 10, 1 / 3, 1, 3, 10, 30, 100))
    yLabelsL <- c("1 / 100", "1 / 30", "1 / 10", "1 / 3", "1", "3", "10", "30", "100")

    # find the interval bounds, raw index corresponds to left bound of the interval
    # +1 takes the right bound of the interval
    idx <- findInterval(yRange, yBreaksL, rightmost.closed = FALSE, left.open = FALSE) + 0:1
    idx <- idx[1L]:idx[2L]
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
    mapping <- aes(x = x, y = y, group = g, linetype = g, color = g)
    scaleCol <- ggplot2::scale_color_manual(values = lineColors)
    scaleLty <- ggplot2::scale_linetype_manual(values = lineTypes)
  }

  g <- ggplot(data = dfLines, mapping = mapping) +
    geom_line() +
    scale_y_continuous(
      name     = parse(text = yName),
      breaks   = yBreaksL,
      labels   = yLabelsL,
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

      # if (is.factor(dfPoints$g)) {
      #   l <- as.character(levels(dfPoints$g))
      # } else {
      #   l <- unique(dfPoints$g)
      # }
      # dfLegendPlot <- data.frame(
      #   x = 0.1,
      #   y = factor(seq_along(dfPoints$x)),
      #   l = rev(l) # y = 1, 2, ... so first one at the bottom, hence reverse the labels
      # )
      #
      # legendPlot <- ggplot(data = dfLegendPlot, aes(x = x, y = y, fill = y, label = l)) +
      #   geom_point(show.legend = FALSE, size = 1.5 * jaspGeomPoint$default_aes$size) +
      #   ggplot2::geom_text(nudge_x = 0.1, size = .6 * getGraphOption("fontsize"), hjust = 0, parse = TRUE) +
      #   ggplot2::xlim(c(0, 1)) +
      #   ggplot2::scale_fill_manual(values = pointColors) +
      #   getEmptyTheme()
    }
  }

  if (!is.null(BF01)) {

    subs <- stringr::str_extract_all(bfSubscripts, "(?<=\\[).+?(?=\\])")[[1]]
    labels <- paste0("BF[", subs, "][", rev(subs),  "] == ", format(c(1 / BF01, BF01), digits = 3))
  	gTextBF <- draw2Lines(labels, parse = TRUE)
    gWheel <- drawBFpizza(
      dat = data.frame(y = c(1, BF01)),
      labels = if (drawPizzaTxt) pizzaTxt else NULL
    )
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

    evidenceTxt <- c(evidenceLevel, "paste('Evidence for ', H[0], ':')")
    gTextEvidence <- draw2Lines(evidenceTxt, parse = TRUE)
  }

  if (addEvidenceArrowText) {

    r <- yBreaksL[2L] - yBreaksL[1L]
    n <- length(yBreaksL)  - 1L

    dfArrow <- data.frame(
      y    = c(yBreaksL[1L] + r * 3 / 4, yBreaksL[n] + r * 1 / 4),
      yend = c(yBreaksL[1L] + r * 1 / 4, yBreaksL[n] + r * 3 / 4)
    )

    # which x-values are in which area?
    xlocation <- c(NA, NA)
    ranges <- findInterval(dfLines$x, xBreaks, rightmost.closed = TRUE, left.open = TRUE)
    yCont <- matrix(NA, length(xBreaks), 2L)

    # perhaps this should be a generic function, could help for legend placement?
    for (i in 1:(ranges[length(ranges)])) {

      # coordinates of y-values in this x-range
      idx <- ranges == i

      if (is.na(xlocation[1L])) {
        # percentage of y-values in that area
        yCont[i, 1L] <- mean(findInterval(
          dfLines$y[idx],
          sort(c(dfArrow$y[1L], dfArrow$yend[1L])),
          rightmost.closed = TRUE, left.open = TRUE
        ) == 1L)

        if (yCont[i, 1L] == 0.0) { # no y-points in this area!
          if (i == length(xBreaks)) {
            xlocation[1L] <- (xBreaks[i] + xBreaks[i - 1L]) / 2.0
          } else {
            xlocation[1L] <- (xBreaks[i] + xBreaks[i + 1L]) / 2.0
          }
        }
      }

      if (is.na(xlocation[2L])) {
        # percentage of y-values in that area
        yCont[i, 2L] <- mean(findInterval(
          dfLines$y[idx],
          sort(c(dfArrow$y[2L], dfArrow$yend[2L])),
          rightmost.closed = TRUE, left.open = TRUE
        ) == 1L)

        if (yCont[i, 2L] == 0.0) { # no y-points in this area!
          if (i == length(xBreaks)) {
            xlocation[2L] <- (xBreaks[i] + xBreaks[i - 1L]) / 2.0
          } else {
            xlocation[2L] <- (xBreaks[i] + xBreaks[i + 1L]) / 2.0
          }
        }
      }

      if (!anyNA(xlocation)) {
        break
      }
    }

    # fallback, minimum percentage
    if (is.na(xlocation[1L])) {
      i <- which.min(yCont[, 1L])
      if (i == length(xBreaks)) {
        xlocation[1L] <- (xBreaks[i] + xBreaks[i - 1L]) / 2.0
      } else {
        xlocation[1L] <- (xBreaks[i] + xBreaks[i + 1L]) / 2.0
      }
    }
    if (is.na(xlocation[2L])) {
      i <- which.min(yCont[, 2L])
      if (i == length(xBreaks)) {
        xlocation[2L] <- (xBreaks[i] + xBreaks[i - 1L]) / 2.0
      } else {
        xlocation[2L] <- (xBreaks[i] + xBreaks[i + 1L]) / 2.0
      }
    }

    dfArrow$x    <- xlocation
    dfArrow$xend <- xlocation

    yvals <- numeric(6)
    yvals[1:3] <- seq(dfArrow$y[1L], dfArrow$yend[1L], length.out = 3L)
    yvals[4:6] <- seq(dfArrow$yend[2L], dfArrow$y[2L], length.out = 3L)
    dfArrowTxt <- data.frame(
      y = yvals,
      x = rep(xlocation + (xBreaks[2L] - xBreaks[1L]) / 4.0, each = 3),
      # additional '' around for are necessary because otherwise it's parsed as a for loop
      label = c("Evidence", "'for'", "H[0]", "Evidence", "'for'", "H[1]")
    )

    g <- g + ggplot2::geom_segment(
      data = dfArrow, aes(x = x, y = y, xend = xend, yend = yend),
      lineend = "round", linejoin = "bevel",
      arrow = grid::arrow(length = grid::unit(0.3, "inches")),
      inherit.aes = FALSE
    ) +
      ggplot2::geom_text(
        data        = dfArrowTxt,
        mapping     = aes(x = x, y = y, label = label),
        parse       = TRUE,
        size        = .40 * getGraphOption("fontsize"),
        inherit.aes = FALSE
      )

  }

  thm <- theme(
    panel.grid.major.y = ggplot2::element_line(colour = "grey", linetype = "dashed"),
    axis.ticks.y.right = ggplot2::element_line(colour = colsRight)
    # axis.text.y.right  = ggplot2::element_text(debug = TRUE)
  )
  g <- themeJasp(g) + rightAxisLine + thm

  if (pointLegend && !is.null(dfPoints)) {
    plot <- gridExtra::arrangeGrob(grobs = list(legendPlot, g), nrow = 2L, ncol = 1L, heights = c(.3, .7))
  } else if (!is.null(BF01)) {

    if (!is.null(linesLegendPlot)) {
      topPlotList <- list(gTextBF, gWheel, linesLegendPlot)
    } else {
      topPlotList <- list(gTextBF, gWheel, gTextEvidence)
    }
    idx <- lengths(topPlotList) == 0L
    layout <- matrix(1:3, 1, 3)
    layout[idx] <- NA_integer_

    f <- tempfile()
    grDevices::png(f)
    topplot <- gridExtra::arrangeGrob(grobs = topPlotList[!idx], layout_matrix = layout)
    plot    <- gridExtra::arrangeGrob(grobs = list(topplot, g), nrow = 2L, ncol = 1L, heights = c(.2, .8))
    grDevices::dev.off()
    if (file.exists(f))
      file.remove(f)

  } else {
    plot <- g
  }

  class(plot) <- c("JASPgraphs", class(plot))
  return(plot)
}
