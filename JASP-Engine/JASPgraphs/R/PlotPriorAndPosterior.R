#' @export
drawBFpizza <- function(dat, size = 2, show.legend = FALSE, label.cex = .75, labels = NULL) {

  if (!is.data.frame(dat))
    dat <- data.frame(y = dat)

  errCheckPlotPriorAndPosterior(dat[["y"]], 2L)


	dat$group <- factor(seq(dat[[1]]))


	dat$y <- dat$y / sum(dat$y)
	nms <- colnames(dat)
	mapping <- ggplot2::aes_string(x = factor(1), y = nms[1], group = nms[2], fill = nms[2], color = nms[2])

	# rotate the wheel so that smaller half is always facing up
	ma <- max(dat[[1]])
	mi <- min(dat[[1]])

	if (dat$y[1L] <= dat$y[2L]) {
	  area <- mi / (mi + ma)
	} else {
	  area <- ma / (mi + ma)
	}
	start <- 0 + area * pi

	g <- ggplot(data = dat, mapping = mapping) +
	  ggplot2::geom_bar(width = 1, stat = "identity", show.legend = show.legend, size = size) +
	  ggplot2::coord_polar(theta = "y", start = start) +
	  ggplot2::scale_fill_manual(values  = c("darkred", "white")) +
	  ggplot2::scale_color_manual(values = c("black", "black")) +
	  theme(
	    plot.margin      = grid::unit(rep(0, 4), "cm"),
	    panel.background = ggplot2::element_rect(fill = "white"),
	    panel.grid       = ggplot2::element_blank(),
	    axis.text        = ggplot2::element_blank(),
	    axis.ticks       = ggplot2::element_blank(),
	    axis.title       = ggplot2::element_blank(),
	    axis.line        = ggplot2::element_blank(),
	    legend.position  = "none"
	  )

	if (!is.null(labels)) {
	  dfTxt <- data.frame(
	    x     = 2,
	    y     = c(dat$y[2L] / 2.0, dat$y[2L] + dat$y[1L] / 2),
	    label = labels
	  )
	  g <- g + ggplot2::geom_text(data = dfTxt, aes(x = x, y = y, label = label),
	                              size = .75 * getGraphOption("fontsize"), inherit.aes = FALSE)
	}

	return(g)
}

#' @export
getEmptyTheme <- function() {
  ggplot2::theme_void()
}

#' @export
getEmptyPlot <- function(axes = FALSE) {


  if (axes) {
  	stop("Not implemented")
  } else {
		ggplot2::ggplot() + ggplot2::geom_blank() + getEmptyTheme()
  }

}

draw2Lines <- function(l, x = 0, parse = FALSE) {

	nLabels <- length(l)
	y <- rep(.5, nLabels)
	diff <- seq(0, nLabels * 0.1, length.out = nLabels)
	diff <- diff - mean(diff)
	y <- y + diff
	dfText <- data.frame(
	  x = x,
	  y = y,
	  l = l
	)
  return(
    ggplot2::ggplot(data = dfText, ggplot2::aes(x = x, y = y, label = l)) +
      ggplot2::geom_text(size = .5 * getGraphOption("fontsize"), parse = parse, hjust = "left") +
      ggplot2::scale_y_continuous(limits = c(0, 1)) +
    	ggplot2::scale_x_continuous(limits = c(-1, 1)) +
      getEmptyTheme()
  )
}

errCheckPlotPriorAndPosterior <- function(x, length = 1L) {
	return(!is.null(x) && (length(x) != length || !is.numeric(x) || anyNA(x)))
}

errCheckPlots <- function(dfLines, dfPoints = NULL, CRI = NULL, median = NULL, BF01 = NULL) {

  if (!all(is.data.frame(dfLines), !is.null(dfLines$x), !is.null(dfLines$y),
           ncol(dfLines) == 2L || !is.null(dfLines$g)))
    stop("dfLines should be a data.frame with $x, $y, and $g!")
  if (!is.null(dfPoints) && !all(is.data.frame(dfPoints), !is.null(dfPoints$x), !is.null(dfPoints$y),
                                 ncol(dfPoints) == 2L || !is.null(dfPoints$g)))
    stop("dfPoints should be a data.frame with $x, $y, and $g!")
  if (errCheckPlotPriorAndPosterior(CRI, 2L))
    stop("CRI should be numeric and have length 2! (left bound, right bound)")
  if (errCheckPlotPriorAndPosterior(median))
    stop("median should be numeric and have length 1!")
  if (errCheckPlotPriorAndPosterior(BF01))
    stop("BF01 should be numeric and have length 1!")

  return(invisible(TRUE))
}

#' @export
hypothesis2BFtxt <- function(hypothesis = c("equal", "smaller", "greater")) {

  hypothesis <- match.arg(hypothesis)
  return(
    switch(
      hypothesis,
      "equal" = list(
        bfSubscripts = 0:1,
        pizzaTxt = c("data | H0", "data | H1")
      ),
      "smaller" = list(
        bfSubscripts = c(0, "\'-\'"),
        pizzaTxt = c("data | H0", "data | H-")
      ),
      "greater" = list(
        bfSubscripts = c(0, "\'+\'"),
        pizzaTxt = c("data | H0", "data | H+")
      )
    )
  )
}

#' @export
pizzaTxtFromBF <- function(x) {

  if (grepl("+", x, fixed = TRUE)) {
    pizzaTxt = c("data | H0", "data | H+")
  } else if(grepl("-", x, fixed = TRUE)) {
    pizzaTxt = c("data | H0", "data | H-")
  } else {
    pizzaTxt = c("data | H0", "data | H1")
  }
  pizzaTxt
}

#' @export
PlotPriorAndPosterior <- function(dfLines, dfPoints = NULL, CRI = NULL, median = NULL, BF01 = NULL, xName = NULL,
                                  yName = "Density", hypothesis = c("equal", "smaller", "greater"),
                                  addPizzaTxt = !is.null(BF01), bfSubscripts = "BF[0][1]",
                                  pizzaTxt = pizzaTxtFromBF(bfSubscripts), ...) {

	errCheckPlots(dfLines, dfPoints, CRI, median, BF01)

  emptyPlot <- list()
  plotArrange <- logical(2L)

  yBreaks <- getPrettyAxisBreaks(c(0, dfLines$y))
  breaksYmax <- yBreaks[length(yBreaks)] # max(dfLines$y)
  obsYmax <- max(dfLines$y)
  newymax <- max(1.1 * obsYmax, breaksYmax)

  mapping <- if (ncol(dfLines) == 2L) aes(x = x, y = y) else aes(x = x, y = y, group = g, linetype = g)
  g <- ggplot2::ggplot(data = dfLines, mapping) +
      geom_line() +
      scale_x_continuous(xName) +
      scale_y_continuous(yName, breaks = yBreaks, limits = c(0, newymax))

  if (!is.null(dfPoints)) {
    g <- g + ggplot2::geom_point(data = dfPoints, ggplot2::aes(x = x, y = y), inherit.aes = FALSE,
                          size = 4, shape = 21, stroke = 1.25, fill = "grey")
  }

  labelsCRI <- NULL
  if (!is.null(CRI)) {
  	dfCI <- data.frame(
  		xmin = CRI[1],
  		xmax = CRI[2],
  		y    = (newymax - obsYmax) / 2 + obsYmax
  	)
  	maxheight <- (newymax - dfCI$y)

    g <- g + ggplot2::geom_errorbarh(
      data = dfCI, ggplot2::aes(y = y, xmin = xmin, xmax = xmax), inherit.aes = FALSE,
      size = 1.25, height = maxheight)
    #maxheight / 8
    #)
    labelsCRI <- paste("95% CI: [",
    							 			bquote(.(formatC(dfCI$xmin, 3, format = "f"))), ", ",
    							 			bquote(.(formatC(dfCI$xmax, 3, format = "f"))), "]", sep = "")
  }

  if (!is.null(median)) {
  	labelsCRI <- c(labelsCRI, paste("Median:", formatC(median, 3, format = "f")))
  }

  if (length(labelsCRI) > 0) {
  	gTextCI <- draw2Lines(labelsCRI, x = -1)
  	plotArrange[1L] <- TRUE
  } else {
  	gTextCI <- emptyPlot
  }

  xr   <- range(dfLines$x)
  idx  <- which.max(dfLines$y)
  xmax <- dfLines$x[idx]
  if (xmax > mean(xr)) {
    legend.coordinates = c(0.20, 0.875)
  } else {
    legend.coordinates = c(0.80, 0.875)
  }

  g <- themeJasp(
    graph                   = g,
    legend.title            = "none",
    legend.position         = legend.coordinates
    # legend.coordinates      = legend.coordinates
  )# + ggplot2::theme(legend.position = legend.coordinates)

  if (!is.null(BF01)) {

  	subs <- stringr::str_extract_all(bfSubscripts, "(?<=\\[).+?(?=\\])")[[1]]
    labels <- paste0("BF[", subs[1L], "][", subs[2L],  "] == ", format(c(BF01, 1 / BF01), digits = 3))
  	gTextBF <- draw2Lines(labels, parse = TRUE)
    gWheel <- drawBFpizza(
      dat = data.frame(y = c(1, BF01)),
      labels = if (addPizzaTxt) pizzaTxt else NULL
    )

    plotArrange[2L] <- TRUE
  } else {
    gWheel <- emptyPlot
    gTextBF <- emptyPlot
  }

  if (!any(plotArrange)) {
    plot <- g
  } else {

    topPlotList <- list(gTextBF, gWheel, gTextCI)
    idx <- lengths(topPlotList) == 0L
    layout <- matrix(1:3, 1, 3)
    layout[idx] <- NA_integer_
    topplot <- gridExtra::arrangeGrob(grobs = topPlotList[!idx], layout_matrix = layout)
    plot    <- gridExtra::arrangeGrob(grobs = list(topplot, g), nrow = 2L, ncol = 1L, heights = c(.3, .7))

  }
  class(plot) <- c("JASPgraphs", class(plot))
  return(plot)
}

#' @export
PlotRobustnessSequential <- function(dfLines, dfPoints = NULL, BF01 = NULL, hasRightAxis = TRUE,
                                     xName = NULL, yName = bfSubscripts, log = TRUE, addEvidenceArrowText = TRUE,
                                     addPizzaTxt = !is.null(BF01),
                                     pointLegend = !is.null(dfPoints),
                                     bfSubscripts = "BF[0][1]",
                                     pizzaTxt = pizzaTxtFromBF(bfSubscripts),
                                     pointColors = c("grey", "white", "black", "red"), ...) {

  errCheckPlots(dfLines = dfLines, dfPoints = dfPoints, BF01 = BF01)
  if (!is.null(dfPoints) && !is.null(BF01)) {
    stop("Cannot provide both a BF pizzaplot and a points legend!")
  }

  yRange <- range(dfLines$y)

  if (all(abs(yRange) <= log10(100))) {

    # steps from 1, 3, 10, 30
    yBreaksL  <- log10(c(1 / 100, 1 / 30, 1 / 10, 1 / 3, 1, 3, 10, 30, 100))
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
    yLabelsL <-  parse(text = paste0("10^", yBreaksL))

  }

  if (hasRightAxis) {

    yBreaksR  <- log10(c(1 / 100, 1 / 30, 1 / 10, 1 / 3, 1, 3, 10, 30, 100))
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
  } else {
    mapping <- aes(x = x, y = y, group = g, linetype = g)
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
    )

  legendPlot <- list()
  if (!is.null(dfPoints)) {
    mapping <- if (ncol(dfPoints) == 2L) aes(x = x, y = y) else aes(x = x, y = y, fill = g)
    g <- g + geom_point(data = dfPoints, mapping = mapping) +
      ggplot2::scale_fill_manual(values = c("red", "gray", "black", "white"))

    if (pointLegend) {

      if (is.factor(dfPoints$g)) {
        l <- as.character(levels(dfPoints$g))
      } else {
        l <- unique(dfPoints$g)
      }
      dfLegendPlot <- data.frame(
        x = 0.1,
        y = factor(seq_along(dfPoints$x)),
        l = rev(l) # y = 1, 2, ... so first one at the bottom, hence reverse the labels
      )

      legendPlot <- ggplot(data = dfLegendPlot, aes(x = x, y = y, fill = y, label = l)) +
        geom_point(show.legend = FALSE, size = 1.5 * jaspGeomPoint$default_aes$size) +
        ggplot2::geom_text(nudge_x = 0.1, size = .6 * getGraphOption("fontsize"), hjust = 0, parse = TRUE) +
        ggplot2::xlim(c(0, 1)) +
        ggplot2::scale_fill_manual(values = pointColors) +
        getEmptyTheme()
    }
  }

  if (!is.null(BF01)) {

    subs <- stringr::str_extract_all(bfSubscripts, "(?<=\\[).+?(?=\\])")[[1]]
    labels <- paste0("BF[", subs, "][", rev(subs),  "] == ", format(c(1 / BF01, BF01), digits = 3))
  	gTextBF <- draw2Lines(labels, parse = TRUE)
    gWheel <- drawBFpizza(
      dat = data.frame(y = c(1, BF01)),
      labels = if (addPizzaTxt) pizzaTxt else NULL
    )

    val <- BF01
    if (val < 1)
      val <- 1 / val

    # val <- exp(abs(dfLines$y[length(dfLines$y)]))
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

    topPlotList <- list(gTextBF, gWheel, gTextEvidence)
    idx <- lengths(topPlotList) == 0L
    layout <- matrix(1:3, 1, 3)
    layout[idx] <- NA_integer_
    topplot <- gridExtra::arrangeGrob(grobs = topPlotList[!idx], layout_matrix = layout)
    plot    <- gridExtra::arrangeGrob(grobs = list(topplot, g), nrow = 2L, ncol = 1L, heights = c(.3, .7))

  } else {
    plot <- g
  }

  class(plot) <- c("JASPgraphs", class(plot))
  return(plot)
}



