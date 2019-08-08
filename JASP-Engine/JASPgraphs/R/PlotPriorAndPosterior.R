getBackgroundRect <- function(debug) {
  if (debug) {
    element_rect(colour = "red", fill = "transparent", size = 5, linetype = 1)
  } else {
    element_rect(colour = "transparent", fill = "transparent", size = 1, linetype = 1)
  }
}

#' @export
getEmptyTheme <- function() {

  t <- theme(
    rect              = getBackgroundRect(getGraphOption("debug")),
    panel.spacing     = unit(0, "null"),
    plot.margin       = ggplot2::margin(),
    panel.background  = element_blank(),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank(),
    axis.ticks        = element_blank(),
    axis.text.x       = element_blank(),
    axis.text.y       = element_blank(),
    axis.title.x      = element_blank(),
    axis.title.y      = element_blank(),
    axis.ticks.length = unit(0,"null")
  )

  # t <- ggplot2::theme_void() +
  #     theme(
  #       panel.spacing = grid::unit(0,"null"),
  #       plot.margin   = rep(grid::unit(0,"null"), 4)
  #     )
  # if (getGraphOption("debug"))
  # t <- t + ggplot2::theme(rect = ggplot2::element_rect(colour = "red", size = 1, linetype = 1, fill = "transparent"))
  return(t)
}

#' @export
getEmptyPlot <- function(axes = FALSE) {


  if (axes) {
  	stop("Not implemented")
  } else {
		ggplot2::ggplot() + ggplot2::geom_blank() + getEmptyTheme()
  }

}

draw2Lines <- function(l, x = 0.5, parse = needsParsing(l), align = c("center", "left", "right"), scaleFont = 0.35) {

  if (is.numeric(align)) {
    hjust <- align
  } else if (is.character(align)) {
    align <- match.arg(align)
    hjust <- switch(
      align,
      "center" = 0.5,
      "left"   = 0.0,
      "right"  = 1.0
    )
  } else {
    stop("incorrect class for align. Expected character of numeric.")
  }

	nLabels <- length(l)
	y <- rep(.5, nLabels)
	diff <- seq(0, nLabels * 0.15, length.out = nLabels)
	diff <- diff - mean(diff)
	y <- y + diff
	dfText <- data.frame(
	  x = x,
	  y = y,
	  l = l
	)
  return(
    ggplot2::ggplot(data = dfText, ggplot2::aes(x = x, y = y, label = l)) +
      ggplot2::geom_text(size = scaleFont * getGraphOption("fontsize"), parse = parse, hjust = hjust) +
      ggplot2::scale_y_continuous(limits = c(0, 1)) +#, expand = c(0, 0)) +
    	ggplot2::scale_x_continuous(limits = c(0, 1)) +#, expand = c(0, 0)) +
      # ggplot2::coord_fixed(ratio = 1) +
      getEmptyTheme()
  )
}

errCheckPlotPriorAndPosterior <- function(x, length = 1L, nullOk = TRUE) {
  if (is.null(x))
    return(!nullOk)
	return(length(x) != length || !is.numeric(x) || anyNA(x))
}

errCheckPlots <- function(dfLines, dfPoints = NULL, CRI = NULL, median = NULL, BF = NULL) {

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
  if (errCheckPlotPriorAndPosterior(BF))
    stop("BF should be numeric and have length 1!")

  return(invisible(TRUE))
}

makeLegendPlot <- function(groupingVariable, colors = NULL, fill = NULL, linetypes = NULL, type = c("point", "line")) {

  type <- match.arg(type)
  if (is.factor(groupingVariable)) {
    l <- as.character(levels(groupingVariable))
  } else {
    l <- unique(groupingVariable)
  }
  parse <- needsParsing(groupingVariable)

  if (type == "point") {

    dfLegendPlot <- data.frame(
      x = 0.1,
      y = factor(seq_along(l)),
      l = rev(l) # y = 1, 2, ... so first one at the bottom, hence reverse the labels
    )

    legendPlot <- ggplot(data = dfLegendPlot, aes(x = x, y = y, fill = y, label = l)) +
      geom_point(show.legend = FALSE, size = 1.15 * jaspGeomPoint$default_aes$size) +
      ggplot2::geom_text(nudge_x = 0.1, size = .35 * getGraphOption("fontsize"), hjust = 0,
                         parse = parse) +
      ggplot2::xlim(c(0, 1)) +
      getEmptyTheme()

  } else {

    dfLegendPlot <- data.frame(
      x    = 0,
      xend = 0.1,
      y    = factor(seq_along(l)),
      yend = factor(seq_along(l)),
      l    = rev(l) # y = 1, 2, ... so first one at the bottom, hence reverse the labels
    )

    legendPlot <- ggplot(data = dfLegendPlot,  aes(x = x, y = y, xend = xend, yend = yend, label = l)) +
      ggplot2::geom_segment(mapping = aes(color = y, linetype = y), show.legend = FALSE,
                            size = 1.15 * jaspGeomLine$default_aes$size) +
      ggplot2::geom_text(nudge_x = 0.15, size = .35 * getGraphOption("fontsize"), hjust = 0,
                         parse = parse) +
      ggplot2::xlim(c(0, 1)) +
      getEmptyTheme()

  }

  if (!is.null(fill))
    legendPlot <- legendPlot + ggplot2::scale_fill_manual(values = rev(fill))
  if (!is.null(colors))
    legendPlot <- legendPlot + ggplot2::scale_color_manual(values = rev(colors))
  if (!is.null(linetypes))
    legendPlot <- legendPlot + ggplot2::scale_linetype_manual(values = rev(linetypes))

  return(legendPlot)
}

makeBFlabels <- function(bfSubscripts, BFvalues, subs = NULL, bfTxt = NULL) {
  
  if (!is.null(bfTxt)) {
    lab <- paste0(bfTxt, " == ", format(BFvalues, digits = getGraphOption("digits")[["BF"]]))
  } else {
    if (is.null(subs))
      subs <- unlist(stringr::str_extract_all(bfSubscripts, "(?<=\\[).+?(?=\\])")) # get everything between []
    if (length(subs) != length(BFvalues))
      stop("bfSubscripts and BFvalues have different length!")
    lab <- paste0("BF[", subs[2:1], "]", "[", subs[1:2], "] == ",
                  format(BFvalues, digits = getGraphOption("digits")[["BF"]])
    )
  }
  return(parseThis(lab))
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
getBFSubscripts <- function(bfType = c("BF01", "BF10", "LogBF10"), hypothesis = c("equal", "smaller", "greater")) {
  
  bfType <- match.arg(bfType)
  hypothesis <- match.arg(hypothesis)
  
  if (bfType == "BF01") {
    subscripts <- switch (hypothesis,
      "equal"   = c("BF[1][0]",   "BF[0][1]"),
      "smaller" = c("BF['-'][0]", "BF[0]['-']"),
      "greater" = c("BF['+'][0]", "BF[0]['+']")
    )
    # subscripts <- switch (hypothesis,
    #   "equal"   = c("BF[0][1]",   "BF[1][0]"),
    #   "smaller" = c("BF[0]['-']", "BF['-'][0]"),
    #   "greater" = c("BF[0]['+']", "BF['+'][0]")
    # )
  } else if (bfType == "BF10") {
    subscripts <- switch (hypothesis,
      "equal"   = c("BF[1][0]",   "BF[0][1]"),
      "smaller" = c("BF['-'][0]", "BF[0]['-']"),
      "greater" = c("BF['+'][0]", "BF[0]['+']")
    )
  } else {
    subscripts <- switch (hypothesis,
      "equal"   = c("log(BF[0][1])",   "log(BF[1][0])"    ),
      "smaller" = c("log(BF[0]['-'])", "log(BF['-'][0])"),
      "greater" = c("log(BF[0]['+'])", "log(BF['+'][0])")
    )
  }
  return(parseThis(subscripts))
}

makeBFwheelAndText <- function(BF, bfSubscripts, pizzaTxt, drawPizzaTxt = is.null(pizzaTxt), bfType) {
  
  # drawBFpizza uses BF01
  bfSubscripts <- rev(bfSubscripts)
  if (bfType == "BF10") {
    BF01 <- 1 / BF
    BFvalues <- c(1 / BF, BF)
  } else if (bfType == "BF01") {
    BF01 <- 1 / BF
    BFvalues <- c(1 / BF, BF)
  } else { # LogBF10
    BF01 <- exp(-BF)
    BFvalues <- c(-BF, BF)
  }
  
  labels <- makeBFlabels(bfTxt = bfSubscripts, BFvalues = BFvalues)
  return(list(
      gTextBF = draw2Lines(labels, x = 0.7),
      gWheel = drawBFpizza(
      dat = data.frame(y = c(1, BF01)),
      labels = if (drawPizzaTxt) pizzaTxt else NULL
    )
  ))
}

#' @title Create a prior-posterior plot.
#'
#' @param dfLines A dataframe with \code{$x}, \code{$y}, and optionally \code{$g}.
#' @param dfPoints A dataframe with \code{$x}, \code{$y}, and optionally \code{$g}.
#' @param BF Numeric, with value of Bayes factor. This MUST correspond to bfType.
#' @param CRI Numeric of length 2, Credible interval of posterior.
#' @param median Numeric, median of posterior.
#' @param xName String or expression, displayed on the x-axis.
#' @param yName String or expression, displayed on the y-axis.
#' @param drawPizzaTxt Logical, should there be text above and below the pizza plot?
#' @param drawCRItxt Logical, should the credible interval be displayed in text?
#' @param bfType String, what is the type of BF? Options are "BF01", "BF10", or "LogBF10".
#' @param hypothesis String, what was the hypothesis? Options are "equal", "smaller", or "greater".
#' @param bfSubscripts String, manually specify the BF labels.
#' @param pizzaTxt String vector of length 2, text to be drawn above and below pizza plot.
#' @param bty List of three elements. Type specifies the box type, ldwX the width of the x-axis, lwdY the width of the y-axis.
#' @param CRItxt String, display the credible interval as \code{paste0(CRItxt, "[", lower, ", ", upper, "]")}.
#' @param medianTxt String, display the median as \code{paste(medianTxt, formatC(median, 3, format = "f"))}.
#' @param ... Unused.
#'
#' @return If BF, CRI, and median are all NULL a ggplot, otherwise a gtable.
#'
#' @example inst/examples/ex-PlotPriorAndPosterior.R
#"
#' @export
PlotPriorAndPosterior <- function(dfLines, dfPoints = NULL, BF = NULL, CRI = NULL, median = NULL, xName = NULL,
                                  yName = "Density", drawPizzaTxt = !is.null(BF), drawCRItxt = !is.null(CRI),
                                  bfType = c("BF01", "BF10", "LogBF10"),
                                  hypothesis = c("equal", "smaller", "greater"),
                                  bfSubscripts = NULL,
                                  pizzaTxt = hypothesis2BFtxt(hypothesis)$pizzaTxt, 
                                  bty = list(type = "n", ldwX = .5, lwdY = .5),
                                  CRItxt = "95% CI: ", medianTxt = "Median:",
                                  ...) {
  
	errCheckPlots(dfLines, dfPoints, CRI, median, BF)
  bfType <- match.arg(bfType)
  hypothesis <- match.arg(hypothesis)

  emptyPlot <- list()

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
      size = 1.0, height = maxheight)
    #maxheight / 8
    #)
    if (drawCRItxt) {
      labelsCRI <- paste0(CRItxt, "[",
                          bquote(.(formatC(dfCI$xmin, 3, format = "f"))), ", ",
                          bquote(.(formatC(dfCI$xmax, 3, format = "f"))), "]")
    }
  }

  if (!is.null(median)) {
  	labelsCRI <- c(labelsCRI, paste(medianTxt, formatC(median, 3, format = "f")))
  }

  if (length(labelsCRI) > 0) {

  	gTextCI <- draw2Lines(labelsCRI, x = 1, align = "right")
  } else {
  	gTextCI <- emptyPlot
  }

  xr   <- range(dfLines$x)
  idx  <- which.max(dfLines$y)
  xmax <- dfLines$x[idx]
  if (xmax > mean(xr)) {
    legend.position = c(0.15, 0.875)
  } else {
    legend.position = c(0.80, 0.875)
  }

  g <- themeJasp(graph = g, legend.title = "none", legend.position = legend.position, bty = bty) +
    theme(
      legend.text = element_text(margin = ggplot2::margin(0, 0, 2, 0)),
      legend.key.height = unit(1, "cm"),
      legend.key.width = unit(1.5, "cm")
    )

  if (!is.null(BF)) {
    if (is.null(bfSubscripts))
      bfSubscripts <- getBFSubscripts(bfType, hypothesis)
    
    tmp <- makeBFwheelAndText(BF, bfSubscripts, pizzaTxt, drawPizzaTxt, bfType)
    gTextBF <- tmp$gTextBF
    gWheel <- tmp$gWheel

  } else {
    gWheel <- emptyPlot
    gTextBF <- emptyPlot
  }

  topPlotList <- list(gTextBF, gWheel, gTextCI)
  if (all(lengths(topPlotList) == 0)) {
    plot <- g
  } else {

    idx <- lengths(topPlotList) == 0L
    layout <- matrix(1:3, 1, 3)
    layout[idx] <- NA_integer_
    layout <- rbind(layout, 4)
    plots2arrange <- c(topPlotList[!idx], list(g))
    
    f <- tempfile()
    png(f)
    plot <- arrangeGrob(
      grobs         = plots2arrange,
      heights       = c(.2, .8),
      layout_matrix = layout,
      widths        = c(.4, .2, .4)
    )
    dev.off()
    if (file.exists(f)) file.remove(f)

  }
  class(plot) <- c("JASPgraphs", class(plot))
  return(plot)
}

