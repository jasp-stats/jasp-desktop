#' @export
drawWheel <- function(dat, size = 2, show.legend = FALSE, label.cex = .75) {


	if (ncol(dat) == 1)
	  dat$group <- factor(seq(dat[[1]]))

	nms <- colnames(dat)
  mapping <- ggplot2::aes_string(x = factor(1), y = nms[1], group = nms[2], fill = nms[2], color = nms[2])

	# rotate the wheel so that smaller half is always facing up
	ma = max(dat[[1]])
	mi = min(dat[[1]])
	area = mi / (mi + ma)
	start = 0 + area * pi

	return(
	    ggplot2::ggplot(data = dat, mapping = mapping) +
	        ggplot2::geom_bar(width = 1, stat = "identity", show.legend = show.legend, size = size) +
	        ggplot2::coord_polar("y", start = start) +
	        ggplot2::scale_fill_manual(values  = c("white", "darkred")) +
	        ggplot2::scale_color_manual(values = c("black", "black")) +
	        ggplot2::theme(
	            plot.margin      = grid::unit(rep(0, 4), "cm"),
	            panel.background = ggplot2::element_rect(fill = "white"),
	            panel.grid       = ggplot2::element_blank(),
	            axis.text        = ggplot2::element_blank(),
	            axis.ticks       = ggplot2::element_blank(),
	            axis.title       = ggplot2::element_blank(),
	            axis.line        = ggplot2::element_blank(),
	            legend.position  = "none"
	        )
	)
}

#' @export
getEmptyTheme <- function() {
  ggplot2::theme_void()
}

#' @export
getEmptyPlot <- function(axes = TRUE) {

  if (!axes) {
    ggplot2::ggplot() + ggplot2::geom_blank() + getEmptyTheme()
  } else {
    stop("Not implemented")
  }

}


draw2Lines <- function(labels, x = 0, parse = FALSE) {

	nLabels <- length(labels)
	y <- rep(.5, nLabels)
	diff <- seq(0, nLabels * 0.1, length.out = nLabels)
	diff <- diff - mean(diff)
	y <- y + diff
  dfText <- data.frame(
      x = x,
      y = y,
      l = labels
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

#' @export
PlotPriorAndPosterior <- function(dfLines, dfPoints = NULL, CRI = NULL, median = NULL, BF = NULL, xName = "", yName = "Density",
																	hypothesis = c("equal", "smaller", "greater"), addDataText = !is.null(BF), ...) {

	hypothesis <- match.arg(hypothesis)
	switch(
		hypothesis,
		"equal" = {
			bfSubscripts <- 0:1
			wheelTxt <- c("data | H0", "data | H1")
		},
		"smaller" = {
			bfSubscripts <- c(0, "\'-\'")
			wheelTxt <- c("data | H0", "data | H-")
		},
		"greater" = {
			bfSubscripts <- c(0, "\'+\'")
			wheelTxt <- c("data | H0", "data | H+")
		}
	)

	if (!all(is.data.frame(dfLines), !is.null(dfLines$x), !is.null(dfLines$y), !is.null(dfLines$g)))
		stop("dfLines should be a data.frame with $x, $y, and $g!")
	if (!is.null(dfPoints) && !all(is.data.frame(dfPoints), !is.null(dfPoints$x), !is.null(dfPoints$y), !is.null(dfPoints$g)))
		stop("dfPoints should be a data.frame with $x, $y, and $g!")
	if (errCheckPlotPriorAndPosterior(CRI, 2L))
		stop("CRI should be numeric and have length 2! (left bound, right bound)")
	if (errCheckPlotPriorAndPosterior(median))
		stop("median should be numeric and have length 1!")
	if (errCheckPlotPriorAndPosterior(BF))
		stop("BF should be numeric and have length 1!")

  ymax <- max(dfLines$y)
  newymax <-  1.15 * ymax
  maxheight <- (newymax - ymax)
  emptyPlot <- getEmptyPlot(FALSE)
  plotArrange <- FALSE

  g <- ggplot2::ggplot(data = dfLines, ggplot2::aes(x = x, y = y, group = g, linetype = g)) +
      ggplot2::geom_line(size = 1.25) +
      scale_x_continuous(xName) +
      scale_y_continuous(yName)

  if (!is.null(dfPoints)) {
    g <- g + ggplot2::geom_point(data = dfPoints, ggplot2::aes(x = x, y = y), inherit.aes = FALSE,
                          size = 4, shape = 21, stroke = 1.25, fill = "grey")
  }

  labelsCRI <- NULL

  if (!is.null(CRI)) {
  	dfCI <- data.frame(
  		xmin = CRI[1],
  		xmax = CRI[2],
  		y    = maxheight <- (newymax - ymax) / 2 + ymax
  	)

    g <- g + ggplot2::geom_errorbarh(
      data = dfCI, ggplot2::aes(y = y, xmin = xmin, xmax = xmax), inherit.aes = FALSE,
      size = 1.25, height = maxheight / 8
    )
    labelsCRI <- paste("95% CI: [",
    							 			bquote(.(formatC(dfCI$xmin, 3, format = "f"))), ", ",
    							 			bquote(.(formatC(dfCI$xmax, 3, format = "f"))), "]", sep = "")
    plotArrange <- TRUE
  }

  if (!is.null(median)) {
  	labelsCRI <- c(labelsCRI, paste("Median:", formatC(median, 3, format = "f")))
  }

  if (length(labelsCRI) > 0) {
  	gTextCI <- draw2Lines(labelsCRI, x = -1)
  	plotArrange <- TRUE
  } else {
  	gTextCI <- emptyPlot
  }



  xr   <- range(dfLines$x)
  idx  <- which.max(dfLines$y)
  xmax <- dfLines$x[idx]
  if (xmax > mean(xr)) {
    legend.coordinates = c(0.15, 0.875)
  } else {
    legend.coordinates = c(0.85, 0.875)
  }

  g <- themeJasp(
    graph                   = g,
    legend.title            = "none",
    legend.position         = legend.coordinates
    # legend.coordinates      = legend.coordinates
  )# + ggplot2::theme(legend.position = legend.coordinates)

  if (!is.null(BF)) {
  	dfBF <- data.frame(y = c(1 / BF, BF))
    gWheel <- drawWheel(dat = dfBF)
    labels <- paste0("BF[", bfSubscripts, "][", rev(bfSubscripts),  "] == ", digits = format(dfBF$y, digits = 3))
    gTextBF <- draw2Lines(labels, parse = TRUE)
    if (addDataText) {
    	dfTxt <- data.frame(
    		x     = 2,
    		y     = c(dfBF$y[2] / 2, dfBF$y[2] + dfBF$y[1] / 2),
    		label = wheelTxt
    	)
    	gWheel <- gWheel + ggplot2::geom_text(data = dfTxt, aes(x = x, y = y, label = label),
						size = .75 * getGraphOption("fontsize"), inherit.aes = FALSE)
    }
    plotArrange <- TRUE
  } else {
    gWheel <- emptyPlot
    gTextBF <- emptyPlot
  }

  if (plotArrange) {
    lay <- rbind(c(2, 1, 3), matrix(4, 3, 3))
    return(gridExtra::arrangeGrob(gWheel, gTextBF, gTextCI, g, layout_matrix = lay))
  } else {
    return(g)
  }
}
