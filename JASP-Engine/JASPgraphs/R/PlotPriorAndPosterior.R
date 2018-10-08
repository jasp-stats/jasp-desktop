#' @export
drawWheel <- function(dat, size = 2, show.legend = FALSE, label.cex = .75) {


	if (ncol(dat) == 1)
	  dat$group <- factor(seq_along(dat[[1]]))

	nms <- colnames(dat)
  mapping <- ggplot2::aes_string(x = 1, y = nms[1], group = nms[2], fill = nms[2], color = nms[2])

	# rotate the wheel so that smaller half is always facing up
	ma = max(dat[[1]])
	mi = min(dat[[1]])
	area = mi / (mi + ma)
	start = 0 + area * pi
	top = -.5*mi
	bottom = .5*ma

	return(
	    ggplot2::ggplot(data = dat, mapping = mapping) +
	        ggplot2::geom_bar(width = 1, stat = "identity", show.legend = show.legend, size = size) +
	        ggplot2::coord_polar("y", start = start) +
	        ggplot2::scale_fill_manual(values  = c("white", "darkred")) +
	        ggplot2::scale_color_manual(values = c("black", "black")) +
	        ggplot2::theme(
	            plot.margin      = grid::unit(c(2, 1, 2, 1.0), "cm"),
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


draw2Lines <- function(labels, parse = FALSE) {

  dfTextBF <- data.frame(
      x = 0,
      y = c(.4, .6),
      l = labels
    )
  return(
    ggplot2::ggplot(data = dfTextBF, ggplot2::aes(x = x, y = y, label = l)) +
      ggplot2::geom_text(size = .5 * JASPgraphs::getGraphOption("fontsize"), parse = parse) +
      ggplot2::scale_y_continuous(limits = c(0, 1)) +
      getEmptyTheme()
  )
}

#' @export
PlotPriorAndPosterior <- function(dfLines, dfPoints, dfCI = NULL, dfBF = NULL, xName = "", yName = "Density",
                              bfSubscripts = 0:1, ...) {

  ymax <- max(dfLines$y)
  newymax <-  1.1 * ymax
  maxheight <- (newymax - ymax)
  emptyPlot <- getEmptyPlot(FALSE)
  emptyTheme <- getEmptyTheme()
  plotArrange <- FALSE

  g <- ggplot2::ggplot(data = dfLines, ggplot2::aes(x = x, y = y, group = g, linetype = g)) +
      ggplot2::geom_line(size = 1.25) +
      scale_x_continuous(xName) +
      scale_y_continuous(yName)

  if (!is.null(dfPoints)) {
    g <- g + ggplot2::geom_point(data = dfPoints, ggplot2::aes(x = x, y = y), inherit.aes = FALSE,
                          size = 4, shape = 21, stroke = 1.25, fill = "grey")
  }
  if (!is.null(dfCI)) {
    if (is.null(dfCI$y))
      dfCI$y <- maxheight <- (newymax - ymax) / 2 + ymax

    g <- g + ggplot2::geom_errorbarh(
      data = dfCI, ggplot2::aes(y = y, xmin = xmin, xmax = xmax), inherit.aes = FALSE,
      size = 1.25, height = maxheight
    )
    labels <- paste("95% CI: [",
                 bquote(.(formatC(dfCI$y[1], 3, format="f"))), ", ",
                 bquote(.(formatC(dfCI$y[2], 3, format="f"))), "]", sep="")
    gTextCI <- draw2Lines(labels)
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

  g <- JASPgraphs::themeJasp(
    graph                   = g,
    legend.title            = "none"#,
    # legend.position         = "manual",
    # legend.coordinates      = legend.coordinates
  ) + ggplot2::theme(legend.position = legend.coordinates)

  if (!is.null(dfBF)) {
    gWheel <- drawWheel(dat = dfBF)
    labels <- paste0("BF[", bfSubscripts, "][", rev(bfSubscripts),  "] == ", digits = format(dfBF$y, digits = 3))
    gTextBF <- draw2Lines(labels, parse = TRUE)
    plotArrange <- TRUE
  } else {
    gWheel <- emptyPlot
    gTextBF <- emptyPlot
  }

  if (plotArrange) {
    lay <- rbind(c(1, 2, 3), matrix(4, 3, 3))
    return(gridExtra::arrangeGrob(gTextBF, gWheel, gTextCI, g, layout_matrix = lay))
  } else {
    return(g)
  }
}

xcoords   <- seq(0, 1, length.out = 1e3)
prior     <- dbeta(xcoords, 1, 1)
posterior <- dbeta(xcoords, 3, 2)


dfLines <- data.frame(
  x = xcoords,
  y = c(prior, posterior),
  g = factor(rep(c("prior", "posterior"), c(length(prior), length(posterior))))
)

dfPoints <- data.frame(
  x = 0.75,
  y = c(dbeta(0.75, 1, 1), dbeta(0.75, 3, 2)),
  g = factor(c("prior", "posterior"))
)

dfBF <- data.frame(
  y = c(dfPoints$y[1] / dfPoints$y[2], dfPoints$y[2] / dfPoints$y[1])
)

dfCI <- data.frame(
  xmin = .5,
  xmax = .7,
  y    = 2
)

# drawWheel(dat = dfBF)

# g0 <- PlotPriorAndPosterior(dfLines, dfPoints, xName = expression(theta))
# print(g0)
# g1 <- PlotPriorAndPosterior(dfLines, dfPoints, dfCI, NULL, expression(theta))
# gridExtra::grid.arrange(g1)
# g2 <- PlotPriorAndPosterior(dfLines, dfPoints, NULL, dfBF, expression(theta))
# gridExtra::grid.arrange(g2)
# undebug(PlotPriorAndPosterior)
g3 <- PlotPriorAndPosterior(dfLines, dfPoints, dfCI, dfBF, expression(theta))
gridExtra::grid.arrange(g3)


#   gridExtra::arrangeGrob(gPlot)
#
#   lay <- rbind(c(1, 2, 3), matrix(4, 3, 3))
#   grid.arrange(grobs = gs[1:4], layout_matrix = lay)
#
# library(gridExtra)
# library(grid)
# library(ggplot2)
# gs <- lapply(1:9, function(ii)
#   grobTree(rectGrob(gp=gpar(fill=ii, alpha=0.5)), textGrob(ii)))
#
