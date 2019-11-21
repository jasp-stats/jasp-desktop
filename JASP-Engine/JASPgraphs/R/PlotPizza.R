#' @title Make a pizza plot
#' @param dat data.frame with \code{$y}, or vector.
#' @param linewidth Scalar, linewidth of the black lines around the pizza.
#' @param scaleText Scalar > 0, the size of the text is equal to \code{scaleText * getGraphOption("fontsize")}.
#' @param show.legend Logical, should a legend be shown?
#' @param labels String vector of length 2, text to be shown above and below plot.
#'
#' @examples
#' JASPgraphs::drawBFpizza(c(1, 4))
#'
#' JASPgraphs::drawBFpizza(
#'   dat = data.frame(y = c(1, 4)),
#'  labels = c("data | H0", "data | H1")
#' )
#'
#' @export
drawBFpizza <- function(dat, linewidth = 1, scaleText = 0.3, show.legend = FALSE, labels = NULL) {

  # NOTE: this function uses polar coordinates! this implies:
  # x = distance from center
  # y = rotation from origin
  
  if (scaleText < 0)
    stop("scaleText should be positive!")
  if (linewidth < 0)
    stop("linewidth should be positive!")

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
	  ggplot2::geom_bar(width = 1, stat = "identity", show.legend = show.legend, size = linewidth) +
	  ggplot2::scale_x_discrete(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
	  ggplot2::coord_polar(theta = "y", start = start) +
	  ggplot2::scale_fill_manual(values  = c("darkred", "white")) +
	  ggplot2::scale_color_manual(values = c("black", "black")) +
	  getEmptyTheme()

	if (!is.null(labels)) {
	  dfTxt <- data.frame(
	    x = 2.1,                                           # distance from wheel
	    y = c(dat$y[2L] / 2.0, dat$y[2L] + dat$y[1L] / 2), # rotation around wheel
	    l = labels
	  )
	  g <- g + ggplot2::geom_text(data = dfTxt, aes(x = .data$x, y = .data$y, label = .data$l),
                                parse = needsParsing(labels),
	                              size = scaleText * getGraphOption("fontsize"), inherit.aes = FALSE)
	}

	return(g)
}
