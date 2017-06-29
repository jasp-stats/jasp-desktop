.graphOptions <- list2env(list(
        fontsize = 18,
        legend.cex = 1.25,
        legend.position = "auto",
        axis.title.cex = 1.5,
        family = NULL,
        legend.coordinates = list(left = .15,
                               mid = .5,
                               right = .8,
                               top = .75,
                               mid = .5,
                               bottom = .25),
        horizontal = FALSE,
        bty = "n"
))

#' @export
getGraphOption <- function(x) {

    return(get(x, envir = .graphOptions))

}

setGraphOption <- function (name, value) {

    assign(name, value, envir = .graphOptions)
    return(invisible(value))

}

#' @export
graphOptions <- function(...) {

    args <- list(...)

    if (!(length(args) && is.null(names(args)))) {

        if (length(args)) {

            for (i in seq_along(args)) {
                setGraphOption(names(args)[[i]], args[[i]])
            }

            return(invisible(args))

        } else {

            return(as.list(.graphOptions))

        }
    }

    args <- unlist(args)
    out <- as.list(.graphOptions)[args]

    if (length(out) == 1)
        out <- out[[1]]

    return(out)
}


# general ----

#' @export
themeJasp = function(graph, xName, yName,
                     plotType = NULL,
                     axis.title.cex = graphOptions("axis.title.cex"),
                     bty = graphOptions("bty"),
                     fontsize = graphOptions("fontsize"),
                     family = graphOptions("family"),
                     horizontal = FALSE,
                     legend.cex = graphOptions("legend.cex"),
                     legend.position = graphOptions("legend.position"),
                     legend.coordinates = graphOptions("legend.coordinates"),
                     legend.title = "auto",
                     # legend.position.left = graphOptions("legend.position.left"),
                     # legend.position.right = graphOptions("legend.position.right"),
                     xyMargin = "auto") {

    # "auto" always means: base this on the graph object.
    # In documentation "otherwise" refers to non "auto" usage.

    # graph: graph object to add theme to
    # fontsize: general size of text
    # legend.title: "auto" for default, "none" for omitting or a element_text element
    # legend.position: otherwise as described in ?ggplot2::theme
    # xyMargin: otherwise a list where the 1st element is the xMargin and the second the yMargin. Both a numeric vector of length 4
    # legend.cex: legend text has size fontsize*legend.cex
    # axis.title.cex = axis label has size fontsize*axis.title.cex
    # family: font family
    # legend.position.left/ legend.position.right: where to place the legend? (units relative to plot window; c(.5, .5) = center)

    # error handling ----
    if (is.character(legend.title)) {

        if (legend.title == "auto") {

            legend.title = ggplot2::element_text(family = family, size = fontsize, hjust = 0.5)

        } else if (legend.title == "none") {

            legend.title = ggplot2::element_blank()

        } else {

            warning("legend.title not understood, omitted instead")
            legend.title = ggplot2::element_blank()

        }

    } else if (!inherits(class(legend.title), "element")) {

        warning("legend.title not understood, omitted instead")
        legend.title = ggplot2::element_blank()

    }

    # actually doing stuff ----
    gBuild <- ggplot2::ggplot_build(graph)


    # TRUE if graph contains data, FALSE if graph is input from drawCanvas()
    hasData <- !identical(gBuild[["data"]], list(data.frame()))

    # possibly redundant.
    if (xyMargin == "auto") {

        # 	    if (horizontal) {
        #
        #     		# get size of axis tick labels
        #     		xBreaks = gBuild$layout$panel_ranges[[1]]$x.labels
        # 			if (is.factor(xBreaks))
        #     		    xBreaks = levels(xBreaks)
        # 			xCex = max(c(0, max(nchar(xBreaks, type = "width")) - 4))
        #     		yCex = 0
        #
        # 	    } else {
        #
        # yBreaks = gBuild$layout$panel_ranges[[1]]$y.labels
        # if (is.factor(yBreaks))
        #     yBreaks = levels(yBreaks)

        xCex <- 0
        yCex <- 0 #max(c(0, max(nchar(yBreaks, type = "width")) - 4))

        # }

        # margins are c(bottom, left, top right)
        xMargin <- c(20 + 5 * xCex, 0, 0, 0) # margin x-label to x-axis
        yMargin <- c(0, 20 + 5 * yCex, 0, 0) # margin y-label to y-axis

    } else {

        xMargin <- xyMargin[[1]]
        yMargin <- xyMargin[[2]]

    }

    # determine legend position
    if (hasData && legend.position == "auto") {

        if (inherits(gBuild$plot$coordinates, "CoordCartesian")) { # normal coordinates

            xCoords <- unlist(lapply(gBuild$data, `[[`, "x"))
            yCoords <- unlist(lapply(gBuild$data, `[[`, "y"))

            idxYmax = which.max(yCoords)[1]
            xAtYmax = xCoords[idxYmax]

            plotCenter = mean(gBuild$layout$panel_ranges[[1]]$x.range, na.rm = TRUE)

            if (isTRUE(xAtYmax > plotCenter)) { # mode right of middle

                legendXY = c(legend.coordinates[["left"]], legend.coordinates[["top"]])

            } else { # mode left of middle

                legendXY = c(legend.coordinates[["right"]], legend.coordinates[["top"]])

            }

            if (!is.null(plotType)) {
                if (plotType == "priorPosterior")
                    legendXY[2] <- legend.coordinates[["top"]]
            }

        } else if (inherits(gBuild$plot$coordinates, "CoordPolar")) { # polar coordinates

            legendXY = "none"

        } else { # something went wrong

            legendXY = "none"

        }
    } else if (legend.position == "topright") {

        legendXY = c(legend.coordinates[["right"]], legend.coordinates[["top"]])

    } else {

        legendXY = "none"

    }

    # remake R's bty = "n" ----
    if (bty == "n") {

        # browser()
        # panelRanges <- gBuild$layout$panel_ranges[[1]]
        # anyXhasLength0 <- any(lengths(panelRanges[1:7]) == 0)
        # anyYhasLength0 <- any(lengths(panelRanges[8:14]) == 0)
        # if (anyXhasLength0 || anyYhasLength0) {
        #
        #     prettyBreak <- getPrettyAxisBreaks(gBuild$data[[2]])
        #     if (anyXhasLength0)
        #
        # }
        # browser()
        xLim <- range(gBuild$layout$panel_ranges[[1]]$x.major_source)
        yLim <- range(gBuild$layout$panel_ranges[[1]]$y.major_source)
        dfX <- data.frame(y = -Inf, yend = -Inf, x = xLim[1], xend = xLim[2])
        dfY <- data.frame(x = -Inf, xend = -Inf, y = yLim[1], yend = yLim[2])

        # xLine <- ggplot2::annotate(geom = "segment", y = -Inf, yend = -Inf, x = xLim[1], xend = xLim[2], lwd = 2.5, inherit.aes = FALSE)
        # yLine <- ggplot2::annotate(geom = "segment", x = -Inf, xend = -Inf, y = yLim[1], yend = yLim[2], lwd = 2.5, inherit.aes = FALSE)
        # xLine <- ggplot2::geom_segment(y = -Inf, yend = -Inf, x = xLim[1], xend = xLim[2], lwd = 2.5,
        #                                position = ggplot2::PositionIdentity, stat = ggplot2::StatIdentity)
        # yLine <- ggplot2::geom_segment(x = -Inf, xend = -Inf, y = yLim[1], yend = yLim[2], lwd = 2.5,
        #                                position = ggplot2::PositionIdentity, stat = ggplot2::StatIdentity)
        # browser()
        mapLines <- ggplot2::aes(x = x, y = y, xend = xend, yend = yend)
        xLine <- ggplot2::geom_segment(data = dfX, mapping = mapLines, lwd = 2.5,
                                       position = ggplot2::PositionIdentity, stat = ggplot2::StatIdentity, inherit.aes = FALSE)
        yLine <- ggplot2::geom_segment(data = dfY, mapping = mapLines, lwd = 2.5,
                                       position = ggplot2::PositionIdentity, stat = ggplot2::StatIdentity, inherit.aes = FALSE)

        graph <- graph + xLine + yLine
    }


    if (horizontal) {

        graph <- graph + ggplot2::coord_flip()

    }

    graph <- graph + themeJaspRaw(legend.position = legendXY, xMargin = xMargin, yMargin = yMargin,
                                  legend.cex = legend.cex, axis.title.cex = axis.title.cex, family = family,
                                  fontsize = fontsize, legend.title = legend.title)

    return(graph)

}

# for manual usage
#' @export
themeJaspRaw = function(legend.position = "none",
                        xMargin = c(0, 0, 0, 0),
                        yMargin = c(0, 0, 0, 0),
                        legend.cex = 1,
                        axis.title.cex = 1,
                        family = NULL,
                        fontsize = graphOptions("fontsize"),
                        legend.title = ggplot2::element_text(family = family, size = fontsize, hjust = 0.5)) {

    ggplot2::theme(
        # axis
        axis.line = ggplot2::element_blank(),
        axis.text = ggplot2::element_text(family = family, size = fontsize),
        axis.ticks.length = grid::unit(.5, "cm"), # tick length
        axis.ticks.x = x_custom(size = 1.25, color = "black"),
        axis.ticks.y = y_custom(size = 1.25, color = "black"),
        axis.title = ggplot2::element_text(family = family, size = axis.title.cex*fontsize),
        # axis.ticks = ggplot2::element_line(size = 1.25, color = "black"), # tick width
        axis.title.x = ggplot2::element_text(margin = ggplot2::margin(xMargin)),
        axis.title.y = ggplot2::element_text(margin = ggplot2::margin(yMargin)),

        # legend
        legend.background = ggplot2::element_rect(color = "white", fill = "white"),
        legend.key = ggplot2::element_rect(color = "white", fill = "white"),
        legend.key.size = grid::unit(2, "cm"),
        legend.text = ggplot2::element_text(family = family, size = legend.cex*fontsize),
        legend.title = legend.title, # ggplot2::element_text(family = family, size = fontsize, hjust = 0.5),
        legend.position = legend.position,

        # panel
        panel.spacing = grid::unit(2, "cm"),
        panel.grid = ggplot2::element_blank(),
        panel.background = ggplot2::element_rect(color = "white", fill = "white"),
        plot.title = ggplot2::element_text(family = family, size = fontsize, hjust = 0.5) # center title
    )

}

# see http://stackoverflow.com/questions/43050399/ggplot-with-bty-n-or-how-to-add-grid-coordinates-to-plot-coordinates?noredirect=1&lq=1
x_custom <- function(...) {
  structure(
    list(...), # this ... information is not used, btw
    class = c("element_custom_x","element_blank", "element") # inheritance test workaround
  )

}
y_custom <- function(...) {
  structure(
    list(...), # this ... information is not used, btw
    class = c("element_custom_y","element_blank", "element") # inheritance test workaround
  )

}

element_grob.element_custom_x <- function (element, x = 0:1, y = 0:1, colour = NULL, size = NULL,
                                           linetype = NULL, lineend = "butt", default.units = "npc", id.lengths = NULL,
                                           ...) {
    gp <- grid::gpar(lwd = ggplot2:::len0_null(size * ggplot2::.pt), col = colour,
                     lty = linetype, lineend = lineend)
    element_gp <- grid::gpar(lwd = ggplot2:::len0_null(element$size * ggplot2::.pt), col = element$colour,
                             lty = element$linetype, lineend = element$lineend)

    if (is.logical(element$arrow) && !element$arrow) {
        arrow <- NULL
    } else {
        arrow <- element$arrow
    }

    g1 <- grid::polylineGrob(x, y, default.units = default.units,
                             gp = utils::modifyList(element_gp, gp),
                             id.lengths = id.lengths, arrow = arrow, ...)

    vertical <- length(unique(element$x)) == 1 && length(unique(element$y)) >= 1

    g2 <- grid::editGrob(g1, y=y + grid::unit(2, "pt"),
                         gp=utils::modifyList(gp, list(col="black")),
                         name="new")

    return(grid::grobTree(g2, g1))

}

element_grob.element_custom_y <- function (element, x = 0:1, y = 0:1, colour = NULL, size = NULL,
                                           linetype = NULL, lineend = "butt", default.units = "npc", id.lengths = NULL,
                                           ...) {

    gp <- grid::gpar(lwd = ggplot2:::len0_null(size * ggplot2::.pt), col = colour,
                     lty = linetype, lineend = lineend)

    element_gp <- grid::gpar(lwd = ggplot2:::len0_null(element$size * ggplot2::.pt), col = element$colour,
                       lty = element$linetype, lineend = element$lineend)

    if (is.logical(element$arrow) && !element$arrow) {
        arrow <- NULL
    } else {
        arrow <- element$arrow
    }

    g1 <- grid::polylineGrob(x, y, default.units = default.units,
                       gp = utils::modifyList(element_gp, gp),
                       id.lengths = id.lengths, arrow = arrow, ...)

    g2 <- grid::editGrob(grob = g1, x = x + grid::unit(2, "pt"),
                         gp = utils::modifyList(gp, list(col="black")),
                         name = "new")

    grid::grobTree(g2, g1)

}

#' @export
grid_arrange_shared_legend <- function(..., plotList = NULL, nrow = 1, ncol = length(list(...)), position = c("bottom", "right")) {

    if (is.null(plotList)) {
        plots <- list(...)
    } else {
        plots <- plotList
    }
    position <- match.arg(position)
    g <- ggplot2::ggplotGrob(plots[[1]] + ggplot2::theme(legend.position = position))$grobs
    legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
    lheight <- sum(legend$height)
    lwidth <- sum(legend$width)
    gl <- lapply(plots, function(x) x + ggplot2::theme(legend.position = "none"))
    gl <- c(gl, nrow = nrow, ncol = ncol)

    # Todo: allow position == "left", "top" & "none".
    combined <- switch(position,
                       "bottom" = gridExtra::arrangeGrob(
                           do.call(gridExtra::arrangeGrob, gl),
                           legend,
                           ncol = 1,
                           heights = grid::unit.c(unit(1, "npc") - lheight, lheight)),
                       "right" = gridExtra::arrangeGrob(
                           do.call(gridExtra::arrangeGrob, gl),
                           legend,
                           ncol = 2,
                           widths = grid::unit.c(unit(1, "npc") - lwidth, lwidth)))

    #grid::grid.newpage()
    grid::grid.draw(combined)

}

# grid_arrange_shared_legend <- function(..., plotList = NULL, nrow = 1, ncol = length(list(...)), position) {
#
#     if (is.null(plotList)) {
#         plots <- list(...)
#     } else {
#         plots <- plotList
#     }
#     position <- match.arg(position, c("bottom", "right"))# TODO implement: , "top", "left", "none"))
    # g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
    # legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
#     lheight <- sum(legend$height)
#     lwidth <- sum(legend$width)
#     gl <- lapply(plots, function(x) x + theme(legend.position = "none"))
#     gl <- c(gl, nrow = nrow, ncol = ncol)
#
#     combined <- switch(position,
#                        "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
#                                               legend,
#                                               ncol = 1,
#                                               heights = unit.c(unit(1, "npc") - lheight, lheight)),
#                        "right" = arrangeGrob(do.call(arrangeGrob, gl),
#                                              legend,
#                                              ncol = 2,
#                                              widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
#     grid.newpage()
#     grid.draw(combined)
#
# }

getPrettyAxisBreaks <- function(dat) {

    if (!all(c("x", "y") %in% names(dat)))
        stop("INTERNAL: getPrettyAxisBreaks must get a dataframe that has a column named x and a column named y.")

    xBreaks <- switch(class(dat$x),
                      "numeric" = pretty(dat$x),
                      unique(dat$x)
    )
    yBreaks <- switch(class(dat$y),
                      "numeric" = pretty(dat$y),
                      unique(dat$y)
    )
    return(list(xBreaks = xBreaks, yBreaks = yBreaks))

}

#' @export
combinePlots = function(graph1, graph2, position = "posterior_pizza") {

	if (position == "posterior_pizza") {

		# read this as: graph 1 x coordinate
		g1x = 0; g1y = 0; g1w = 1; g1h = .75
		g2x = .38; g2y = .7; g2w = .3; g2h = .3

	} else if (is.list(position)) {


	}

	return(
		cowplot::ggdraw() +
			cowplot::draw_plot(graph1, x = g1x, y = g1y, width = g1w, height = g1h) +
			cowplot::draw_plot(graph2, x = g2x, y = g2y, width = g2w, height = g2h) #+
		# cowplot::draw_label(l0, x = .175, y = .875, size = fontsize) +
		# cowplot::draw_label(l1, x = .525, y = .95, size = fontsize) +
		# cowplot::draw_label(l2, x = .525, y = .75, size = fontsize) +
		# cowplot::draw_label(l3, x = .75, y = .875, size = fontsize)
	)

}

#' @export
ggplotBtyN <- function(graph = drawCanvas(), xBreaks = NULL, yBreaks = NULL, ...) {

    if (any(is.null(xBreaks), is.null(yBreaks)))
        gBuild = ggplot2::ggplot_build(graph)

    if (is.null(xBreaks))
        xBreaks = gBuild$layout$panel_ranges[[1]]$x.major_source

    if (is.null(yBreaks))
        yBreaks = gBuild$layout$panel_ranges[[1]]$y.major_source


    # get x-lim/ y-lim and add 5% of the total range
    xL = c(xBreaks[1], xBreaks[length(xBreaks)]) #+ c(-1, 1) * .02*diff(range(xBreaks))
    yL = c(yBreaks[1], yBreaks[length(yBreaks)]) #+ c(-1, 1) * .01 # *diff(range(xBreaks))

    return(
        graph +
            ggplot2::annotate(geom = "segment", y = -Inf, yend = -Inf,
                              x = xL[1], xend = xL[2], ...) +
            ggplot2::annotate(geom = "segment", x = -Inf, xend = -Inf,
                              y = yL[1], yend = yL[2], ...)

    )

}

# low-level plots ----
#' @export
addLabels <- function(graph, labels, positions = "posterior_pizza", fontsize = graphOptions("fontsize")) {

	if (positions == "posterior_pizza") {

		# read this as: label 1 x coordinate
		labelx = c(.175, .525, .525, .75)
		labely = c(.875, .95, .75, .875)

	} else if (is.list(positions)) {

	}

	for (l in seq_along(labels)) {

		graph = graph + cowplot::draw_label(labels[[l]], x = labelx[l], y = labely[l], size = fontsize)

	}


	return(
		graph
	)


}

#' @export
drawBars <- function(graph = drawCanvas(), dat, mapping = NULL, stat="identity", fill="gray80", show.legend = FALSE, ...) {

    if (is.null(mapping)) {

        nms <- colnames(dat)

        mapping <- switch(as.character(length(nms)),
                          "1" = ggplot2::aes_string(x = nms[1]),
                          "2" = ggplot2::aes_string(x = nms[1], y = nms[2]),
                          "3" = ggplot2::aes_string(x = nms[1], y = nms[2], group = nms[3], linetype = nms[3])
        )

    }

    if ("fill" %in% names(mapping)) {
        return(
            graph + ggplot2::geom_bar(data = dat, mapping = mapping,
                                      stat = stat, show.legend = show.legend, ...)
        )
    } else {
        return(
            graph + ggplot2::geom_bar(data = dat, mapping = mapping,
                                      stat = stat, fill = fill, show.legend = show.legend, ...)
        )
    }
}

#' @export
drawCanvas <- function(xName, yName, breaks = NULL, xBreaks = NULL, yBreaks = NULL, dat = NULL, xLabels = NULL, yLabels = NULL) {

    if (!is.null(dat) && is.null(breaks))
        breaks <- getPrettyAxisBreaks(dat)

    if (!is.null(breaks)) {
        if (is.null(xBreaks)) {
            xBreaks <- breaks$xBreaks
        }

        if (is.null(yBreaks)) {
            yBreaks <- breaks$yBreaks
        }
    }

    # perhaps mode?
    xLab <- switch(class(xBreaks),
                   "character" = ggplot2::scale_x_discrete(name = xName, breaks = unique(xBreaks), labels = unique(xBreaks)),
                   "factor" =  ggplot2::scale_x_discrete(name = xName, breaks = unique(xBreaks), labels = levels(xBreaks)),
                   "numeric" = ggplot2::scale_x_continuous(name = xName, breaks = xBreaks),
                   "integer" = ggplot2::scale_x_continuous(name = xName, breaks = xBreaks),
                   "NULL" = NULL
    )


    yLab <- switch(class(yBreaks),
                   "character" =  ggplot2::scale_y_discrete(name = yName, breaks = yBreaks, labels = yBreaks),
                   "factor" =  ggplot2::scale_y_discrete(name = yName, breaks = yBreaks, labels = yBreaks),
                   "numeric" = ggplot2::scale_y_continuous(name = yName, breaks = yBreaks),
                   "integer" = ggplot2::scale_y_continuous(name = yName, breaks = yBreaks),
                   "NULL" = NULL
    )


    if (!is.null(xLabels)) {
        if (length(xLabels) == length(xLab$breaks)) {
            xLab$labels <- xLabels
        } else {
            warning("length(xLabels) did not match length of breaks. Argument ignored.")
        }
    }

    if (!is.null(yLabels)) {
        if (length(yLabels) == length(yLab$breaks)) {
            yLab$labels <- yLabels
        } else {
            warning("length(yLabels) did not match length of breaks. Argument ignored.")
        }
    }


    graph <- ggplot2::ggplot() + # data.frame(x = 0, y = 0), ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_blank() + xLab + yLab # + ggplot2::geom_point()

    # graph <- ggplotBtyN(graph, xBreaks = xBreaks, yBreaks = yBreaks, size = 2)

    return(graph)

}

#' @export
drawHeatmap <- function(graph = drawCanvas(), dat, mapping = NULL, jaspColors = TRUE, interpolate = TRUE,
                        show.legend = FALSE, rotation = 0, n = 5, ...) {

    if (!rotation %in% c(0, 90, 180, 270))
        stop("rotation must be either c(0, 90, 180, 270).")

    if (rotation != 0) {

        if (is.data.frame(dat)) {

            x <- unique(dat$x)
            y <- unique(dat$y)
            dat <- matrix(dat$z, length(x), length(y))

        }

        dat <- rotateMatrix(dat, rotation = rotation)

    }

    # convert matrix to dataframe
    if (is.matrix(dat)) {

        dat <- data.frame(
            x = rep(seq_len(nrow(dat)), ncol(dat)),
            y = rep(seq_len(ncol(dat)), each = nrow(dat)),
            z = c(dat)
        )

    }

    if (is.null(mapping)) {

        nms <- colnames(dat)
        stopifnot(length(nms) == 3)
        mapping <- ggplot2::aes_string(x = nms[1], y = nms[2], fill = nms[3])

    }

    if (isTRUE(jaspColors)) {

        gradient <- ggplot2::scale_fill_gradientn(colours = c(colorBrewerJasp(n)))

    } else if (is.numeric(jaspColors) && jaspColors > 1) {

        # todo

    } else {

        gradient <- NULL

    }

    return(
        graph + ggplot2::geom_raster(data = dat, mapping = mapping, interpolate = interpolate,
                                     show.legend = show.legend) + gradient
    )
}

rotateMatrix <- function(x, rotation = 90) {

    # helper function for drawHeatmap()

    if (rotation == 90) {

        return(t(apply(x, 2, rev)))

    } else if (rotation == 180) {

        return(rotateMatrix(rotateMatrix(x)))

    } else { # rotation  == 270

         return(apply(t(x), 2, rev))
    }
}

#' @export
drawLines <- function(graph = drawCanvas(), dat, mapping = NULL, size = 1.25, show.legend = TRUE, ...) {

    if (is.null(mapping)) {

        nms = colnames(dat)

        mapping <- switch(as.character(length(nms)),
                          "2" = ggplot2::aes_string(x = nms[1], y = nms[2]),
                          "3" = ggplot2::aes_string(x = nms[1], y = nms[2], color = nms[3])
        )

    } else if (is.character(mapping)) {

        mapping <- switch(mapping,
            "PriorPosterior" = ggplot2::aes_string(x = nms[1], y = nms[2], linetype = nms[3])
        )

    }

    return(
        graph +
            ggplot2::geom_line(data = dat, mapping = mapping, size = size, show.legend = show.legend, ...)
    )

}

#' @export
drawPoints <- function(graph = drawCanvas(), dat, mapping = NULL, size = 1.25,
                       shape = 21, fill = "gray", show.legend = TRUE, ...) {

    if (is.null(mapping)) {

        nms = colnames(dat)

        mapping <- switch(as.character(length(nms)),
                          "2" = ggplot2::aes_string(x = nms[1], y = nms[2]),
                          "3" = ggplot2::aes_string(x = nms[1], y = nms[2], color = nms[3], shape = nms[3])
        )

    }

    args = list(data = dat, mapping = mapping, size = size, shape = shape, fill = fill, show.legend = show.legend, ...)
    args[names(args) %in% names(mapping)] <- NULL

    return(graph + do.call(ggplot2::geom_point, args))

}

#' @export
drawSmooth <- function(graph = NULL, dat = NULL, mapping = NULL, size = 2,
                       color = "gray", show.legend = FALSE, se = FALSE, ...) {

    if (is.null(dat) && is.null(graph))
        stop("Argument dat and graph cannot both be NULL.")

    if (is.null(dat))
        dat <- ggplot2::ggplot_build(graph)$data[[1]][c("x", "y")]

    if (is.null(graph))
        graph <- drawCanvas()

    if (is.null(mapping))
        mapping = ggplot2::aes_(x = dat[[1]], y = dat[[2]])

    return(
        graph +
            ggplot2::geom_smooth(data = dat, mapping = mapping,
                                 size = size, color = color, show.legend = show.legend, se = se, ...)
    )

}

#' @export
drawViolin <- function(graph = drawCanvas(), dat = NULL, mapping = NULL, size = 2,
                       show.legend = FALSE) {

    if (is.null(mapping)) {

        mapping <- switch(as.character(length(dat)),
            "2" = ggplot2::aes_(x = dat[[1]], y = dat[[2]]),
            "3" = ggplot2::aes_(x = dat[[1]], y = dat[[2]], color = dat[[3]])
        )
    }


    return(
        graph +
            geom_violin(mapping = NULL, data = NULL, show.legend = show.legend, ...)
    )

}

#' @export
drawWheel = function(graph = NULL, dat, mapping = NULL, size = 3, show.legend = FALSE, label.cex = .75) {

	if (is.null(mapping)) {

		if (ncol(dat) == 1)
		    dat$group <- factor(seq_along(dat[[1]]))

		nms <- colnames(dat)
		mapping <- ggplot2::aes_string(x = 1, y = nms[1], group = nms[2], fill = nms[2], color = nms[2])

	}

	# rotate the wheel so that smaller half is always facing up
	ma = max(dat$y)
	mi = min(dat$y)
	area = mi / (mi + ma)
	start = 0 + area * pi
	top = -.5*mi
	bottom = .5*ma

	return(
	    ggplot2::ggplot(data = dat, mapping = mapping) +
	        ggplot2::geom_bar(width = 1, stat = "identity", show.legend = show.legend, size = size) +
	        ggplot2::coord_polar("y", start = start) +
	        ggplot2::scale_fill_manual(values = c("white", "darkred")) +
	        ggplot2::scale_color_manual(values = c("black", "black")) +
	        ggplot2::theme(
	            plot.margin = grid::unit(c(2, 1, 2, 1.0), "cm"),
	            panel.background = ggplot2::element_rect(fill = "white"),
	            panel.grid = ggplot2::element_blank(),
	            axis.text = ggplot2::element_blank(),
	            axis.ticks = ggplot2::element_blank(),
	            axis.title = ggplot2::element_blank(),
	            axis.line = ggplot2::element_blank(),
	            legend.position = "none"
	        )
	)
}


# high-level plots ----
#' @export
priorPosteriorPlot <- function(dat, xName, yName = "Density") {

    stopifnot(length(dat) == 3)
    nms1 <- names(dat[[1]])
    nms2 <- names(dat[[2]])
    breaks <- getPrettyAxisBreaks(dat[[1]])

    linesArgs <- list(mapping = ggplot2::aes_string(x = nms1[1], y = nms1[2], linetype = nms1[3]), color = "black")
    pointsArgs <- list(ggplot2::aes_string(x = nms2[1], y = nms2[2]), color = "black", fill = "gray", size = 4, shape = 21, stroke = 1.25)
    g <- makeGraph(dat = dat, xName = xName, yName = yName, breaks = breaks,
                   graphType = c("drawLines", "drawPoints"),
                   graphArgs = list(linesArgs, pointsArgs),
                   themeArgs = list(plotType = "priorPosteriorPlot", legend.title = "none"))

    if (!is.null(dat)) {

        g2 <- drawWheel(dat = dat[[3]])
        g3 <- combinePlots(g, g2, position = "posterior_pizza")

        l1 = bquote(atop(BF[0][1] == .(dat[[3]]$y[1]), BF[1][0] == .(dat[[3]]$y[2]))) # BF text
        l2 = "Data|H0" # Pizza plot text
        l3 = "Data|H1" # Pizza plot text

        medianText = paste("median = ", .5)
        CIlow = .1; CIhigh = .2
        CIText = paste("95% CI: [",  bquote(.(formatC(CIlow,3, format="f"))), ", ",
                       bquote(.(formatC(CIhigh,3, format="f"))), "]", sep="")

        # phantom so text is right aligned
        l4 = bquote(atop(phantom(.(CIText))~.(medianText), phantom(.(medianText))~.(CIText)))
        labels = list(l1, l2, l3, l4)

        g = addLabels(g3, labels, position = "posterior_pizza")

    }
    return(g)

}

# convenience plots ----
#' @export
makeGraph <- function(dat, graphType, xName, yName, breaks = NULL, graphArgs = NULL, themeArgs = NULL) {

    # attempt to understand "graphType"
    graphFun <- understandGraphType(graphType = graphType)

    # make an empty graph
    graph <- drawCanvas(xName = xName, yName = yName, breaks = breaks)

    if (!is.null(dat)) { # if there is data

        # do some checks on the length of the graphArguments
        if (!is.null(graphArgs) && length(graphArgs) != length(graphFun)) {

            if (length(graphArgs) != 1L)
                stop(sprintf("Argument graphArgs has length %d but it must have either length 1 or length(graphType) (%d).",
                             length(graphArgs), length(graphFun)))

            graphArgs <- rep_len(graphArgs, length.out = length(graphFun))

        }

        # add the plots
        for (i in seq_along(graphFun)) {
            graph <- do.call(what = graphFun[[i]], args = c(list(graph = graph, dat = dat[[i]]), graphArgs[[i]]))
        }
    }

    graph <- ggplotBtyN(graph = graph)
    graph <- do.call(themeJasp, c(list(graph = graph), themeArgs))

    return(graph + ggplot2::xlab(xName) + ggplot2::ylab(yName))

}

understandGraphType <- function(graphType) {

    # lazy vectorization
    if (length(graphType) > 1) {
        return(lapply(graphType, understandGraphType))
    }

    # actual function
    if (is.function(graphType)) {

        graphName <- as.character(substitute(graphType))
        if (!exists(graphName, where = asNamespace("JASPgraphs"), mode = "function"))
            stop(sprintf("Argument 'graphType = %s' is not a function in JASPgraphs.", graphName))

        graphFun <- graphType

    } else if (is.character(graphType)) {

        graphType <- tolower(graphType) # ignore any case
        if (substr(graphType, 1, 4) != "draw") { # if missing 'draw' at the beginning
            graphType <- paste0("draw", graphType)
        }
        # ensure camelcaps
        substr(graphType, 5, 5) <- toupper(substr(graphType, 5, 5))

        # all currently available graphs
        allGraphs <- c(
            "drawLines",
            "drawPoints",
            "drawBars",
            "drawHeatmap",
            # high level plots
            "priorPosteriorCor"
        )

        # find match between available and existing graphtypes
        choice <- pmatch(x = graphType, table = allGraphs, nomatch = NA)

        if (is.na(choice)) # uh oh...
            stop(sprintf("Argument graphType is misspecified. I understood: %s", as.character(graphType[1])))

        # get the function from namespace to avoid any mixups
        if (substr(allGraphs[choice], 1, 4) == "draw") {

            graphFun <- getFromNamespace(x = allGraphs[choice], ns = "JASPgraphs")

        } else {

            graphFun <- switch(choice,
               "priorPosteriorCor" = list(drawLines, drawPoints, drawWheel, combinePlots)

            )

        }

    } else {

        warning(sprintf("Argument 'graphType' is not a function or character string but of class: %s", class(graphType)))
        return(NULL)

    }

    return(graphFun)

}

areArgumentsUsed <- function(args, fun, verbose = TRUE) {

    # probably won't work since arguments are passed via ...
    if (is.list(names)) {
        nmsArgs <- names(args)
    } else {
        nmsArgs <- args
    }

    funArgs <- names(formals(fun))
    rmArgs <- !(nmsArgs %in% funArgs)
    unUsedArgs <- nmsArgs[rmArgs]


    argsTheme <- names(formals(themeJasp))
    rmArgsGraph <-!(argsGraph %in% names(graphArgs))
    rmArgsTheme <-!(argsTheme %in% names(themeArgs))
    unUsedGraph <- argsGraph[rmArgsGraph]
    unUsedTheme <- argsTheme[rmArgsTheme]

    # notify a user of any unused arguments
    if (length(unUsedGraph) > 0)
        warning(paste0("The following arguments are unused in argsGraph: ",
                       paste(rmArgsGraph, collapse = ", ")))
}

setLegendNames <- function(graph, names) {

    if (!is.list(graph) && identical(names(graph), c("data", "layout", "plot"))) {

        gBuild <- graph

    } else if (!is.ggplot(graph)) {

        stop("input must be a ggplot object")

    } else {

        gBuild = ggplot2::ggplot_build(graph)

    }

    gBuild$plot$labels

    for (i in seq_along(gBuild$plot$layers)) {

            class(gBuild$plot$layers[[i]]$show.legend)
    }
    gBuild$plot$layers

    gBuild$plot$layers[[1]]$geom
    gBuild$plot$layers[[2]]$geom
    gBuild$plot$layers[[3]]$geom
}

# unused ----
#' @export
colorBrewerJasp = function(n) {

    # n should be the number of (interpolated) colors returned
    return(grDevices::colorRampPalette(c("#00a9e6", "#00ba63", "#fb8b00", "#c75327", "#909090"))(n))

}

colorGradientJasp = function(n) {

    # n should be an intensity measure in the domain [0-1]

    gradient = grDevices::colorRamp(c("#00a9e6", "#00ba63", "#fb8b00", "#c75327", "#909090"))(n)
    # convert rgb to hex
    gradient = apply(gradient, 1, function(x) rgb(x[1], x[2], x[3], maxColorValue = 255))
    return(gradient)

}

# deprecated?
#' @export
multiplot <- function(..., plotList = NULL, file, cols = 1, layout=NULL) {

    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotList)

    numPlots = length(plots)

    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots / cols)),
                         ncol = cols, nrow = ceiling(numPlots / cols))
    }

    if (numPlots == 1) {

        print(plots[[1]])

    } else {

        # Set up the page
        grid::grid.newpage()
        grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow(layout), ncol(layout))))

        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

            print(plots[[i]], vp = grid::viewport(layout.pos.row = matchidx$row,
                                                  layout.pos.col = matchidx$col))

        }
    }
}