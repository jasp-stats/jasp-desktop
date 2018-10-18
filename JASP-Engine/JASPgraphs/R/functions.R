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
ggplotBtyN <- function(graph = drawAxis(), xBreaks = NULL, yBreaks = NULL, ...) {

    if (any(is.null(xBreaks), is.null(yBreaks)))
        gBuild = ggplot2::ggplot_build(graph)

    xyBreaks <- getMajorSource(gBuild)
    if (is.null(xBreaks))
        xBreaks = xyBreaks$x

    if (is.null(yBreaks))
        yBreaks = xyBreaks$y


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

xAxisBreaksToAxisScale <- function(xBreaks = waiver(), xName = waiver(), xLabels = waiver(), xLimits = waiver(), position = "bottom", ...) {

    return(
        switch(class(xBreaks),
               "character" = ggplot2::scale_x_discrete(name = xName, breaks = xBreaks, labels = xLabels, limits = xLimits, position = position, ...),
               "factor" =    ggplot2::scale_x_discrete(name = xName, breaks = xBreaks, labels = xLabels, limits = xLimits, position = position, ...),
               "numeric" = ggplot2::scale_x_continuous(name = xName, breaks = xBreaks, labels = xLabels, limits = xLimits, position = position, ...),
               "integer" = ggplot2::scale_x_continuous(name = xName, breaks = xBreaks, labels = xLabels, limits = xLimits, position = position, ...),
               "NULL" = NULL
        )
    )
}

yAxisBreaksToAxisScale <- function(yBreaks = waiver(), yName = waiver(), yLabels = waiver(), yLimits = waiver(), position = "left", ...) {

    return(
        switch(class(yBreaks),
               "character" =  ggplot2::scale_y_discrete(name = yName, breaks = yBreaks, labels = yLabels, limits = yLimits, position = position, ...),
               "factor" =     ggplot2::scale_y_discrete(name = yName, breaks = yBreaks, labels = yLabels, limits = yLimits, position = position, ...),
               "numeric" =  ggplot2::scale_y_continuous(name = yName, breaks = yBreaks, labels = yLabels, limits = yLimits, position = position, ...),
               "integer" =  ggplot2::scale_y_continuous(name = yName, breaks = yBreaks, labels = yLabels, limits = yLimits, position = position, ...),
               "NULL" = NULL
        )
    )
}

#' @export
addAxis <- function(graph, breaks = NULL, name = waiver(), labels = waiver(), limits = waiver(), position = "left", ...) {

    if (position %in% c("left", "right")) {# vertical axis
        axis <- yAxisBreaksToAxisScale(yBreaks = breaks, yName = name, yLabels = labels, yLimits = limits, ...)
    } else { # horizontal axis
        axis <- xAxisBreaksToAxisScale(xBreaks = breaks, xName = name, xLabels = labels, xLimits = limits, ...)
    }

    return(graph + axis)

}

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
drawAxis <- function(graph = NULL, xName = waiver(), yName = waiver(), breaks = waiver(), xBreaks = waiver(),
                     yBreaks = waiver(), dat = NULL, xLabels = waiver(), yLabels = waiver(), xLimits = waiver(),
                     yLimits = waiver(), force = FALSE,
                     secondaryXaxis = waiver(), secondaryYaxis = waiver(),
                     xTrans = "identity", yTrans = "identity") {

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

    if (!is.null(xBreaks) && !is.waive(xBreaks) && is.waive(xLimits))
        xLimits <- range(xBreaks)

    if (!is.null(yBreaks) && !is.waive(yBreaks) && is.waive(yLimits))
        yLimits <- range(yBreaks)

    if (is.null(graph))
        graph <- ggplot2::ggplot()

    if (force && is.waive(graph[["data"]])) {
        dftmp <- data.frame(x = range(xBreaks), y = range(yBreaks))
        graph <- graph + ggplot2::geom_line(data = dftmp, mapping = ggplot2::aes(x = x, y = y), color = "white", alpha = 0)
    }
    graph <- graph + ggplot2::xlab(xName) + ggplot2::ylab(yName)

    if (!is.waive(secondaryXaxis) && !inherits(secondaryXaxis, "AxisSecondary"))
        secondaryXaxis <- do.call(ggplot2::sec_axis, secondaryXaxis)

    if (!is.waive(secondaryYaxis) && !inherits(secondaryYaxis, "AxisSecondary"))
        secondaryYaxis <- do.call(ggplot2::sec_axis, secondaryYaxis)

    if (length(graph[["layers"]]) > 0) {
        graph <- addAxis(graph, breaks = xBreaks, name = xName, labels = xLabels, limits = xLimits, position = "bottom", sec.axis = secondaryXaxis, trans = xTrans)
        graph <- addAxis(graph, breaks = yBreaks, name = yName, labels = yLabels, limits = yLimits, position = "left", sec.axis = secondaryYaxis, trans = yTrans)
    }

    return(graph)

}


#' @export
drawBars <- function(graph = drawAxis(), dat, mapping = NULL, stat="identity", fill="gray80", width = NULL, show.legend = FALSE, ...) {

    if (is.null(mapping)) {

        nms <- colnames(dat)

        mapping <- switch(as.character(length(nms)),
                          "1" = ggplot2::aes_string(x = nms[1]),
                          "2" = ggplot2::aes_string(x = nms[1], y = nms[2]),
                          "3" = ggplot2::aes_string(x = nms[1], y = nms[2], group = nms[3], linetype = nms[3])
        )

    }

    args = list(data = dat, mapping = mapping, fill = fill, stat=stat, width = width, show.legend = show.legend, ...)
    args[names(args) %in% names(mapping)] <- NULL

    return(graph + do.call(ggplot2::geom_bar, args))


    # if ("fill" %in% names(mapping)) {
    #     return(
    #         graph + ggplot2::geom_bar(data = dat, mapping = mapping,
    #                                   stat = stat, show.legend = show.legend, ...)
    #     )
    # } else {
    #     return(
    #         graph + ggplot2::geom_bar(data = dat, mapping = mapping,
    #                                   stat = stat, fill = fill, show.legend = show.legend, ...)
    #     )
    # }
}

#' @export
drawHeatmap <- function(graph = drawAxis(), dat, mapping = NULL, fillColor = TRUE, interpolate = FALSE,
                        show.legend = FALSE, rotation = c("0", "90", "180", "270"), n = 5,
                        geom = c("raster", "tile", "rect"),
                        ...) {

    geom <- match.arg(geom)
    rotation <- match.arg(rotation)

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

    if (isTRUE(fillColor)) {

        gradient <- ggplot2::scale_fill_gradientn(colours = c(colorBrewerJasp(n)))

    } else if (is.character(fillColor)) {

        gradient <- ggplot2::scale_fill_gradientn(colours = fillColor)

    } else if (is.numeric(fillColor) && fillColor > 1) {

        # todo

    } else {

        gradient <- NULL

    }

    args = list(data = dat, mapping = mapping, interpolate = interpolate, show.legend = show.legend, ...)
    args[names(args) %in% names(mapping)] <- NULL

    f <- getFromNamespace(paste0("geom_", geom), "ggplot2")
    if (geom != "raster")
        args[["interpolate"]] <- NULL

    return(
        graph + do.call(f, args) + gradient
    )

    # return(
    #     graph + ggplot2::geom_raster(data = dat, mapping = mapping, interpolate = interpolate,
    #                                  show.legend = show.legend) + gradient
    # )
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
drawLines <- function(graph = drawAxis(), dat, mapping = NULL, size = 1.25,
                      alpha = 1, show.legend = TRUE, ...) {

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

    args = list(data = dat, mapping = mapping, size = size, alpha = alpha, show.legend = show.legend, ...)
    args[names(args) %in% names(mapping)] <- NULL

    return(graph + do.call(ggplot2::geom_line, args))

    # return(
    #     graph +
    #         ggplot2::geom_line(data = dat, mapping = mapping, size = size, show.legend = show.legend, ...)
    # )

}

#' @export
drawPoints <- function(graph = drawAxis(), dat, mapping = NULL, size = 1.25,
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
drawSmooth <- function(graph = NULL, dat = NULL, mapping = NULL, size = 2, method = "auto",
                       color = "gray", show.legend = FALSE, se = FALSE, alpha = 1, ...) {

    if (is.null(dat) && is.null(graph))
        stop("Argument dat and graph cannot both be NULL.")

    if (is.null(dat))
        dat <- ggplot2::ggplot_build(graph)$data[[1]][c("x", "y")]

    if (is.null(graph))
        graph <- drawAxis()

    if (is.null(mapping))
        mapping = ggplot2::aes_(x = dat[[1]], y = dat[[2]])

    color <- scales::alpha(color, alpha) # workaround since somehow geom_smooth doesn't use alpha
    args = list(data = dat, mapping = mapping, size = size, color = color, se = se, alpha = alpha, show.legend = show.legend, ...)
    args[names(args) %in% names(mapping)] <- NULL

    return(graph + do.call(ggplot2::geom_smooth, args))

    # return(
    #     graph +
    #         ggplot2::geom_smooth(data = dat, mapping = mapping, alpha = 0.01,
    #                              size = size, color = color, show.legend = show.legend, se = se, ...)
    # )

}

#' @export
drawViolin <- function(graph = drawAxis(), dat = NULL, mapping = NULL, size = 2,
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


# convenience plots ----
#' @export
makeGraph <- function(dat, graphType, xName, yName, breaks = NULL, graphArgs = NULL, themeArgs = NULL) {

    # attempt to understand "graphType"
    graphFun <- understandGraphType(graphType = graphType)

    # make an empty graph
    graph <- drawAxis(xName = xName, yName = yName, breaks = breaks)

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
