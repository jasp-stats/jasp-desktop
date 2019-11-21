grid_arrange_shared_legend <- function(..., plotList = NULL, nrow = 1, ncol = length(list(...)), position = c("bottom", "right")) {

    if (is.null(plotList)) {
        plots <- list(...)
    } else {
        plots <- plotList
    }
    position  <- match.arg(position)
    g         <- ggplot2::ggplotGrob(plots[[1]] + ggplot2::theme(legend.position = position))$grobs
    legend    <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
    lheight   <- sum(legend$height)
    lwidth    <- sum(legend$width)
    gl        <- lapply(plots, function(x) x + ggplot2::theme(legend.position = "none"))
    gl        <- c(gl, nrow = nrow, ncol = ncol)

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

addAxis <- function(graph, breaks = NULL, name = waiver(), labels = waiver(), limits = waiver(), position = "left", ...) {

    if (position %in% c("left", "right")) {# vertical axis
        axis <- yAxisBreaksToAxisScale(yBreaks = breaks, yName = name, yLabels = labels, yLimits = limits, ...)
    } else { # horizontal axis
        axis <- xAxisBreaksToAxisScale(xBreaks = breaks, xName = name, xLabels = labels, xLimits = limits, ...)
    }

    return(graph + axis)

}

#' @title (Deprecated) draw an empty ggplot with just axes.
#' 
#' @param graph ggplot object.
#' @param xName name for x-axis.
#' @param yName name for y-axis.
#' @param breaks a list with $xBreaks and $yBreaks or waiver().
#' @param xBreaks x-axis breaks.
#' @param yBreaks y-axis breaks.
#' @param dat data.frame.
#' @param xLabels labels for x-axis.
#' @param yLabels labels for y-axis.
#' @param xLimits limits for x-axis.
#' @param yLimits limits for y-axis.
#' @param force force the axes to be present at the cost of drawing an invisible geom.
#' @param secondaryXaxis secondary x-axis.
#' @param secondaryYaxis secondary y-axis.
#' @param xTrans transformation function for the x-axis.
#' @param yTrans transformation function for the y-axis.
#'
#' @export
drawAxis <- function(graph = NULL, xName = waiver(), yName = waiver(), breaks = waiver(), xBreaks = waiver(),
                     yBreaks = waiver(), dat = NULL, xLabels = waiver(), yLabels = waiver(), xLimits = waiver(),
                     yLimits = waiver(), force = FALSE,
                     secondaryXaxis = waiver(), secondaryYaxis = waiver(),
                     xTrans = "identity", yTrans = "identity") {

    warning("This function will be deprecated.")
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

    if (!is.null(xBreaks) && !is.waive(xBreaks) && is.waive(xLimits)) {
        xLimits <- range(xBreaks)
        if (is.waive(xLabels))
          xLabels <- axesLabeller(xBreaks)
    }

    if (!is.null(yBreaks) && !is.waive(yBreaks) && is.waive(yLimits)) {
        yLimits <- range(yBreaks)
        if (is.waive(yLabels))
          yLabels <- axesLabeller(yBreaks)
    }

    if (is.null(graph))
        graph <- ggplot2::ggplot()

    if (force && is.waive(graph[["data"]])) {
        dftmp <- data.frame(x = range(xBreaks), y = range(yBreaks))
        graph <- graph + ggplot2::geom_line(data = dftmp, mapping = ggplot2::aes(x = .data$x, y = .data$y), color = "white", alpha = 0)
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

# # @export
# drawBars <- function(graph = drawAxis(), dat, mapping = NULL, stat="identity", fill="gray80", width = NULL, show.legend = FALSE, ...) {
# 
#     if (is.null(mapping)) {
# 
#         nms <- colnames(dat)
# 
#         mapping <- switch(as.character(length(nms)),
#                           "1" = ggplot2::aes_string(x = nms[1]),
#                           "2" = ggplot2::aes_string(x = nms[1], y = nms[2]),
#                           "3" = ggplot2::aes_string(x = nms[1], y = nms[2], group = nms[3], linetype = nms[3])
#         )
# 
#     }
# 
#     args = list(data = dat, mapping = mapping, fill = fill, stat=stat, width = width, show.legend = show.legend, ...)
#     args[names(args) %in% names(mapping)] <- NULL
# 
#     return(graph + do.call(ggplot2::geom_bar, args))
# 
# }

#' @title Deprecated: use \code{ggplot2::geom_line} instead.
#'
#' @param graph ggplot2 object
#' @param dat data frame
#' @param mapping mapping from aes
#' @param size size
#' @param alpha transparancy
#' @param show.legend show legend?
#' @param ... other arguments to geom_line
#'
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
}

#' @title Deprecated: use \code{ggplot2::geom_point} instead.
#'
#' @param graph ggplot2 object
#' @param dat data frame
#' @param mapping mapping from aes
#' @param size size
#' @param shape shape
#' @param fill color for filling
#' @param show.legend show legend?
#' @param ... other arguments to geom_point
#'
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

#' @title Deprecated: use \code{ggplot2::geom_smooth} instead.
#'
#' @param graph ggplot2 object
#' @param dat data frame
#' @param mapping mapping from aes
#' @param size size
#' @param method statistical method to draw regression line (e.g., lm)
#' @param color line color
#' @param show.legend show legend?
#' @param se show standard errors?
#' @param alpha transparancy
#' @param ... other arguments to geom_smooth
#'
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

}
