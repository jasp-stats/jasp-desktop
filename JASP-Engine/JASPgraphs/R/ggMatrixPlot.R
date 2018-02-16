addVerticalLabels <- function(totalGraph, graphs2add, x, yIncrement, width, height, yOffset = 0) {
    
    for (i in seq_along(graphs2add)) {
        if (!is.null(graphs2add[[i]])) {
            totalGraph <- totalGraph + 
                cowplot::draw_plot(graphs2add[[i]], 
                                   x = x, 
                                   y = yOffset + yIncrement * (i-1), 
                                   width = width, 
                                   height = height)
        }
    }
    return(totalGraph)
}

addHorizontalLabels <- function(totalGraph, graphs2add, y, xIncrement, width, height, xOffset = 0) {

    for (i in seq_along(graphs2add)) {
        if (!is.null(graphs2add[[i]])) {
            totalGraph <- totalGraph + 
                cowplot::draw_plot(graphs2add[[i]], 
                                   x = xOffset + xIncrement * (i-1), 
                                   y = y, 
                                   width = width, 
                                   height = height)
        }
    }
    return(totalGraph)
}

addCenterPlots <- function(totalGraph, plotMatrix, xIncrement, yIncrement, xOffset, yOffset) {
    
    for (i in seq_len(nrow(plotMatrix))) {
        for (j in seq_len(ncol(plotMatrix))) {
            if (!is.null(plotMatrix[[i, j]])) {
                totalGraph <- totalGraph + 
                    cowplot::draw_plot(plotMatrix[[i, j]], 
                                       x = xOffset + xIncrement * (j-1), 
                                       y = yOffset + yIncrement * (i-1), 
                                       width = xIncrement, 
                                       height = yIncrement)
            }
        }
    }
    return(totalGraph)
    
}

makeRect <- function(col = "red", size = 2, fill = scales::alpha("black", 0)) {
    
    # this function exists for debugging purposes
    
    dfrect <- data.frame(xmin = 0, xmax = 1, ymin = 0, ymax = 1)
    
    return(invisible(
        ggplot2::ggplot(data = dfrect, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) + 
            ggplot2::geom_rect(fill = fill, size = size, color = col) + 
            ggplot2::theme_void()
    ))
}

replicateOrStop <- function(x, n) {
    
    if (length(x) == 1) {
        x <- rep(x, n)
    } else if (length(x) != n) {
        stop(sprintf(
            fmt = "Argument %s should either have length 1 or length %d.", 
            deparse(substitute(x)), n
        ))
    }
        
    return(x)
}

#' @export
makeLabels <- function(label, angle = 0, size = 1, family = "serif") {
    
    UseMethod("makeLabels", label)
    
}

#' @export
makeLabels.default <- function(label, angle = 0, size = 1, family = "serif") {
    
    if (is.null(label))
        return(NULL)
    
    # recursive vectorization
    if (length(label) > 1)
        return(lapply(label, makeLabels.default, angle = angle, size = size, family = family))
    
    # draws text in center (!) of plot.
    df <- data.frame(x=0, y=0.0)
    parse <- is.call(label)
    if (!parse) {
        df$label <- label
    } else {
        eq <- as.character(as.expression(label))
        df$label <- eq
    }
    
    return(
        ggplot2::ggplot(df, aes(x, y, label = label)) +
            ggplot2::geom_text(angle = angle, size = size, family = family, parse = parse) +
            ggplot2::theme_void()
    )
}

#' @export
makeLabels.list <- function(label, angle = 0, size = 1, family = "serif") {
    
    nLabels <- length(label)
    angle <- replicateOrStop(angle, nLabels)
    size <- replicateOrStop(size, nLabels)
    output <- vector("list", nLabels)
    names(output) <- names(label)

    for (i in seq_along(label)) {
        
        if (is.null(label[[i]]))
            next
        
        output[[i]] <- makeLabels.default(label[[i]], angle = angle[[i]], size = size[[i]], family = family)
        
    }
    
    return(output)
    
}


#' @export
ggMatrixPlot <- function(plotList = NULL, nr = NULL, nc = NULL,
                         ...,
                         leftLabels = NULL,
                         topLabels = NULL,
                         rightLabels = NULL,
                         bottomLabels = NULL,
                         debug = FALSE) {
    UseMethod("ggMatrixPlot", plotList)
}

#' @export
ggMatrixPlot.matrix <- function(plotList = NULL, nr = NULL, nc = NULL,
                                layout = NULL,
                                ...,
                                leftLabels = NULL,
                                topLabels = NULL,
                                rightLabels = NULL,
                                bottomLabels = NULL,
                                debug = FALSE) {

    # dim cannot be NULL since plotList is a matrix
    nr <- dim(plotList)[1L]
    nc <- dim(plotList)[2L]
    dimNms <- dimnames(plotList)

    if (!is.null(dimNms[[1L]]) && is.null(leftLabels))
        leftLabels <- dimNms[[1L]]
    
    if (!is.null(dimNms[[2L]]) && is.null(topLabels))
        topLabels <- dimNms[[2L]]

    leftLabels <- makeLabels(leftLabels, angle = 90, size = 5)
    topLabels <- makeLabels(topLabels, angle = 0, size = 5)
    rightLabels <- makeLabels(rightLabels, angle = 270, size = 5)
    bottomLabels <- makeLabels(bottomLabels, angle = 0, size = 5)

    return(ggMatrixPlot.default(
        plotList = plotList,
        nr = nr, 
        nc = nc,
        ... = ...,
        leftLabels = leftLabels,
        topLabels = topLabels,
        rightLabels = rightLabels,
        bottomLabels = bottomLabels
    ))
    
}

#' @export
ggMatrixPlot.list <- function(plotList = NULL, nr = NULL, nc = NULL,
                              layout = NULL,
                                ...,
                                leftLabels = NULL,
                                topLabels = NULL,
                                rightLabels = NULL,
                                bottomLabels = NULL,
                                debug = FALSE) {
    if (is.null(layout)) { # was layout supplied?
        stop("Either supply plotList as a matrix or provide a layout argument")
    } else if (!is.matrix(layout)) { # is layout layout a matrix?
        stop(sprintf("layout must be a matrix but instead was of mode %s and class %s", mode(layout), class(layout)))
    } else if (length(layout) != length(plotList)) { # does layout have correct length?
        stop(sprintf("length of layout (%d) does not match length of plotList (%d). Use NULL entries in plotList to specify empty plots.",
                     length(layout), length(plotList)))
    } else if (!all(seq_along(layout) %in% layout)) { # does layout have all required values?
        stop("Layout must consist of unique integers starting from 1.")
    } else { # layout is good layout
        plotList <- plotList[layout]
        nr <- nrow(layout)
        nc <- ncol(layout)
        dim(plotList) <- dim(layout)
    }
    
    leftLabels <- makeLabels(leftLabels, angle = 90, size = 5)
    topLabels <- makeLabels(topLabels, angle = 0, size = 5)
    rightLabels <- makeLabels(rightLabels, angle = 270, size = 5)
    bottomLabels <- makeLabels(bottomLabels, angle = 0, size = 5)

    return(ggMatrixPlot.default(
        plotList = plotList,
        nr = nr,
        nc = nc,
        ... = ...,
        leftLabels = leftLabels,
        topLabels = topLabels,
        rightLabels = rightLabels,
        bottomLabels = bottomLabels
    ))
    
}
#' @export
ggMatrixPlot.default <- function(plotList = NULL, nr = NULL, nc = NULL,
                                 layout = NULL,
                                 ...,
                                 leftLabels = NULL,
                                 topLabels = NULL,
                                 rightLabels = NULL,
                                 bottomLabels = NULL,
                                 debug = FALSE) {

    if (is.null(plotList) && debug) {
        
        leftLabels <- lapply(seq_len(nr), function(x) makeRect())
        rightLabels <- lapply(seq_len(nr), function(x) makeRect("navajowhite4"))
        topLabels <- lapply(seq_len(nc), function(x) makeRect("blue"))
        bottomLabels <- lapply(seq_len(nc), function(x) makeRect("orange"))
        plotList <- lapply(seq_len(nc*nr), function(x) makeRect("green", fill = scales::alpha("springgreen", .5)))
        dim(plotList) <- c(nr, nc)
        
    }
    
    # ugly artefact of the lazy vectorization with lapply in makeLabels
    if (nr == 1) {
        leftLabels <- list(leftLabels)
        rightLabels <- list(rightLabels)
    }
    if (nc == 1) {
        topLabels <- list(topLabels)
        bottomLabels <- list(bottomLabels)
    }

    hasLeftLab <- !is.null(leftLabels)
    hasTopLab <- !is.null(topLabels)
    hasRightLab <- !is.null(rightLabels)
    hasBottomLab <- !is.null(bottomLabels)

    dots <- list(...)
    defArgs <- list(xOffset = 0, yOffset = 0) # fill this thing with more default arguments
    nmsDots <- names(dots)
    defArgs[names(defArgs) %in% nmsDots] <- dots[nmsDots[nmsDots %in% names(defArgs)]]
    
    firstColHeight <- .05
    firstRowWidth <- .05
    
    firstColWidth <- (1 - firstRowWidth*(hasLeftLab + hasRightLab)) / nc
    firstRowHeight <- (1 - firstRowWidth*(hasTopLab + hasBottomLab)) / nr
    # browser()
    # empty plot
    totalGraph <- cowplot::ggdraw(xlim = c(0, 1), ylim = c(0, 1))
    # labels left of plots
    if (hasLeftLab) {
        totalGraph <- addVerticalLabels(totalGraph,
                                        graphs2add = leftLabels,
                                        x = 0, 
                                        yIncrement = firstRowHeight, 
                                        width = firstRowWidth,
                                        height = firstRowHeight,
                                        yOffset = hasBottomLab*firstColHeight + defArgs$yOffset)
    }
    # labels above plots
    if (hasTopLab) {
        totalGraph <- addHorizontalLabels(totalGraph, 
                                          graphs2add = topLabels,
                                          y = 1 - firstColHeight, 
                                          xIncrement = firstColWidth, 
                                          width = firstColWidth, 
                                          height = firstColHeight,
                                          xOffset = hasLeftLab*firstRowWidth + defArgs$xOffset)
    }
    
    if (hasRightLab) { # labels right of plots
        totalGraph <- addVerticalLabels(totalGraph, 
                                        graphs2add = rightLabels,
                                        x = 1 - firstRowWidth, 
                                        yIncrement = firstRowHeight, 
                                        width = firstRowWidth,
                                        height = firstRowHeight,
                                        yOffset = hasBottomLab*firstColHeight + defArgs$yOffset)
    }
    
    if (hasBottomLab) { # labels below plots
        totalGraph <- addHorizontalLabels(totalGraph,
                                          graphs2add = bottomLabels,
                                          y = 0, 
                                          xIncrement = firstColWidth, 
                                          width = firstColWidth, 
                                          height = firstColHeight,
                                          xOffset = hasLeftLab*firstRowWidth + defArgs$xOffset)
    }
    
    # actually include plots
    totalGraph <- addCenterPlots(totalGraph, plotList,
                                 xIncrement = firstColWidth, 
                                 yIncrement = firstRowHeight, 
                                 xOffset = hasLeftLab*firstRowWidth,
                                 yOffset = hasBottomLab*firstColHeight)
    return (totalGraph)
    
}
