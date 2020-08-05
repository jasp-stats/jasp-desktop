makeRect <- function(col = "red", size = 2, fill = scales::alpha("black", 0)) {

  # this function exists for debugging purposes

  dfrect <- data.frame(xmin = 0, xmax = 1, ymin = 0, ymax = 1)

  return(invisible(
    ggplot2::ggplot(data = dfrect, aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin, 
                                       ymax = .data$ymax)) +
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

makeLabels <- function(label, angle = 0, size = 1, family = getGraphOption("family"),
                       vjust = "center", hjust = "center", x = .5, y = .5) {
  UseMethod("makeLabels", label)
}

makeLabels.default <- function(label, angle = 0, size = 1, family = getGraphOption("family"),
                               vjust = "center", hjust = "center", x = .5, y = .5) {

  if (is.null(label))
    return(NULL)

  # recursive vectorization
  if (length(label) > 1)
    return(lapply(label, makeLabels.default, angle = angle, size = size, family = family,
                  vjust = vjust, hjust = hjust, x = x, y = y))

  # draws text in center (!) of plot.
  df <- data.frame(x = x, y = y)
  parse <- is.call(label)
  if (!parse) {
    df$label <- label
  } else {
    eq <- as.character(as.expression(label))
    df$label <- eq
  }

  # should inherit current theme from graphOptions
  scx <- ggplot2::scale_x_continuous(limits = c(0, 1))
  scy <- ggplot2::scale_y_continuous(limits = c(0, 1))
  g <-  ggplot2::ggplot(df, aes(x, y, label = label)) +
    scx + scy +
    ggplot2::theme_void() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "transparent", color = "transparent"))
  if (!is.null(family)) {
    return(
      g + ggplot2::geom_text(angle = angle, size = size, family = family, vjust = vjust, hjust = hjust,
                             parse = parse)
    )
  } else {
    return(
      g + ggplot2::geom_text(angle = angle, size = size, parse = parse, vjust = vjust, hjust = hjust)
    )
  }
}

makeLabels.list <- function(label, angle = 0, size = 1, family = getGraphOption("family"),
                            vjust = "center", hjust = "center", x = .5, y = .5) {

  nLabels <- length(label)
  angle <- replicateOrStop(angle, nLabels)
  size <- replicateOrStop(size, nLabels)
  output <- vector("list", nLabels)
  names(output) <- names(label)

  for (i in seq_along(label)) {

    if (is.null(label[[i]]))
      next

    output[[i]] <- makeLabels.default(label[[i]], angle = angle[[i]], size = size[[i]], family = family,
                                      vjust = vjust, hjust = hjust, x = x, y = y)

  }

  return(output)

}

modifyAxesLabels <- function(removeXYlabels, plotList) {

  # Error handling
  if (!is.character(removeXYlabels) || !(all(removeXYlabels %in% c('x', 'y', 'xy', 'none'))))
    stop("removeXYlabels must be a character and contain either 'x', 'y', 'xy', or 'none'")

  d1 <- dim(plotList)
  if (is.matrix(removeXYlabels)) {
    d0 <- dim(removeXYlabels)
    if (!all(d0 == d1)) {
      stop(sprintf("Dim(removeXYlabels) does not match dim(plotList) / layout. Got (%d, %d) and expected: (%d, %d).",
                   d0[1], d0[2], d1[1], d1[2]))
    }
  } else {
    if (!(length(removeXYlabels) == 1 || length(plotList)))
      stop("removeXYlabels should either have length 1 or length(plotList).")

    removeXYlabels <- matrix(removeXYlabels, nrow = d1[1], ncol = d1[2])
  }

  # actually remove labels
  mrg <- 0
  mrgLR <- .05
  x <- ggplot2::xlab("")
  xt <- ggplot2::theme(plot.margin = ggplot2::margin(b = mrg, r = mrgLR))
  y <- ggplot2::ylab("")
  yt <- ggplot2::theme(plot.margin = ggplot2::margin(l = mrgLR, r = mrgLR))
  xyt <- ggplot2::theme(plot.margin = ggplot2::margin(b = mrg, l = mrgLR, r = mrgLR, t = mrg))
  transp <- NULL

  for (i in which(lengths(plotList) > 0)) {
    switch(
      removeXYlabels[i],
      "x" = {plotList[[i]] <- plotList[[i]] + x + xt + transp},
      "y" = {plotList[[i]] <- plotList[[i]] + y + yt + transp},
      "xy" = {plotList[[i]] <- plotList[[i]] + x + y + xyt + transp}
    )
  }

  return(plotList)

}

getAxesLabels <- function(g) {

  axesLabels <- try({
    filename <- tempfile()
    png(filename = filename)
    grobs <- ggplot2::ggplotGrob(g)
    dev.off()
    if(file.exists(filename))
      file.remove(filename)

    i1 <- which(grobs[["layout"]][["name"]] == "xlab-t")
    i2 <- which(grobs[["layout"]][["name"]] == "xlab-b")
    i3 <- which(grobs[["layout"]][["name"]] == "ylab-l")
    i4 <- which(grobs[["layout"]][["name"]] == "ylab-r")
    grobs

    list(
      xtop = grobs[["grobs"]][[i1]][["children"]][[1]][["label"]],
      xbottom = grobs[["grobs"]][[i2]][["children"]][[1]][["label"]],
      yleft = grobs[["grobs"]][[i3]][["children"]][[1]][["label"]],
      yright = grobs[["grobs"]][[i4]][["children"]][[1]][["label"]]
    )
  })

  if (inherits(axesLabels, "try-error")) {
    warning("JASPgraphs could not obtain axes labels from graph")
    return(list(xtop = NULL, xbottom = NULL, yleft = NULL, yright = NULL))
  } else {
    return(axesLabels)
  }

}

scaleAxesLabels <- function(scaleXYlabels, plotList) {

  # Error handling
  if (!is.numeric(scaleXYlabels) || !(all(scaleXYlabels > 0)) || !(all(scaleXYlabels < 1)))
    stop("removeXYlabels must be numeric, larger than 0 and smaller than 1")

  d1 <- dim(plotList)
  if (is.matrix(scaleXYlabels)) {
    d0 <- dim(scaleXYlabels)
    if (!all(d0[1] == prod(d1))) {
      stop(sprintf("dim(removeXYlabels)[1] does not match prod(dim(plotList)) / layout. Got (%d, %d) and expected: prod(%d, %d).",
                   d0[1], d0[2], d1[1], d1[2]))
    }
  } else {
    if (!(length(scaleXYlabels) == 2 || length(plotList)))
      stop("removeXYlabels should either have length 2 or length(plotList).")

    scaleXYlabels <- matrix(scaleXYlabels, nrow = prod(d1), ncol = 2, byrow = TRUE)
  }

  fontsize <- getGraphOption("fontsize")
  fam <- getGraphOption("family")
  for (i in seq_along(plotList)) {

    axesLabels <- getAxesLabels(plotList[[i]])
    c1 <- !(is.null(axesLabels$xbottom) || identical(axesLabels$xbottom, "")) # are there x-axes labels ?
    c2 <- !(is.null(axesLabels$yleft) || identical(axesLabels$yleft, "")) # are there y-axes labels ?

    if (c1 && c2) {

      plotList[[i]] <- plotList[[i]] +
        ggplot2::theme(
          axis.title.x = ggplot2::element_text(family = fam, margin = ggplot2::margin(t = 10),
                                               size = scaleXYlabels[i, 1]*fontsize),
          axis.title.y = ggplot2::element_text(family = fam, margin = ggplot2::margin(r = 10),
                                               size = scaleXYlabels[i, 2]*fontsize)
        )

    } else if (c1) {

      plotList[[i]] <- plotList[[i]] +
        ggplot2::theme(
          axis.title.x = ggplot2::element_text(family = fam, margin = ggplot2::margin(t = 10),
                                               size = scaleXYlabels[i, 1]*fontsize))

    } else if (c2) {

      plotList[[i]] <- plotList[[i]] +
        ggplot2::theme(
          axis.title.y = ggplot2::element_text(family = fam, margin = ggplot2::margin(r = 10),
                                               size = scaleXYlabels[i, 2]*fontsize)
        )

    }
  }

  return(plotList)
}

#' @title ggMatrixPlot
#' @param plotList a list of ggplot2 objects
#' @param layout a matrix that specifies the position of each plot, akin to the layout for base plots.
#' @param nr number of rows
#' @param nc number of columns
#' @param ... ignored.
#' @param leftLabels labels left of the plots in plotList.
#' @param topLabels labels above the plots in plotList.
#' @param rightLabels labels right of the plots in plotList.
#' @param bottomLabels labels below the plots in plotList.
#' @param removeXYlabels Whether to remove the x and y axes titles.
#' @param labelSize two scalars for the magnification of the the x and y labels respectively.
#' @param labelPos relative position for the labels around the plots. The first column contains x-coordinates and the second y-coordinates. The first row is top, second right, third bottom, and fourth left (TRouBLe).
#' @param scaleXYlabels two scalars for the magnification of the the x and y labels respectively. 
#' @param debug create an debug plot (see examples).
#'
#' @details This function is intended to be calles with a matrix as first argument, althought other input is also supported.
#'
#' @example inst/examples/ex-ggMatrixPlot.R
#' @export
ggMatrixPlot <- function(plotList = NULL, nr = NULL, nc = NULL,
                         ...,
                         leftLabels     = NULL,
                         topLabels      = NULL,
                         rightLabels    = NULL,
                         bottomLabels   = NULL,
                         removeXYlabels = c("xy", "x", "y", "none"),
                         labelSize      = .4*graphOptions("fontsize"),
                         labelPos       = matrix(.5, 4, 2),
                         scaleXYlabels  = c(.9,.9),
                         debug          = FALSE) {

  UseMethod("ggMatrixPlot", plotList)
}

#' @rdname ggMatrixPlot
#' @export
ggMatrixPlot.matrix <- function(plotList = NULL, nr = NULL, nc = NULL,
                                layout = NULL,
                                ...,
                                leftLabels     = NULL,
                                topLabels      = NULL,
                                rightLabels    = NULL,
                                bottomLabels   = NULL,
                                removeXYlabels = c("xy", "x", "y", "none"),
                                labelSize      = .4*graphOptions("fontsize"),
                                labelPos       = matrix(.5, 4, 2),
                                scaleXYlabels  = c(.9,.9),
                                debug          = FALSE) {

  # dim cannot be NULL since plotList is a matrix
  nr <- dim(plotList)[1L]
  nc <- dim(plotList)[2L]
  dimNms <- dimnames(plotList)

  if (!is.null(dimNms[[1L]]) && is.null(leftLabels))
    leftLabels <- dimNms[[1L]]

  if (!is.null(dimNms[[2L]]) && is.null(topLabels))
    topLabels <- dimNms[[2L]]

  topLabels    <- makeLabels(topLabels,    angle = 0,   size = labelSize, x = labelPos[1, 1], y = labelPos[1, 2])
  rightLabels  <- makeLabels(rightLabels,  angle = 270, size = labelSize, x = labelPos[2, 1], y = labelPos[2, 2])
  bottomLabels <- makeLabels(bottomLabels, angle = 0,   size = labelSize, x = labelPos[3, 1], y = labelPos[3, 2])
  leftLabels   <- makeLabels(leftLabels,   angle = 90,  size = labelSize, x = labelPos[4, 1], y = labelPos[4, 2])

  return(ggMatrixPlot.default(
    plotList = plotList,
    nr = nr,
    nc = nc,
    ... = ...,
    leftLabels     = leftLabels,
    topLabels      = topLabels,
    rightLabels    = rightLabels,
    bottomLabels   = bottomLabels,
    removeXYlabels = removeXYlabels,
    labelPos       = labelPos,
    scaleXYlabels  = scaleXYlabels,
    debug          = debug
  ))

}

#' @rdname ggMatrixPlot
#' @export
ggMatrixPlot.list <- function(plotList = NULL, nr = NULL, nc = NULL,
                              layout = NULL,
                              ...,
                              leftLabels     = NULL,
                              topLabels      = NULL,
                              rightLabels    = NULL,
                              bottomLabels   = NULL,
                              removeXYlabels = c("xy", "x", "y", "none"),
                              labelSize      = .4*graphOptions("fontsize"),
                              labelPos       = matrix(.5, 4, 2),
                              scaleXYlabels  = c(.9,.9),
                              debug          = FALSE) {
  if (is.null(layout)) { # was layout supplied?
    stop("Either supply plotList as a matrix or provide a layout argument")
  } else if (!is.matrix(layout)) { # is layout layout a matrix?
    stop(sprintf("layout must be a matrix but instead was of mode %s and class %s", mode(layout), class(layout)))
  } else if (length(layout) != length(plotList)) { # does layout have correct length?
    stop(sprintf("length of layout (%d) does not match length of plotList (%d). Use NULL entries in plotList to specify empty plots.",
                 length(layout), length(plotList)))
  } else if (!all(seq_along(layout) %in% layout)) { # does layout have all required values?
    stop("Layout must consist of unique integers starting from 1.")
  } else {# layout is good layout
    plotList <- plotList[layout]
    nr <- nrow(layout)
    nc <- ncol(layout)
    dim(plotList) <- dim(layout)
  }

  topLabels    <- makeLabels(topLabels,    angle = 0,   size = labelSize, x = labelPos[1, 1], y = labelPos[1, 2])
  rightLabels  <- makeLabels(rightLabels,  angle = 270, size = labelSize, x = labelPos[2, 1], y = labelPos[2, 2])
  bottomLabels <- makeLabels(bottomLabels, angle = 0,   size = labelSize, x = labelPos[3, 1], y = labelPos[3, 2])
  leftLabels   <- makeLabels(leftLabels,   angle = 90,  size = labelSize, x = labelPos[4, 1], y = labelPos[4, 2])

  return(ggMatrixPlot.default(
    plotList = plotList,
    nr = nr,
    nc = nc,
    ... = ...,
    leftLabels     = leftLabels,
    topLabels      = topLabels,
    rightLabels    = rightLabels,
    bottomLabels   = bottomLabels,
    removeXYlabels = removeXYlabels,
    labelPos       = labelPos,
    scaleXYlabels  = scaleXYlabels,
    debug          = debug
  ))

}

#' @rdname ggMatrixPlot
#' @export
ggMatrixPlot.default <- function(plotList = NULL, nr = NULL, nc = NULL,
                                 layout = NULL,
                                 ...,
                                 leftLabels     = NULL,
                                 topLabels      = NULL,
                                 rightLabels    = NULL,
                                 bottomLabels   = NULL,
                                 removeXYlabels = c("xy", "x", "y", "none"),
                                 labelSize      = .4*graphOptions("fontsize"),
                                 labelPos       = matrix(.5, 4, 2),
                                 scaleXYlabels  = c(.9,.9),
                                 debug          = FALSE) {

  removeXYlabels <- match.arg(removeXYlabels)
  if (is.null(plotList) && debug) {

    if (is.null(nr))
      nr <- 3
    if (is.null(nc))
      nc <- 2

    leftLabels    <- lapply(seq_len(nr),    function(x) makeRect())
    rightLabels   <- lapply(seq_len(nr),    function(x) makeRect("navajowhite4"))
    topLabels     <- lapply(seq_len(nc),    function(x) makeRect("blue"))
    bottomLabels  <- lapply(seq_len(nc),    function(x) makeRect("orange"))
    plotList      <- lapply(seq_len(nc*nr), function(x) makeRect("green", fill = scales::alpha("springgreen", .5)))
    dim(plotList) <- c(nr, nc)

  }
  # plotList <- modifyAxesLabels(removeXYlabels, plotList)
  if (!is.null(scaleXYlabels) && !isTRUE(all.equal(scaleXYlabels, c(1,1)))) {
    plotList <- scaleAxesLabels(scaleXYlabels, plotList)
  }

  hasLeftLab   <- !is.null(leftLabels)
  hasTopLab    <- !is.null(topLabels)
  hasRightLab  <- !is.null(rightLabels)
  hasBottomLab <- !is.null(bottomLabels)

  # ugly artefact of the lazy vectorization with lapply in makeLabels
  if (nr == 1) {
    leftLabels <- list(leftLabels)
    rightLabels <- list(rightLabels)
  }
  if (nc == 1) {
    topLabels <- list(topLabels)
    bottomLabels <- list(bottomLabels)
  }

  w <- .25
  h <- .25
  width <- rep(1, nc)
  height <- rep(1, nr)
  
  # names for the gtable to ease further work
  gtNames <- matrix(paste0("graph-", rep(seq_len(nr), nc), "-", rep(seq_len(nc), each = nr)), nr, nc)

  # labels left of plots
  if (hasLeftLab) {
    width <- c(w, width)
    plotList <- cbind(
      leftLabels,
      plotList
    )
    gtNames <- cbind(paste0("ylab-l-", seq_len(nr)), gtNames)
  }
  # labels right of plots
  if (hasRightLab) {
    width <- c(width, w)
    plotList <- cbind(
      plotList,
      rightLabels
    )
    gtNames <- cbind(gtNames, paste0("ylab-r-", seq_len(nr)))
  }

  if (hasTopLab || hasBottomLab) {

    # empty ggplot to fill corners of the plot
    empty <- list(NULL)#ggplot2::ggplot() + ggplot2::theme_void())

    # labels below of plots
    if (hasTopLab) {
      nms2add <- paste0("xlab-t-", seq_len(nc))
      height <- c(h, height)
      if (hasLeftLab) {
        topLabels <- c(empty, topLabels)
        nms2add <- c("empty", nms2add)
      }
      if (hasRightLab) {
        topLabels <- c(topLabels, empty)
        nms2add <- c(nms2add, "empty")
      }
      plotList <- rbind(topLabels, plotList)
      gtNames <- rbind(nms2add, gtNames)
    }

    # labels above of plots
    if (hasBottomLab) {
      nms2add <- paste0("xlab-b-", seq_len(nc))
      height <- c(height, h)
      if (hasLeftLab) {
        bottomLabels <- c(empty, bottomLabels)
        nms2add <- c("empty", nms2add)
      }
      if (hasRightLab) {
        bottomLabels <- c(bottomLabels, empty)
        nms2add <- c(nms2add, "empty")
      }
      plotList <- rbind(plotList, bottomLabels)
      gtNames <- rbind(gtNames, nms2add)
    }
  }

  # make layout matrix
  d <- dim(plotList)
  layout <- matrix(NA, d[1L], d[2L])
  idx <- which(lengths(plotList) > 0)
  layout[idx] <- seq_along(idx)
  layout[is.na(layout)] <- length(idx) + 1
  
  totalGraph <- JASPgraphsPlot$new(
      subplots     = c(plotList[lengths(plotList) > 0]),
      names        = c(gtNames[lengths(plotList) > 0]),
      layout       = layout,
      heights      = height,
      widths       = width
    )

  return(totalGraph)

}
