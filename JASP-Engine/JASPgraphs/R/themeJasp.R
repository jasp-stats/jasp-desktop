#' @export
themeJasp = function(graph,
                     setAxesBreaks = FALSE,
                     plotType = NULL,
                     xAxis = TRUE,
                     yAxis = TRUE,
                     axis.title.cex = getGraphOption("axis.title.cex"),
                     bty = getGraphOption("bty"),
                     fontsize = getGraphOption("fontsize"),
                     family = getGraphOption("family"),
                     horizontal = FALSE,
                     legend.title = "auto",
                     legend.position = getGraphOption("legend.position"),
                     axisTickLength = getGraphOption("axisTickLength"),
                     axisTickWidth = getGraphOption("axisTickWidth"),
                     xyMargin = "auto") {

    # "auto" always means: base this on the graph object.
    # In documentation "otherwise" refers to non "auto" usage.

    # graph: graph object to add theme to
    # fontsize: general size of text
    # legend.title: "none" for omitting or an element_text element
    # legend.position: otherwise as described in ?ggplot2::theme
    # xyMargin: otherwise a list where the 1st element is the xMargin and the second the yMargin. Both a numeric vector of length 4
    # legend.cex: legend text has size fontsize*legend.cex
    # axis.title.cex = axis label has size fontsize*axis.title.cex
    # family: font family
    # legend.position.left/ legend.position.right: where to place the legend? (units relative to plot window; c(.5, .5) = center)
    #
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


    # TRUE if graph contains data, FALSE if graph is input from drawAxis()
    hasData <- !identical(gBuild[["data"]], list(data.frame()))

    # possibly redundant.
    if (xyMargin == "auto") {

        xCex <- 0
        yCex <- 0 #max(c(0, max(nchar(yBreaks, type = "width")) - 4))

        # margins are c(bottom, left, top right)
        xMargin <- c(20 + 5 * xCex, 0, 0, 0) # margin x-label to x-axis
        yMargin <- c(0, 20 + 5 * yCex, 0, 0) # margin y-label to y-axis

    } else {

        xMargin <- xyMargin[[1]]
        yMargin <- xyMargin[[2]]

    }

    # remake R's bty = "n" ----
    if (is.list(bty) && bty[["type"]] == "n") {

        mapLines <- ggplot2::aes(x = x, y = y, xend = xend, yend = yend)

        xyBreaks <- getMajorSource(gBuild)

        xBreaks <- xyBreaks$x
        if (length(xBreaks) > 0 && isTRUE(xAxis)) {
            xLim <- range(xBreaks)
            dfX <- data.frame(y = -Inf, yend = -Inf, x = xLim[1], xend = xLim[2])
            xLine <- ggplot2::geom_segment(data = dfX, mapping = mapLines, lwd = bty[["ldwX"]],
                                           position = ggplot2::PositionIdentity, stat = ggplot2::StatIdentity, inherit.aes = FALSE)
        } else {
            xLine <- NULL
        }

        yBreaks <- xyBreaks$y
        if (length(yBreaks) > 0 && isTRUE(yAxis)) {
            yLim <- range(yBreaks)
            dfY <- data.frame(x = -Inf, xend = -Inf, y = yLim[1], yend = yLim[2])
            yLine <- ggplot2::geom_segment(data = dfY, mapping = mapLines, lwd = bty[["lwdY"]],
                                           position = ggplot2::PositionIdentity, stat = ggplot2::StatIdentity, inherit.aes = FALSE)
        } else {
            yLine <- NULL
        }

        graph <- graph + xLine + yLine
    }


    if (horizontal)
        graph <- graph + ggplot2::coord_flip()


    graph <- graph + themeJaspRaw(legend.position = legend.position, xMargin = xMargin, yMargin = yMargin,
                                  axis.title.cex = axis.title.cex, family = family,
                                  fontsize = fontsize, legend.title = legend.title,
                                  axisTickLength = axisTickLength, axisTickWidth = axisTickWidth)

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
                        axisTickLength = getGraphOption("axisTickLength"),
                        axisTickWidth = getGraphOption("axisTickWidth"),
                        fontsize = getGraphOption("fontsize"),
                        legend.title = ggplot2::element_text(family = family, size = fontsize, hjust = 0.5),
                        Xvjust = NULL, Yvjust = NULL) {

    # TODO: use x_custom & y_custom with ggplot!
    ggplot2::theme(
        # generics
        rect = getBackgroundRect(getGraphOption("debug")),
        # axis
        axis.line = element_blank(),
        axis.text = ggplot2::element_text(family = family, size = fontsize),
        axis.ticks.length = axisTickLength, # tick length
        # axis.ticks.x = x_custom(size = 1.25, color = "black"),
        # axis.ticks.y = y_custom(size = 1.25, color = "black"),
        axis.title = ggplot2::element_text(family = family, size = axis.title.cex*fontsize),
        axis.ticks = ggplot2::element_line(size = axisTickWidth, color = "black"), # tick width
        axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 15)),
        axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 10)),
        axis.text.x = ggplot2::element_text(family = family, colour = "black", size = fontsize,
                                            margin = ggplot2::margin(t = 7), vjust = Xvjust),
        axis.text.y = ggplot2::element_text(family = family, colour = "black", size = fontsize,
                                            margin = ggplot2::margin(r = 7), vjust = Yvjust),

        # legend
        legend.background     = element_rect(color = "transparent", fill = "transparent"),
        legend.box.background = element_rect(color = "transparent", fill = "transparent"),
        legend.key            = element_rect(color = "transparent", fill = "transparent"),
        legend.key.size       = unit(2, "cm"),
        legend.text           = element_text(family = family, size = legend.cex*fontsize),
        legend.title          = legend.title, # ggplot2::element_text(family = family, size = fontsize, hjust = 0.5),
        legend.position       = legend.position,

        # panel
        panel.border = element_blank(),
        panel.spacing = grid::unit(2, "cm"),
        panel.grid = ggplot2::element_blank(),
        panel.background = ggplot2::element_rect(color = "white", fill = "white"),

        # plot
        plot.background = ggplot2::element_rect(fill = "transparent", color = "transparent"),
        plot.margin = ggplot2::margin(),
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

element_grob.element_custom_x <- function(element, x = 0:1, y = 0:1, colour = NULL, size = NULL,
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

element_grob.element_custom_y <- function(element, x = 0:1, y = 0:1, colour = NULL, size = NULL,
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


themeTest <- ggplot2::theme(
  axis.ticks.x = x_custom(size = 1.25, color = "black"),
  axis.ticks.y = y_custom(size = 1.25, color = "black"),
  axis.ticks.length = grid::unit(2, "mm")
)
