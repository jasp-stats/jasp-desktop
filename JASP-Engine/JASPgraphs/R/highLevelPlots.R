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

#' @export
JASPQQplot <- function(x, y = NULL, xName = "", yName = "", ...) {

    if (is.data.frame(x) && is.null(y)) {
        dat <- x
    } else if (is.numeric(x) && is.numeric(y)) {
        dat <- data.frame(x = x, y = y)
    } else {
        stop("Incorrect input.")
    }
    mapping <- aes(x = x, y = y)
    xBreaks <- getPrettyAxisBreaks(dat$x)
    yBreaks <- getPrettyAxisBreaks(dat$y)
    
    datLine <- data.frame(
        x = c(max(yBreaks[1], xBreaks[1]), 
              min(yBreaks[length(yBreaks)], xBreaks[length(xBreaks)])),
        y = c(max(yBreaks[1], xBreaks[1]), 
              min(yBreaks[length(yBreaks)], xBreaks[length(xBreaks)]))
    )
    mapLine <- aes(x = x, y = y)
    
    graph <- ggplot2::ggplot() +
        ggplot2::geom_line(data = datLine, mapping = mapLine, size = 1, col = "darkred") +
        ggplot2::geom_point(data = dat, mapping = mapping, inherit.aes = FALSE, 
                            size = 3, fill = "grey", col = "black", stroke = .5, shape = 21) +
        ggplot2::scale_x_continuous(name = xName, breaks = xBreaks) +
        ggplot2::scale_y_continuous(name = yName, breaks = yBreaks)
    graph <- themeJasp(graph, ...)
    
    return(graph)
}

# p <- JASPgraphs::JASPQQplot(standResid, 
#                             xName = "Theoretical Quantiles",
#                             yName = "Standardized Residuals")
