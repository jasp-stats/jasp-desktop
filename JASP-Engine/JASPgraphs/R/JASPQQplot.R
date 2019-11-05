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
