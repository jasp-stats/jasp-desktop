
#'@export
withinSubjectScatter <- function(x, y = NULL, nameX = colnames(x), nameY = colnames(y)) {

    if (is.data.frame(x) && is.null(y)) {
        df <- x
    } else if (all(is.list(x), is.list(y),
                   length(x) == length(y))) {
        l <- length(x)
        if (is.null(nameX))
            nameX <- vector("list", l)
        if (is.null(nameY))
            nameY <- vector("list", l)

        return(Map(withinSubjectScatter,
            x = x,
            y = y,
            nameX = nameX,
            nameY = nameY
        ))

    } else if (is.vector(x) && is.vector(y)) {
        df <- data.frame(
            x = x,
            y = y
        )
    } else {
        stop("Bad input to withinSubjectScatter().")
    }

    graph <- drawPoints(dat = df, mapping = aes(x = x, y = y)) +
        ggplot2::geom_abline()

    if (!is.null(nameX))
        graph <- graph + ggplot2::xlab(nameX)

    if (!is.null(nameX))
        graph <- graph + ggplot2::ylab(nameY)

    return(themeJasp(graph))

}
