
#' @export
plotThis <- function(g, ...) {

    dots <- list(...)
    if (is.null(dots[["width"]]))
        width <- 480
    if (is.null(dots[["height"]]))
        height <- 320

    # use dev.new if x11() chrashes.
    grDevices::dev.new(...)
    # x11(width = width, height = height)
    print(g)

}

sameLengths <- function(...) {
    # checks if all input arguments have the same length
    return(length(unique(lengths(list(...)))) == 1)
}

#' @export
getPrettyAxisBreaks <- function(x, ...) {
    force(x)
    UseMethod("getPrettyAxisBreaks", x)
}

#' @method getPrettyAxisBreaks numeric
#' @export
getPrettyAxisBreaks.numeric <- function(x, ...) {
    return(pretty(x, ...))
}

#' @method getPrettyAxisBreaks default
#' @export
getPrettyAxisBreaks.default <- function(x, ...) {
    if (is.numeric(x)) 
        return(pretty(x, ...))
    else return(unique(x))
}

#' @method getPrettyAxisBreaks data.frame
#' @export
getPrettyAxisBreaks.data.frame <- function(x, ...) {

    if (!all(c("x", "y") %in% names(x)))
        stop("INTERNAL: getPrettyAxisBreaks must get a dataframe that has a column named x and a column named y.")

    xBreaks <- getPrettyAxisBreaks(x$x, ...)
    yBreaks <- getPrettyAxisBreaks(x$y, ...)
    # xBreaks <- switch(class(x$x),
    #                   "numeric" = pretty(x$x),
    #                   unique(x$x)
    # )
    # yBreaks <- switch(class(x$y),
    #                   "numeric" = pretty(x$y),
    #                   unique(x$y)
    # )
    return(list(xBreaks = xBreaks, yBreaks = yBreaks))

}
