#' @export
getPrettyAxisBreaks <- function(x, ...) {
    force(x)
    UseMethod("getPrettyAxisBreaks", x)
}

#' @export
axesBreaks <- function(x, ...) {
  getPrettyAxisBreaks(x, ...)
}

# @method getPrettyAxisBreaks numeric
#' @export
getPrettyAxisBreaks.numeric <- function(x, ...) {
    return(pretty(x, ...))
}

#' @export
getPrettyAxisBreaks.factor <- function(x, ...) {
    return(unique(x))
}

#' @export
getPrettyAxisBreaks.character <- function(x, ...) {
    return(unique(x))
}

# @method getPrettyAxisBreaks default
#' @export
getPrettyAxisBreaks.default <- function(x, ...) {
    if (is.numeric(x))
        return(pretty(x, ...))
    else return(unique(x))
}

# @method getPrettyAxisBreaks data.frame

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