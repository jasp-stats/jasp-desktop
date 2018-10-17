
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


