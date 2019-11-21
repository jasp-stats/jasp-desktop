#' @title Compute axis breaks 
#' @param x the object to compute axis breaks for
#'
#' @param ... if x is numeric, this is passed to pretty
#' @details this is just a wrapper for pretty with slightly different defaults. 
#'
#' @export
getPrettyAxisBreaks <- function(x, ...) {
  force(x)
  UseMethod("getPrettyAxisBreaks", x)
}

#' @export
getPrettyAxisBreaks.numeric <- function(x, ...) {

  dots <- list(...)

  if (is.null(dots[["high.u.bias"]]))
    xr <- range(x)
  if (xr[2] - xr[1] > 1e3) { # big numbers, adjust pretty to favor less breaks
    high.u.bias <- 2
  } else { # default value
    high.u.bias <-  1.5
  }
  dots[["x"]] <- x
  return(do.call(base::pretty, dots))
}

#' @export
getPrettyAxisBreaks.factor <- function(x, ...) {
  return(unique(x))
}

#' @export
getPrettyAxisBreaks.character <- function(x, ...) {
  return(unique(x))
}

#' @export
getPrettyAxisBreaks.default <- function(x, ...) {
  if (is.numeric(x))
    return(pretty(x, ...))
  else return(unique(x))
}
