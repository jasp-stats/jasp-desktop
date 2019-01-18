#' @title Tell that this string should be parsed.
#'
#' @param x the object to be parsed, or the object to test if it will be parsed.
#'
#' @details \code{parseThis} adds an attribute to a character or factor such that JASPgraphs knows it should be
#' parsed. \code{isParsed} return \code{TRUE} if a character has said attribute.


#' @rdname parseThis
#' @export
parseThis <- function(x) {
  UseMethod("parseThis", x)
}

#' @export
parseThis.character <- function(x) {attr(x, "parse") <- TRUE; x}
#' @export
parseThis.factor    <- function(x) {attr(x, "parse") <- TRUE; x}

#' @rdname parseThis
#' @export
isParsed <- function(x) {
  UseMethod("isParsed", x)
}

#' @export
isParsed.character <- function(x) isTRUE(attr(x, "parse"))
#' @export
isParsed.factor    <- function(x) isTRUE(attr(x, "parse"))
#' @export
isParsed.data.frame <- function(x) {
  nc <- ncol(x)
  out <- logical(nc)
  for (i in seq_len(nc)) {
    if (is.factor(x[[i]]) || is.character(x[[i]])) {
      out[i] <- isParsed(x[[i]])
    }
  }
  return(out)
}