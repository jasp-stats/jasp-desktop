#' @title Tell that this string should be parsed.
#'
#' @param x the object to be parsed, or the object to test if it will be parsed.
#'
#' @details \code{parseThis} adds an attribute to a character or factor such that JASPgraphs knows it should be
#' parsed. \code{needsParsing} return \code{TRUE} if a character has said attribute.


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
needsParsing <- function(x) {
  UseMethod("needsParsing", x)
}

#' @export
needsParsing.character <- function(x) isTRUE(attr(x, "parse"))
#' @export
needsParsing.factor    <- function(x) isTRUE(attr(x, "parse"))
#' @export
needsParsing.data.frame <- function(x) {
  nc <- ncol(x)
  out <- logical(nc)
  for (i in seq_len(nc)) {
    if (is.factor(x[[i]]) || is.character(x[[i]])) {
      out[i] <- needsParsing(x[[i]])
    }
  }
  return(out)
}