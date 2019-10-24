#Some functions that act as a bridge between R and JASP. If JASP isn't running then all columnNames are expected to not be encoded

# four convenience functions to encode/ decode jasp column names. The key difference is that
# the first two look for exact matches whereas the bottom two do pattern matching.
encodeColumnName     <- function(x, fun = get0(".encodeColumnName"), ...)     return(.applyEnDeCoder(x, fun, ...))
decodeColumnName     <- function(x, fun = get0(".decodeColumnName"), ...)     return(.applyEnDeCoder(x, fun, ...))
encodeAllColumnNames <- function(x, fun = get0(".encodeAllColumnNames"), ...) return(.applyEnDeCoder(x, fun, ...))
decodeAllColumnNames <- function(x, fun = get0(".decodeAllColumnNames"), ...) return(.applyEnDeCoder(x, fun, ...))

# internal function that applies a decoding or encoding function (or actually any function) to R objects
# as long as they are character
.applyEnDeCoder <- function(x, fun, ...) {
  # get0 returns NULL if not found
  if (is.null(fun) || !is.function(fun))
    return(x)
  UseMethod(".applyEnDeCoder", x)
}

# default does nothing
.applyEnDeCoder.default <- function(x, fun) return(x)

.applyEnDeCoder.character <- function(x, fun) {
  for (i in seq_along(x))
    x[i] <- fun(x[i])
  return(x)
}

.applyEnDeCoder.list <- function(x, fun, recursive = FALSE) {
  # this function calls the .character method directly to avoid dispatching to .list and starting recursion.
  if (recursive) {
    return(rapply(x, f = .applyEnDeCoder.character, classes = "character", how = "replace"))
  } else {
    for (i in seq_along(x))
      if (is.character(x[[i]]))
        x[[i]] <- fun(x[[i]])

    return(x)
  }
}

.applyEnDeCoder.data.frame <- function(x, fun) {
  dnames <- dimnames(x)
  for (i in seq_along(dimnames(x)))
    .applyEnDeCoder.character(dnames[[i]])
  dimnames(x) <- dnames
  return(x)
}
