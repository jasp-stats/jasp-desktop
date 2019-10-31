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

.applyEnDeCoder.factor <- function(x, fun) {
  levels(x) <- .applyEnDeCoder.character(levels(x), fun)
  return(x)
}

.applyEnDeCoder.list <- function(x, fun, recursive = FALSE) {
  # this function calls the .character method directly to avoid dispatching to .list and starting recursion.
  if (recursive) {
    return(rapply(x, f = .applyEnDeCoder.character, classes = "character", how = "replace"))
  } else {
    for (i in seq_along(x))
      if (is.character(x[[i]]))
        x[[i]] <- .applyEnDeCoder.character(x[[i]], fun)

    return(x)
  }
}

.applyEnDeCoder.data.frame <- function(x, fun) {
  dnames <- dimnames(x)
  for (i in seq_along(dimnames(x)))
    .applyEnDeCoder.character(dnames[[i]], fun)
  dimnames(x) <- dnames
  return(x)
}

.decodeplot <- function(x, ...) {
  UseMethod(".decodeplot", x)
}

.decodeplot.JASPgraphsPlot <- function(x) {
  for (i in seq_along(x$subplots))
    x$subplots[[i]] <- .decodeplot(x$subplots[[i]], returnGrob = FALSE)
  return(x)
}

.decodeplot.gg <- function(x, returnGrob = TRUE) {
  # TODO: do not return a grid object!
  # we can do this by automatically replacing the scales and geoms, although this is quite a lot of work.
  # alternatively, those edge cases will need to be handled by the developer.
  labels <- x[["labels"]]
  for (i in seq_along(labels))
    if (!is.null(labels[[i]]))
      labels[[i]] <- decodeAllColumnNames(labels[[i]])

    x[["labels"]] <- labels
    if (returnGrob)
      return(.decodeplot.gTree(ggplot2::ggplotGrob(x)))
    else
      return(x)
}

.decodeplot.recordedplot <- function(x) {
  .decodeplot.gTree(grid::grid.grabExpr(gridGraphics::grid.echo(x)))
}

.decodeplot.gtable <- function(x) rapply(x, f = decodeAllColumnNames, classes = "character", how = "replace")
.decodeplot.grob   <- function(x) rapply(x, f = decodeAllColumnNames, classes = "character", how = "replace")
.decodeplot.gTree  <- function(x) rapply(x, f = decodeAllColumnNames, classes = "character", how = "replace")
.decodeplot.gDesc  <- function(x) rapply(x, f = decodeAllColumnNames, classes = "character", how = "replace")

.decodeplot.qgraph <- function(x) {
  labels <- x[["graphAttributes"]][["Nodes"]][["labels"]]
  names  <- x[["graphAttributes"]][["Nodes"]][["names"]]
  labels <- decodeAllColumnNames(labels)
  names  <- decodeAllColumnNames(names)
  x[["graphAttributes"]][["Nodes"]][["labels"]] <- labels
  x[["graphAttributes"]][["Nodes"]][["names"]]  <- names
  return(x)
}
