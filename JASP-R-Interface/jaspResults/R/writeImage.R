#Originally in common.R in package JASP, extracted here and changed for standalone use in jaspResults/jaspTools

tryToWriteImageJaspResults <- function(...)
{
  tryCatch(
    suppressWarnings(return(writeImageJaspResults(...))),
    error	= function(e) { return(list(error = e$message)) }
  )
}

getImageLocation <- function(type="png") {
  root <- file.path(tempdir(), "jaspResults", "plots")
  if (! dir.exists(root))
    dir.create(root, recursive=TRUE)
  numPlots <- length(list.files(root))
  list(
    root = root,
    relativePath = paste(numPlots + 1, type, sep=".")
  )
}

writeImageJaspResults <- function(width=320, height=320, plot, obj=TRUE, relativePathsvg=NULL, ppi=300, backgroundColor="white", location=getImageLocation("png"))
{
  # Set values from JASP'S Rcpp when available
  if (exists(".fromRCPP")) {
    location        <- .fromRCPP(".requestTempFileNameNative", "svg")
    backgroundColor <- .fromRCPP(".imageBackground")
    ppi             <- .fromRCPP(".ppi")
  }

  # TRUE if called from analysis, FALSE if called from editImage
  if (is.null(relativePathsvg))
    relativePathsvg <- location$relativePath

  image                           <- list()
  fullPathpng                     <- paste(location$root, relativePathsvg, sep="/")
  plotEditingOptions              <- NULL
  root                            <- location$root
  base::Encoding(relativePathsvg) <- "UTF-8"
  base::Encoding(root)            <- "UTF-8"
  oldwd                           <- getwd()
  setwd(root)
  on.exit(setwd(oldwd))

  # convert width & height from pixels to inches. ppi = pixels per inch. 4 is a magic number.
  width  <- width  / ppi * 4
  height <- height / ppi * 4

  plot2draw <- decodeplot(plot)

  if (ggplot2::is.ggplot(plot2draw) || inherits(plot2draw, c("gtable"))) {

    # TODO: ggsave adds very little when we use a function as device...
    ggplot2::ggsave(
      filename  = relativePathsvg,
      plot      = plot2draw,
      device    = function(filename, ...) svglite::svglite(file = filename, ...),
      dpi       = ppi,
      width     = width,
      height    = height,
      units     = "in", 
      bg        = backgroundColor,
      limitsize = FALSE # only necessary if users make the plot ginormous.
    )

    #If we have JASPgraphs available we can get the plotEditingOptions for this plot
    if(requireNamespace("JASPgraphs", quietly = TRUE))
      plotEditingOptions <- JASPgraphs::plotEditingOptions(graph=plot, asJSON=TRUE)

  } else {
    
    isRecordedPlot <- inherits(plot2draw, "recordedplot")

    # Open graphics device and plot
    svglite::svglite(file = relativePathsvg, width = width, height = height, bg = backgroundColor)

    if (is.function(plot2draw) && !isRecordedPlot) {

      if (obj) dev.control('enable') # enable plot recording
      eval(plot())
      if (obj) plot2draw <- recordPlot() # save plot to R object

    } else if (isRecordedPlot) { # function was called from editImage to resize the plot

      .redrawPlot(plot2draw) #(see below)
    } else if (inherits(plot2draw, "qgraph")) {

      qgraph:::plot.qgraph(plot2draw)

    } else {
      plot(plot2draw)
    }

    dev.off()
  }
  
  # Save path & plot object to output
  image[["png"]] <- relativePathsvg

  if (obj) {
    image[["obj"]]         <- plot2draw
    image[["editOptions"]] <- plotEditingOptions
  }

  return(image)
}

# Source: https://github.com/Rapporter/pander/blob/master/R/evals.R#L1389
# THANK YOU FOR THIS FUNCTION!
redrawPlotJaspResults <- function(rec_plot)
{
  if (getRversion() < '3.0.0')
  {
    #@jeroenooms
    for (i in 1:length(rec_plot[[1]]))
      if ('NativeSymbolInfo' %in% class(rec_plot[[1]][[i]][[2]][[1]]))
          rec_plot[[1]][[i]][[2]][[1]] <- getNativeSymbolInfo(rec_plot[[1]][[i]][[2]][[1]]$name)
  } else
  #@jjallaire
    for (i in 1:length(rec_plot[[1]]))
    {
      symbol <- rec_plot[[1]][[i]][[2]][[1]]
      if ('NativeSymbolInfo' %in% class(symbol))
      {
        if (!is.null(symbol$package)) name <- symbol$package[['name']]
        else                          name <- symbol$dll[['name']]

        pkg_dll       <- getLoadedDLLs()[[name]]
        native_symbol <- getNativeSymbolInfo(name = symbol$name, PACKAGE = pkg_dll, withRegistrationInfo = TRUE)
        rec_plot[[1]][[i]][[2]][[1]] <- native_symbol
      }
    }

  if (is.null(attr(rec_plot, 'pid')) || attr(rec_plot, 'pid') != Sys.getpid()) {
    warning('Loading plot snapshot from a different session with possible side effects or errors.')
    attr(rec_plot, 'pid') <- Sys.getpid()
  }

  suppressWarnings(grDevices::replayPlot(rec_plot))
}

decodeplot <- function(x, ...) {
  UseMethod("decodeplot", x)
}

decodeplot.JASPgraphsPlot <- function(x) {
  for (i in seq_along(x$subplots))
    x$subplots[[i]] <- decodeplot(x$subplots[[i]], returnGrob = FALSE)
  return(x)
}

decodeplot.gg <- function(x, returnGrob = TRUE) {
  # TODO: do not return a grid object!
  # we can do this by automatically replacing the scales and geoms, although this is quite a lot of work.
  # alternatively, those edge cases will need to be handled by the developer.
  labels <- x[["labels"]]
  for (i in seq_along(labels))
    if (!is.null(labels[[i]]))
      labels[[i]] <- decodeAllColumnNames(labels[[i]])
    
    x[["labels"]] <- labels
    if (returnGrob) {
      grDevices::png(f <- tempfile())
      on.exit({
        grDevices::dev.off()
        if (file.exists(f))
          file.remove(f)
      })
      return(decodeplot.gTree(ggplot2::ggplotGrob(x)))
    } else {
      return(x)
    }
}

decodeplot.recordedplot <- function(x) {
  decodeplot.gTree(grid::grid.grabExpr(gridGraphics::grid.echo(x)))
}

decodeplot.gtable <- function(x) rapply(x, f = decodeAllColumnNames, classes = "character", how = "replace")
decodeplot.grob   <- function(x) rapply(x, f = decodeAllColumnNames, classes = "character", how = "replace")
decodeplot.gTree  <- function(x) rapply(x, f = decodeAllColumnNames, classes = "character", how = "replace")
decodeplot.gDesc  <- function(x) rapply(x, f = decodeAllColumnNames, classes = "character", how = "replace")

decodeplot.qgraph <- function(x) {
  labels <- x[["graphAttributes"]][["Nodes"]][["labels"]]
  names  <- x[["graphAttributes"]][["Nodes"]][["names"]]
  labels <- decodeAllColumnNames(labels)
  names  <- decodeAllColumnNames(names)
  x[["graphAttributes"]][["Nodes"]][["labels"]] <- labels
  x[["graphAttributes"]][["Nodes"]][["names"]]  <- names
  return(x)
}

decodeplot.function <- function(x) {

  f <- tempfile()
  on.exit({
    grDevices::dev.off()
    if (file.exists(f))
      file.remove(f)
  })
  
  grDevices::png(f)
  grDevices::dev.control('enable') # enable plot recording
  
  eval(x())
  out <- grDevices::recordPlot()

  return(decodeplot.recordedplot(out))
}

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


