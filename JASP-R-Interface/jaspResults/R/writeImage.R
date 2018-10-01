#Originally in common.R in package JASP, extracted here and changed for standalone use in jaspResults/jaspTools

tryToWriteImageJaspResults <- function(...)
{
  tryCatch(
    suppressWarnings(return(writeImageJaspResults(...))),
    error	= function(e) { return(list(error = e$message)) }
  )
}

writeImageJaspResults <- function(width=320, height=320, plot, obj = TRUE, relativePathpng = NULL)
{
  # Initialise output object
  image <- list()

  # Create png file location
  location <- .fromRCPP(".requestTempFileNameNative", "png")
  # TRUE if called from analysis, FALSE if called from editImage
  if (is.null(relativePathpng))
    relativePathpng <- location$relativePath

  fullPathpng <- paste(location$root, relativePathpng, sep="/")

  base::Encoding(relativePathpng) <- "UTF-8"

  root <- location$root
  base::Encoding(root) <- "UTF-8"
  oldwd <- getwd()
  setwd(root)
  on.exit(setwd(oldwd))

  if (ggplot2::is.ggplot(plot)) {
    ppi <- .fromRCPP(".ppi")

    # fix for mac
    if (Sys.info()["sysname"] == "Darwin") ppi <- ppi / 2

    ggplot2::ggsave(relativePathpng, plot, "png",
                    width  = 1.5*width/ppi,
                    height = 1.5*height/ppi,
                    dpi    = 2*ppi,
                    bg     = "transparent")
  } else {
    # Operating System information
  	type <- "cairo"
  	if (Sys.info()["sysname"]=="Darwin"){
  	    type <- "quartz"
  	}
  	# Calculate pixel multiplier
  	pngMultip <- .fromRCPP(".ppi") / 96
    isRecordedPlot <- inherits(plot, "recordedplot")

    # Open graphics device and plot
    grDevices::png(filename=relativePathpng, width=width * pngMultip,
                   height=height * pngMultip, bg="transparent",
                   res=72 * pngMultip, type=type)

    if (is.function(plot) && !isRecordedPlot) {
      if (obj) dev.control('enable') # enable plot recording
      eval(plot())
      if (obj) plot <- recordPlot() # save plot to R object
    } else if (isRecordedPlot) { # function was called from editImage to resize the plot
      .redrawPlot(plot) #(see below)
    } else if (inherits(plot, "qgraph")) {
      qgraph::plot.qgraph(plot)
    } else if (inherits(plot, c("gtable", "ggMatrixplot", "JASPgraphs"))) {
			gridExtra::grid.arrange(plot)
    } else {
      plot(plot)
    }
    dev.off()
  }


  # Save path & plot object to output
  image[["png"]] <- relativePathpng
  if (obj) image[["obj"]] <- plot

  # Return relative paths in list
  image
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
