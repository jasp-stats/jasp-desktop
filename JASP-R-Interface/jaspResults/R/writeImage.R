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

writeImageJaspResults <- function(width=320, height=320, plot, obj=TRUE, relativePathpng=NULL, ppi=300, backgroundColor="white", location=getImageLocation("png"))
{
  # Set values from JASP'S Rcpp when available
  if (exists(".fromRCPP")) {
    location        <- .fromRCPP(".requestTempFileNameNative", "png")
    backgroundColor <- .fromRCPP(".imageBackground")
    ppi             <- .fromRCPP(".ppi")
  }

  # TRUE if called from analysis, FALSE if called from editImage
  if (is.null(relativePathpng))
    relativePathpng <- location$relativePath

  image                           <- list()
  fullPathpng                     <- paste(location$root, relativePathpng, sep="/")
  plotEditingOptions              <- NULL
  root                            <- location$root
  base::Encoding(relativePathpng) <- "UTF-8"
  base::Encoding(root)            <- "UTF-8"
  oldwd                           <- getwd()
  setwd(root)
  on.exit(setwd(oldwd))

  type <- "cairo" # Operating System information (where to draw to)
  if(Sys.info()["sysname"]=="Darwin")
    type <- "quartz"


  if (ggplot2::is.ggplot(plot) || inherits(plot, c("gtable", "ggMatrixplot", "JASPgraphs"))) {

    pngMultip <- ppi / 96
    ggplot2::ggsave(
    	filename  = relativePathpng, 
    	plot      = plot, 
    	device    = grDevices::png,
    	width     = width  * pngMultip,
    	height    = height * pngMultip,
    	dpi       = ppi,
      bg        = backgroundColor,
    	res       = 72 * pngMultip,
    	type      = type,
    	limitsize = FALSE # because we supply png as a function, we specify pixels rather than inches
    )

    #If we have JASPgraphs available we can get the plotEditingOptions for this plot
    if(requireNamespace("JASPgraphs", quietly = TRUE))
      plotEditingOptions <- JASPgraphs::plotEditingOptions(graph=plot, asJSON=TRUE)

  } else {
    
  	# Calculate pixel multiplier
  	pngMultip <- ppi / 96
    isRecordedPlot <- inherits(plot, "recordedplot")

    # Open graphics device and plot
    grDevices::png(filename=relativePathpng, width=width * pngMultip,
	               height=height * pngMultip, bg=backgroundColor,
                   res=72 * pngMultip, type=type)

    if (is.function(plot) && !isRecordedPlot) {

      if (obj) dev.control('enable') # enable plot recording
      eval(plot())
      if (obj) plot <- recordPlot() # save plot to R object

    } else if (isRecordedPlot) { # function was called from editImage to resize the plot

      .redrawPlot(plot) #(see below)
    } else if (inherits(plot, "qgraph")) {

      qgraph:::plot.qgraph(plot)

    } else {
      plot(plot)
    }

    dev.off()
  }


  # Save path & plot object to output
  image[["png"]] <- relativePathpng

  if (obj) {
    image[["obj"]]         <- plot
    image[["editOptions"]] <- plotEditingOptions
  }



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
