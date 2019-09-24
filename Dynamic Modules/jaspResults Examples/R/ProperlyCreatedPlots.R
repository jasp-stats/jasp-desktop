ProperlyCreatedPlots <- function(jaspResults, dataset, options)
{
  ready <- options$weAreReady
  
  .properPlot(jaspResults, options, ready)
}

.properPlot <- function(jaspResults, options, ready) {
  # first check if we actually want the plot, or if it was previously computed
  if (!options$weWantThisPlot || !is.null(jaspResults[["plot"]]))
    return()
  
  # now create the jaspPlot object
  plot <- createJaspPlot(title="This is our plot", width=300, height=300)
  
  # set dependencies
  plot$dependOn(c("weWantThisPlot", "weAreReady"))
  
  # set citations
  plot$addCitation("This is a citation")
  
  # fix it's position in the output
  plot$position <- 1
  
  # now we assign the plot to jaspResults
  jaspResults[["plot"]] <- plot
  
  # check if we actually want to calculate anything or if we're done all together
  if (!ready)
    return()
  
  # now in a subfunction we'll go ahead and fill the plot up;
  # note that we only have to supply plot; it's pass-by-reference, so any change to plot is reflected in jaspResults
  .fillProperPlot(plot)
  
  # done
  return()
}

.fillProperPlot <- function(plot) {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(x=wt, y=mpg)) + ggplot2::geom_point()
  plot$plotObject <- p
}

# example of plot that has a lot of duplication, but does the same thing in essence;
# you should try to avoid duplication at all cost and be mindful when it's possible to simply exit the function instead of creating nested if's,
# additionally do not set errors when an analysis is simply not ready
.lessOptimalPlot <- function(jaspResults, dataset, options, ready) {
  if (options$weWantThisPlot && ready) {
	  
    if (is.null(jaspResults[["plot"]])) {
		
      p <- ggplot2::ggplot(mtcars, ggplot2::aes(x=wt, y=mpg)) + ggplot2::geom_point()
      jaspResults[["plot"]] <- createJaspPlot(plot = p, title = "This is our plot", height = 300, width = 300)
      jaspResults[["plot"]]$dependOn(options =c("weWantThisPlot", "weAreReady"))
      jaspResults[["plot"]]$position <- 1
	  jaspResults[["plot"]]$addCitation("This is a citation")
	  
    }
	
  } else if (options$weWantThisPlot) {
	
    jaspResults[["plot"]] <- createJaspPlot(plot = NULL, title = "This is our plot", height = 300, width = 300)
    jaspResults[["plot"]]$dependOn(options =c("weWantThisPlot", "weAreReady"))
    jaspResults[["plot"]]$position <- 1
	jaspResults[["plot"]]$addCitation("This is a citation")
	
  }
}