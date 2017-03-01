# Saving Images from `JASP`
### Erik-Jan van Kesteren, 08/02/2017

## Introduction
A frequently requested feature in `JASP` is to export the plots to a vector file format, such as `eps` or `svg`. This would allow the plots to be incorporated in publications more easily, as publishers commonly require vector-based graphics. Currently, the plots are rendered in `png` only, for display in the output <a name="outputpaneref">pane</a>[&#185;](#outputpane) of the application. With the change proposed here, we will modernise the R image output functions in order to:

1. allow for vector graphics in `eps` format to be output and saved by the user.
2. create modularity in the program such that other file formats we might want to export to in the future will be easy to implement.
3. maintain full backwards compatibility with existing `JASP` files and code. 

(And 4: do not impact performance of `JASP`)

This document also includes a detailed [checklist](#implementation-guide) for implementing the newly proposed system in existing `JASP` analyses.

## Current system
Currently, figure output in `JASP` is handled by two functions: `.beginSaveImage()` and `.endSaveImage()`. The plotting code is wrapped in these two functions, meaning all output that the plotting function makes is routed to the png *graphics device* opened by `.beginSaveImage()`. In pseudocode:

```r
.beginSaveImage()

data <- "select relevant data"
data <- transform(data)
plot(data) # this outputs the plot

.endSaveImage()
```

The current system does not allow for easy outputting to multiple file formats. In fact, `R` itself is not very happy with outputting to multiple formats at the same time ([source](http://stackoverflow.com/questions/7942612/how-can-i-plot-to-multiple-devices-at-the-same-time#answer-7942670)): only one graphics device can be opened at a time.


## Proposed system
1. Represent an image as a function (base graphics) or as an object (ggplot).
2. Plot this image as normal using a single function, which also returns an abstract portable representation of this image.
3. Save the abstract representation (a "recordedplot" or "ggplot" object) to the state.
4. Upon request by the user, find the plot object in the state and output it to the requested format at the requested file location.
5. Enjoy :)

For the proposed system, I've developed three new common functions in `common.R`:
- `.writeImage()`
- `saveImage()` (note the lack of the '.') and `.redrawPlot()`

The following section will deal with these functions in reasonable detail.

### .writeImage
This function completely replaces both `.beginSaveImage()` and `.endSaveImage()`. Its input parameters are `width`, `height`, `plot` and `obj`. The first two are obviously the plot's width and height in pixels. The last parameter `obj` is a boolean value indicating whether the abstract representation should be saved. In general, we want this to be the case. Hence, its default value is `TRUE`.

The `plot` parameter accepts two classes of inputs:
1. a function that can be evaluated within this function. Preferably, this function has no arguments.
2. an object that plots upon applying the `print()` method. In the case of `JASP`, this will likely be a ggplot object.

Using this function thus requires the current (base graphics) plot to be wrapped in a function. This can be done in several ways, for example (in pseudocode):

1. Wrap the entire plotting routine in a function, i.e.:
  ```r
  plotfunction <- function(){
    data <- "select relevant data"
    plotData <- transform(data)
    plot(plotData, plotParameters, ...) # this outputs the plot
  }
  ```
2. Wrap only the construction of the plot in a function, i.e.:
  ```r
  data <- "select relevant data"
  plotData <- transform(data)
  
  plotfunction <- function(){
    plot(plotData, plotParameters, ...)
  }
  ```
This is where programmers will have to be creative in order to keep the code legible and clear.

Proper use of this `.writeImage()` function is then as follows (in pseudocode):

```r
.plotFunc() <- function() {
  plot(plotData, plotParameters)
}
imgObj <- .writeImage(width = options$plotWidth, 
                        height = options$plotHeight, 
                        plot = .plotFunc)

plot[["data"]] <- imgObj[["png"]]
plot[["obj"]] <- imgObj[["obj"]]
plot[["convertible"]] <- TRUE
plot[["status"]] <- "complete"
```

### saveImage and .redrawPlot
These functions handle the redrawing of the plot in the format the user specifies. For this, `saveImage` is called directly from the `JASP` output pane when the user clicks the "save as" option in the image menu. How this is done is beyond the scope of this document (ask the programmers). 

The function takes as its input a plot name (i.e. filename), the requested format (file extension), and the height and width of the original plot (to make sure the output looks the same as the original plot). It then looks for the plot object that was saved in the state with the procedure and prints it to a graphics device. 

For ggplot objects, this is simple: print(plotObject). For recordedplot objects, however, this is a bit more complex. This is where the function `.redrawPlot` comes in: it takes a recordedplot object and prints it to the graphics device. 

This is where the flexibility of output formats comes in: to add an output format, the programmer needs only to add this format to an `if..else` statement in this function and to the list of supported formats in the `C++` part of the interface.

## Goals
Let's see if the proposed system achieves the goals we set:

1. it allows for vector graphics in `eps` format to be output and saved by the user.
2. it creates modularity for future file formats by embedding the saving functionality in a common function.
3. it maintains full backwards compatibility with existing `JASP` files and code as the current functions will be kept as-is until *all* plotting functions have been migrated to the new system. 
4. performance is not impacted.

## Implementation guide
This section is a step-by step guide/checklist for implementing the `.writeImage()` function in existing `JASP` analyses. 

Let's take a simple hypothetical histogram plot function example in `JASP`:
```r
.histPlot <- function(dataset, options, perform){
  histPlot <- list("title" = "Histogram") # output list
  
  if (perform == "run") {
    histPlot[["width"]] <- options$plotWidth
    histPlot[["height"]] <- options$plotHeight
    histPlot[["custom"]] <- list(width = "plotWidth",
                                 height = "plotHeight")
                                         
    image <- .beginSaveImage(options$plotWidth, options$plotHeight)
    
    d <- dataset[[.v(options$variable)]] # Select data
    d <- d[!is.na(d)] # Remove empty
    
    hist(d, breaks = options$breaks) # plot hist
    
    if (options$abline){
      abline(v = mean(d)) # optional vertical line
    }
    
    content <- .endSaveImage(image)
    
    histPlot[["data"]] <- content
    histPlot[["status"]] <- "complete"
    
  }  else {
    
    histPlot[["width"]] <- options$plotWidth
    histPlot[["height"]] <- options$plotHeight
    histPlot[["custom"]] <- list(width = "plotWidth",
                                 height = "plotHeight")
    histPlot[["data"]] <- ""

  }
  
  histPlot
  
}
```

1. Locate the plot function and understand how and where the plot is created. This means (a) locating the `.beginSaveImage()` and `.endSaveImage()` functions, (b) knowing which list object contains the plot path, title, width, and height, (c) knowing exactly which lines write output to the graphics device. In the example above, those lines are `hist(d, breaks = options$breaks)` and `abline(v = mean(d))`. 

2. Create a plot object or plot function from the plot. In our example, one way would be:
  ```r
  histPlotFunc <- function(){
    hist(d, breaks = options$breaks) 
    if (options$abline){
      abline(v = mean(d)) # optional vertical line
    }    
  }
  ```
  Keep an eye on the environment the `histPlotFunc()` is called in: if its parent environment does not contain options$abline or d, the function will not know what to do!
  
3. Remove the `.beginSaveImage()` and `.endSaveImage()` functions and replace it with a `.writeImage()` call:
  ```r
  imgObj <- .writeImage(width = options$plotWidth,
                        height = options$plotHeight,
                        plot = histPlotFunc)
  
  histPlot[["data"]] <- imgObj[["png"]]
  histPlot[["obj"]] <- imgObj[["obj"]]
  histplot[["convertible"]] <- TRUE
  histPlot[["status"]] <- "complete"
  ```
  The "convertible" element of the list is an element that is used by the javascript to determine whether a menu option to save the image should be added.
  

With minimal changes, we have now implemented image saving functionality into the plots of this analysis.

Next, we need to make the plot objects available to be retrieved by the javascript app.

4. We are going to add the state objects to a new element of the state that is returned, which will have the name "figures". `state[["figures"]]` is a list where each element is a plot object with as its name the original path of the plot, i.e., `plot[["data"]]`.
  ```r
  # Initialise figstate
  state <- .retrieveState()
	figstate <- try(state[["figures"]], silent = TRUE)
	if (class(figstate) == "try-error") figstate <- list()
  
  # Save the correlation plot object (if it exists)
  if (!is.null(corrPlot[["obj"]])){
		figstate[[corrPlot[["data"]]]] <- corrPlot[["obj"]]
    
    # remove the corrplot "object" from the returned plot variable so it remains small
		corrPlot <- corrPlot[names(corrPlot)!="obj"] 
	}
  
  results[["plots"]] <- list(distributionPlots=distrPlots, matrixPlot=corrPlot,
														 splitPlots=splitPlots, title="Plots")
  
  # Return the figstate along with the other state objects and results
  return(list(results=results, status="complete", 
              state=list(options=options, results=results, figures=figstate), 
              keep=keep))
  ```

---
<a id="outputpane">1</a>. Remember that the output pane is simply a stripped-down webbrowser. Web-browsers easily incorporate `png` and `jpg` in the html page, but not `eps`!	[&#8629;](#outputpaneref)
