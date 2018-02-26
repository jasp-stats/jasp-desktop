
extendJaspTableAndCreateJaspResults <- function()
{
	return(function()
	{
		library(Rcpp)
		env <- globalenv()

		if(exists("jaspResults", env))
		{
			print("Destroying all currently active jaspObjects, R will crash if you try to use any objects you still have loaded, and creating a *fresh* jaspResults.")
			destroyAllAllocatedObjects()
		}
		

		env$jaspResults <- create_cpp_jaspResults("Analysis Test")

		setRefClass(
			"jaspTableExtended", 
			contains=c("Rcpp_jaspTable"), 
			methods=list(
				addColumnInfo 	= function(name=NULL, title=NULL, overtitle=NULL, type=NULL, format=NULL, combine=NULL) { addColumnInfoHelper(name, title, type, format, combine, overtitle) }, 
				addFootnote 	= function(message="", symbol=NULL, col_names=NULL, row_names=NULL) { addFootnoteHelper(message, symbol, col_names, row_names) },
				finalize		= function() {}
				),
			where=globalenv()
		)

		return("jaspResults has been created and can now be used to test/develop your analysis, try something like: 'jaspResults$print()' or 'jaspResults[[\"aTable\"]] <- createJaspTable()'.")
	})
}

# initJaspResults is a function that creates jaspResults for use in a stand-alone R environment
initJaspResults <- extendJaspTableAndCreateJaspResults()

checkForJaspResultsInit <- function()
{
	env <- globalenv()

	if(!exists("jaspResults", env))
		initJaspResults()
}

###########################
## JASP Results Wrappers ##
###########################

# These functions should be roughly the same in name and functionality as those found in JASP under common.R

# if title is left unset (aka "") then, when added to a container/results, it will take the fieldname as title.
# width and height should be set to some global default setting and only in exceptional cases manually. Maybe we could take it from JASPplot?
# aspectRatio of > 0 sets height to width * aspectRatio.
createJaspPlot <- function(plot=NULL, title="", width=320, height=320, aspectRatio=0, error=NULL, errorMessage="", dependencies=NULL)
{
  checkForJaspResultsInit()

  jaspPlotObj  <- create_cpp_jaspPlot(title) # If we use R's constructor it will garbage collect our objects prematurely.. #new(jaspResultsModule$jaspPlot, title)

  if(aspectRatio > 0 && !is.null(width) && width != 0)  height = aspectRatio * width
  else if(aspectRatio > 0)                              width = height / aspectRatio;

  jaspPlotObj$width  <- width
  jaspPlotObj$height <- height
  jaspPlotObj$aspectRatio <- aspectRatio

  if(!is.null(error) || errorMessage != "")
  {
    if(is.null(error))  jaspPlotObj$error <- "errorMsgSet"
    else                jaspPlotObj$error <- error
    jaspPlotObj$errorMessage  <- errorMessage
  }

  if(!is.null(plot))
	{
		writtenImage <- tryCatch(
			.writeImage(width=width, height=height, plot),
			error	= function(e) { jaspPlotObj$errorMessage <- e$message; return(NULL) }
		)

		if(!is.null(writtenImage))
		{
			jaspPlotObj$filePathPng <- writtenImage[["png"]]
      jaspPlotObj$plotObject <- plot
		}
	}

  if(!is.null(dependencies))
    jaspPlotObj$dependOnTheseOptions(dependencies)

  return(jaspPlotObj)
}

createJaspContainer <- function(title="")
{
  checkForJaspResultsInit()
  return(create_cpp_jaspContainer(title)) # If we use R's constructor it will garbage collect our objects prematurely.. #new(jaspResultsModule$jaspContainer, title))
}

createJaspTable <- function(title="", data=NULL, colNames=NULL, colTitles=NULL, colFormats=NULL, rowNames=NULL, rowTitles=NULL, dependencies=NULL)
{
  checkForJaspResultsInit()

  jaspObj <- create_cpp_jaspTable(title) # If we use R's constructor it will garbage collect our objects prematurely.. #new(jaspResultsModule$jaspTable, title)
  jaspObj <- as(jaspObj, "jaspTableExtended") #We extend it so we may use addColumnInfo and addFootnote. Sadly enough this breaks for tables coming from a container.. This does however work in JASP and I cant get it to work in stand-alone.

  if(!is.null(data))
    jaspObj$setData(data)

  if(!is.null(colNames))
    jaspObj$setColNames(colNames)

  if(!is.null(colTitles))
    jaspObj$setColTitles(colTitles)

  if(!is.null(colFormats))
    jaspObj$setColFormats(colFormats)

  if(!is.null(rowNames))
    jaspObj$setRowNames(rowNames)

  if(!is.null(rowTitles))
    jaspObj$setRowTitles(rowTitles)

  if(!is.null(dependencies))
    jaspObj$dependOnTheseOptions(dependencies)

  return(jaspObj)
}

createJaspHtml <- function(text="", elementType="p")
{
  checkForJaspResultsInit()
  #return(new(jaspResultsModule$jaspHtml, text, elementType))
  htmlObj <- create_cpp_jaspHtml(text) # If we use R's constructor it will garbage collect our objects prematurely.. #
  htmlObj$elementType <- elementType

  return(htmlObj)
}

createJaspState <- function(object=NULL, title="", dependencies=NULL)
{
  checkForJaspResultsInit()

  stateObj <- jaspResultsModule$create_cpp_jaspState(title) # If we use R's constructor it will garbage collect our objects prematurely.. #

  if(!is.null(object))
    stateObj$object <- object

  if(!is.null(dependencies))
    stateObj$dependOnTheseOptions(dependencies)

  return(stateObj)
}