.onAttach <- function(libname, pkgname) {
	require(Rcpp)
	message(sprintf("jaspResults version: %s", packageVersion("jaspResults")))
	env <- globalenv()

	if(exists("jaspResults", env))
	{
		message("Destroying all currently active jaspObjects, R will crash if you try to use any objects you still have loaded, and creating a *fresh* jaspResults.")
		destroyAllAllocatedObjects()
		destroyAllAllocatedRObjects()
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
	
	e <- getNamespace("jaspResults")
	unlockBinding('.__C__Rcpp_jaspTable', e)
	
	# these functions were obtained using dput on a method 
	# e.g., dput(createJaspTable()$addColumnInfo)
	# the refClassName was adjusted.
	if (is.null(e$.__C__Rcpp_jaspTable@refMethods$addColumnInfo)) {
		e$.__C__Rcpp_jaspTable@refMethods$addColumnInfo <- 
			new("refMethodDef", 
					.Data = function (name = NULL, title = NULL, overtitle = NULL, type = NULL, format = NULL, combine = NULL) {
						addColumnInfoHelper(name, title, type, format, combine, overtitle)
					}, 
					mayCall = "addColumnInfoHelper", 
					name = "addColumnInfo", 
					refClassName = "Rcpp_jaspTable", 
					superClassMethod = ""
			)
	}
	if (is.null(e$.__C__Rcpp_jaspTable@refMethods$addFootnote)) {
		e$.__C__Rcpp_jaspTable@refMethods$addFootnote <- 
			new("refMethodDef", 
					.Data = function (message = "", symbol = NULL, col_names = NULL, row_names = NULL) {
						addFootnoteHelper(message, symbol, col_names, row_names)
					}, 
					mayCall = "addFootnoteHelper", 
					name = "addFootnote",
					refClassName = "Rcpp_jaspTable",
					superClassMethod = ""
			)
	}
	if (is.null(e$.__C__Rcpp_jaspTable@refMethods$finalize)) {
		e$.__C__Rcpp_jaspTable@refMethods$finalize <- 
			new("refMethodDef", 
					.Data = function () {}, 
					mayCall = character(0), 
					name = "finalize", 
					refClassName = "Rcpp_jaspTable", 
					superClassMethod = "finalize#Rcpp_jaspTable"
			)
	}
	lockBinding('.__C__Rcpp_jaspTable', e)
	
	message("jaspResults has been created and can now be used to test/develop your analysis, try something like:\njaspResults$print()\nor\njaspResults[[\"aTable\"]] <- createJaspTable()")
	return(invisible(TRUE))
	
}

initJaspResults <- function() .onAttach()


checkForJaspResultsInit <- function()
{
	if(!exists("jaspResults", .GlobalEnv))
		.onAttach()
}

is.JaspResultsObj <- function(x) {
	isS4(x) && 
		inherits(x, c("Rcpp_jaspPlot", "Rcpp_jaspHtml", "Rcpp_jaspResultsClass", 
									"Rcpp_jaspIntlist", "Rcpp_jaspContainer", "Rcpp_jaspStringlist", 
									"Rcpp_jaspObject", "Rcpp_jaspDoublelist", "Rcpp_jaspBoollist", 
									"Rcpp_jaspTable", "Rcpp_jaspState",
									"jaspTableExtended"
									))
}

destroyAllAllocatedRObjects <- function() {

	# some attempt to clear out R objects with invalid pointers	
	s <- search()
	envs2Search <- s[!(startsWith(s, "package:") | startsWith(s, "tools:") | s == "Autoloads")]
	
	for (envName in envs2Search) {
		
		nms2rm <- character()
		env <- as.environment(envName)
		
		for (n in names(env)) {
			if (is.JaspResultsObj(env[[n]])) {
				
				# check if externalpoint of object is invalid
				if (isTRUE(try(silent = TRUE, identical(
					env[[n]]$.pointer,
					new("externalptr")
					)))) {
					nms2rm <- c(nms2rm, n)
				}
			}
		}
		# delete objects from env
		if (length(nms2rm) > 0)
				rm(list = nms2rm, envir = env)
	}
}

jaspResultsCalledFromJasp <- function() {
  # a variety of tests to check if a createJasp*() function is called from JASP
  return(
    exists("jaspResultsModule", mode = "S4") && 
      inherits(jaspResultsModule, "Module") && 
      identical(slotNames(jaspResultsModule), ".xData")
  )
}

###########################
## JASP Results Wrappers ##
###########################

# These functions should be roughly the same in name and functionality as those found in JASP under common.R

# if title is left unset (aka "") then, when added to a container/results, it will take the fieldname as title.
# width and height should be set to some global default setting and only in exceptional cases manually. Maybe we could take it from JASPplot?
# aspectRatio of > 0 sets height to width * aspectRatio.
createJaspPlot <- function(plot=NULL, title="", width=320, height=320, aspectRatio=0, error=NULL, errorMessage="", dependencies=NULL, position=NULL)
{
  if (jaspResultsCalledFromJasp()) 
  {
    jaspPlotObj <- jaspResultsModule$create_cpp_jaspPlot(title)
  } 
  else 
  {
    checkForJaspResultsInit()
    jaspPlotObj  <- create_cpp_jaspPlot(title) # If we use R's constructor it will garbage collect our objects prematurely.. #new(jaspResultsModule$jaspPlot, title)
  }

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

  jaspPlotObj$plotObject <- plot

  if(!is.null(dependencies))
    jaspPlotObj$dependOnOptions(dependencies)

  if(is.numeric(position))
    jaspPlotObj$position = position

  return(jaspPlotObj)
}

createJaspContainer <- function(title="", dependencies=NULL, position=NULL)
{
  if (jaspResultsCalledFromJasp()) 
  {
    container <- jaspResultsModule$create_cpp_jaspContainer(title)
  } 
  else 
  {
    checkForJaspResultsInit()
    container <- create_cpp_jaspContainer(title) # If we use R's constructor it will garbage collect our objects prematurely.. #new(jaspResultsModule$jaspContainer, title))
  }

  if(!is.null(dependencies))
    container$dependOnOptions(dependencies)

  if(is.numeric(position))
    container$position = position

  return(container)
}

createJaspTable <- function(title="", data=NULL, colNames=NULL, colTitles=NULL, overtitles=NULL, colFormats=NULL, rowNames=NULL, rowTitles=NULL, dependencies=NULL, position=NULL)
{
  if (jaspResultsCalledFromJasp()) 
  {
    jaspObj <- jaspResultsModule$create_cpp_jaspTable(title)
  } 
  else 
  {
    checkForJaspResultsInit()
    jaspObj <- create_cpp_jaspTable(title) # If we use R's constructor it will garbage collect our objects prematurely.. #new(jaspResultsModule$jaspTable, title)
    jaspObj <- as(jaspObj, "jaspTableExtended") #We extend it so we may use addColumnInfo and addFootnote. Sadly enough this breaks for tables coming from a container.. This does however work in JASP but I cant get it to work in stand-alone. (This might be fixed by DvB)
  }

  if(!is.null(data))
    jaspObj$setData(data)

  if(!is.null(colNames))
    jaspObj$setColNames(colNames)

  if(!is.null(colTitles))
    jaspObj$setColTitles(colTitles)

  if(!is.null(overtitles))
    jaspObj$setColOvertitles(overtitles)

  if(!is.null(colFormats))
    jaspObj$setColFormats(colFormats)

  if(!is.null(rowNames))
    jaspObj$setRowNames(rowNames)

  if(!is.null(rowTitles))
    jaspObj$setRowTitles(rowTitles)

  if(!is.null(dependencies))
    jaspObj$dependOnOptions(dependencies)

	if(is.numeric(position))
		jaspObj$position = position

  return(jaspObj)
}

createJaspHtml <- function(text="", elementType="p", class="", dependencies=NULL, title="hide me", position=NULL) # if you change "hide me" here then also change it in Common.R and in HtmlNode.js or come up with a way to define it in such a way to make it show EVERYWHERE...
{
  if (jaspResultsCalledFromJasp()) 
  {
    htmlObj <- jaspResultsModule$create_cpp_jaspHtml(text)
  } 
  else 
  {
    checkForJaspResultsInit()
    htmlObj <- create_cpp_jaspHtml(text)
  }

  htmlObj$elementType <- elementType
  htmlObj$class       <- class
  htmlObj$title       <- title

  if(!is.null(dependencies))
    htmlObj$dependOnOptions(dependencies)

	if(is.numeric(position))
		htmlObj$position = position


  return(htmlObj)
}


createJaspState <- function(object=NULL, title="", dependencies=NULL, position=NULL)
{
  if (jaspResultsCalledFromJasp()) 
  {
    stateObj <- jaspResultsModule$create_cpp_jaspState(title)
  } 
  else 
  {
    checkForJaspResultsInit()
    stateObj <- create_cpp_jaspState(title)
  }

  if(!is.null(object))
    stateObj$object <- object

  if(!is.null(dependencies))
    stateObj$dependOnOptions(dependencies)

	if(is.numeric(position))
    stateObj$position = position

  return(stateObj)
}
