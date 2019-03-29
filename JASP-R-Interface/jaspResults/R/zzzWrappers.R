.onAttach <- function(libname, pkgname) {
	require(Rcpp)
	message(sprintf("jaspResults version: %s", packageVersion("jaspResults")))
	env <- globalenv()

	if (exists("jaspResults", env)) {
		message("Destroying all currently active jaspObjects, R will crash if you try to use any objects you still have loaded, and creating a *fresh* jaspResults.")
		destroyAllAllocatedObjects()
		destroyAllAllocatedRObjects()
	}

	env$jaspResults <- jaspResultsR$new(create_cpp_jaspResults("Analysis Test", NULL))

	message("jaspResults has been created and can now be used to test/develop your analysis, try something like:\njaspResults$print()\nor\njaspResults[[\"aTable\"]] <- createJaspTable()")
	return(invisible(TRUE))
	
}

#For use inside jaspResults to store plots and states (as is obvious from the name)
.plotStateStorage <- new.env()

initJaspResults <- function() .onAttach()

checkForJaspResultsInit <- function() {if (!exists("jaspResults", .GlobalEnv)) .onAttach()}

is.JaspResultsObj <- function(x) {
	inherits(x, "R6") && 
	inherits(x, c("jaspResultsR", "jaspContainerR", "jaspObjR", "jaspPlotR", "jaspTableR", "jaspHtmlR", "jaspStateR"))
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

createJaspPlot <- function(plot=NULL, title="", width=320, height=320, aspectRatio=0, error=NULL, dependencies=NULL, position=NULL) {
	return(jaspPlotR$new(plot = plot, title = title, width = width, height = height, aspectRatio = aspectRatio, error = error, dependencies = dependencies, position = position))
}

createJaspContainer <- function(title="", dependencies=NULL, position=NULL) {
	return(jaspContainerR$new(title = title, dependencies = dependencies, position = position))
}

createJaspTable <- function(title="", data=NULL, colNames=NULL, colTitles=NULL, overtitles=NULL, colFormats=NULL, rowNames=NULL, rowTitles=NULL, dependencies=NULL, position=NULL) {
	return(jaspTableR$new(title = title, data = data, colNames = colNames, colTitles = colTitles, overtitles = overtitles, colFormats = colFormats, rowNames = rowNames, rowTitles = rowTitles, dependencies = dependencies, position = position))
}

createJaspHtml <- function(text="", elementType="p", class="", dependencies=NULL, title="hide me", position=NULL) {
	# if you change "hide me" here then also change it in Common.R and in HtmlNode.js or come up with a way to define it in such a way to make it show EVERYWHERE...
	return(jaspHtmlR$new(text = text, elementType = elementType, class = class, dependencies = dependencies, title = title, position = position))
}

createJaspState <- function(object=NULL, title="", dependencies=NULL, position=NULL) {
	return(jaspStateR$new(object = object, title = title, dependencies = dependencies, position = position))
}

# also imported but that doesn't work in JASP
R6Class <- R6::R6Class

# R6 definitions
jaspObjR <- R6Class(
	classname = "jaspObjR", 
	cloneable = FALSE,
	public    = list(
		initialize                     = function()                            {stop("You should not create a new jaspObject!")},
		getJaspObject                  = function()                            {private$jaspObject},
		addCitation                    = function(x)                           {private$jaspObject$addCitation(x)},
		addMessage                     = function(x)                           {private$addMessage(x)},
		print                          = function()                            {private$jaspObject$print()},
		printHtml                      = function()                            {private$jaspObject$printHtml()},
		toHtml                         = function()                            {private$jaspObject$toHtml()},
		setError                       = function(x)                           {private$jaspObject$setError(x)},
		getError                       = function()                            {private$jaspObject$getError()},
		dependOn                      = function(options=NULL, optionsFromObject=NULL, optionContainsValue=NULL) {
			if (!is.null(options)) {
				if (!is.character(options))
					stop("please provide a character vector in `options`")
				private$jaspObject$dependOnOptions(options)
			}
			
			if (!is.null(optionsFromObject)) {
				if (is.JaspResultsObj(optionsFromObject)) {
					private$jaspObject$copyDependenciesFromJaspObject(optionsFromObject$getJaspObject())
				} else if (is.list(optionsFromObject)) {
					for (object in optionsFromObject)
						if (is.JaspResultsObj(object))
							private$jaspObject$copyDependenciesFromJaspObject(object$getJaspObject())
				} else {
					stop("please provide a (list of) jasp object(s) in `optionsFromObject`")
				}
			}
				
			if (!is.null(optionContainsValue)) {
				if (!is.list(optionContainsValue) || is.null(names(optionContainsValue)))
					stop("please provide a named list in `optionContainsValue`")
				for (i in seq_along(optionContainsValue)) {
					name <- names(optionContainsValue)[i]
					value <- optionContainsValue[[i]]
					if (length(value) != 1 || !is.atomic(value))
						stop("value provided in `optionContainsValue` must be of type atomic and length 1")
					private$jaspObject$setOptionMustContainDependency(name, value)
				}
			}
		}
	),
	active = list(
		position = function(x) {if (missing(x)) private$jaspObject$position else private$jaspObject$position <- as.numeric(x)},
		title    = function(x) {if (missing(x)) private$jaspObject$title    else private$jaspObject$title    <- x},
		warning  = function(x) {if (missing(x)) private$jaspObject$warning  else private$jaspObject$warning  <- x}
		
	),
	private   = list(
		jaspObject = NULL,
		finalize = function() print(paste0("Finalize called on ", class(self)[1L]))
	)
)
print.jaspObjR <- function(x, ...) {
	# TODO: print actual information depending on object type
	x$print()
}

jaspContainerR <- R6Class(
	classname = "jaspContainerR", 
	inherit   = jaspObjR, 
	cloneable = FALSE,
	public    = list(
		initialize = function(title = "", dependencies = NULL, position = NULL) {
			if (jaspResultsCalledFromJasp()) {
				container <- jaspResultsModule$create_cpp_jaspContainer(title)
			} else {
				checkForJaspResultsInit()
				container <- create_cpp_jaspContainer(title) # If we use R's constructor it will garbage collect our objects prematurely.. #new(jaspResultsModule$jaspContainer, title))
			}
			
			if (!is.null(dependencies))
				container$dependOnOptions(dependencies)
			
			if (is.numeric(position))
				container$position = position
			
			private$jaspObject <- container
			return()
		},
		setField   = function(field, value) {private$jaspObject[[field]] <- value$getJaspObject(); private$children[[field]] <- value},
		getField   = function(field)        {private$children[[field]]}
	),
	active    = list(
		length = function(value) { if (missing(value)) { private$jaspObject$length } else {stop("property 'length' is read-only!") }}
	),
	private   = list(
		children = list(),
		finalizer = function() print("Hoi")
	)
)

`[[<-.jaspContainerR` <- function(x, field, value) {x$setField(field, value); return(x)}
`[[.jaspContainerR`   <- function(x, field)        {x$getField(field)}

jaspResultsR <- R6Class(
	classname = "jaspResultsR",
	inherit   = jaspContainerR,
	cloneable = FALSE,
	public    = list(
		initialize       = function(x) {
			if (!missing(x) && isS4(x) && inherits(x, "Rcpp_jaspResultsClass")) {
				private$jaspObject = x
			} else if (inherits(x, "jaspResultsR")) {
				# this if is needed because JASP and R call jasprResults in different ways
				private$jaspObject = x$getJaspObject()
			} else {
			  stop("You should not create a new jaspResultsR object!")
			}
		},
		startProgressbar        = function(ntick, updateMs) {if (missing(updateMs)) private$jaspObject$startProgressbar(ntick) else private$jaspObject$startProgressbar(ntick, updateMs)},
		getResults              = function()                {private$jaspObject$getResults()},
		progressbarTick         = function()                {private$jaspObject$progressbarTick()},
		setOptions              = function(options)         {private$jaspObject$setOptions(options)},
		send                    = function()                {private$jaspObject$send()},
		setErrorMessage         = function(msg)             {private$jaspObject$setErrorMessage(msg)},
		changeOptions           = function(options)         {private$jaspObject$changeOptions(options)},
		getKeepList             = function()                {private$jaspObject$getKeepList()},
		complete                = function()                {private$jaspObject$complete()},
		getPlotObjectsForState  = function()                {private$jaspObject$getPlotObjectsForState()},
		getOtherObjectsForState = function()                {private$jaspObject$getOtherObjectsForState()}
	),
	active = list(
		relativePathKeep = function(x) {if (missing(x)) private$jaspObject$relativePathKeep else private$jaspObject$relativePathKeep <- x}
	)
)


jaspPlotR <- R6Class(
	classname = "jaspPlotR", 
	inherit   = jaspObjR,
	cloneable = FALSE,
	public = list(
		initialize = function(plot=NULL, title="", width=320, height=320, aspectRatio=0, error=NULL, 
							  dependencies=NULL, position=NULL) {
			if (jaspResultsCalledFromJasp()) {
				jaspPlotObj <- jaspResultsModule$create_cpp_jaspPlot(title)
			} else {
				checkForJaspResultsInit()
				jaspPlotObj  <- create_cpp_jaspPlot(title) # If we use R's constructor it will garbage collect our objects prematurely.. #new(jaspResultsModule$jaspPlot, title)
			}
			
			if(aspectRatio > 0 && !is.null(width) && width != 0)  height = aspectRatio * width
			else if(aspectRatio > 0)                              width = height / aspectRatio;
			
			jaspPlotObj$width  <- width
			jaspPlotObj$height <- height
			jaspPlotObj$aspectRatio <- aspectRatio
			
			if (!is.null(error))
				jaspPlotObj$setError(error)
			
			# if(!is.null(error) || errorMessage != "") {
			# 	if(is.null(error))  jaspPlotObj$error <- "errorMsgSet"
			# 	else                jaspPlotObj$error <- error
			# 	jaspPlotObj$errorMessage  <- errorMessage
			# }
			if (!is.null(plot))
				jaspPlotObj$plotObject <- plot
			
			if(!is.null(dependencies))
				jaspPlotObj$dependOnOptions(dependencies)
			
			if(is.numeric(position))
				jaspPlotObj$position = position
			
			private$jaspObject <- jaspPlotObj
			return()
		},
		addFootnote = function(footnote)     {private$jaspObject$addFootnote(footnote)}
	),
	active = list(
		plotObject   = function(x) {if (missing(x)) private$jaspObject$plotObject   else private$jaspObject$plotObject   <- x},
		aspectRatio  = function(x) {if (missing(x)) private$jaspObject$aspectRatio  else private$jaspObject$aspectRatio  <- x},
		width        = function(x) {if (missing(x)) private$jaspObject$width        else private$jaspObject$width        <- x},
		height       = function(x) {if (missing(x)) private$jaspObject$height       else private$jaspObject$height       <- x},
		# errorMessage = function(x) {if (missing(x)) private$jaspObject$errorMessage else private$jaspObject$errorMessage <- x},
		status       = function(x) {if (missing(x)) private$jaspObject$status       else private$jaspObject$status       <- x},
		filePathPng  = function(x) {if (missing(x)) private$jaspObject$filePathPng  else private$jaspObject$filePathPng  <- x}
	)
)

jaspTableR <- R6Class(
	classname = "jaspTableR", 
	inherit   = jaspObjR,
	cloneable = FALSE,
	public = list(
		initialize = function(title="", data=NULL, colNames=NULL, colTitles=NULL, overtitles=NULL, colFormats=NULL, rowNames=NULL, rowTitles=NULL, dependencies=NULL, position=NULL) {
			if (jaspResultsCalledFromJasp()) {
				jaspObj <- jaspResultsModule$create_cpp_jaspTable(title)
			} else {
				checkForJaspResultsInit()
				jaspObj <- create_cpp_jaspTable(title) # If we use R's constructor it will garbage collect our objects prematurely.. #new(jaspResultsModule$jaspTable, title)
				# jaspObj <- as(jaspObj, "jaspTableExtended") #We extend it so we may use addColumnInfo and addFootnote. Sadly enough this breaks for tables coming from a container.. This does however work in JASP but I cant get it to work in stand-alone. (This might be fixed by DvB)
			}
			
			if (!is.null(data))
				jaspObj$setData(data)
			
			if (!is.null(colNames))
				jaspObj$setColNames(colNames)
			
			if (!is.null(colTitles))
				jaspObj$setColTitles(colTitles)
			
			if (!is.null(overtitles))
				jaspObj$setColOvertitles(overtitles)
			
			if (!is.null(colFormats))
				jaspObj$setColFormats(colFormats)
			
			if (!is.null(rowNames))
				jaspObj$setRowNames(rowNames)
			
			if (!is.null(rowTitles))
				jaspObj$setRowTitles(rowTitles)
			
			if (!is.null(dependencies))
				jaspObj$dependOnOptions(dependencies)
			
			if (is.numeric(position))
				jaspObj$position <- position
			private$jaspObject <- jaspObj
			return()
		},
		addColumnInfo = function(name = NULL, title = NULL, overtitle = NULL, type = NULL, format = NULL, combine = NULL) {
			private$jaspObject$addColumnInfoHelper(name, title, type, format, combine, overtitle)
		},
		addFootnote = function(message = "", symbol = NULL, col_names = NULL, row_names = NULL) {
			private$jaspObject$addFootnoteHelper(message, symbol, col_names, row_names)
		},
		addRows         = function(row, rowNames = NULL) {
			if (is.null(rowNames)) {
				private$jaspObject$addRows(row) 
			} else {
				private$jaspObject$addRows(row, rowNames)
			}
		},
		setField        = function(field, value)         {private$jaspObject[[field]] <- value},
		getField        = function(field)                {private$jaspObject[[field]]},
		setData         = function(data)                 {private$jaspObject$setData(data)},
		addColumns      = function(cols)                 {private$jaspObject$addColumns(cols)},
		
		setExpectedRows = function(x)            {private$jaspObject$setExpectedRows(x)}
	),
	active = list(
		transpose                = function(x) {if (missing(x)) private$jaspObject$transpose                else private$jaspObject$transpose                <- x},
		transposeWithOvertitle   = function(x) {if (missing(x)) private$jaspObject$transposeWithOvertitle   else private$jaspObject$transposeWithOvertitle   <- x},
		status                   = function(x) {if (missing(x)) private$jaspObject$status                   else private$jaspObject$status                   <- x},
		showSpecifiedColumnsOnly = function(x) {if (missing(x)) private$jaspObject$showSpecifiedColumnsOnly else private$jaspObject$showSpecifiedColumnsOnly <- x}
	)
)
`[[<-.jaspTableR` <- function(x, field, value) {x$setField(field, value); return(x)}
`[[.jaspTableR`   <- function(x, field)        {x$getField(field)}

jaspHtmlR <- R6Class(
	classname = "jaspHtmlR", 
	inherit   = jaspObjR,
	cloneable = FALSE,
	public = list(
		initialize = function(text="", elementType="p", class="", dependencies=NULL, title="hide me", position=NULL) {
			# if you change "hide me" here then also change it in Common.R and in HtmlNode.js or come up with a way to define it in such a way to make it show EVERYWHERE...
			if (jaspResultsCalledFromJasp()) {
				htmlObj <- jaspResultsModule$create_cpp_jaspHtml(text)
			} else {
				checkForJaspResultsInit()
				htmlObj <- create_cpp_jaspHtml(text)
			}
			
			htmlObj$elementType <- elementType
			htmlObj$class       <- class
			htmlObj$title       <- title
			
			if (!is.null(dependencies))
				htmlObj$dependOnOptions(dependencies)
			
			if (is.numeric(position))
				htmlObj$position = position
			
			private$jaspObject <- htmlObj
			return()
		}
	),
	active = list(
		html        = function(value) {if (missing(value)) private$jaspObject$html        else private$jaspObject$html        <- value},
		text        = function(value) {if (missing(value)) private$jaspObject$text        else private$jaspObject$text        <- value},
		title       = function(value) {if (missing(value)) private$jaspObject$title       else private$jaspObject$html        <- value},
		class       = function(value) {if (missing(value)) private$jaspObject$class       else private$jaspObject$class       <- value},
		elementType = function(value) {if (missing(value)) private$jaspObject$elementType else private$jaspObject$elementType <- value}
	)
	
)

jaspStateR <- R6Class(
	classname = "jaspStateR", 
	inherit   = jaspObjR,
	cloneable = FALSE,
	public = list(
		initialize = function(object=NULL, title="", dependencies=NULL, position=NULL) {
			if (jaspResultsCalledFromJasp()) {
				stateObj <- jaspResultsModule$create_cpp_jaspState(title)
			} else {
				checkForJaspResultsInit()
				stateObj <- create_cpp_jaspState(title)
			}
			if (!is.null(object))
				stateObj$object <- object
			
			if (!is.null(dependencies))
				stateObj$dependOnOptions(dependencies)
			
			if (is.numeric(position))
				stateObj$position <-  position
			private$jaspObject <-  stateObj
			return()
		}
	),
	active = list(
		object = function(x) {if (missing(x)) private$jaspObject$object else private$jaspObject$object <- x}
	)
)