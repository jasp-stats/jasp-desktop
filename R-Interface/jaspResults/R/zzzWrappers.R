.onAttach <- function(libname, pkgname) {
	env <- globalenv()

	if (exists("jaspResults", env)) {
		destroyAllAllocatedObjects()
		destroyAllAllocatedRObjects()
		rm(list=ls(envir=.plotStateStorage), envir=.plotStateStorage)
	}

	env$jaspResults <- jaspResultsR$new(create_cpp_jaspResults("Analysis Test", NULL))
	
	return(invisible(TRUE))
}

#For use inside jaspResults to store plots and states (as is obvious from the name)
.plotStateStorage <- new.env()

initJaspResults <- function() .onAttach()

signalAnalysisAbort <- function(message = "", call = NULL) {
  abort <- structure(
    list(
      message = message,
      call = call
    ),
    class = c("jaspAnalysisAbort", "condition")
  )
  signalCondition(abort)
}

startProgressbar <- function(expectedTicks, label="") {
	if (!is.numeric(expectedTicks) || !is.character(label))
		stop("`expectedTicks` must be numeric and `label` a character", domain = NA)
	if (nchar(label) > 80) # if you update this value, also update it in the progressbar in jaspwidgets.js
		warning("The progressbar label is more than 80 characters, label will be truncated", domain = NA)
		
	if (jaspResultsCalledFromJasp())
		jaspResultsModule$cpp_startProgressbar(expectedTicks, label)
	else
		cpp_startProgressbar(expectedTicks, label)
}

progressbarTick <- function() { 
	if (jaspResultsCalledFromJasp())
		jaspResultsModule$cpp_progressbarTick()
	else
		cpp_progressbarTick()
}

checkForJaspResultsInit <- function() {if (!exists("jaspResults", .GlobalEnv)) .onAttach()}

is.JaspResultsObj <- function(x) {
	inherits(x, "R6") && 
  inherits(x, c("jaspResultsR", "jaspContainerR", "jaspObjR", "jaspOutputObjR", "jaspPlotR", "jaspTableR", "jaspHtmlR", "jaspStateR", "jaspColumnR"))
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

createJaspPlot <- function( plot = NULL, title = "",    width = 320,   height = 320,    aspectRatio = 0,           error = NULL,  dependencies = NULL,         position = NULL)
  return(jaspPlotR$new(     plot = plot, title = title, width = width, height = height, aspectRatio = aspectRatio, error = error, dependencies = dependencies, position = position))

createJaspContainer <- function(  title = "",     dependencies = NULL,          position = NULL,     initCollapsed = FALSE)
  return(jaspContainerR$new(      title = title,  dependencies = dependencies,  position = position, initCollapsed = initCollapsed))

createJaspTable <- function( title="",       data = NULL, colNames = NULL,     colTitles = NULL,       overtitles = NULL,       colFormats = NULL,       rowNames = NULL,     rowTitles = NULL,      dependencies = NULL,         position = NULL,     expectedRows = NULL,          expectedColumns = NULL)
  return(jaspTableR$new(      title = title,  data = data, colNames = colNames, colTitles = colTitles,  overtitles = overtitles, colFormats = colFormats, rowNames = rowNames, rowTitles = rowTitles, dependencies = dependencies, position = position, expectedRows = expectedRows,  expectedColumns = expectedColumns))

# if you change "hide me" here then also change it in Common.R and in HtmlNode.js or come up with a way to define it in such a way to make it show EVERYWHERE...
createJaspHtml <- function( text = "",    elementType = "p",         maxWidth = "15cm",   class = "",    dependencies = NULL,         title = "hide me", position = NULL)
    return(jaspHtmlR$new(   text = text,  elementType = elementType, maxWidth = maxWidth, class = class, dependencies = dependencies, title = title,     position = position))

createJaspState <- function(object = NULL,   dependencies = NULL)
  return(jaspStateR$new(    object = object, dependencies = dependencies))

createJaspColumn <- function(columnName = "",         dependencies = NULL)
  return(jaspColumnR$new(    columnName = columnName, dependencies = dependencies))

# also imported but that doesn't work in JASP
R6Class <- R6::R6Class

# inheritance structure:
# 1. jaspResults	->	-
# 2. jaspObj			->	2.1. jaspState
#									->	2.2. jaspOutputObj	->	2.2.1. jaspHtml
#																					->	2.2.2. jaspContainer
#																					->	2.2.3. jaspPlot
#																					->	2.2.4. jaspTable
#																					->	2.2.5. jaspColumn

# R6 definitions

jaspResultsR <- R6Class(
	classname = "jaspResultsR",
	cloneable = FALSE,
	public    = list(
		initialize = function(x) {
			if (!missing(x) && isS4(x) && inherits(x, "Rcpp_jaspResultsClass"))
				private$jaspObject = x
      else if (inherits(x, "jaspResultsR")) # this if is needed because JASP and R call jasprResults in different ways
				private$jaspObject = private$getJaspObject(x)
			else
			  stop("You should not create a new jaspResultsR object!", domain = NA)
		},

		addCitation = function(x) {
			if (!is.character(x)) 
				stop("Citation must be a character (vector)", domain = NA)
			for (i in seq_along(x))
				private$jaspObject$addCitation(x[i])
		},
		print           = function()	private$jaspObject$print(),
		printHtml       = function()	private$jaspObject$printHtml(),
		setError        = function(x)	private$jaspObject$setError(x),
		getError        = function()	private$jaspObject$getError(),
		length          = function()    private$jaspObject$length
	),
	private = list(
		children    = list(),
		jaspObject  = NULL,
		jaspCppToR6 = function(cppObj) {
			return(switch(
				class(cppObj),
        "Rcpp_jaspPlot"      = jaspPlotR$new(     jaspObject = cppObj),
        "Rcpp_jaspTable"     = jaspTableR$new(    jaspObject = cppObj),
        "Rcpp_jaspContainer" = jaspContainerR$new(jaspObject = cppObj),
        "Rcpp_jaspColumn"    = jaspColumnR$new(   jaspObject = cppObj),
        "Rcpp_jaspState"     = jaspStateR$new(    jaspObject = cppObj),
        "Rcpp_jaspHtml"      = jaspHtmlR$new(     jaspObject = cppObj),
				stop(sprintf("Invalid call to jaspCppToR6. Expected jaspResults object but got %s", class(cppObj)), domain = NA)
			))
		},
    #These two functions should be the exact same as those on jaspContainer
		setField	= function(field, value) {
			private$jaspObject[[field]] <- private$getJaspObject(value);
			private$children[[field]]   <- value;
		},
		getField	= function(field) {
			#maybe changing the dependencies removed this object when we weren't looking!
			if (is.null(private$jaspObject[[field]]) && !is.null(private$children[[field]]))
				private$children[[field]] <- NULL

			#other way 'round is also quite possible, we just regenerated jaspResults from state/json and now the R6 class doesn't know anything about it...
			if (!is.null(private$jaspObject[[field]]) && is.null(private$children[[field]]))
				private$children[[field]] <- private$jaspCppToR6(private$jaspObject[[field]])

			return(private$children[[field]])
		},
		getJaspObject           = function(R6obj)   R6obj$.__enclos_env__$private$jaspObject,
		getResults              = function()        private$jaspObject$getResults(),
		setOptions              = function(options) private$jaspObject$setOptions(options),
		send                    = function()        private$jaspObject$send(),
		setErrorMessage         = function(msg)     private$jaspObject$setErrorMessage(msg),
		changeOptions           = function(options) private$jaspObject$changeOptions(options),
		getKeepList             = function()        private$jaspObject$getKeepList(),
		complete                = function()        private$jaspObject$complete(),
		getPlotObjectsForState  = function()        private$jaspObject$getPlotObjectsForState(),
    getOtherObjectsForState = function()        private$jaspObject$getOtherObjectsForState()
	),
	active = list(
	  status = function(x) { if (missing(x)) private$jaspObject$status else private$jaspObject$status <- x },
	  info   = function(x) { if (missing(x)) private$jaspObject$info   else private$jaspObject$info   <- x }
	)
)

`[[<-.jaspResultsR` <- function(x, field, value) {
	x$.__enclos_env__$private$setField(field, value)
	return(x)
}
`[[.jaspResultsR`   <- function(x, field)
	x$.__enclos_env__$private$getField(field)
	
print.jaspResultsR <- function(x, ...) 	# TODO: make this a pretty summary print (But please do this in std::string jaspObject::toString() and the overrides)
	x$print()

jaspObjR <- R6Class(
	classname = "jaspObjR", 
	cloneable = FALSE,
	public    = list(
		initialize = function()	stop("You should not create a new jaspObject!", domain = NA),
		print      = function()	private$jaspObject$print(),
		dependOn   = function(options=NULL, optionsFromObject=NULL, optionContainsValue=NULL) {
			if (!is.null(options)) {
				if (!is.character(options))
					stop("please provide a character vector in `options`", domain = NA)
				private$jaspObject$dependOnOptions(options)
			}
			
			if (!is.null(optionsFromObject)) {
				if (is.JaspResultsObj(optionsFromObject)) {
					private$jaspObject$copyDependenciesFromJaspObject(private$getJaspObject(optionsFromObject))
				} else if (is.list(optionsFromObject)) {
					for (object in optionsFromObject)
						if (is.JaspResultsObj(object))
							private$jaspObject$copyDependenciesFromJaspObject(private$getJaspObject(object))
				} else {
					stop("please provide a (list of) jasp object(s) in `optionsFromObject`", domain = NA)
				}
			}
				
			if (!is.null(optionContainsValue)) {
				if (!is.list(optionContainsValue) || is.null(names(optionContainsValue)))
					stop("please provide a named list in `optionContainsValue`", domain = NA)
				for (i in seq_along(optionContainsValue)) {
					name <- names(optionContainsValue)[i]
					value <- optionContainsValue[[i]]
					private$jaspObject$setOptionMustContainDependency(name, value)
				}
			}
		}
	),
	private	= list(
		jaspObject    = NULL,
		getJaspObject = function(R6obj) R6obj$.__enclos_env__$private$jaspObject
	)
)

print.jaspObjR <- function(x, ...) 	# TODO: print actual information depending on object type
	x$print()

jaspStateR <- R6Class(
	classname = "jaspStateR",
	inherit   = jaspObjR,
	cloneable = FALSE,
	public    = list(
		initialize = function(object=NULL, dependencies=NULL, jaspObject=NULL) {
			if (!is.null(jaspObject)) {
			  private$jaspObject <- jaspObject
			  return()
			} else if (jaspResultsCalledFromJasp()) {
				stateObj <- jaspResultsModule$create_cpp_jaspState("")
			} else {
				checkForJaspResultsInit()
				stateObj <- create_cpp_jaspState("")
			}
			if (!is.null(object))
				stateObj$object <- object
			
			if (!is.null(dependencies))
				stateObj$dependOnOptions(dependencies)

			private$jaspObject <-  stateObj
			return()
		}
	),
	active = list(
		object = function(x) { if (missing(x)) private$jaspObject$object else private$jaspObject$object <- x }
	)
)

jaspOutputObjR <- R6Class(
	classname = "jaspOutputObjR",
	inherit   = jaspObjR,
	cloneable = FALSE,
	public    = list(
		initialize  = function()	stop("You should not create a new jaspOutputObject!", domain = NA),
		printHtml   = function()	private$jaspObject$printHtml(),
		setError    = function(x)	private$jaspObject$setError(x),
		getError    = function()	private$jaspObject$getError(),
		addCitation = function(x) {
			if (!is.character(x)) 
				stop("Citation must be a character (vector)", domain = NA)
			for (i in seq_along(x))
				private$jaspObject$addCitation(x[i])
		}
	),
	active	= list(
		position = function(x) { if (missing(x)) private$jaspObject$position else private$jaspObject$position <- as.numeric(x) },
		title    = function(x) { if (missing(x)) private$jaspObject$title    else private$jaspObject$title    <- x },
		info     = function(x) { if (missing(x)) private$jaspObject$info     else private$jaspObject$info     <- x }
	)
)

.jaspHtmlPixelizer <- function(maxWidth)
{
  if(is.numeric(maxWidth)) return(paste0(as.character(maxWidth), "px"))
  return(maxWidth)
}

jaspHtmlR <- R6Class(
	classname = "jaspHtmlR",
	inherit   = jaspOutputObjR,
	cloneable = FALSE,
	public    = list(
	initialize = function(text="", elementType="p", maxWidth="15cm", class="", dependencies=NULL, title="hide me", position=NULL , info=NULL, jaspObject = NULL) {
			# if you change "hide me" here then also change it in Common.R and in HtmlNode.js or come up with a way to define it in such a way to make it show EVERYWHERE...
			if (!is.null(jaspObject)) {
			  private$jaspObject <- jaspObject
			  return()
			} else if (jaspResultsCalledFromJasp()) {
				htmlObj <- jaspResultsModule$create_cpp_jaspHtml(text)
			} else {
				checkForJaspResultsInit()
				htmlObj <- create_cpp_jaspHtml(text)
			}
			
			htmlObj$elementType <- elementType
			htmlObj$class       <- class
			htmlObj$maxWidth    <- .jaspHtmlPixelizer(maxWidth)
			htmlObj$title       <- title
			
            if (!is.null(dependencies))
			    htmlObj$dependOnOptions(dependencies)

            if (!is.null(info))
			    htmlObj$info <- info
			
			if (is.numeric(position))
				htmlObj$position = position

			private$jaspObject <- htmlObj
			return()
		}
	),
	active = list(
		text        = function(value) { if (missing(value)) private$jaspObject$text        else private$jaspObject$text        <- value },
    class       = function(value) { if (missing(value)) private$jaspObject$class       else private$jaspObject$class       <- value },
    maxWidth    = function(value) { if (missing(value)) private$jaspObject$maxWidth    else private$jaspObject$maxWidth    <- .jaspHtmlPixelizer(value) },
		elementType = function(value) { if (missing(value)) private$jaspObject$elementType else private$jaspObject$elementType <- value }
	)
)

jaspContainerR <- R6Class(
	classname = "jaspContainerR",
	inherit   = jaspOutputObjR,
	cloneable = FALSE,
	public    = list(
		initialize = function(title = "", dependencies = NULL, position = NULL, info=NULL, initCollapsed = FALSE, jaspObject = NULL) {
			if (!is.null(jaspObject)) {
				private$jaspObject <- jaspObject
				return()
			} else if (jaspResultsCalledFromJasp()) {
				container <- jaspResultsModule$create_cpp_jaspContainer(title)
			} else {
				checkForJaspResultsInit()
				container <- create_cpp_jaspContainer(title) # If we use R's constructor it will garbage collect our objects prematurely.. #new(jaspResultsModule$jaspContainer, title))
			}

			if (!is.null(dependencies))
				container$dependOnOptions(dependencies)

			if (is.numeric(position))
				container$position <- position

			if (is.logical(initCollapsed))
				container$initCollapsed <- initCollapsed

			if (!is.null(info))
				container$info <- info

			private$jaspObject <- container
			return()
		},
		length = function() private$jaspObject$length
	),
  active = list(
    initCollapsed  = function(x) if (missing(x)) private$jaspObject$initCollapsed   else private$jaspObject$initCollapsed   <- x
  ),
	private	= list(
		children    = list(),
		jaspObject  = NULL,
		jaspCppToR6 = function(cppObj) {
			return(switch(
				class(cppObj),
        "Rcpp_jaspPlot"      = jaspPlotR$new(      jaspObject = cppObj ),
        "Rcpp_jaspTable"     = jaspTableR$new(     jaspObject = cppObj ),
        "Rcpp_jaspContainer" = jaspContainerR$new( jaspObject = cppObj ),
        "Rcpp_jaspColumn"    = jaspColumnR$new(    jaspObject = cppObj ),
        "Rcpp_jaspState"     = jaspStateR$new(     jaspObject = cppObj ),
        "Rcpp_jaspHtml"      = jaspHtmlR$new(      jaspObject = cppObj ),
				stop(sprintf("Invalid call to jaspCppToR6. Expected jaspResults object but got %s", class(cppObj)), domain = NA)
			))
		},
    #These two functions should be the exact same as those for jaspResults
		setField   = function(field, value) {
			private$jaspObject[[field]] <- private$getJaspObject(value);
			private$children[[field]]   <- value;
		},
		getField   = function(field) {
			#maybe changing the dependencies removed this object when we weren't looking!
			if (is.null(private$jaspObject[[field]]) && !is.null(private$children[[field]]))
				private$children[[field]] <- NULL

			#other way 'round is also quite possible, we just regenerated jaspResults from state/json and now the R6 class doesn't know anything about it...
			if (!is.null(private$jaspObject[[field]]) && is.null(private$children[[field]]))
				private$children[[field]] <- private$jaspCppToR6(private$jaspObject[[field]])

			return(private$children[[field]]);
		}
	)
)

`[[<-.jaspContainerR` <- function(x, field, value) {
	x$.__enclos_env__$private$setField(field, value)
	return(x)
}
`[[.jaspContainerR`   <- function(x, field)
	x$.__enclos_env__$private$getField(field)
	
jaspPlotR <- R6Class(
	classname = "jaspPlotR",
	inherit   = jaspOutputObjR,
	cloneable = FALSE,
	public    = list(
		initialize = function(plot=NULL, title="", width=320, height=320, aspectRatio=0, error=NULL, 
		                      dependencies=NULL, position=NULL , info=NULL, jaspObject = NULL) {
			if (!is.null(jaspObject)) {
			  private$jaspObject <- jaspObject
			  return()
      }
      else if (jaspResultsCalledFromJasp())
      {
        private$jaspObject <- jaspResultsModule$create_cpp_jaspPlot(title)
      }
      else
      {
				checkForJaspResultsInit()
        private$jaspObject  <- create_cpp_jaspPlot(title) # If we use R's constructor it will garbage collect our objects prematurely.. #new(jaspResultsModule$jaspPlot, title)
			}

      jaspPlotObj <- private$jaspObject
			
			if (aspectRatio > 0 && !is.null(width) && width != 0)
				height = aspectRatio * width
			else if (aspectRatio > 0)
				width = height / aspectRatio
			
			jaspPlotObj$width  <- width
			jaspPlotObj$height <- height
			jaspPlotObj$aspectRatio <- aspectRatio
			
			if (!is.null(error))
				jaspPlotObj$setError(error)
			
			if (!is.null(plot))
				jaspPlotObj$plotObject <- plot
			
			if(!is.null(dependencies))
				jaspPlotObj$dependOnOptions(dependencies)

            if (!is.null(info))
			    jaspPlotObj$info <- info
			
			if(is.numeric(position))
				jaspPlotObj$position = position
			
			return()
		}
	),
	active = list(
		plotObject  = function(x) if (missing(x)) private$jaspObject$plotObject   else private$jaspObject$plotObject   <- x,
		aspectRatio = function(x) if (missing(x)) private$jaspObject$aspectRatio  else private$jaspObject$aspectRatio  <- x,
		width       = function(x) if (missing(x)) private$jaspObject$width        else private$jaspObject$width        <- x,
		height      = function(x) if (missing(x)) private$jaspObject$height       else private$jaspObject$height       <- x,
		status      = function(x) if (missing(x)) private$jaspObject$status       else private$jaspObject$status       <- x
	)
)

.jaspTableSetExpectedSize <- function(table=NULL, rows=NULL, cols=NULL) {

  if(is.null(table))
    stop(".jaspTableSetExpectedSize expects a table!", domain = NA)

  inputTypes <- c(mode(rows), mode(cols))

  if (!all(inputTypes %in% c("numeric", "NULL")))	stop("Please use numeric values to set the expected size", domain = NA)

  if (!is.null(rows) && !is.null(cols))		table$setExpectedSize(cols, rows)
  else if (!is.null(rows))        				table$setExpectedRows(rows)
  else if(!is.null(cols))         				table$setExpectedColumns(cols)
  else                                    stop("Enter cols, rows or both in setExpectedSize!", domain = NA)
}


jaspTableR <- R6Class(
	classname = "jaspTableR",
	inherit   = jaspOutputObjR,
	cloneable = FALSE,

	public    = list(
	initialize = function(title="", data=NULL, colNames=NULL, colTitles=NULL, overtitles=NULL, colFormats=NULL, rowNames=NULL, rowTitles=NULL, dependencies=NULL, position=NULL , info=NULL, expectedRows=NULL, expectedColumns=NULL, jaspObject=NULL) {
			if (!is.null(jaspObject)) {
			  private$jaspObject <- jaspObject
			  return()
			} else if (jaspResultsCalledFromJasp()) {
				jaspObj <- jaspResultsModule$create_cpp_jaspTable(title)
			} else {
				checkForJaspResultsInit()
				jaspObj <- create_cpp_jaspTable(title) # If we use R's constructor it will garbage collect our objects prematurely.. #new(jaspResultsModule$jaspTable, title)
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

            if (!is.null(info))
			    jaspObj$info <- info
			
			if (is.numeric(position))
				jaspObj$position <- position

      if (!(is.null(expectedRows) & is.null(expectedColumns)))
        .jaspTableSetExpectedSize(jaspObj, rows=expectedRows, cols=expectedColumns);

				
			private$jaspObject <- jaspObj
			return()
		},

	addColumns  = function(cols) private$jaspObject$addColumns(cols),
   	setData     = function(data) private$jaspObject$setData(data),

	addFootnote = function(message = "", symbol = NULL, colNames = NULL, rowNames = NULL) {
      if (is.null(message) || !is.character(message) || length(message) == 0)
      {
        print("addFootnote got a strange message:")
        print(message)
        stop("jaspTable$addFootnote expects \"message\" to be a string!", domain = NA)
      }

		  # if the developer doesn't want a footnote pointing to a specific cell/col/row and doesn't specify "Note.",
		  # then we add this to ensure footnotes look consistent across analyses
      if (is.null(symbol)	&& is.null(colNames) && is.null(rowNames))
        symbol <- gettext("<em>Note.</em>", domain = "R-JASP")
        
			private$jaspObject$addFootnoteHelper(message, symbol, colNames, rowNames)
		},

    addColumnInfo = function(name = NULL, title = NULL, overtitle = NULL, type = NULL, format = NULL, combine = NULL) {
			if (!is.null(type)) {
				permittedTypes <- c("integer", "number", "pvalue", "string", "separator")
				if (!type %in% permittedTypes)
					stop("type must be ", paste0("`", permittedTypes, "`", collapse=", "), " (provided type: `", type, "`)", domain = NA)

				if (is.null(format) && type == "number")
					format <- "sf:4;dp:3"
				else if (type == "pvalue")
					format <- "dp:3;p:.001"
			}
			private$jaspObject$addColumnInfoHelper(name, title, type, format, combine, overtitle)
    },

    addRows = function(rows, rowNames = NULL) {

      maxElementLength <- 0 # Lets check if the user means a single row...
      if(is.list(rows) & !is.data.frame(rows))  maxElementLength <- max(unlist(lapply(rows, length)))
      else if(is.vector(rows))                  maxElementLength <- 1

      if(maxElementLength == 1)
      {
        if (is.null(rowNames))    private$jaspObject$addRow(rows)
        else                      private$jaspObject$addRow(rows, rowNames)
      }
      else
      {
        if (is.null(rowNames))    private$jaspObject$addRows(rows)
        else                      private$jaspObject$addRows(rows, rowNames)
      }
    },

    setExpectedSize     = function(rows=NULL, cols=NULL)      { .jaspTableSetExpectedSize(private$jaspObject, rows=rows, cols=cols); },

    getColumnName       = function(columnIndex)               { return( private$jaspObject$colNames           [[columnIndex]]);               },
    setColumnName       = function(columnIndex, newName)      {         private$jaspObject$colNames$insert(     columnIndex,  newName);       },
    getColumnTitle      = function(columnName)                { return( private$jaspObject$colTitles          [[columnName]]);                },
    setColumnTitle      = function(columnName, newTitle)      {         private$jaspObject$colTitles$insert(    columnName,   newTitle);      },
    getColumnOvertitle  = function(columnName)                { return( private$jaspObject$colOvertitles      [[columnName]]);                },
    setColumnOvertitle  = function(columnName, newOvertitle)  {         private$jaspObject$colOvertitles$insert(columnName,   newOvertitle);  },
    getColumnFormat     = function(columnName)                { return( private$jaspObject$colFormats         [[columnName]]);                },
    setColumnFormat     = function(columnName, newFormat)     {         private$jaspObject$colFormats$insert(   columnName,   newFormat);     },
    getColumnCombine    = function(columnName)                { return( private$jaspObject$colCombines        [[columnName]]);                },
    setColumnCombine    = function(columnName, newCombine)    {         private$jaspObject$colCombines$insert(  columnName,   newCombine);    },
    getColumnType       = function(columnName)                { return( private$jaspObject$colTypes           [[columnName]]);                },
    setColumnType       = function(columnName, newType)       {         private$jaspObject$colTypes$insert(     columnName,   newType);       },
    getRowName          = function(rowIndex)                  { return( private$jaspObject$rowNames           [[rowIndex]]);                  },
    setRowName          = function(rowIndex, newName)         {         private$jaspObject$rowNames$insert(     rowIndex,     newName);       },
    getRowTitle         = function(rowName)                   { return( private$jaspObject$rowTitles          [[rowName]]);                   },
    setRowTitle         = function(rowName, newTitle)         {         private$jaspObject$rowTitles$insert(    rowName,      newTitle);      }
	),
	active = list(
		transpose                = function(x) if (missing(x)) private$jaspObject$transpose                else private$jaspObject$transpose                <- x,
		transposeWithOvertitle   = function(x) if (missing(x)) private$jaspObject$transposeWithOvertitle   else private$jaspObject$transposeWithOvertitle   <- x,
		status                   = function(x) if (missing(x)) private$jaspObject$status                   else private$jaspObject$status                   <- x,
    showSpecifiedColumnsOnly = function(x) if (missing(x)) private$jaspObject$showSpecifiedColumnsOnly else private$jaspObject$showSpecifiedColumnsOnly <- x
	),
	private = list(
		setField = function(field, value) private$jaspObject[[field]] <- value,
		getField = function(field)        return(private$jaspObject[[field]])
	)
)

`[[<-.jaspTableR` <- function(x, field, value) {
	x$.__enclos_env__$private$setField(field, value)
	return(x)
}
`[[.jaspTableR`   <- function(x, field)
	x$.__enclos_env__$private$getField(field)

jaspColumnR <- R6Class(
  classname = "jaspColumnR",
  inherit   = jaspOutputObjR,
  cloneable = FALSE,
  public    = list(
    initialize = function(columnName="", dependencies=NULL, scalarData=NULL, ordinalData=NULL, nominalData=NULL, nominalTextData=NULL, info=NULL, jaspObject = NULL) {
      if (!is.null(jaspObject)) {
        private$jaspObject <- jaspObject
        return()
      }

      if (columnName == "")
        stop("You MUST specify a name for the column you want to change the data of", domain = NA)

      if (jaspResultsCalledFromJasp()) {
        columnObj <- jaspResultsModule$create_cpp_jaspColumn(columnName)
      } else {
        checkForJaspResultsInit()
        columnObj <- create_cpp_jaspColumn(columnName)
      }

      if(!is.null(scalarData))      columnObj$setScale(scalarData)
      if(!is.null(ordinalData))     columnObj$setOrdinal(ordinalData)
      if(!is.null(nominalData))     columnObj$setNominal(nominalData)
      if(!is.null(nominalTextData)) columnObj$setNominalText(nominalTextData)
	  if (!is.null(info))			columnObj$info <- info

      if (!is.null(dependencies))
        columnObj$dependOnOptions(dependencies)

      private$jaspObject <- columnObj
      return()
    },
    setScale        = function(scalarData)  private$jaspObject$setScale(scalarData),
    setOrdinal      = function(ordinalData) private$jaspObject$setOrdinal(ordinalData),
    setNominal      = function(nominalData) private$jaspObject$setNominal(nominalData),
    setNominalText  = function(nominalData) private$jaspObject$setNominalText(nominalData)
  )
)
