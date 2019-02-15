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

initJaspResults <- function() .onAttach()

checkForJaspResultsInit <- function() {if (!exists("jaspResults", .GlobalEnv)) .onAttach()}

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

createJaspPlot <- function(plot=NULL, title="", width=320, height=320, aspectRatio=0, error=NULL, errorMessage="", dependencies=NULL, position=NULL) {
	return(jaspPlotR$new(plot = plot, title = title, width = width, height = height, aspectRatio = aspectRatio, error = error, errorMessage = errorMessage, dependencies = dependencies, position = position))
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
