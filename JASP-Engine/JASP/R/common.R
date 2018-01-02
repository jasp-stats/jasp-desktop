#
# Copyright (C) 2013-2015 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

run <- function(name, title, dataKey, options, resultsMeta, stateKey, requiresInit=TRUE, perform="run") {

	dataKey <- rjson::fromJSON(dataKey)
	options <- rjson::fromJSON(options)
	resultsMeta <- rjson::fromJSON(resultsMeta)
	stateKey <- rjson::fromJSON(stateKey)

	analysis <- eval(parse(text=name))
	
	env <- new.env()
	env$callback <- callback
	env$time <- Sys.time()
	env$i <- 1

	if (perform == "init") {
	
		the.callback <- function(...) list(status="ok")
		
	} else {
	
		the.callback <- function(...) {
			
			t <- Sys.time()
			
			cat(paste("Callback", env$i, ":", t - env$time, "\n"))
			
			env$time <- t
			env$i <- env$i + 1
			
			return(env$callback(...))
		}
	}

	dataset <- NULL
	if (! is.null(dataKey)) {
		cols <- .getDataSetCols(dataKey, options)
		if (perform == "run")
			dataset <- do.call(.readDataSetToEnd, cols)
		else
			dataset <- do.call(.readDataSetHeader, cols)
	}

	oldState <- NULL
	if ('state' %in% names(formals(analysis))) {
		oldState <- .getStateFromKey(stateKey, options)
	}
	
	if (perform == "init" && ! requiresInit) {

		emptyResults <- try(.createEmptyResults(resultsMeta), silent=TRUE)
		if (inherits(emptyResults, "try-error")) {
			msg <- .sanitizeForJson(emptyResults)
			return(paste("{ \"status\" : \"error\", \"results\" : { \"error\" : 1, \"errorMessage\" : \"", msg, "\" } }", sep=""))
		}	

		results <- list()
		results[["results"]] <- emptyResults
		results[["results"]][["title"]] <- title
		results[["status"]] <- "inited"

		json <- try({ rjson::toJSON(results) })
		
		if (inherits(results, "try-error"))
			return(paste("{ \"status\" : \"error\", \"results\" : { \"error\" : 1, \"errorMessage\" : \"", "Unable to jsonify", "\" } }", sep=""))

		return(json)

	}

	results <- tryCatch(expr={
		
				withCallingHandlers(expr={
					
					analysis(dataset=dataset, options=options, perform=perform, callback=the.callback, state=oldState)
					
				}, 
				error=.addStackTrace)
				
	},
	error=function(e) e)

	if (inherits(results, "expectedError")) {
		
		errorResponse <- paste("{ \"status\" : \"error\", \"results\" : { \"title\" : \"error\", \"error\" : 1, \"errorMessage\" : \"", results$message, "\" } }", sep="")
		
		errorResponse
		
	} else if (inherits(results, "error")) {
		
		error <- .sanitizeForJson(results)
		
		stackTrace <- .sanitizeForJson(results$stackTrace)
		stackTrace <- paste(stackTrace, collapse="<br><br>")
		
		errorMessage <- .generateErrorMessage(type='exception', error=error, stackTrace=stackTrace)
		errorResponse <- paste("{ \"status\" : \"exception\", \"results\" : { \"title\" : \"error\", \"error\" : 1, \"errorMessage\" : \"", errorMessage, "\" } }", sep="")
		
		errorResponse
		
	} else if (is.null(results)) {
	
		"null"
	
	} else {
	
		keep <- newState <- NULL
	
		if ("state" %in% names(results)) {

			newState <- results$state
			results$state <- NULL

		}
		
		if ("results" %in% names(results)) {
		
			results <- try(.parseResults(results, title, resultsMeta, oldState), silent=TRUE)
			if (inherits(results, "try-error")) {
				msg <- .sanitizeForJson(results)
				return(paste("{ \"status\" : \"error\", \"results\" : { \"error\" : 1, \"errorMessage\" : \"", msg, "\" } }", sep=""))
			}	
			
			if (! is.null(names(newState)))
				newState[["figures"]] <- .imgToState(results$results)
			
			results <- .imgToResults(results)
			results$results <- .addCitationToResults(results$results)

			keep <- .saveState(newState)$relativePath
			results$keep <- c(results$keep, keep)  # keep the state file
			
		} else {
		
			results <- .addCitationToResults(results)
		}
		
		json <- try({ rjson::toJSON(results) })
		
		if (class(json) == "try-error") {
		
			return(paste("{ \"status\" : \"error\", \"results\" : { \"error\" : 1, \"errorMessage\" : \"", "Unable to jsonify", "\" } }", sep=""))
			
		} else {
		
			return(json)
		}
	}

}

checkPackages <- function() {
	rjson::toJSON(.checkPackages())
}

.sanitizeForJson <- function(obj) {
	# Removes elements that are not translatable to json
	#
	# Args:
	# - obj: character string or obj coercible to string (e.g. a try-error)
	#
	# Return:
	# - character string ready to be put into rjson::toJSON
	#
	str <- as.character(obj)
	str <- gsub("\"", "'", str, fixed=TRUE)
	str <- gsub("\\n", "<br>", str)
	str <- gsub("\\\\", "", str)
	return(str)
}

.createEmptyResults <- function(resultsMeta) {
	# Dispatches functions which build a fully completed results & .meta list that mimics the output 
	# of the init phase of an analysis (empty tables, empty plots)
	#
	# Args:
	# - resultsMeta: full (named) list taken from the analysis json file, 
	#                contains all output descriptions of tables/plots/containers 
	#
	# Return:
	# - results list with row lists for tables and png's for plots and a .meta entry
	#
	if (length(resultsMeta) == 0)
		stop("No items to show, please add a results description or set init to true")
	
	showItems <- logical(length(resultsMeta))
	for (i in 1:length(resultsMeta)) {
		item <- resultsMeta[[i]]
		if (! is.null(item[["show"]]) && item[["show"]] && item[["type"]] != "container")
			showItems[i] <- TRUE
	}
	if (sum(showItems) == 0)
		stop("None of the result items has show set to 'true', cannot skip init phase")
		
	results <- .resultsFromResultsMeta(resultsMeta[showItems])
	results[[".meta"]] <- .getAnalysisMeta(results, resultsMeta)
	
	return(results)
}

.resultsFromResultsMeta <- function(resultsMeta) {
	# Builds a fully completed results list which mimics the output of the init phase 
	# of an analysis (empty tables, empty plots)
	#
	# Args:
	# - resultsMeta: condensed (named) list of results elements that had 
	#                "show": true in the analysis json file
	#
	# Return:
	# - results list with row lists for tables and png's for plots
	#
	results <- list()
	for (i in 1:length(resultsMeta)) {
		item <- resultsMeta[[i]]
		name <- names(resultsMeta)[i]
		type <- item[["type"]]
		if (type == "table") {
			table <- .makeEmptyTable(item[["columns"]])
			if (is.null(table)) # show=TRUE in the table but in none of the columns
				next
			results[[name]] <- .convertTable(table, resultsMeta, name)
		} else if (type == "image") {
			results[[name]] <- .convertPlot(plot.new, resultsMeta, oldState=NULL, name, createObj=FALSE)
		} else {
			stop(paste("Cannot create empty placeholder for element of type", type))
		}
	}
	return(results)
}

.makeEmptyTable <- function(colsMeta) {
	# Creates a list table without data based on which columns have the flag "show": true;
	# used in analyses that skip the init phase
	#
	# Args:
	# - colsMeta: full (unnamed) list of table columns taken from the analysis json file
	#
	# Return:
	# - a list with all columns to be shown with a single dot-filled row
	#
	cols <- NULL
	for (i in 1:length(colsMeta)) {
		col <- colsMeta[[i]]
		if (! is.null(col[["show"]]) && col[["show"]])
			cols <- c(cols, col[["name"]])
	}
	if (length(cols) == 0)
		return(NULL)
	
	table <- matrix(".", nrow=1, ncol=length(cols))
	colnames(table) <- cols
	return(table)
}

.parseResults <- function(results, title, resultsMeta, oldState) {
	# Dispatches functions that (1) convert data.frames/matrices
	# to row lists, (2) convert ggplot2/recordedPlot objects to png's, (3) add .meta,
	# and (4) add a keep object containing all png's
	#
	# Args:
	# - results: list with (non-parsed) output elements from an analysis
	# - title: the title of the analysis, taken from the analysis json file
	# - resultsMeta: full (named) list taken from the analysis json file, 
	#                contains all output descriptions of tables/plots/containers 
	# - oldState: state file that was retrieved before the current analysis ran
	#
	# Return:
	# - results list with row lists for tables and png's for plots, a .meta entry and keep;
	#   can be returned from run as is
	#
	if (is.null(resultsMeta)) # old analysis type
		return(results)
	
	if (length(resultsMeta) == 0)
		stop("Cannot locate any items in the results description")
	
	if (! is.null(title))
		results[["results"]][["title"]] <- title

	if (! ".meta" %in% names(results$results))
		results[["results"]][[".meta"]] <- .getAnalysisMeta(results$results, resultsMeta)

	resultsMeta <- .simplifyResultsMeta(resultsMeta) # this basically flattens the nested table/images in containers

	results$results <- .convertResults(results$results, resultsMeta, oldState) # make the results Qt ready
	
	if (is.null(results$keep))
		results$keep <- .getKeepImages(results$results)
	
	return(results)
}

.getAnalysisMeta <- function(results, resultsMeta) {
	# Builds the .meta element of the analysis and dispatches a function to discover 
	# nested structures. .meta describes all elements in results such as their name, title and type
	#
	# Args:
	# - results: list with (parsed or non-parsed) output elements from an analysis
	# - resultsMeta: full (named) list taken from the analysis json file, 
	#                contains all output descriptions of tables/plots/containers 
	#
	# Return:
	# - the description of the results ready to be returned by run as is
	#
	meta <- list()
	for (itemName in names(resultsMeta)) {
		if (! itemName %in% names(results))
			next
		item <- resultsMeta[[itemName]]
		type <- item[["type"]]
		if (type == "image" || type == "table") {
			meta[[length(meta)+1]] <- list(type=type, name=itemName)
		} else if (type == "container") {
			meta[[length(meta)+1]] <- .analysisMetaFromResults(result=results[itemName], objectName=itemName)
		} else {
			stop(paste0("Unknown type (", type, ")  provided in results description"))
		}
	}
	return(meta)
}

.analysisMetaFromResults <- function(result, inCollection=FALSE, parent="", objectName="", depth=0) {
	# Recursive function that loops through the results and describes the contents for the .meta description;
	# it is specifically designed to parse results that are of type 'container'.
	# Note: collections can contain objects and objects can contain collections
	#
	# Args:
	# - result: list with (parsed or non-parsed) output elements from an analysis [recursive]
	# - inCollection: boolean specifying whether the current branch in the results is of type collection.
	#                 collections are a repetition of structures, this means that we can describe
	#                 the content of the collection by only seeing one of its structures
	# - parent: character name of the direct parent of the current result, can be
	#           'object' or 'collection'
	# - objectName: name of the container in the analysis json, only there for debug purposes
	# - depth: current depth of the recursion, only there for debug purposes
	#
	# Return:
	# - list with the .meta description of a single results element
	#
	meta <- list()
	parsedNames <- NULL
	for (i in 1:length(result)) {	
		name <- ""
		if (! is.null(names(result)))
			name <- names(result)[i]
		
		type <- .getResultType(result[i])
		if (is.null(type)) # probably a title
			next
		else if (type == "collection") {
			inCollection <- TRUE
			collectionType <- .getTerminalType(result[[i]][["collection"]][[1]])  # expecting list(collection=list(...)), let's skip that
			if (! is.null(collectionType) && (collectionType == "image" || collectionType == "table")) { # we found the end; the collection is not nested
				meta[[length(meta)+1]] <- list(name=name, type=type, meta=collectionType)
				next
			}
		} 
		else if (type == "image" || type == "table") {
			if (name %in% parsedNames)
				next
			meta[[length(meta)+1]] <- list(name=name, type=type)
			parsedNames <- c(parsedNames, name)
			next
		}
		
		nextLevel <- result[[i]]
		if (type == "collection")
			nextLevel <- result[[i]][["collection"]]
		
		if (parent == "object") # we want list(list()) instead of just list() as each branch may be a different type
			meta[[length(meta)+1]] <- list(name=name, type=type, meta=.analysisMetaFromResults(nextLevel, inCollection, parent=type, objectName=objectName, depth=depth+1))
		else
			meta <- list(name=name, type=type, meta=.analysisMetaFromResults(nextLevel, inCollection, parent=type, objectName=objectName, depth=depth+1))
		
		if (inCollection) # we only needed to follow a single branch and have done so
			break
	}
	
	if (length(meta) == 0) { # this is an error, we could not figure out the type
		classes <- paste(class(result), collapse=", ")
		childClasses <- paste(class(unlist(result, recursive=FALSE)), collapse=", ")
		resultNames <- paste(names(result), collapse=", ")
		childNames <- paste(names(unlist(result, recursive=FALSE)), collapse=", ")
		if (resultNames == "") resultNames <- "<unnamed>"
		if (childNames == "") childNames <- "<unnamed>"		
		stop(paste0("Could not create meta for ", objectName, " at depth ", depth, " (class: ", classes, ", names: ", resultNames, ", class children: ", childClasses, ", names children: ", childNames, ")"))
	}

	return(meta)
}

.getResultType <- function(result) {
	# Describes the type of the result to the recursive .analysisMetaFromResults()
	# (in which it's called repeatedly); these types are later stored in .meta
	#
	# Args:
	# - result: list with (parsed or non-parsed) output elements from an analysis [arbitrary depth]
	#
	# Return:
	# - NULL if unknown/invalid type, otherwise a string with 'collection', 'object',
	#   'image' or 'table'
	#
	types <- NULL
	acceptedClasses <- c("recordedplot", "ggplot", "function", "list", "matrix", "data.frame")
	for (item in result) { # look through the contents of the current level in results
		if (! any(acceptedClasses %in% class(item))) # unknown type (a title?)
			next
		type <- NULL
		if ("collection" %in% names(item))
			type <- "collection"
		else
			type <- .getTerminalType(item)
		
		if (! is.null(type))
			types <- c(types, type)
		else
			types <- c(types, "object")
	}

	type <- unique(types)
	if (length(type) != 1) # multiple different types, not sure which to pick
		return(NULL)
	
	return(type)
}

.getTerminalType <- function(result) {
	# Determines if the result is of type 'table' or 'image' (at which point .analysisMetaFromResults()
	# knows that it has reached the terminal node)
	#
	# Args:
	# - result: list with (parsed or non-parsed) output elements from an analysis [arbitrary depth]
	#
	# Return:
	# - NULL if not a plot/table, otherwise a string with 'image' or 'table'
	#
	
	# old analysis
	if (is.list(result) && "data" %in% names(result)) {
		if (is.list(result[["data"]]) && "schema" %in% names(result))
			return("table")
		if (is.character(result[["data"]]) && grepl("\\.png$", result[["data"]]))
			return("image")
	}
	# new analysis
	if (is.data.frame(result) || is.matrix(result))
		return("table")
	if (any(c("recordedplot", "ggplot", "function") %in% class(result)))
		return("image")

	return(NULL)
}

.simplifyResultsMeta <- function(resultsMeta) {
	# Changes the resultsMeta so that each table or plot is placed on the top level,
	# specifically meant for container types
	#
	# Args:
	# - resultsMeta: full (named) list taken from the analysis json file, 
	#                contains all output descriptions of tables/plots/containers 
	#
	# Return:
	# - named list identical to input except for the fact that containers are removed
	#   and their contents are placed on the top level
	#
	items <- list()
	for (itemName in names(resultsMeta)) {
		item <- resultsMeta[[itemName]]
		type <- item[["type"]]
		if (type == "container") {
			container <- item[["items"]]
			if (is.list(container) && is.null(names(container))) # it is an array of items
				container <- unlist(container, recursive=FALSE)
			if (is.null(names(container)))
				stop(paste("Could not figure out the container structure in", itemName))
			for (containerItemName in names(container))
				items[[containerItemName]] <- container[[containerItemName]]
		} else {
			items[[itemName]] <- item
		}
	}
	return(items)
}

.getKeepImages <- function(results) {
	# Finds all the png's that were created with writeImage()
	#
	# Args:
	# - results: list with (parsed) output elements from an analysis
	#
	# Return:
	# - vector of names of each png detected in the results
	#
	keep <- NULL
	items <- unlist(results, use.name=FALSE)
	if (is.list(items)) { # guess it had a formula or something
		keep <- lapply(items, function(x) {
			if (is.character(x) && grepl("\\.png$", x))
				return(x)
		})
		keep <- unlist(keep)
	} else { # unlist worked fine
		items <- items[is.character(items)]
		indices <- which(grepl("\\.png$", items))
		if (length(indices) > 0)
			keep <- items[indices]
	}
	return(keep)
}

.convertResults <- function(results, resultsMeta, oldState, name=NULL) {
	# Recursive function that iterates over results and calls functions to convert
	# data.frames/matrices to row lists and ggplot2/recordedPlot objects to png's
	#
	# Args:
	# - results: list with (non-parsed) output elements from an analysis [recursive]
	# - resultsMeta: expanded (named) list taken from the analysis json file and 
	#                parsed by .simplifyResultsMeta(), contains all output descriptions of tables/plots
	# - oldState: state file that was retrieved before the current analysis ran
	# - name: last name of a results element that was found in resultsMeta, intended for
	#         container types that have dynamic names and do not have a jasp.name attribute.
	#         E.g., if descriptives is a table in resultsMeta, then results->descriptives->contBinom->table
	#         resolves to name="descriptives" [recursive]
	#
	# Return:
	# - results list with row lists for tables and png's for plots
	#
	for (i in 1:length(results)) {
		result <- results[[i]]
		
		type <- .getTerminalType(result)
		if (is.null(type) && ! "list" %in% class(result)) # e.g., a title
			next
		
		proposal <- names(results)[i]
		if (is.character(proposal) && proposal %in% names(resultsMeta))
			name <- proposal
		
		if (! is.null(type) && (type == "image" || type == "table")) {
			if (type == "table") {
				results[[i]] <- .convertTable(result, resultsMeta, name)
			} else { # image
				results[[i]] <- .convertPlot(result, resultsMeta, oldState, name)
			}
		} else { # go deeper
			results[[i]] <- .convertResults(result, resultsMeta, oldState, name)
		}
		
	}
	return(results)
}

.convertPlot <- function(plotObj, resultsMeta, oldState, name, createObj=TRUE) {
	# Converts ggplot2/recordedPlot objects to png's, given they have not been converted previously
	#
	# Args:
	# - plotObj: plot.new function if initing and ggplot2 or recordedPlot object if running
	# - resultsMeta: expanded (named) list taken from the analysis json file and 
	#                parsed by .simplifyResultsMeta(), contains all output descriptions of tables/plots
	# - oldState: state file that was retrieved before the current analysis ran
	# - name: NULL or a character string with the name of the plot object in the analysis json
	# - createObj: boolean specifying if a plot object should be added to the output
	#
	# Return:
	# - list with the converted plot along with the title, width, height, etc.
	#
	if (! any(class(plotObj) %in% c("recordedplot", "ggplot", "function")))
		return(plotObj)
	
	attr <- attributes(plotObj)
	plotMeta <- .getItemMeta(resultsMeta, attr, name, type="image")
	if (is.null(plotMeta))
		stop("Could not find meta description for current plot")
		
	plot <- list()
	plot[["title"]] <- .getItemTitle(plotObj, attr, plotMeta)

	if (! is.null(attr) && "jasp.error" %in% names(attr)) { # error specified in analysis
		plot[["data"]] <- ""
		plot[["error"]] <- list(error="badData", errorMessage=attr[["jasp.error"]])
		return(plot)
	}

	p <- plotObj
	attributes(p) <- NULL
	if (identical(p, plot.new))
		plot[["status"]] <- "running"
	else
		plot[["status"]] <- "complete"

	existingPlot <- .getExistingPlotName(plotObj, oldState)
	if (! is.null(existingPlot)) {
		plot[["obj"]] <- plotObj
		plot[["data"]] <- existingPlot
	} else { # this plot was not previously made
		content <- tryCatch(
			.writeImage(width=plotMeta[["width"]], height=plotMeta[["height"]], plot=plotObj, obj=createObj),
			error = function(e) e
		)
		if (inherits(content, "error")) {
			plot[["data"]] <- ""
			plot[["error"]] <- list(error="badData", errorMessage=content[["message"]])
			return(plot)
		}
		plot[["obj"]] <- content[["obj"]]
		plot[["data"]] <- content[["png"]]
	}

	plot[["convertible"]] <- TRUE
	plot[["width"]]  <- plotMeta[["width"]]
	plot[["height"]] <- plotMeta[["height"]]

	if (! is.null(attr) && "jasp.footnotes" %in% names(attr))
		plot[["footnotes"]] <- attr[["jasp.footnotes"]]

	return(plot)
}

.convertTable <- function(table, resultsMeta, name) {
	# Converts data.frame/matrix to a row list with the required schema
	#
	# Args:
	# - table: data.frame, matrix or a row list
	# - resultsMeta: expanded (named) list taken from the analysis json file and 
	#                parsed by .simplifyResultsMeta(), contains all output descriptions of tables/plots
	# - name: NULL or a character string with the name of the table in the analysis json
	#
	# Return:
	# - list with the converted table along with the title, status, schema taken from the resultsMeta, etc.
	#
	attr <- attributes(table)
	tableMeta <- .getItemMeta(resultsMeta, attr, name, type="table")
	if (is.null(tableMeta))
		stop("Could not find meta description for current table")
	
	footnotes <- customSchema <- error <- NULL
	if (! is.null(attr) && (is.matrix(table) || is.data.frame(table))) {
		if ("jasp.footnotes" %in% names(attr))
			footnotes <- attr[["jasp.footnotes"]]
		if ("jasp.schema" %in% names(attr))
			customSchema <- attr[["jasp.schema"]]
		if ("jasp.error" %in% names(attr))
			error <- attr[["jasp.error"]]
	}
	
	if (is.matrix(table))
		table <- as.data.frame(table)
		
	if (is.jasp.data.frame(table)) # at this point we want to have the default drop behaviour again
		attr(table, "class") <- "data.frame"

	combine <- .getCellsToCombine(tableMeta[["columns"]])
	if (is.data.frame(table) && ! is.null(combine)) # if it's a row list row combining should be handled in the analysis
		table <- .combineCells(table, combine)

	if (is.data.frame(table))
		table <- list(data=.toListTable(table, tableMeta[["columns"]]))
	
	table[["title"]] <- .getItemTitle(table, attr, tableMeta)
	table[["status"]] <- "complete"
	
	if (is.null(table[["schema"]]))
		table[["schema"]] <- .createTableSchema(table, tableMeta[["columns"]], customSchema)
	
	if (! is.null(footnotes))
		table <- .addTableFootnotes(table, footnotes)
	
	if (! is.null(error))
		table[["error"]] <- list(errorType="badData", errorMessage=error)
	
	return(table)
}

.getItemMeta <- function(resultsMeta, attr, name, type) {
	# Retrieves the appropriate meta description of an output item; it will attempt to
	# find it by name (from .convertResults()) or in the attributes (from the object itself)
	#
	# Args:
	# - resultsMeta: expanded (named) list taken from the analysis json file and 
	#                parsed by .simplifyResultsMeta(), contains all output descriptions of tables/plots
	# - attr: named list with attributes of the output item
	# - name: NULL or a character string with the name of the item
	# - type: 'table' or 'image'
	#
	# Return:
	# - NULL if not found and otherwise a selected (named) list for a specific output item, 
	#   taken from the analysis json file
	#
	itemMeta <- NULL
	if ("jasp.name" %in% names(attr)) { # found in attributes
		itemMeta <- resultsMeta[[attr[["jasp.name"]]]]
	} else if (! is.null(name)) { # found in results list names
		itemMeta  <- resultsMeta[[name]]
	}
	if (! is.null(itemMeta) && itemMeta[["type"]] == type)
		return(itemMeta)
	return(NULL)
}

.getItemTitle <- function(obj, attr, itemMeta) {
	# Retrieves the title of an output item; it will attempt to find it in the obj, 
	# meta or results. Multiple sources must be checked as the title may be dynamic
	#
	# Args:
	# - obj: the output item itself (plot obj, data.frame, row list, etc.)
	# - attr: named list with attributes of the output item
	# - itemMeta: selected (named) list for a specific output item, taken from the analysis json file
	#
	# Return:
	# - character string with the title or "" if none was found
	#
	title <- ""
	if (! is.null(itemMeta[["title"]]))
		title <- itemMeta[["title"]]
	else if ("jasp.title" %in% names(attr))
		title <- attr[["jasp.title"]]
	else if (is.list(obj) && ! is.null(obj[["title"]]))
		title <- obj[["title"]]
	return(title)
}

.getExistingPlotName <- function(plotObj, oldState) {
	# Checks if a plot object has previously been converted to png and if so, returns it
	#
	# Args:
	# - plotObj: plot.new function, or ggplot2 or recordedPlot object
	# - oldState: state file that was retrieved before the current analysis ran
	#
	# Return:
	# - the png name if the object was found in the state and NULL otherwise
	#
	if (is.null(oldState) || ! "figures" %in% names(oldState))
		return(NULL)

	plotName <- NULL
	attributes(plotObj) <- NULL
	samePlot <- vapply(oldState[["figures"]], 
		function(obj) {
			attributes(obj) <- NULL
			identical(plotObj, obj) # note: all.equal is very slow for deeply nested lists
		}, logical(1))
	if (any(samePlot))
		plotName <- names(oldState[["figures"]])[samePlot]
	return(plotName)
}

.getCellsToCombine <- function(colsMeta) {
	# Checks if any columns have the flag "combine": true and if so, returns them
	#
	# Args:
	# - colsMeta: full (unnamed) list of table columns taken from the analysis json file
	#
	# Return:
	# - NULL if no columns have the combine flag, otherwise the names of the columns that do
	#
	toCombine <- lapply(colsMeta,
		function(col) {
			if (! is.null(col[["combine"]]))
				col[["name"]]
		})
	toCombine <- unlist(toCombine)
	if (length(toCombine) > 0)
		return(toCombine)
	return(NULL)
}

.combineCells <- function(table, which) {
	# Adds .isNewGroup flag to rows that have cells that must be combined
	#
	# Args:
	# - table: data.frame
	# - which: character vector of columns with cells to combine
	#
	# Return:
	# - data.frame with one additional column named ".isNewGroup" containing logicals
	#
	if (nrow(table) == 0 || ! any(which %in% colnames(table)))
		return(table)
	if (nrow(table) == 1)
		return(cbind(table, .isNewGroup=TRUE))
	
	grouping <- table[which]
	isNewGroup <- logical(nrow(grouping))
	isNewGroup[1] <- TRUE
	lastRow <- grouping[1, ]
	lastDiff <- ncol(grouping)
	ctr <- 1
	for (i in 2:nrow(grouping)) {
		row <- grouping[i, ]
		if (all(row != lastRow)) {
			isNewGroup[i] <- TRUE
		} else if (any(row != lastRow)) {
			nDiff <- sum(row != lastRow)
			if (ctr > 1 && nDiff > lastDiff) {
				isNewGroup[i] <- TRUE
			}
			lastDiff <- nDiff
		}
		lastRow <- row
		ctr <- ctr + 1
		if (isNewGroup[i]) {
			ctr <- 1
			lastDiff <- ncol(grouping)
		}
	}
	table <- cbind(table, .isNewGroup=isNewGroup)
	return(table)
}

.createTableSchema <- function(table, colsMeta, customMeta) {
	# Dispatches functions that create the schema which describes the table (column 
	# meta for columns not in the table is discarded)
	#
	# Args:
	# - table: row list
	# - colsMeta: full (unnamed) list of table columns taken from the analysis json file
	# - customMeta: unnamed list taken from a jasp.schema attribute containing custom meta;
	#               the meta may contain complete new columns, or changes to existing columns
	#
	# Return:
	# - an unnamed schema list that describes the actual columns in the table 
	#   (with stuff like format="sf:4;dp:3")
	#
	if (! is.null(customMeta)) # a custom fields list was added to the table
		colsMeta <- .addCustomTableMeta(colsMeta, customMeta)

	fields <- .matchMetaToTable(table, colsMeta)
	return(list(fields=fields))
}

.addCustomTableMeta <- function(colsMeta, customMeta) {
	# Adds (dynamic) analysis-specified column meta to the (static)json-specified column meta
	#
	# Args:
	# - colsMeta: full (unnamed) list of table columns taken from the analysis json file
	# - customMeta: unnamed list taken from a jasp.schema attribute containing custom meta;
	#               the meta may contain complete new columns, or changes to existing columns
	#
	# Return:
	# - a valid schema list that contains both static and dynamic properties;
	#   dynamic properties take precedence over duplicate static properties
	#
	for (custom in customMeta) {
		if (! "name" %in% names(custom))
			stop("Could not locate 'name' in custom table meta")
		
		metaAdded <- FALSE
		name <- custom[["name"]]
		for (i in 1:length(colsMeta)) {
			col <- colsMeta[[i]]
			if (col[["name"]] == name) {
				colsMeta[[i]] <- Reduce(modifyList, list(col, custom))
				metaAdded <- TRUE
				break
			}
		}
		
		if (! metaAdded) # it is not an existing column
			colsMeta[[length(colsMeta)+1]] <- custom
	}
	return(colsMeta)
}

.matchMetaToTable <- function(table, colsMeta) {
	# Finds columns that exist in both the meta and the table and returns the corresponding meta
	#
	# Args:
	# - table: row list
	# - colsMeta: full (unnamed) list of table columns taken from the analysis json file and 
	#             possibly combined with custom supplementary meta (see .addCustomTableMeta())
	#
	# Return:
	# - basically a subset of colsMeta with only the elements that are actually in the table
	#
	if (length(table[["data"]]) == 0)
		stop("Could not find any data in the table, cannot fetch items from the results description")

	metaColNames <- vapply(colsMeta, function(col) col[["name"]], "character")
	row <- table[["data"]][[1]] # any row works and we know at least one exists
	indices <- which(metaColNames %in% names(row))
	if (length(indices) == 0)
		stop("Could not locate any of the columns specified in the results description")

	schema <- colsMeta[indices]
	return(schema)
}

.toListTable <- function(df, colsMeta) {
	# Converts a data.frame to a list of rows and then dispatches functions to perform
	# type coercion and .clean()'ing
	#
	# Args:
	# - df: data.frame
	# - colsMeta: full (unnamed) list of table columns taken from the analysis json file
	#
	# Return:
	# - list with sub-lists for each rows; each row contains named cells that are
	#   string, numeric or integer
	#
	if (! is.data.frame(df) && is.list(df))
		return(df)

	if (! is.data.frame(df))
		stop("expecting output to be a data.frame")

	df <- .coerceColTypes(df, colsMeta)
	rowList <- do.call(mapply, c(FUN=list, df, SIMPLIFY=FALSE, USE.NAMES=FALSE))
	rowList <- lapply(rowList, .clean)

	return(rowList)
}

.coerceColTypes <- function(df, colsMeta) {
	# Coerces columns of a data.frame to the types that are specified in the 
	# corresponding column descriptions (numeric/integer)
	#
	# Args:
	# - df: data.frame
	# - colsMeta: full (unnamed) list of table columns taken from the analysis json file
	#
	# Return:
	# - data.frame with correct modes for each column
	#
	for (col in colsMeta) {
		name <- col[["name"]]
		if (! name %in% colnames(df))
			next
		if (any(df[[name]] == "."))
			next
		type <- col[["type"]]
		if (type == "integer" && ! is.integer(df[[name]])) {
			df[[name]] <- suppressWarnings(as.integer(as.character(df[[name]])))
		} else if (type == "number" && ! is.numeric(df[[name]])) {
			df[[name]] <- suppressWarnings(as.numeric(as.character(df[[name]])))
		}
	}
	return(df)
}

.addTableFootnotes <- function(table, footnotes) {
	# Adds footnotes to a table
	#
	# Args:
	# - table: row list
	# - footnotes: object created by .newFootnotes()
	#
	# Return:
	# - row list with footnotes added at the top level and inside the rows
	#
	footnotes <- as.list(footnotes)
	for (i in 1:length(footnotes)) {
		footnote <- footnotes[[i]]
		row <- footnote[["row"]]
		cols <- footnote[["cols"]]
		if (is.null(row) && is.null(cols))
			next
		if (is.character(cols)) {
			for (col in cols) { # add note to all cols
				newFootnote <- list()
				newFootnote[[col]] <- list(i-1) # the numbering of footnote indices starts at 0
				if (! is.numeric(row)) { # add note to all rows for the col
					for (j in 1:length(table[["data"]])) {
						footnotesRow <- table[["data"]][[j]][[".footnotes"]]
						table[["data"]][[j]][[".footnotes"]] <- c(footnotesRow, newFootnote)
					}
				} else { # add note to a single row
					footnotesRow <- table[["data"]][[row]][[".footnotes"]]
					table[["data"]][[row]][[".footnotes"]] <- c(footnotesRow, newFootnote)
				}
			}
		}
	}
	table[["footnotes"]] <- footnotes
	return(table)
}

jasp.data.frame <- function(colnames=NULL, ..., stringsAsFactors=FALSE) {
	# Creates a data.frame with class 'jasp.data.frame'; the function works just like data.frame(),
	# except when creating empty data.frames with column names, e.g.:
	# x <- jasp.data.frame(colnames=c("col1", "col2", "col3")) # or:
	# x <- setNames(data.frame(matrix(ncol=3, nrow=0)), c("col1", "col2", "col3"))
	#
	# Args:
	# - colnames: character vector of column names
	# - ...: arguments passed on to data.frame()
	# - stringsAsFactors: setting for stringsAsFactors passed on to data.frame()
	#
	# Return:
	# - jasp.data.frame which has more robust methods for rbind'ing, cbind'ing, etc.
	#
	x <- data.frame(..., stringsAsFactors=stringsAsFactors)
	if (is.character(colnames) && nrow(x) == 0) {
		x <- data.frame(matrix(ncol=length(colnames), nrow=0))
		colnames(x) <- colnames
	}
	class(x) <- c("jasp.data.frame", "data.frame")
	return(x)
}

is.jasp.data.frame <- function(df) {
	inherits(df, "jasp.data.frame")
}

.extractAttr <- function(pattern, ...) {
	# Extracts all attributes matching 'pattern' from the supplied arguments
	#
	# Args:
	# - pattern: character string containing a regular expression
	# - ...: anything really as long as it is not nested (e.g., data.frames, matrices, vectors, lists)
	#
	# Return:
	# - list with all the named attributes matching the pattern
	#
	args <- list(...)
	if (length(args) == 0)
		return(NULL)

	attrList <- lapply(args, attributes)
	attr <- unlist(attrList, recursive=FALSE) # we do not want to unlist the attributes themselves
	matches <- attr[grepl(pattern, names(attr))]
	if (length(matches) == 0)
		return(NULL)

	return(matches)
}

.restoreAttrJaspDataFrame <- function(df, jaspAttr) {
	# Ensures that the data.frame still has the correct class and attributes
	#
	# Args:
	# - df: data.frame
	# - jaspAttr: list of attributes starting with "jasp."
	#
	# Return:
	# - data.frame with class "jasp.data.frame" and its attributes restored
	#
	class(df) <- c("jasp.data.frame", "data.frame")
	attributes(df) <- c(attributes(df), jaspAttr)
	return(df)
}

`[.jasp.data.frame` <- function(x, i, j, drop=FALSE) {
	# Extracts parts of a data.frame as usual, but with the added benefit that 
	# (1) attributes starting with "jasp." are never dropped and 
	# (2) a data.frame is never reduced ('dropped') to a vector
	#
	# Args:
	# - see ?`[.data.frame`
	#
	# Return:
	# - see ?`[.data.frame`
	#
	jaspAttr <- .extractAttr("^jasp\\.", x)
	x <- `[.data.frame`(x, i, j, drop)
	x <- .restoreAttrJaspDataFrame(x, jaspAttr)
	return(x)
}

rbind.jasp.data.frame <- function(..., stringsAsFactors=FALSE) {
	# Row binds to a data.frame as usual, but with the added benefit that 
	# (1) names remain intact when row binding to an empty data.frame, 
	# (2) attributes starting with "jasp." are never dropped,
	# (3) a 'mixed' vector with characters and numeric can be supplied and will be coerced appropriately,
	# (4) a data.frame never creates factors and so rbinding new char values to a col works
	#
	# Args:
	# - see ?rbind.data.frame
	#
	# Return:
	# - see ?rbind.data.frame
	#
	jaspAttr <- .extractAttr("^jasp\\.", ...)
	args <- list(...)
	jaspTable <- NULL
	for (i in 1:length(args)) {
		arg <- args[[i]]
		classes <- class(arg)
		if ("jasp.data.frame" %in% classes) { # identify the jasp data frame element so we know what names to store
			jaspTable <- arg
		} else if (is.vector(arg) && is.character(arg)) { # find possible vectors that may have mixed types
			args[[i]] <- lapply(arg, function(val) { # characters vector will be a mixed list
				valAsNum <- suppressWarnings(as.numeric(val))
				ifelse(is.na(valAsNum), val, valAsNum)
			})
		}
	}
	names <- colnames(jaspTable) # we need to save the col names of the jasp data frame

	x <- do.call(rbind.data.frame, c(args, list(stringsAsFactors=stringsAsFactors)))
	x <- .restoreAttrJaspDataFrame(x, jaspAttr)

	if (length(names) > 0)
		colnames(x) <- names

	return(x)
}

cbind.jasp.data.frame <- function(..., stringsAsFactors=FALSE) {
	# Column binds to a data.frame as usual, but with the added benefit that 
	# (1) attributes starting with "jasp." are never dropped,
	# (2) a data.frame never will never have any factors by default
	#
	# Args:
	# - see ?cbind.data.frame
	#
	# Return:
	# - see ?cbind.data.frame
	#
	jaspAttr <- .extractAttr("^jasp\\.", ...)
	x <- cbind.data.frame(..., stringsAsFactors=stringsAsFactors)
	x <- .restoreAttrJaspDataFrame(x, jaspAttr)
	return(x)
}

subset.jasp.data.frame <- function(x, ...) {
	# Subsets a data.frame as usual, but with the added benefit that 
	# attributes starting with "jasp." are never dropped
	#
	# Args:
	# - see ?subset.data.frame
	#
	# Return:
	# - see ?subset.data.frame
	#
	jaspAttr <- .extractAttr("^jasp\\.", x)
	x <- subset.data.frame(x, ...)
	x <- .restoreAttrJaspDataFrame(x, jaspAttr)
	return(x)
}

isTryError <- function(obj){
    if (is.list(obj)){
        return(any(sapply(obj, function(obj) {
            inherits(obj, "try-error")
        }))
        )
    } else {
        return(any(sapply(list(obj), function(obj){
            inherits(obj, "try-error")
        })))
    }
}

.addCitationToTable <- function(table) {

	if ("citation" %in% names(table) ) {

		cite <- c(.fromRCPP(".baseCitation"), table$citation)
		
		for (i in seq_along(cite))
			base::Encoding(cite[[i]]) <- "UTF-8"
		
		table$citation <- cite

	} else {
	
		cite <- .fromRCPP(".baseCitation")
		base::Encoding(cite) <- "UTF-8"
	
		table$citation <- list(cite)
	}
	
	table
}

.addCitationToResults <- function(results) {

	if ("status" %in% names(results)) {
	
		res <- results$results
		
	} else {
	
		res <- results
	}
	
	for (m in res$.meta) {

		item.name <- m$name
		
		if (item.name %in% names(res)) {
	
			if (m$type == "table") {
	
				res[[item.name]] <- .addCitationToTable(res[[item.name]])
				
			} else if (m$type == "tables") {

				for (i in .indices(res[[item.name]]))
					res[[item.name]][[i]] <- .addCitationToTable(res[[item.name]][[i]])
			}
		}
	}
	
	
	if ("status" %in% names(results)) {
	
		results$results <- res
		
	} else {
	
		results <- res
	}
	
	results
}

.getDataSetCols <- function(dataKey, options) {
	# Parses the data key so JASP knows how to read the different columns in the dataset
	#
	# Args:
	# - dataKey: named list (from the analysis json) with the possible entries 'factor', 
	#            'numeric', 'ordinal', 'auto' and 'excludeNA'; the values for these entries 
	#            should be names of options
	# - options: named list (from the analysis json) with options to run the analysis with
	#
	# Return:
	# - named list with structure colType=>optionVals (e.g., list(columns.as.factor="contBinom"))
	#
  if (length(dataKey) == 0)
    stop("no columns found to be imported")
  
  colsToLoad <- vector("list", length(dataKey))
  names(colsToLoad) <- names(dataKey)
  for (colType in names(dataKey)) {
    opts <- dataKey[[colType]]
    if (! is.null(opts))
      colsToLoad[[colType]] <- .optsToColNames(opts, options)
    else 
      colsToLoad[[colType]] <- NULL
  }
  
  auto <- excludeNA <- NULL
  if ("auto" %in% names(colsToLoad)) {
    auto <- colsToLoad[["auto"]]
    colsToLoad[[auto]] <- NULL
  }
  
  if ("excludeNA" %in% names(colsToLoad)) {
    excludeNA <- colsToLoad[["excludeNA"]]
    colsToLoad[["excludeNA"]] <- NULL
  }
  
  names(colsToLoad) <- sapply(names(colsToLoad), function(name) paste0("columns.as.", name))
  
  if (! is.null(auto))
    colsToLoad[["columns"]] <- auto
  
  if (! is.null(excludeNA))
    colsToLoad[["exclude.na.listwise"]] <- excludeNA

	return(colsToLoad)
}

.optsToColNames <- function(opts, options) {
	# Looks up the options associated with a given colType in the general options list 
	# and returns their values
	#
	# Args:
	# - opts: options that correspond with a certain colType (e.g., in 
	#         "factor": "fixedFactor" this is "fixedFactor")
	# - options: named list (from the analysis json) with options to run the analysis with
	#
	# Return:
	# - NULL if the options have no values, otherwise the values as a character vector
	#
  if (is.null(names(options)))
    stop("expecting the JASP options to be a named list")

  if (any(! opts %in% names(options))) {
		indices <- which(! opts %in% names(options))
		stop(paste0("Trying to find dataset columns to read, but it appears some options don't exist: '", paste(opts[indices], collapse=", "), "'"))
	}  

  result <- NULL
  for (opt in opts) {
    value <- options[[opt]]
    if (is.character(value) && value != "")
      result <- c(result, value)
  }
  return(result)
}

.readDataSetToEnd <- function(columns=c(), columns.as.numeric=c(), columns.as.ordinal=c(), columns.as.factor=c(), all.columns=FALSE, exclude.na.listwise=c(), ...) {

	if (is.null(columns) && is.null(columns.as.numeric) && is.null(columns.as.ordinal) && is.null(columns.as.factor) && all.columns == FALSE)
		return (data.frame())

	dataset <- .fromRCPP(".readDatasetToEndNative", unlist(columns), unlist(columns.as.numeric), unlist(columns.as.ordinal), unlist(columns.as.factor), all.columns != FALSE)
	dataset <- .excludeNaListwise(dataset, exclude.na.listwise)
	
	dataset
}

.readDataSetHeader <- function(columns=c(), columns.as.numeric=c(), columns.as.ordinal=c(), columns.as.factor=c(), all.columns=FALSE, ...) {

	if (is.null(columns) && is.null(columns.as.numeric) && is.null(columns.as.ordinal) && is.null(columns.as.factor) && all.columns == FALSE)
		return (data.frame())

	dataset <- .fromRCPP(".readDataSetHeaderNative", unlist(columns), unlist(columns.as.numeric), unlist(columns.as.ordinal), unlist(columns.as.factor), all.columns != FALSE)
	
	dataset
}


.vdf <- function(df, columns=c(), columns.as.numeric=c(), columns.as.ordinal=c(), columns.as.factor=c(), all.columns=FALSE, exclude.na.listwise=c(), ...) {
	new.df <- NULL
	namez <- NULL

	for (column.name in columns) {

		column <- df[[column.name]]

		if (is.null(new.df)) {
			new.df <- data.frame(column)
		} else {
			new.df <- data.frame(new.df, column)
		}

		namez <- c(namez, column.name)
	}

	for (column.name in columns.as.ordinal) {

		column <- as.ordered(df[[column.name]])
		
		if (length(column) == 0) {
			.quitAnalysis("Error: no data! Check for missing values.")
		}
		if (is.null(new.df)) {
			new.df <- data.frame(column)
		} else {
			new.df <- data.frame(new.df, column)
		}

		namez <- c(namez, column.name)
	}

	for (column.name in columns.as.factor) {

		column <- as.factor(df[[column.name]])
		
		if (length(column) == 0) {
			.quitAnalysis("Error: no data! Check for missing values.")
		}
		if (is.null(new.df)) {
			new.df <- data.frame(column)
		} else {
			new.df <- data.frame(new.df, column)
		}

		namez <- c(namez, column.name)
	}

	for (column.name in columns.as.numeric) {

		column <- as.numeric(as.character(df[[column.name]]))
		
		if (length(column) == 0) {
			.quitAnalysis("Error: no data! Check for missing values.")
		}
		if (is.null(new.df)) {
			new.df <- data.frame(column)
		} else {
			new.df <- data.frame(new.df, column)
		}

		namez <- c(namez, column.name)
	}

	if (is.null(new.df))
		return (data.frame())

	names(new.df) <- .v(namez)

	new.df <- .excludeNaListwise(new.df, exclude.na.listwise)

	new.df
}

.excludeNaListwise <- function(dataset, exclude.na.listwise) {

	if ( ! is.null(exclude.na.listwise)) {
	
		rows.to.exclude <- c()
		
		for (col in .v(exclude.na.listwise))
			rows.to.exclude <- c(rows.to.exclude, which(is.na(dataset[[col]])))
		
		rows.to.exclude <- unique(rows.to.exclude)

		rows.to.keep <- 1:dim(dataset)[1]
		rows.to.keep <- rows.to.keep[ ! rows.to.keep %in% rows.to.exclude]
		
		new.dataset <- dataset[rows.to.keep,]
		
		if (class(new.dataset) != "data.frame") {   # HACK! if only one column, R turns it into a factor (because it's stupid)
		
			dataset <- na.omit(dataset)
			
		} else {
		
			dataset <- new.dataset
		}
	}
	
	dataset
}

.fromRCPP <- function(x, ...) {

	if (length(x) != 1 || ! is.character(x)) {
		stop("Invalid type supplied, expected character")
	}

	collection <- c(
		".requestTempFileNameNative",
		".readDatasetToEndNative",
		".readDataSetHeaderNative",
		".callbackNative",
		".requestStateFileNameNative",
		".baseCitation",
		".ppi")

	if (! x %in% collection) {
		stop("Unknown RCPP object")
	}

	if (exists(x)) {
		obj <- eval(parse(text = x))
	} else {
		location <- getAnywhere(x)
		if (length(location[["objs"]]) == 0) {
			stop("Could not locate object")
		}
		obj <- location[["objs"]][[1]]
	}

	if (is.function(obj)) {
		args <- list(...)
		do.call(obj, args)
	} else {
		return(obj)
	}

}

.saveState <- function(state) {
	result <- list()
	relativePath <- NULL
	fullPath <- NULL
	if (base::exists(".requestStateFileNameNative")) {

		location <- .fromRCPP(".requestStateFileNameNative")

		relativePath <- location$relativePath
		root <- location$root
		
		# when run in jasptools do not save the state, but store it internally
		searchPath <- search()
		if ("package:jasptools" %in% searchPath) {
			jasptools:::.setInternal("state", state)
			return(list(relativePath = relativePath, root = root))
		}

		base::Encoding(relativePath) <- "UTF-8"
		base::Encoding(root) <- "UTF-8"

		oldwd <- getwd()
		setwd(root)
		on.exit(setwd(oldwd))
		
		try(suppressWarnings(base::save(state, file=relativePath, compress=FALSE)), silent = FALSE)
	}
  result <- list(relativePath = relativePath, root = root)
  return(result)
}

.retrieveState <- function() {

	state <- NULL
	
	if (base::exists(".requestStateFileNameNative")) {

		location <- .fromRCPP(".requestStateFileNameNative")

		relativePath <- location$relativePath
		root <- location$root

		base::Encoding(relativePath) <- "UTF-8"
		base::Encoding(root) <- "UTF-8"

		oldwd <- getwd()
		setwd(root)
		on.exit(setwd(oldwd))
		
		base::tryCatch(
		  base::load(relativePath),
		  error=function(e) e,
		  warning=function(w) w
		)
	}
	
	state
}

.getStateFromKey <- function(stateKey, options) {
	# Loads the state and then parses it based on a key, so only the reusable items are returned
	#
	# Args:
	# - stateKey: named list (from the analysis json) where each name corresponds to a
	#             name in the state and the value to options in the options list.
	#             the named entry 'baseSets' contains a list with placeholders that must be
	#             replaced in the values of the other lists
	# - options: named list (from the analysis json) with options to run the analysis with
	#
	# Return:
	# - NULL if no statekey was found or no reusable items were found; otherwise
	#   the reusable items in the state as they were returned from the analysis
	#
	state <- .retrieveState()
	if (! is.null(state)) {
		if (! is.null(stateKey)) { # new method, from json
			if ("baseSets" %in% names(stateKey)) # special placeholders will be used in state items
				key <- .addBaseSetsState(stateKey)
			else # no special placeholders in any set of options
				key <- stateKey
		} else if ("key" %in% names(attributes(state))) { # old method, from analysis
			key <- attributes(state)$key
		} else {
			return(NULL)
		}
		state <- .getStateItems(state=state, options=options, key=key)
	}
	return(state)
}

.addBaseSetsState <- function(stateKey) {
	# Replaces placeholders in state keys with the options associated with those 
	# placeholders
	#
	# Args:
	# - stateKey: named list (from the analysis json) where each name corresponds to a
	#             name in the state and the value to options in the options list.
	#             the named entry 'baseSets' contains a list with placeholders that must be
	#             replaced in the values of the other lists
	#
	# Return:
	# - the altered state keys, now only containing options from the options list
	#
	baseSets <- stateKey[["baseSets"]]
	stateKey[["baseSets"]] <- NULL
	key <- lapply(stateKey, function(x) {
		if (any(names(baseSets) %in% x)) {
			sets <- baseSets[which(names(baseSets) %in% x)]
			set <- unlist(sets, use.names=FALSE)
			x <- x[which(! x %in% names(baseSets))]
			return(c(x, set))
		}
		return(x)
	})
	return(key)
}

.shortToLong <- function(dataset, rm.factors, rm.vars, bt.vars) {

	f  <- rm.factors[[length(rm.factors)]]
	df <- data.frame(factor(unlist(f$levels), unlist(f$levels)))
	
	names(df) <- .v(f$name)
	
	row.count <- dim(df)[1]
	

	i <- length(rm.factors) - 1
	while (i > 0) {
	
		f <- rm.factors[[i]]
	
		new.df <- df

		j <- 2
		while (j <= length(f$levels)) {
		
			new.df <- rbind(new.df, df)
			j <- j + 1
		}
		
		df <- new.df
		
		row.count <- dim(df)[1]

		cells <- rep(unlist(f$levels), each=row.count / length(f$levels))
		cells <- factor(cells, unlist(f$levels))
		
		df <- cbind(cells, df)
		names(df)[[1]] <- .v(f$name)
		
		i <- i - 1
	}
	
	ds <- subset(dataset, select=.v(rm.vars))
	ds <- t(as.matrix(ds))
	
	df <- cbind(df, dependent=as.numeric(c(ds)))
	
	for (bt.var in bt.vars) {

		cells <- rep(dataset[[.v(bt.var)]], each=row.count)
		new.col <- list()
		new.col[[.v(bt.var)]] <- cells
		
		df <- cbind(df, new.col)
	}
	
	subjects <- 1:(dim(dataset)[1])
	subjects <- as.factor(rep(subjects, each=row.count))
	
	df <- cbind(df, subject=subjects)

	df
}

.v <- function(variable.names, prefix="X") {

	vs <- c()

	for (v in variable.names)
		vs[length(vs)+1] <- paste(prefix, .toBase64(v), sep="")

	vs
}

.unv <- function(variable.names) {

	vs <- c()
	
	for (v in variable.names) {
	
		if (nchar(v) == 0)
			stop(paste("bad call to .unv() : v is empty"))
	
		firstChar <- charToRaw(substr(v, 1, 1))
	
		if (firstChar >= 0x41 && firstChar <= 0x5A) {  # A to Z
		
			vs[length(vs)+1] <- .fromBase64(substr(v, 2, nchar(v)))
			
		} else {
		
		  vs[length(vs)+1] <- v
		  
		}
	}
	
	vs
}

.vf <- function(formula) {
	
	in.pieces <- .decompose(formula)
	ved <- .jrapply(in.pieces, .v)
	.compose(ved)
}

.unvf <- function(formula) {
	
	in.pieces <- .decompose(formula)
	unved <- .jrapply(in.pieces, .unv)
	
	interaction.symbol <- "\u2009\u273B\u2009"
	base::Encoding(interaction.symbol) <- "UTF-8"
	
	.compose(unved, interaction.symbol)
}

.decompose <- function(formulas) {

	lapply(as.list(formulas), function(formula) {

		sides <- strsplit(formula, "~", fixed=TRUE)[[1]]
		
		lapply(sides, function(formula) {
			
			terms <- strsplit(formula, "+", fixed=TRUE)
			
			lapply(terms, function(term) {
			components <- strsplit(term, ":")
			components <- sapply(components, stringr::str_trim, simplify=FALSE)
			
			})[[1]]
		})
	})
}

.compose <- function(formulas, i.symbol=":") {

	sapply(formulas, function(formula) {
		
		formula <- sapply(formula, function(side) {
			
			side <- sapply(side, function(term) {
			
				term <- sapply(term, function(component) { base::Encoding(component) <- "UTF-8" ; component })
			
				paste(term, collapse=i.symbol)
			})
			
			paste(side, collapse=" + ")
		})
		
		paste(formula, collapse=" ~ ")
	})
}


.jrapply <- function(X, FUN) {
	
	if (is.list(X) && length(X) > 0) {
		
		for (i in 1:length(X)) {
			X[[i]] <- .jrapply(X[[i]], FUN)
		}
	}
	else {
		X <- FUN(X)
	}
	
	X
}

.shouldContinue <- function(value) {

	base::identical(value, 0) || base::identical(value, as.integer(0)) || (is.list(value) && value$status == "ok")
}

callback <- function(results=NULL, progress=NULL) {

	ret <- 0

	if (base::exists(".callbackNative")) {

		if (is.null(results)) {
			json.string <- "null"
		} else {
			json.string <- rjson::toJSON(.imgToResults(results))
		}
		
		if (is.null(progress)) {
			progress <- -1
		} else if (! is.numeric(progress)) {
			stop("Provide a numeric value to the progress updater")
		}
		
		response <- .fromRCPP(".callbackNative", json.string, progress)
		
		if (is.character(response)) {
		
			ret <- rjson::fromJSON(base::paste("[", response, "]"))[[1]]
			
		} else {
		
			ret <- response
		}
	}

	ret
}

.cat <- function(object) {
	
	cat(rjson::toJSON(object))
}

.dataFrameToRowList <- function(df, discard.column.names=FALSE) {

	if (dim(df)[1] == 0 || dim(df)[2] == 0)
		return(list())
		
	column.names <- names(df)
	rows <- list()

	for (i in 1:dim(df)[1]) {
	
		row <- list()
		
		for (j in 1:length(column.names))
			row[[j]] <- df[i,j]
		
		if ( ! discard.column.names)
			names(row) <- column.names
		
		rows[[i]] <- row
	}

	rows
}

.indices <- function(v) {

	indices <- c()

	if (length(v) > 0)
		indices <- 1:length(v)
	
	indices
}

.seqx <- function(from, to) {

	if (from > to)
		seq <- c()
	else
		seq <- from:to
		
	seq
}

.beginSaveImage <- function(width=320, height=320) {

	type <- "cairo"
	
	if (Sys.info()["sysname"]=="Darwin")  # OS X
		type <- "quartz"
	
	pngMultip <- .fromRCPP(".ppi") / 96
		
	# create png file location
	location <- .fromRCPP(".requestTempFileNameNative", "png")
	root <- location$root
	relativePath <- location$relativePath
	base::Encoding(relativePath) <- "UTF-8"
	base::Encoding(root) <- "UTF-8"

	setwd(root)

	grDevices::png(filename=relativePath, width=width * pngMultip,
								height=height * pngMultip, bg="transparent", 
								res=72 * pngMultip, type=type)
		
	relativePath
}

.endSaveImage <- function(filename) {

	grDevices::dev.off()
	
	filename
}

.extractErrorMessage <- function(error) {

	split <- base::strsplit(as.character(error), ":")[[1]]
	last <- split[[length(split)]]
	stringr::str_trim(last)
}

.clean <- function(value) {
    # Clean function value so it can be reported in json/html
    
	if (is.list(value)) {
	    if (is.null(names(value))) {
	        for (i in length(value)) {
			    value[[i]] <- .clean(value[[i]])
			}
		} else {
		    for (name in names(value)) {
			    value[[name]] <- .clean(value[[name]])
			}
		}
		return(value)
	}

	if (is.null(value)) {
	    return ("")
	}

	if (is.character(value)) {
	    return(value)
	}

	if (is.finite(value)) {
	    return(value)
	}

	if (is.na(value)) {
	    return("NaN")
	}
    
    if (identical(value, numeric(0))) {
        return("")
    }

	if (value == Inf) {
	    return("\u221E")
	}
		
	if (value == -Inf) {
	    return("-\u221E")
	}
		
	stop("could not clean value")
}

.parseMessage <- function(message, class, ...) {
	args <- list(...)
	
	if (class == "error") {
		# If a grouping argument is added, the message 'after grouping on {{}}' is automatically included.
		if (! is.null(args[['grouping']])) {
			message <- paste(message, .messages('error', 'grouping'))
		}
	}
	
	# Find all {{string}}'s that needs to be replaced by values.
	toBeReplaced <- regmatches(message, gregexpr("(?<=\\{{)\\S*?(?=\\}})", message, perl=TRUE))[[1]]
	if (base::identical(toBeReplaced, character(0)) == FALSE) { # Were there any {{string}}'s?
		
		if (all(toBeReplaced %in% names(args)) == FALSE) { # Were all replacements provided in the arguments?
			missingReplacements <- toBeReplaced[! toBeReplaced %in% names(args)]
			stop('Missing required replacement(s): "', paste(missingReplacements, collapse=','), '"')
		}
		
		for (i in 1:length(toBeReplaced)) {
			value <- args[[ toBeReplaced[i] ]]
			if (length(value) > 1) { # Some arguments may have multiple values, e.g. amount = c('< 3', '> 5000').
				if (toBeReplaced[i] %in% c('variables', 'grouping')) {
					value <- paste(value, collapse=', ')
				} else {
					value <- paste(value, collapse=' or ')
				}
			}
			message <- gsub(paste0('{{', toBeReplaced[i], '}}'), value, message, fixed=TRUE)
		}
		
	}
	
	# Find all values we do not want in the output, e.g. we do not want to show !=
	replaceInMessage <- list('!=' = '≠', '==' = '=')
	for (i in 1:length(replaceInMessage)) {
		if (grepl(names(replaceInMessage)[i], message)) {
			message <- gsub(names(replaceInMessage)[i], replaceInMessage[[i]], message)
		}
	}
	
	return(message)
}


.newFootnotes <- function() {
	
	footnotes <- new.env()
	footnotes$footnotes <- list()
	footnotes$next.symbol <- 0
	
	class(footnotes) <- c("footnotes", class(footnotes))
	
	footnotes
}

as.list.footnotes <- function(footnotes) {
	
	footnotes$footnotes
}

.addFootnote <- function(footnotes, text, symbol=NULL, row=NULL, cols=NULL) {
	
	if (! is.null(row) && ! is.numeric(row))
		stop("Expecting row to be a numeric index")
	
	if (! is.null(cols) && ! is.character(cols))
		stop("Expecting cols to be a (vector of) character name(s)")
	
	if (length(footnotes$footnotes) == 0) {
		
		if (is.null(symbol)) {
		
			symbol <- footnotes$next.symbol
			footnotes$next.symbol <- symbol + 1	
		}
		
		footnotes$footnotes <- list(list(symbol=symbol, text=text, row=row, cols=cols))
		
		return(0)
		
	} else {
		
		for (i in 1:length(footnotes$footnotes)) {
			
			footnote <- footnotes$footnotes[[i]]
			
			if ("text" %in% names(footnote)) {
				existingMessage <- footnote$text
			} else {
				existingMessage <- footnote
			}
			
			if (existingMessage == text)
				return(i-1)
		}
		
		if (is.null(symbol)) {
		
			symbol <- footnotes$next.symbol
			footnotes$next.symbol <- symbol + 1	
		}

		new.footnote <- list(symbol=symbol, text=text, row=row, cols=cols)
		
		index <- length(footnotes$footnotes)+1
		footnotes$footnotes[[index]] <- new.footnote
		
		return(index-1)
	}
}


.diff <- function(one, two) {

	# returns TRUE if different or not really comparable
	# returns a list of what has changed if non-identical named lists provided
	
	if (is.null(names(one)) == ( ! is.null(names(two))))  # if one list has names, and the other not
		return(TRUE)
	
	changed <- list()
	
	if (is.null(names(one)) == FALSE) {
		
		names1 <- names(one)
		names2 <- names(two)
		
		for (name in names1) {
			
			if (name %in% names2) {
				
				item1 <- one[[name]]
				item2 <- two[[name]]
				
				if (base::identical(item1, item2) == FALSE) {
				
					changed[[name]] <- TRUE
					
				} else {
				
					changed[[name]] <- FALSE
				}
				
			} else {
				
				changed[[name]] <- TRUE
				
			}
			
		}
		
		for (name in names2) {
			
			if ((name %in% names1) == FALSE)
				changed[[name]] <- TRUE
		}
		
	} else if (base::identical(one, two)) {
		
		return(FALSE)
		
	} else {
	
		return (TRUE)
	}
	
	changed
}


.optionsChanged <- function(opts1, opts2, subset=NULL) {
	
  changed <- .diff(opts1, opts2)
  if (! is.list(changed)) {
    return(TRUE)
  }
  
	if (! is.null(subset)) {
	  changed <- changed[names(changed) %in% subset]
	  if (length(changed) == 0) {
	    stop(paste0("None of the gui options (", paste(subset, collapse=", "), ") is in the options list."))
	  }
	}
  
  if (sum(sapply(changed, isTRUE)) > 0) {
    return(TRUE)
  }
  
  return(FALSE)
}


.getStateItems <- function(state, options, key) {
	
  if (is.null(names(state)) || is.null(names(state$options)) || 
      is.null(names(options)) || is.null(names(key))) {
    return(NULL)
  }

  result <- list()
  for (item in names(state)) {
		
		if (item %in% names(key) == FALSE) {
      result[[item]] <- state[[item]]
      next
    }
    
    change <- .optionsChanged(state$options, options, key[[item]])
    if (change == FALSE) {
      result[[item]] <- state[[item]]
		}
    
  }
	
	if (length(names(result)) > 0) {
		return(result)
	}
  
  return(NULL)
}


.writeImage <- function(width=320, height=320, plot, obj = TRUE){
	# Initialise output object
	image <- list()

	# Operating System information
	type <- "cairo"  
	if (Sys.info()["sysname"]=="Darwin"){
	    type <- "quartz"
	}
	
	# Calculate pixel multiplier
	pngMultip <- .fromRCPP(".ppi") / 96
	
	# Create png file location
	location <- .fromRCPP(".requestTempFileNameNative", "png")
	relativePathpng <- location$relativePath
	base::Encoding(relativePathpng) <- "UTF-8"

	root <- location$root
	base::Encoding(root) <- "UTF-8"
	oldwd <- getwd()
	setwd(root)
	on.exit(setwd(oldwd))
	# Open graphics device and plot
	grDevices::png(filename=relativePathpng, width=width * pngMultip,
	               height=height * pngMultip, bg="transparent", 
	               res=72 * pngMultip, type=type)
	
	if (inherits(plot, "function")) {
		if (obj) dev.control('enable') # enable plot recording
		eval(plot())
		if (obj) plot <- recordPlot() # save plot to R object
	} else {
		print(plot)
	}
	dev.off()
	
	# Save path & plot object to output
	image[["png"]] <- relativePathpng
	if (obj) image[["obj"]] <- plot
	
	# Return relative paths in list
	image
}


# not .saveImage() because RInside (interface to CPP) cannot handle that
saveImage <- function(plotName, format, height, width){
	# Retrieve plot object from state
	state <- .retrieveState()
	plt <- state[["figures"]][[plotName]]

	# create file location string
	location <- .fromRCPP(".requestTempFileNameNative", "png") # to extract the root location
	root <- location$root
	base::Encoding(root) <- "UTF-8"
	oldwd <- getwd()
	setwd(root)
	on.exit(setwd(oldwd))
	
	# Get file size in inches by creating a mock file and closing it
	pngMultip <- .fromRCPP(".ppi") / 96
	png(filename="dpi.png", width=width * pngMultip,
			height=height * pngMultip,res=72 * pngMultip)
	insize <- dev.size("in")
	dev.off()

	relativePath <- paste0("temp.", format)

    # Open correct graphics device
	if (format == "eps") {
		
		grDevices::cairo_ps(filename=relativePath, width=insize[1],
												height=insize[2], bg="transparent")

	} else if (format == "tiff") {

		hiResMultip <- 300/72
		grDevices::tiff(filename=relativePath,
										width = width * hiResMultip,
										height = height * hiResMultip,
										res = 300, bg="transparent",
										compression = "lzw",
										type = "cairo")

	} else if (format == "pdf") {

		grDevices::cairo_pdf(filename=relativePath, width=insize[1],
												height=insize[2], bg="transparent")

	} else { # add optional other formats here in "else if"-statements
		stop("Format incorrectly specified")
	}

	# Plot and close graphics device
	if (class(plt) == "recordedplot"){
		.redrawPlot(plt) #(see below)
	} else if ("gg" %in% tolower(class(plt))){
		print(plt) #ggplots
	}
	dev.off()

	# Create JSON string for interpretation by JASP front-end
	result <- paste0("{ \"status\" : \"imageSaved\", \"results\" : { \"name\" : \"",
										relativePath , "\" } }")
	# Return result
	result
}

# Source: https://github.com/Rapporter/pander/blob/master/R/evals.R#L1389
# THANK YOU FOR THIS FUNCTION!
.redrawPlot <- function(rec_plot) {
	if (getRversion() < '3.0.0') {
	  for (i in 1:length(rec_plot[[1]])) {
	    #@jeroenooms
	    if ('NativeSymbolInfo' %in% class(rec_plot[[1]][[i]][[2]][[1]])) {
	        rec_plot[[1]][[i]][[2]][[1]] <- getNativeSymbolInfo(rec_plot[[1]][[i]][[2]][[1]]$name)
	    }
	  }
	} else {
    for (i in 1:length(rec_plot[[1]])) {
      #@jjallaire
      symbol <- rec_plot[[1]][[i]][[2]][[1]]
      if ('NativeSymbolInfo' %in% class(symbol)) {
        if (!is.null(symbol$package)) {
            name <- symbol$package[['name']]
        } else {
            name <- symbol$dll[['name']]
        }
        pkg_dll <- getLoadedDLLs()[[name]]
        native_sumbol <- getNativeSymbolInfo(name = symbol$name,
                                            PACKAGE = pkg_dll, withRegistrationInfo = TRUE)
        rec_plot[[1]][[i]][[2]][[1]] <- native_sumbol
      }
    }
	}
	if (is.null(attr(rec_plot, 'pid')) || attr(rec_plot, 'pid') != Sys.getpid()) {
    warning('Loading plot snapshot from a different session with possible side effects or errors.')
    attr(rec_plot, 'pid') <- Sys.getpid()
	}
	suppressWarnings(grDevices::replayPlot(rec_plot))
}

# This recursive function removes all non-jsonifyable image objects from a 
# result list, while retaining the structure of said list.
.imgToResults <- function(lst) {

	if (! "list" %in% class(lst))
		return(lst) # we are at an end node or have a non-list/custom object, stop
	
	if (all(c("data", "obj") %in% names(lst)) && is.character(lst[["data"]])) {
		# found a figure! remove its object!
		lst <- lst[names(lst) != "obj"]
	}

	# recurse into next level
	return(lapply(lst, .imgToResults))
}

# This recursive function takes a results object and extracts all the figure 
# objects from it, irrespective of their location within the nested structure. 
# It then returns a named list of image objects.
.imgToState <- function(lst) {

	result <- list()
	
	if (!is.list(lst))
		return(NULL) # we are at an end node, stop

	if (all(c("data", "obj") %in% names(lst)) && is.character(lst[["data"]])) {
		# Found a figure, add to the list!
		result[[lst[["data"]]]] <- lst[["obj"]]
		return(result)
	}

	# Recurse into the next level (unname to avoid concatenating list names 
	# such as (name1.name2."data"))
	return(unlist(lapply(unname(lst), .imgToState), recursive = FALSE))

}


as.modelTerms <- function(object, ...) UseMethod("as.modelTerms")
as.modelTerms.list <- function(object) structure(object, class = "modelTerms")
as.modelTerms.formula <- function(formula) structure(sapply(attr(terms(formula), "term.labels"), strsplit, ":"), class="modelTerms")
formula.modelTerms <- function(modelTerms, env = parent.frame()) {
  # Converts a modelTerms list into a one-side R formula
  #
  # Args:
  #   modelTerms:  A list of interaction terms, each term being a list of variable names involved in the interaction
  #   env:         An environement associated with the variables in the formula, see ?as.formula
  #
  # Value:
  #   A formula. See ?formula
  #
  terms = sapply(modelTerms, function(x) paste0(unlist(x), collapse = ":"))
  terms = terms[terms != ""]
  formula.rhs = paste(terms, collapse = " + ")
  if (formula.rhs != "") as.formula(paste(" ~ ", formula.rhs), env = env)
}


b64 <- function(x, ...) UseMethod("b64")   ## Translate names in x to 'base64' 
d64 <- function(x, ...) UseMethod("d64")   ## Untranslate names in x from 'base64' 

b64.character <- function(x, values, prefix = "X", ...) {
  if (missing(values)) 
    return(.v(x, prefix = prefix))
  
  for (value in values)
    x = gsub(value, b64(value), x)
  x
}

d64.character <- function(x, values, ...) {
  if (missing(values)) 
    return(.unv(x))
  
  for (value in values)
    x = gsub(value, d64(value), x)
  x
}

b64.default <- function(object, ...) {
  if (!is.null(dimnames(object))) {
    dimnames(object) = lapply(dimnames(object), b64, ...)
  }
  if (!is.null(names(object))) {
    names(object) = b64(names(object), ...)
  }
  object
}

d64.default <- function(object, ...) {
  if (!is.null(dimnames(object))) {
    dimnames(object) = lapply(dimnames(object), d64, ...)
  }
  if (!is.null(names(object))) {
    names(object) = d64(names(object), ...)
  }
  object
}

b64.modelTerms = function(object, ...) structure(b64(unclass(object), ...), class="modelTerms")
d64.modelTerms = function(object, ...) structure(d64(unclass(object), ...), class="modelTerms")
b64.formula = function(formula, ...) as.formula(b64(as.modelTerms(formula), ...))
d64.formula = function(formula, ...) as.formula(d64(as.modelTerms(formula), ...))

b64.matrix <- function(x, ...) {
  dimnames(x) <- rapply(dimnames(x), b64, ..., classes = "character", how="replace")
  x
}
d64.matrix <- function(x, ...) {
  dimnames(x) <- rapply(dimnames(x), d64, ..., classes = "character", how="replace")
  x
}
b64.data.frame <- function(x, ...) {
  colnames(x) = b64(colnames(x))
  rownames(x) = b64(rownames(x))
  x
}
d64.data.frame <- function(x, ...) {
  colnames(x) = d64(colnames(x))
  rownames(x) = d64(rownames(x))
  x
}
b64.call <- function(x, which = seq_along(x)[-1], ...) {
  x <- as.list(x)
  x[which] = lapply(x[which], b64, ...)
  as.call(x)
}
d64.call <- function(x, which = seq_along(x)[-1], ...) {
  x <- as.list(x) # which relies on this (and lazy evaluation): must be fist for next statement to work!
  x[which] = lapply(x[which], d64, ...)
  as.call(x)
}
b64.name <- function(x, ...) {
  as.name(b64(as.character(x)))
}
d64.name <- function(x, ...) {
  as.name(d64(as.character(x)))
}
b64.list <- function(x, ...) {
  rapply(x, b64, ..., how = "replace")
}
d64.list <- function(x, ...) {
  rapply(x, d64, ..., how = "replace")
}
	
.newProgressbar <- function(ticks, callback, skim=5, response=FALSE, parallel=FALSE) {
  # This closure normally returns a progressbar function that expects to be called "ticks" times.
  # If used in a parallel environment it returns a structure to the master process which is
  # updated in the separate processes by .updateParallelProgressbar().
  
	ticks <- suppressWarnings(as.integer(ticks))
	if (is.na(ticks) || ticks <= 0)
		stop("Invalid value provided to 'ticks', expecting positive integer")
	
	if (! is.function(callback))
		stop("The value provided to 'callback' does not appear to be a function")
	
	if (! is.numeric(skim) || skim < 0 || skim >= 100)
		stop("Invalid value provided to 'skim', expecting numeric value in the range of 0-99")
	
	if (parallel)
		response <- TRUE
	
	progress <- 0
	tick <- (100 - skim) / ticks
	createEmpty <- TRUE
	
	updater <- function(results=NULL, complete=FALSE) {
		if (createEmpty) {
			createEmpty <<- FALSE
		} else if (complete) {
			progress <<- 100
		} else {
			progress <<- progress + tick
		}
		
		if (progress > 100)
			progress <<- 100
			
		output <- callback(results=results, progress=round(progress))
		
		if (response)
			return(output)
	}
	
	updater() # create empty progressbar
	
	if (parallel)
		return(structure(list(updater=updater), class="JASP-progressbar"))
	
	return(updater)
}

# Update the progressbar in a parallel environment.
# It requires the progressbar from .newProgressbar() (this structure itself remains in the master process); 
# if the callback indicates a change in UI options the cluster is stopped with a warning.
.updateParallelProgressbar <- function(progressbar, cluster, results=NULL, complete=FALSE) {
	
	if (! inherits(progressbar, "JASP-progressbar"))
		stop("Object provided in 'progressbar' is not of class JASP progressbar")
	
	if (! inherits(cluster, "cluster"))
		stop("Object provided in 'cluster' is not of class cluster")
	
	response <- progressbar$updater(results, complete)
	
	if (! .shouldContinue(response)) {
		snow::stopCluster(cluster)
		stop("Cancelled by callback")
	}
	
	invisible(response)
}

# Create a cluster to perform parallel computations. 
# You can pass it objects (and a progressbar) to be exported to the cluster.
# To be used in combination with the foreach package.
.makeParallelSetup <- function(pb=NULL, objs=NULL, env=NULL) {
	
	nCores <- parallel::detectCores(TRUE) - 1
	if (is.na(nCores) || nCores == 0)
		nCores <- 1
		
	cl <- snow::makeSOCKcluster(nCores)
	doSNOW::registerDoSNOW(cl)
	if (! is.null(objs) && ! is.null(env))
		snow::clusterExport(cl, objs, envir=env)
	
	dopar <- foreach::`%dopar%`
	
	progress <- NULL
	if (! is.null(pb))
		progress <- function() .updateParallelProgressbar(pb, cl)
	
	stopCluster <- substitute(try(snow::stopCluster(cl), silent=TRUE))
		
	return(list(cl=cl, progress=list(progress=progress), dopar=dopar, stopCluster=stopCluster))
}

# Compatibility for linux users with R < 3.3
if (exists("R.version") && isTRUE(R.version$minor < 3.3)) {

	startsWith <- function(x, prefix) {
		start <- substring(x, 1, nchar(prefix))
		return(start == prefix)
	}

	endsWith <- function(x, suffix) {
		end <- substring(x, nchar(x) - nchar(suffix) + 1)
		return(end == suffix)
	}

}
