#
# Copyright (C) 2017 University of Amsterdam
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

MLClusteringRandomForest <- function(dataset = NULL, options, perform = "run",
									 callback = function(...) 0, ...) {
									  # callback = function(...) list(status = "ok"), ...) {

	print(str(options))
	## Read Dataset ## ----
	variables <- unlist(options[["predictors"]])
	target <- options[["target"]]
	if (target == "") # default for empty target
		target <- NULL

	variables.to.read <- c(target, variables)

	if (is.null(dataset)) { # how to handle factors?

		if (perform == "run") {

			dataset <- .readDataSetToEnd(columns = variables.to.read, exclude.na.listwise = NULL)

		} else {

			dataset <- .readDataSetHeader(columns = variables.to.read)

		}

	} else {

		if (!Sys.getenv("RSTUDIO") == "1")
			dataset <- .vdf(dataset, columns = variables.to.read)

	}

	# This problem can occur when reading the dataset with the columns argument: (reference: http://stackoverflow.com/questions/41441665/how-to-fix-a-malformed-factor)
	# vec <- structure(c(1L,2L, 33L), .Label = c("first", "second"), class = "factor")
	# print(vec)
	# levels(vec) <- levels(vec)
	# print(vec)

	# This fixes it:
	colIdx <- sapply(dataset, is.factor)
	if (is.logical(colIdx)) {

		seqFactor <- which(colIdx)
		for (var in seqFactor)
			levels(dataset[[var]]) <- levels(dataset[[var]])

	}
	print("str(dataset)")
	print(str(dataset))

	# ensures order of variables matches order of columns in dataset (first column is target)
	variables <- variables[match(.unv(colnames(dataset)), variables, nomatch = 0L)]

	## TODO: Retrieve State ## ----

	# state <- .retrieveState()

	toFromState <- NULL

	# if (!is.null(state)) {  # is there state?
	#
	# 	diff <- .diff(options, state$options)  # compare old and new options
	#
	# 	# if nothing important was changed retrieve state
	# 	if (is.list(diff) && diff[['variables']] == FALSE) {
	#
	# 		toFromState <- state[["results"]]
	#
	# 	}
	#
	# }

	## Initialize Results ## ----

	results <- list(
		title = "Random Forest Regression",
		.meta = list(
			list(name = "title",                     type = "title"),
			list(name = "tableSummary",   type = "table"),				
			list(name = "tableVariableImportance",   type = "table"),		
			list(name = "plotVariableImportance",    type = "image"),
			list(name = "plotTreesVsModelError",     type = "image")
		)
	)

	## Do Analysis ## ----
	errorList <- NULL

	if (is.null(toFromState) && !is.null(variables) && !is.null(target)) { # implies old state was unusable

		# check for errors
		anyErrors <- .hasErrors(dataset = dataset, perform = perform, type = c("infinity", "variance"))
		.pprint(anyErrors)
		doUpdate <- base::identical(anyErrors, FALSE)
		.pprint(doUpdate)
		if (doUpdate) { # do analysis

			toFromState <- .MLRFAnalysis(dataset, purpose = "regression", perform = perform,
										 options = options, variables = variables, target = target)

		} else { # show error message

			errorList <- list(errorType = "badData", errorMessage = anyErrors[["message"]])

		}

	} else { # implies results are retrieved from state

		doUpdate <- TRUE

	}

	## Create Output ## ----

	if (doUpdate) { # no errors were found

		if (options[["tableSummary"]])
			results[["tableSummary"]] <- .MLRFSummary(toFromState = toFromState, variables = variables, perform = perform)			

		if (options[["tableVariableImportance"]])
			results[["tableVariableImportance"]] <- .MLRFVarImpTb(toFromState = toFromState, variables = variables, perform = perform)

		if (options[["plotTreesVsModelError"]])
			results[["plotTreesVsModelError"]] <- .MLRFplotTreesVsModelError(toFromState = toFromState, options = options,
																					 perform = perform)

		if (options[["plotVariableImportance"]])
			results[["plotVariableImportance"]] <- .MLRFplotVariableImportance(toFromState = toFromState, options = options,
																			   variables = variables, perform = perform)

	} else { # add error messages

		# Create an empty table to show the error
		results[["tableVariableImportance"]] <- .MLRFVarImpTb(toFromState = NULL, variables = variables, perform = perform)
		results[["tableVariableImportance"]][["error"]] <- errorList

	}
	## Save State ##


	## Exit Analysis ##
	if (perform == "init") {

		return(list(results=results, status="inited"))#, state=state))

	} else {

		return(list(results=results, status="complete"))#, state=state))

	}
}