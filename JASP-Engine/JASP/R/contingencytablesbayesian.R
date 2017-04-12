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

ContingencyTablesBayesian <- function(dataset, options, perform, callback, ...) {

	layer.variables <- c()  

	for (layer in options$layers)
		layer.variables <- c(layer.variables, unlist(layer$variables))

	counts.var <- options$counts
	if (counts.var == "")
		counts.var <- NULL

	factors <- c(unlist(options$rows), unlist(options$columns), layer.variables)

	if (is.null(dataset))
	{
		if (perform == "run") {
			dataset <- .readDataSetToEnd(columns.as.factor=factors, columns.as.numeric=counts.var, exclude.na.listwise=counts.var)
		} else {
			dataset <- .readDataSetHeader(columns.as.factor=factors, columns.as.numeric=counts.var)
		}
	}
	
	old.state <- .retrieveState()

	results <- list()
	
	### META

	meta <- list()
	
	results[["title"]] <- "Bayesian Contingency Tables"
	
	### CONTINGENCY TABLES

	cont.tables <- list()
	plots       <- list()
	
	rows    <- as.vector(options$rows,    "character")
	columns <- as.vector(options$columns, "character")
	
	if (length(rows) == 0)
		rows <- ""
	
	if (length(columns) == 0)
		columns <- ""

	analyses <- data.frame("columns"=columns, stringsAsFactors=FALSE)
	analyses <- cbind(analyses, "rows"=rep(rows, each=dim(analyses)[1]), stringsAsFactors=FALSE)
	
	for (layer in options$layers) {
	
		layer.vars <- as.vector(layer$variables, "character")
		analyses <- cbind(analyses, rep(layer.vars, each=dim(analyses)[1]), stringsAsFactors=FALSE)
		names(analyses)[dim(analyses)[2]] <- layer$name
	}
	
	analyses <- .dataFrameToRowList(analyses)

	new.state <- list()
	new.state$results <- list()
	new.state$options <- options
	
	complete <- TRUE


	next.table.index <- 1
	next.plot.index  <- 1
	keep <- list()
	plotGroups <- list()

	for (i in seq_along(analyses)) {
	
		analysis <- analyses[[i]]
		
		if ("results" %in% names(old.state) && i <= length(old.state$results)) {
			last.results <- old.state$results[[i]]
		} else {
			last.results <- NULL
		}
	
		res <- .contTablesBayesian(dataset, options, populate=FALSE, analysis, last.results)
				
		for (plot in res$plots) {

			plots[[next.plot.index]] <- plot
			next.plot.index <- next.plot.index + 1
		}
		
		keep <- c(keep, res$keep)
		
		if ("state" %in% names(res)) {
		
			new.state$results[[i]] <- res$state
			
		} else {
		
			new.state$results[i] <- list(NULL)
		}
		
		complete <- complete && res$complete
	}
	
	if (perform == "run" && complete == FALSE) {
		
		next.table.index <- 1
		next.plot.index  <- 1

		for (i in seq_along(analyses))
		{					
			analysis <- analyses[[i]]
		
			if ("results" %in% names(old.state) && i <= length(old.state$results)) {
				last.results <- old.state$results[[i]]
			} else {
				last.results <- NULL
			}
		
			res <- .contTablesBayesian(dataset, options, populate=TRUE, analysis, last.results)
			
			for (plot in res$plots) {

				plots[[next.plot.index]] <- plot
				next.plot.index <- next.plot.index + 1
			}
			
			keep <- c(keep, res$keep)
			
			if ("state" %in% names(res)) {
			
				new.state$results[[i]] <- res$state
				
			} else {
			
				new.state$results[i] <- list(NULL)
			}
		}		
	}
	
	
	if (!is.null(res$tables[["counts.table"]])) {
	
		results[["Counts Table"]] <- res$tables[["counts.table"]]
		meta[[length(meta)+1]] <- list(name="Counts Table", type="table")

	}
	if (!is.null(res$tables[["tests.table"]])) {
	
		results[["Tests Table"]] <- res$tables[["tests.table"]]
		meta[[length(meta)+1]] <- list(name="Tests Table", type="table")

	}
	if (!is.null(res$tables[["odds.ratio.table"]])) {
	
		results[["Odds Ratio Table"]] <- res$tables[["odds.ratio.table"]]
		meta[[length(meta)+1]] <- list(name="Odds Ratio Table", type="table")

	}	
	if (!is.null(res$tables[["cramersV.table"]])) {
	
		results[["Kramers V Table"]] <- res$tables[["cramersV.table"]]
		meta[[length(meta)+1]] <- list(name="Kramers V Table", type="table")

	}
	
	meta[[length(meta)+1]] <- list(name="plots", type="collection", meta=list(name="plotGroups", type="object",
																 meta=list(list(name="LogORplot", type="image"))))
	
	results[[".meta"]] <- meta
		
	for (i in seq_along(plots)) {
		
		title <- ifelse(plots[[i]]$title == "Log odds ratio", "", plots[[i]]$title)
		plots[[i]]$title <- "Log odds ratio"
		plotGroups[[i]] <- list()
		plotGroups[[i]][["LogORplot"]] <- plots[[i]]
		plotGroups[[i]][["title"]] <- title
		plotGroups[[i]][["name"]] <- title
		
	}
	
	results[["plots"]] <- list(title=ifelse(length(plots) > 1, "Plots", "Plot"), collection=plotGroups)
	
	if (perform == "run") {
	
		list(results=results, status="complete", state=new.state, keep=keep)
		
	} else {
	
		status <- ifelse(complete, "complete", "inited")
		list(results=results, status=status, state=new.state, keep=keep)
	}
}


.contTablesBayesianCitations <- function() {

	list(
		"Morey, R. D., & Rouder, J. N. (2015). BayesFactor (Version 0.9.11-3)[Computer software].",
		"Jamil, T., Ly, A., Morey, R. D., Love, J., Marsman, M., & Wagenmakers, E.-J. (2015). Default Gunel and Dickey Bayes factors for contingency tables. Manuscript submitted for publication.",
		"Gunel, E., & Dickey, J. (1974). Bayes factors for independence in contingency tables. Biometrika, 61, 545-557.")
}

.contTablesBayesian <- function(dataset, options, populate, analysis, state) {

	# analysis is a list of the form :
	# list(columns="var.name", rows="var.name", "Layer 1"="var.name", etc...)
	
	counts.var <- options$counts
	if (counts.var == "")
		counts.var <- NULL

	if (populate) {
	
		all.vars <- c(unlist(analysis), counts.var)
		dataset <- subset(dataset, select=.v(all.vars))	
	}
	
	groups <- .contTablesCreateGroups(dataset, analysis)

	# create count matrices for each group

	group.matrices <- .contTablesCreateGroupMatrices(dataset, analysis$rows, analysis$columns, groups, counts.var, options$rowOrder=="descending", options$columnOrder=="descending", populate)

	# retrieve states

	counts.state <- NULL
	if ("counts" %in% names(state))
		counts.state <- state$counts

	tests.state <- NULL
	if ("tests" %in% names(state))
		tests.state <- state$tests
	
	odds.ratio.state <- NULL
	if ("odds.ratio" %in% names(state))
		odds.ratio.state <- state$odds.ratio
		
	CramersV.state <- NULL
	if ("CramersV" %in% names(state))
		CramersV.state <- state$CramersV	
	
	plots.state <- NULL
	if ("plots.state" %in% names(state))
		plots.state <- state$plots.state
		
	#plotsCV.state <- NULL
	#if ("plotsCV.state" %in% names(state))
	#	plotsCV.state <- state$plotsCV.state	
	
	old.options <- NULL
	if ("options" %in% names(state))
		old.options <- state$options
		

	status <- list(error=FALSE)
	tables <- list()
	plots  <- list()
	keep   <- list()
	new.state <- list()
	complete  <- TRUE
	
	
	res <- .contTablesBayesianCreateCountsTable(dataset, analysis, group.matrices, groups, footnotes, options, populate, counts.state, old.options, status)

	tables[["counts.table"]] <- res$table
	new.state$counts <- res$state
	status <- res$status
	complete <- complete && res$complete
	

	res <- .contTablesBayesianCreateTestsTable(dataset, analysis, group.matrices, groups, footnotes, options, populate, tests.state, old.options, status)

	tables[["tests.table"]] <- res$table
	bf.results      <- res$state
	new.state$tests <- res$state
	status <- res$status
	complete <- complete && res$complete
	
		
	res <- .contTablesBayesianCreateOddsRatioTable(dataset, analysis, group.matrices, groups, footnotes, options, populate, bf.results, odds.ratio.state, old.options, status)

	tables[["odds.ratio.table"]] <- res$table
	odds.ratio.results   <- res$state
	new.state$odds.ratio <- res$state
	complete <- complete && res$complete
	
	res <- .contTablesBayesianCreateCramerVTable(dataset, analysis, group.matrices, groups, footnotes, options, populate, bf.results, CramersV.state, old.options, status)

	tables[["cramersV.table"]] <- res$table
	CramersV.results   <- res$state
	new.state$CramersV <- res$state
	complete <- complete && res$complete
	
	
	res <- .contTablesBayesianCreateOddsRatioPlots(dataset, analysis, group.matrices, groups, options, populate, odds.ratio.results, bf.results, plots.state, old.options, status)
	
	plots <- c(plots, res$plots)
	new.state$plots.state <- res$state
	keep     <- res$keep
	complete <- complete && res$complete
	
	#res <- .contTablesBayesianCreateCramerVPlots(dataset, analysis, group.matrices, groups, options, populate, CramersV.results, bf.results, plotsCV.state, old.options, status)
	
	#plots <- c(plots, res$plots)
	#new.state$plotsCV.state <- res$state
	#keep     <- res$keep
	#complete <- complete && res$complete
		
	new.state$options <- options
	
	if (length(options$rows) == 0 || length(options$columns) == 0)
		complete <- TRUE
	
	list(tables=tables, plots=plots, keep=keep, state=new.state, complete=complete)
}

.contTablesBayesianCreateOddsRatioPlots <- function(dataset, analysis, counts.matrices, groups, options, populate, odds.ratio.results, bf.results, state, state.options, status) {

	if (options$plotPosteriorOddsRatio == FALSE || identical(dim(counts.matrices[[1]]), as.integer(c(2,2))) == FALSE)
		return(list(complete=TRUE))

	if (is.null(state.options) ||
		base::identical(state.options$rows, options$rows) == FALSE ||
		base::identical(state.options$columns, options$columns) == FALSE ||
		base::identical(state.options$counts, options$counts) == FALSE ||
		base::identical(state.options$layers, options$layers) == FALSE ||
		base::identical(state.options$columnOrder, options$columnOrder) == FALSE ||
		base::identical(state.options$rowOrder, options$rowOrder) == FALSE ||
		base::identical(state.options$samplingModel, options$samplingModel) == FALSE ||
		base::identical(state.options$hypothesis, options$hypothesis) == FALSE ||
		base::identical(state.options$oddsRatioCredibleIntervalInterval, options$oddsRatioCredibleIntervalInterval) == FALSE ||
		base::identical(state.options$plotPosteriorOddsRatioAdditionalInfo, options$plotPosteriorOddsRatioAdditionalInfo) == FALSE ||
		base::identical(state.options$priorConcentration, options$priorConcentration) == FALSE) {
		
		state <- NULL
	}

	plots <- list()
	keep  <- list()
	new.state <- list()
	
	complete <- TRUE
	
	for (i in 1:length(counts.matrices)) {
	
		counts.matrix <- counts.matrices[[i]]
		
		if ( ! is.null(groups)) {
		
			group <- groups[[i]]
			
		} else {
		
			group <- NULL
		}
		
		plot.state <- NULL
		if (i <= length(state))
			plot.state <- state[[i]]

		odds.ratio.result <- NULL
		if (i <= length(odds.ratio.results))
			odds.ratio.result <- odds.ratio.results[[i]]
		
		bf.result <- NULL
		if (i <= length(bf.results))
			bf.result <- bf.results[[i]]

		res <- .contTablesBayesianCreateOddsRatioPlot(analysis$rows, counts.matrix, options, populate, group, odds.ratio.result, bf.result, plot.state, state.options, status)

		complete <- complete && res$complete
		
		plots[[length(plots)+1]] <- res$plot
		
		keep <- c(keep, res$keep)
		
		new.state[[length(new.state)+1]] <- res$state
	}
	
	complete <- complete && (populate || is.null(state) == FALSE || length(options$rows) == 0 || length(options$columns) == 0)

	list(plots=plots, keep=keep, state=new.state, complete=complete)
}

.contTablesBayesianCreateOddsRatioPlot <- function(var.name, counts.matrix, options, populate, group, odds.ratio.result, bf.result, plot.state, state.options, status) {
	
	odds.ratio.plot  <- list()
	image <- NULL
	
    group[group == ""] <- "Total"
    
	
	if (length(group) == 0) {
	
		odds.ratio.plot[["title"]] <- "Log odds ratio"
		
	} else if (length(group) > 0) {
		
		layer.levels <- paste(names(group),"=", group)
		layer.levels <- gsub(pattern = " = Total", layer.levels, replacement = "")

		plot.title <- paste(layer.levels, collapse="; ")
		odds.ratio.plot[["title"]] <- plot.title
	}
	
	
	width  <- 530
	height <- 400
	
	odds.ratio.plot[["width"]]  <- width
	odds.ratio.plot[["height"]] <- height
	odds.ratio.plot[["citation"]] <- .contTablesBayesianCitations()
	

	if (is.null(plot.state) == FALSE) {
	
	
		odds.ratio.plot[["data"]] <- plot.state$keep
		
		if ("status" %in% names(plot.state) && plot.state$status$error) {
			odds.ratio.plot[["error"]] <- list(errorType="badData", errorMessage=paste0("Plotting is not possible: " , plot.state$status$errorMessage))
			
			}
		
		return(list(plot=odds.ratio.plot, state=plot.state, keep=plot.state$keep, complete=TRUE))
	}
	
	if (populate && status$error == FALSE) {
	
		if (is.null(bf.result$BF) == FALSE && inherits(bf.result$BF, "try-error")) {
	
			message <- .extractErrorMessage(bf.result$BF)
			status <- list(error=TRUE, errorMessage=message)
		
		} else if (options$samplingModel == "hypergeometric") {
		
			status <- list(error=TRUE, errorMessage="Odds ratio for this model not yet implemented")
		
		} else {
		
			BF10 <- bf.result$BF$BF10
		
			if (is.na(BF10)) {
		
				status <- list(error=TRUE, errorMessage="The Bayes factor is undefined")
			
			} else if (is.infinite(1 / BF10)) {

				status <- list(error=TRUE, errorMessage="The Bayes factor is too small")
			
			} else if (is.infinite(BF10)) {
		
				status <- list(error=TRUE, errorMessage="The Bayes factor is infinite")
			}
		}
	}
	
	if (populate && status$error == FALSE) {
	
		# image <- .beginSaveImage(width, height)

		p <- try(silent=TRUE, expr={
			
			if (options$samplingModel == "poisson"
				|| options$samplingModel == "jointMultinomial"
				|| options$samplingModel == "independentMultinomialColumnsFixed"
				|| options$samplingModel == "independentMultinomialRowsFixed") {
			
				if (options$hypothesis == "groupTwoGreater") {
				
					oneSided <- "left"
			
				} else if (options$hypothesis == "groupOneGreater") {
				
					oneSided <- "right"
			
				} else {
				
					oneSided <- FALSE
				}
				
			} else {
			
				oneSided <- FALSE
			}
			.plotFunc <- function() {
				.contTablesBayesianPlotPosterior(
					samples = odds.ratio.result$log.odds.ratio.samples,
					CI = c(odds.ratio.result$lower.ci, odds.ratio.result$upper.ci),
					medianSamples = odds.ratio.result$median,
					BF = bf.result$BF$BF10,
					selectedCI = options$oddsRatioCredibleIntervalInterval,
					addInformation = options$plotPosteriorOddsRatioAdditionalInfo, oneSided=oneSided, options=options)
			}
		})
		
		plot <- .writeImage(width = width, height = height, plot = .plotFunc, obj = TRUE)
		# plot <- .endSaveImage(image)

		if (inherits(p, "try-error")) {
			
			errorMessage <- .extractErrorMessage(p)
			
			if (errorMessage == "not enough data") {
			
				errorMessage <- "The Bayes factor is too small"
					
			} else if (errorMessage == "'from' cannot be NA, NaN or infinite") {
			
				errorMessage <- "The Bayes factor is too small"
			}
			
			status <- list(error=TRUE, errorMessage=errorMessage)
		}
	
	}

	if (populate && status$error == FALSE) {
	
		odds.ratio.plot[["data"]] <- plot[["png"]]
		odds.ratio.plot[["obj"]] <- plot[["obj"]]
		odds.ratio.plot[["convertible"]] <- TRUE
		odds.ratio.plot[["status"]] <- "complete"
		
		new.plot.state <- list(keep=plot)
		
		complete <- TRUE
	
	} else {
		
		.plotFunc = function() {
			.contTablesBayesianPlotPosterior(dontPlotData=TRUE,addInformation=options$plotPosteriorOddsRatioAdditionalInfo)
		}
		plot <- .writeImage(width = width, height = height, plot = .plotFunc, obj = TRUE)
	
		# image <- .beginSaveImage(width, height)
		# .contTablesBayesianPlotPosterior(dontPlotData=TRUE,addInformation=options$plotPosteriorOddsRatioAdditionalInfo)
		# plot <- .endSaveImage(image)
		
		# odds.ratio.plot[["data"]] <- plot
		odds.ratio.plot[["data"]] <- plot[["png"]]
		odds.ratio.plot[["obj"]] <- plot[["obj"]]
		odds.ratio.plot[["convertible"]] <- TRUE

		if (status$error) {
		
			new.plot.state <- list(keep=plot, status=status)

			message <- status$errorMessage
			odds.ratio.plot[["error"]]  <- list(error = "badData", errorMessage=paste("Plotting is not possible:", message))
			odds.ratio.plot[["status"]] <- "complete"
			complete <- TRUE
			
		} else {
		
			new.plot.state <- NULL
			complete <- FALSE
		}
	}
	
	list(plot=odds.ratio.plot, state=new.plot.state, keep=plot, complete=complete)
}



.contTablesBayesianCreateCountsTable <- function(dataset, analysis, counts.matrices, groups, footnotes, options, populate, state, state.options, status) {

	if (is.null(state.options) ||
		base::identical(state.options$rows, options$rows) == FALSE ||
		base::identical(state.options$columns, options$columns) == FALSE ||
		base::identical(state.options$counts, options$counts) == FALSE ||
		base::identical(state.options$layers, options$layers) == FALSE ||
		base::identical(state.options$columnOrder, options$columnOrder) == FALSE ||
		base::identical(state.options$rowOrder, options$rowOrder) == FALSE) {
			
		state <- NULL
	}

	counts.table <- list()
	
	counts.table[["title"]] <- "Bayesian Contingency Tables"
	
	
	### SETUP COUNTS TABLE SCHEMA

	counts.fields <- list()
	
	if (length(analysis) >= 3) {
	
		for (j in length(analysis):3)
			counts.fields[[length(counts.fields)+1]] <- list(name=analysis[[j]], type="string", combine=TRUE)
	}
	
	counts.fields[[length(counts.fields)+1]] <- list(name=analysis$rows, type="string", combine=TRUE)

	
	lvls <- c()
	
	if (analysis$columns == "") {
	
		lvls <- c(".", ". ")
	
	} else if (is.factor(dataset[[ .v(analysis$columns) ]] )) {
	
		lvls <- base::levels(dataset[[ .v(analysis$columns) ]])
		if (options$columnOrder == "descending") {
			lvls <- base::rev(lvls)
		} else {
			lvls <- lvls
		}

	} else if (populate) {
	
		lvls <- base::unique(dataset[[ .v(analysis$columns) ]])
		
		if (options$columnOrder == "descending") {
			lvls <- base::rev(lvls, decreasing = TRUE)
		} else {
			lvls <- lvls
		}
	}
	
	
	row.lvls <- c()
	
	if (analysis$rows == "") {
	
		row.lvls <- c(".", ". ")
	
	} else if (is.factor(dataset[[ .v(analysis$rows) ]] )) {
	
		row.lvls <- base::levels(dataset[[ .v(analysis$rows) ]])
		if (options$rowOrder == "descending") {
			row.lvls <- base::rev(row.lvls)
		} else {
			row.lvls <- row.lvls
		}

	} else if (populate) {
	
		row.lvls <- base::unique(dataset[[ .v(analysis$rows) ]])
		
		if (options$rowOrder == "descending") {
			row.lvls <- base::rev(row.lvls, decreasing = TRUE)
		} else {
			row.lvls <- row.lvls
		}
	}
	
	counts.fp <- FALSE  # whether the counts are float point or not; changes formatting
	
	if (options$counts != "") {

		counts <- dataset[[ .v(options$counts) ]]
		if (identical(counts, as.integer(counts)) == FALSE)  # are the counts floating point?
			counts.fp <- TRUE
	}
	

	overTitle <- unlist(analysis$columns)
	if (overTitle == "")
		overTitle <- "."
		
	
	for (column.name in lvls) {

		private.name <- base::paste(column.name,"[counts]", sep="")
		
		if (counts.fp || options$countsExpected) {
		
			counts.fields[[length(counts.fields)+1]] <- list(name=private.name, title=column.name, overTitle=overTitle, type="number", format="sf:4;dp:2")
		
		} else {
		
			counts.fields[[length(counts.fields)+1]] <- list(name=private.name, title=column.name, overTitle=overTitle, type="integer")
		}
		
		if (options$countsExpected) {
		
			private.name <- base::paste(column.name,"[expected]", sep="")
			counts.fields[[length(counts.fields)+1]] <- list(name=private.name, title=column.name, overTitle=overTitle, type="number", format="sf:4;dp:2")
		}
	}
	
	# Totals columns
	
	if (counts.fp || options$countsExpected) {
	
		counts.fields[[length(counts.fields)+1]] <- list(name="total[counts]",   title="Total", type="number", format="sf:4;dp:2")	
		
	} else {
	
		counts.fields[[length(counts.fields)+1]] <- list(name="total[counts]", title="Total", type="integer")
	}

	if (options$countsExpected) {
	
		counts.fields[[length(counts.fields)+1]] <- list(name="total[expected]", title="Total", type="number", format="sf:4;dp:2")
	}

	schema <- list(fields=counts.fields)

	counts.table[["schema"]] <- schema
	
	counts.rows <- list()
	new.state <- list(rows=list(), status=list(error=FALSE))
	
	if (is.null(state) == FALSE)
		status <- state$status

	if (populate && options$counts != "") {
	
		counts <- dataset[[ .v(options$counts) ]]
		
		if (any(counts < 0) || any(is.infinite(counts))) {
	
			populate <- FALSE
			status <- list(error=TRUE, errorMessage="Counts may not contain negative numbers or infinities")
		}
	}
	
	for (i in 1:length(counts.matrices)) {
	
		counts.matrix <- counts.matrices[[i]]
		
		if ( ! is.null(groups)) {
		
			group <- groups[[i]]
			
		} else {
		
			group <- NULL
		}
		
		rows.state <- NULL
		if (is.null(state) == FALSE && i <= length(state$rows))
			rows.state <- state$rows[[i]]
	
		next.rows <- .contTableBayesianCreateCountsRows(analysis$rows, counts.matrix, options, populate, group, rows.state)
		
		counts.rows <- c(counts.rows, next.rows)
		new.state$rows[[length(new.state$rows)+1]] <- next.rows
	}
	
	counts.table[["data"]] <- counts.rows
	
	complete <- populate || is.null(state) == FALSE || length(options$rows) == 0 || length(options$columns) == 0 || status$error
	
	counts.table[["status"]] <- ifelse(complete, "complete", "running")
	
	if (status$error) {
	
		new.state$status <- status
	
		counts.table[["status"]] <- "complete"
		counts.table[["error"]] <- list(errorType="badData", errorMessage=status$errorMessage)
	}
	
	if (is.null(state) && populate == FALSE) {
	
		# when there's no previous state, and no results were populated
		# there are no results, and no state worth keeping
	
		new.state <- NULL
	}
	
	list(table=counts.table, state=new.state, complete=complete, status=status)
}

.contTablesCreateGroups <- function(dataset, analysis) {

	# the following creates a 'groups' list
	# a 'group' represents a combinations of the levels from the layers
	# if no layers are specified, groups is null

	if (length(analysis) >= 3)  # if layers are specified
	{
		lvls <- base::levels(dataset[[ .v(analysis[[3]]) ]])
		
		if (length(lvls) < 2) {
		
			lvls <- ""
			
		} else {
		
			lvls <- c(lvls, "")  # blank means total
		}

		# here we create all combinations of the levels from the layers
		# it is easiest to do this with a data frame
		# at the end we convert this to a list of rows

		groups <- data.frame(lvls, stringsAsFactors=FALSE)
		base::names(groups) <- analysis[[3]]
		
		if (length(analysis) >= 4) {
		
			for (j in 4:length(analysis))
			{
				lvls <- base::levels(dataset[[ .v(analysis[[j]]) ]])
				lvls <- c(lvls, "")  # blank means total
			
				groups <- cbind(rep(lvls, each=dim(groups)[1]), groups, stringsAsFactors=FALSE)
				names(groups)[1] <- analysis[[j]]
			}
		}
		
		# convert all the combinations to a list of rows
		
		groups <- .dataFrameToRowList(groups)
		
	} else {  # if layers are not specified
	
		groups <- NULL
	}
	
	groups
}

.contTablesBayesianCreateTestsTable <- function(dataset, analysis, counts.matrices, groups, footnotes, options, populate, state, state.options, status) {

	if (is.null(state.options) ||
		base::identical(state.options$rows, options$rows) == FALSE ||
		base::identical(state.options$columns, options$columns) == FALSE ||
		base::identical(state.options$counts, options$counts) == FALSE ||
		base::identical(state.options$layers, options$layers) == FALSE ||
		base::identical(state.options$columnOrder, options$columnOrder) == FALSE ||
		base::identical(state.options$rowOrder, options$rowOrder) == FALSE ||
		base::identical(state.options$samplingModel, options$samplingModel) == FALSE ||
		base::identical(state.options$priorConcentration, options$priorConcentration) == FALSE ||
		base::identical(state.options$hypothesis, options$hypothesis) == FALSE) {
		
		state <- NULL
	}
	
	if (status$error)
		populate <- FALSE

	table <- list()
	
	table[["title"]] <- "Bayesian Contingency Tables Tests"
	

	fields <- list()
	
	if (length(analysis) >= 3) {
	
		for (j in length(analysis):3)
			fields[[length(fields)+1]] <- list(name=analysis[[j]], type="string", combine=TRUE)
	}
	
	fields[[length(fields)+1]] <- list(name="type[BF]", title="", type="string")
	fields[[length(fields)+1]] <- list(name="value[BF]", title="Value", type="number", format="sf:4;dp:3")
	fields[[length(fields)+1]] <- list(name="type[N]", title="", type="string")
	fields[[length(fields)+1]] <- list(name="value[N]", title="Value", type="integer")
	
	schema <- list(fields=fields)
	
	table[["schema"]] <- schema
	

	rows <- list()
	new.state <- list()
	footnotes <- .newFootnotes()
	
	for (i in 1:length(counts.matrices)) {
	
		counts.matrix <- counts.matrices[[i]]
		
		if ( ! is.null(groups)) {
		
			group <- groups[[i]]
			
		} else {
		
			group <- NULL
		}
		
		rows.state <- NULL
		if (i <= length(state))
			rows.state <- state[[i]]
		
		res <- .contTablesBayesianCreateTestsRows(analysis$rows, counts.matrix, footnotes, options, populate, group, rows.state)

		rows <- c(rows, res$rows)
		new.state[[length(new.state)+1]] <- res$state
	}
	
	complete <- populate || is.null(state) == FALSE || length(options$rows) == 0 || length(options$columns) == 0
	
	table[["data"]] <- rows
	table[["footnotes"]] <- as.list(footnotes)
	table[["citation"]] <- .contTablesBayesianCitations()
	
	if (complete)
		table[["status"]] <- "complete"
	
	if (status$error) {
		table[["status"]] <- "complete"
		table[["error"]] <- list(errorType="badData")
	}
	
	if (populate == FALSE && is.null(state))
		new.state <- NULL

	list(table=table, state=new.state, complete=complete, status=status)
}

.contTablesBayesianCreateOddsRatioTable <- function(dataset, analysis, counts.matrices, groups, footnotes, options, populate, bf.results, state, state.options, status) {

	if (options$oddsRatio == FALSE && options$plotPosteriorOddsRatio == FALSE)
		return(list(table=NULL, state=NULL, complete=TRUE))

	if (is.null(state.options) ||
		base::identical(state.options$rows, options$rows) == FALSE ||
		base::identical(state.options$columns, options$columns) == FALSE ||
		base::identical(state.options$counts, options$counts) == FALSE ||
		base::identical(state.options$layers, options$layers) == FALSE ||
		base::identical(state.options$columnOrder, options$columnOrder) == FALSE ||
		base::identical(state.options$rowOrder, options$rowOrder) == FALSE ||
		base::identical(state.options$samplingModel, options$samplingModel) == FALSE ||
		base::identical(state.options$hypothesis, options$hypothesis) == FALSE ||
		base::identical(state.options$priorConcentration, options$priorConcentration) == FALSE ||
		base::identical(state.options$oddsRatioCredibleIntervalInterval, options$oddsRatioCredibleIntervalInterval) == FALSE ||
		base::identical(state.options$plotPosteriorOddsRatioAdditionalInfo, options$plotPosteriorOddsRatioAdditionalInfo) == FALSE) {
		
		state <- NULL
	}
	
	if (status$error)
		populate <- FALSE

	table <- list()
	
	table[["title"]] <- "Log Odds Ratio"
	
	fields <- list()
	
	if (length(analysis) >= 3) {
	
		for (j in length(analysis):3)
			fields[[length(fields)+1]] <- list(name=analysis[[j]], type="string", combine=TRUE)
	}

	ci.label <- paste(100 * options$oddsRatioCredibleIntervalInterval, "% Credible Interval", sep="")
		
	fields[[length(fields)+1]] <- list(name="value[oddsRatio]", title="Log Odds Ratio", type="number", format="sf:4;dp:3")
	fields[[length(fields)+1]] <- list(name="low[oddsRatio]", title="Lower", overTitle=ci.label, type="number", format="dp:3")
	fields[[length(fields)+1]] <- list(name="up[oddsRatio]",  title="Upper", overTitle=ci.label, type="number", format="dp:3")
	
	schema <- list(fields=fields)
	
	table[["schema"]] <- schema

	rows  <- list()
	new.state <- list()
	footnotes <- .newFootnotes()
	
	next.is.new.group <- TRUE
	complete <- TRUE
	
	for (i in 1:length(counts.matrices)) {
	
		counts.matrix <- counts.matrices[[i]]
		
		if ( ! is.null(groups)) {
		
			group <- groups[[i]]
			
		} else {
		
			group <- NULL
		}
		
		rows.state <- NULL
		if (i <= length(state))
			rows.state <- state[[i]]
		
		bf.result <- NULL
		if (i <= length(bf.results))
			bf.result <- bf.results[[i]]

		res <- .contTablesBayesianCreateOddsRatioRows(analysis$rows, counts.matrix, footnotes, options, populate, group, bf.result, rows.state, state.options, status)

		next.rows <- res$rows
		complete <- complete && res$complete

		if (next.is.new.group) {
		
			next.rows[[1]][[".isNewGroup"]] <- TRUE
			next.is.new.group <- FALSE
			
		} else if ( ! is.null(group) && group[[length(group)]] == "") {
		
			next.rows[[1]][[".isNewGroup"]] <- TRUE
			next.is.new.group <- TRUE
		}
		
		rows <- c(rows, next.rows)
		
		new.state[[length(new.state)+1]] <- res$state
	}
	
	complete <- complete && (populate || is.null(state) == FALSE || length(options$rows) == 0 || length(options$columns) == 0)
	
	table[["data"]] <- rows
	table[["footnotes"]] <- as.list(footnotes)
	table[["citation"]] <- .contTablesBayesianCitations()
	
	if (complete)
		table[["status"]] <- "complete"
	
	if (status$error) {
	
		table[["status"]] <- "complete"
		table[["error"]] <- list(errorType="badData")
	}
	
	if (options$oddsRatio == FALSE)
		table <- NULL

	list(table=table, state=new.state, complete=complete, status=status)
}


.contTablesBayesianCreateCramerVTable <- function(dataset, analysis, counts.matrices, groups, footnotes, options, populate, bf.results, state, state.options, status) {

	if (options$effectSize == FALSE)
		return(list(table=NULL, state=NULL, complete=TRUE))

	if (is.null(state.options) ||
		base::identical(state.options$rows, options$rows) == FALSE ||
		base::identical(state.options$columns, options$columns) == FALSE ||
		base::identical(state.options$counts, options$counts) == FALSE ||
		base::identical(state.options$layers, options$layers) == FALSE ||
		base::identical(state.options$columnOrder, options$columnOrder) == FALSE ||
		base::identical(state.options$rowOrder, options$rowOrder) == FALSE ||
		base::identical(state.options$samplingModel, options$samplingModel) == FALSE ||
		base::identical(state.options$priorConcentration, options$priorConcentration) == FALSE ||
		base::identical(state.options$effectSizeCredibleIntervalInterval, options$effectSizeCredibleIntervalInterval) == FALSE ||
		base::identical(state.options$hypothesis, options$hypothesis) == FALSE) {
		
		state <- NULL
	}
		
	if (status$error)
		populate <- FALSE

	table <- list()
	
	table[["title"]] <- "Cramer's V"
	
	fields <- list()
	
	if (length(analysis) >= 3) {
	
		for (j in length(analysis):3)
			fields[[length(fields)+1]] <- list(name=analysis[[j]], type="string", combine=TRUE)
	}

	ci.label <- paste(100 * options$effectSizeCredibleIntervalInterval, "% Credible Interval", sep="")
		
	fields[[length(fields)+1]] <- list(name="value[CramerV]", title="Cramer's V", type="number", format="sf:4;dp:3")
	fields[[length(fields)+1]] <- list(name="low[CramerV]", title="Lower", overTitle=ci.label, type="number", format="dp:3")
	fields[[length(fields)+1]] <- list(name="up[CramerV]",  title="Upper", overTitle=ci.label, type="number", format="dp:3")
	
	schema <- list(fields=fields)
	
	table[["schema"]] <- schema



	rows  <- list()
	new.state <- list()
	footnotes <- .newFootnotes()
	
	next.is.new.group <- TRUE
	complete <- TRUE
	
	for (i in 1:length(counts.matrices)) {
	
		counts.matrix <- counts.matrices[[i]]
		
		if ( ! is.null(groups)) {
		
			group <- groups[[i]]
			
		} else {
		
			group <- NULL
		}
		
		rows.state <- NULL
		if (i <= length(state))
			rows.state <- state[[i]]
		
		bf.result <- NULL
		if (i <= length(bf.results))
			bf.result <- bf.results[[i]]

		res <- .contTablesBayesianCreateCramerVRows(analysis$rows, counts.matrix, footnotes, options, populate, group, bf.result, rows.state, state.options, status)

		next.rows <- res$rows
		complete <- complete && res$complete

		if (next.is.new.group) {
		
			next.rows[[1]][[".isNewGroup"]] <- TRUE
			next.is.new.group <- FALSE
			
		} else if ( ! is.null(group) && group[[length(group)]] == "") {
		
			next.rows[[1]][[".isNewGroup"]] <- TRUE
			next.is.new.group <- TRUE
		}
		
		rows <- c(rows, next.rows)
		
		new.state[[length(new.state)+1]] <- res$state
	}
	
	complete <- complete && (populate || is.null(state) == FALSE || length(options$rows) == 0 || length(options$columns) == 0)
	
	table[["data"]] <- rows
	table[["footnotes"]] <- as.list(footnotes)
	table[["citation"]] <- .contTablesBayesianCitations()
	
	if (complete)
		table[["status"]] <- "complete"
	
	if (status$error) {
	
		table[["status"]] <- "complete"
		table[["error"]] <- list(errorType="badData")
	}
	
	if (options$effectSize == FALSE)
		table <- NULL

	list(table=table, state=new.state, complete=complete, status=status)
}

.contTablesBayesianCreateTestsRows <- function(var.name, counts.matrix, footnotes, options, populate, group, state) {

	row <- list()
	
	new.state <- list()
	
	for (layer in names(group)) {
	
		level <- group[[layer]]
		
		if (level == "") {

			row[[layer]] <- "Total"
						
		} else {
		
			row[[layer]] <- level
		}
	}
		
	if (options$samplingModel == "poisson") {
	
		if (options$hypothesis=="groupsNotEqual") {
		
			if (options$bayesFactorType == "BF10"){
				bfLabel <- "BF\u2081\u2080 Poisson"
			} else if (options$bayesFactorType == "BF01") {
				bfLabel <- "BF\u2080\u2081 Poisson"
			} else if (options$bayesFactorType == "LogBF10") {
				bfLabel <-	"Log\u2009(\u2009BF\u2081\u2080\u2009) Poisson"
			}
				
		} else if (options$hypothesis=="groupOneGreater") {
			
			if (options$bayesFactorType == "BF10"){
				bfLabel <- "BF\u208A\u2080 Poisson"
			} else if (options$bayesFactorType == "BF01"){
				bfLabel <- "BF\u2080\u208A Poisson"
			} else if (options$bayesFactorType == "LogBF10") {
				bfLabel <-	"Log\u2009(\u2009BF\u2081\u2080\u2009) Poisson"
			}
						 
		} else if(options$hypothesis =="groupTwoGreater") { 
		
			if (options$bayesFactorType == "BF10"){
				bfLabel <- "BF\u208B\u2080 Poisson"
			} else if (options$bayesFactorType == "BF01"){
				bfLabel <- "BF\u2080\u208B Poisson"
			} else if (options$bayesFactorType == "LogBF10") {
				bfLabel <-"Log\u2009(\u2009BF\u2081\u2080\u2009) Poisson"
			}				
		}
		
		sampleType <- "poisson"
		fixedMargin <- NULL
		
	} else if (options$samplingModel == "jointMultinomial") {

		if (options$hypothesis=="groupsNotEqual") {
		
			if (options$bayesFactorType == "BF10"){
				bfLabel <- "BF\u2081\u2080 joint multinomial"
			} else if (options$bayesFactorType == "BF01") {
				bfLabel <- "BF\u2080\u2081 joint multinomial"
			} else if (options$bayesFactorType == "LogBF10") {
				bfLabel <-	"Log\u2009(\u2009BF\u2081\u2080\u2009) joint multinomial"
			}
				
		} else if (options$hypothesis=="groupOneGreater") {
			
			if (options$bayesFactorType == "BF10"){
				bfLabel <- "BF\u208A\u2080 joint multinomial"
			} else if (options$bayesFactorType == "BF01"){
				bfLabel <- "BF\u2080\u208A joint multinomial"
			} else if (options$bayesFactorType == "LogBF10") {
				bfLabel <-	"Log\u2009(\u2009BF\u208A\u2080\u2009) joint multinomial"
			}
						 
		} else if(options$hypothesis =="groupTwoGreater") { 
		
			if (options$bayesFactorType == "BF10"){
				bfLabel <- "BF\u208B\u2080 joint multinomial"
			} else if (options$bayesFactorType == "BF01"){
				bfLabel <- "BF\u2080\u208B joint multinomial"
			} else if (options$bayesFactorType == "LogBF10") {
				bfLabel <-"Log\u2009(\u2009BF\u208B\u2080\u2009) joint multinomial"
			}				
		}
	
		sampleType <- "jointMulti"
		fixedMargin <- NULL
		
	} else if (options$samplingModel =="independentMultinomialRowsFixed") {

		if (options$hypothesis=="groupsNotEqual") {
		
			if (options$bayesFactorType == "BF10"){
				bfLabel <- "BF\u2081\u2080 independent multinomial"
			} else if (options$bayesFactorType == "BF01") {
				bfLabel <- "BF\u2080\u2081 independent multinomial"
			} else if (options$bayesFactorType == "LogBF10") {
				bfLabel <-	"Log\u2009(\u2009BF\u2081\u2080\u2009) independent multinomial"
			}
				
		} else if (options$hypothesis=="groupOneGreater") {
			
			if (options$bayesFactorType == "BF10"){
				bfLabel <- "BF\u208A\u2080 independent multinomial"
			} else if (options$bayesFactorType == "BF01"){
				bfLabel <- "BF\u2080\u208A independent multinomial"
			} else if (options$bayesFactorType == "LogBF10") {
				bfLabel <-	"Log\u2009(\u2009BF\u208A\u2080\u2009) independent multinomial"
			}
						 
		} else if(options$hypothesis =="groupTwoGreater") { 
		
			if (options$bayesFactorType == "BF10"){
				bfLabel <- "BF\u208B\u2080 independent multinomial"
			} else if (options$bayesFactorType == "BF01"){
				bfLabel <- "BF\u2080\u208B independent multinomial"
			} else if (options$bayesFactorType == "LogBF10") {
				bfLabel <-"Log\u2009(\u2009BF\u208B\u2080\u2009) independent multinomial"
			}				
		}
				
		sampleType <- "indepMulti"
		fixedMargin <- "rows"
		
	} else if (options$samplingModel=="independentMultinomialColumnsFixed") {
	
		if (options$hypothesis=="groupsNotEqual") {
		
			if (options$bayesFactorType == "BF10"){
				bfLabel <- "BF\u2081\u2080 independent multinomial"
			} else if (options$bayesFactorType == "BF01"){
				bfLabel <- "BF\u2080\u2081 independent multinomial"
			} else if (options$bayesFactorType == "LogBF10") {
				bfLabel <-"Log\u2009(\u2009BF\u2081\u2080\u2009) independent multinomial"
			}
				
		} else if(options$hypothesis=="groupOneGreater") {
			
			if (options$bayesFactorType == "BF10"){
				bfLabel <- "BF\u208A\u2080 independent multinomial"
			} else if (options$bayesFactorType == "BF01"){
				bfLabel <- "BF\u2080\u208A independent multinomial"
			} else if (options$bayesFactorType == "LogBF10") {
				bfLabel <-"Log\u2009(\u2009BF\u208A\u2080\u2009) independent multinomial"
			}
							 
		} else if (options$hypothesis=="groupTwoGreater"){
			
			if (options$bayesFactorType == "BF10"){
				bfLabel <- "BF\u208B\u2080 independent multinomial"
			
			} else if (options$bayesFactorType == "BF01"){
				bfLabel <- "BF\u2080\u208B independent multinomial"
				
			} else if (options$bayesFactorType == "LogBF10") {
				bfLabel <-"Log\u2009(\u2009BF\u208B\u2080\u2009) independent multinomial"
			}				
		}
			
		sampleType <- "indepMulti"
		fixedMargin <- "cols"
		
	} else if (options$samplingModel=="hypergeometric") {

		if (options$bayesFactorType == "BF10"){
			bfLabel <- "BF\u2081\u2080 hypergeometric"
		} else if (options$bayesFactorType == "BF01"){
			bfLabel <- "BF\u2080\u2081 hypergeometric"
		} else if (options$bayesFactorType == "LogBF10") {
			bfLabel <-"Log\u2009(\u2009BF\u2081\u2080\u2009) hypergeometric"
		}
		
		sampleType <- "hypergeom"
		fixedMargin <- NULL
		
	} else {
	
		stop("wtf?")
	}
	
	results <- state
	if (is.null(results))
		results <- list(N=NULL, BF=NULL)
	
	if (populate && is.null(results$N))
		results$N = base::sum(counts.matrix)

	if (populate && is.null(results$BF)) {
	
		results$BF <- try({
		
			BF <- BayesFactor::contingencyTableBF(counts.matrix, sampleType=sampleType, priorConcentration=options$priorConcentration, fixedMargin=fixedMargin)
			bf1 <- exp(as.numeric(BF@bayesFactor$bf))
			lbf1 <- as.numeric(BF@bayesFactor$bf)
			
			if (options$samplingModel=="hypergeometric"){
			
				list(BF=BF, BF10=bf1, LogBF10=lbf1)		
			
			} else {
				
				ch.result = BayesFactor::posterior(BF, iterations = 10000)
			
				if (options$hypothesis=="groupOneGreater" && options$samplingModel=="poisson") {
										
					#ch.result = BayesFactor::posterior(BF, iterations = 10000)
					theta <- as.data.frame(ch.result)
				
					odds.ratio<-(theta[,1]*theta[,4])/(theta[,2]*theta[,3])
					logOR<-log(odds.ratio)
					prop.consistent <- 1-mean(logOR < 0)
					bf1 <- bf1 * prop.consistent / 0.5
					lbf1 <- lbf1 + log(prop.consistent) - log(0.5)
				
				} else if (options$hypothesis=="groupTwoGreater"  && options$samplingModel=="poisson") {
			
					#ch.result = BayesFactor::posterior(BF, iterations = 10000)
					theta <- as.data.frame(ch.result)
				
					odds.ratio<-(theta[,1]*theta[,4])/(theta[,2]*theta[,3])
					logOR<-log(odds.ratio)
					prop.consistent <- mean(logOR < 0)
					bf1 <- bf1 * prop.consistent / 0.5
					lbf1 <- lbf1 + log(prop.consistent) - log(0.5)
			
				} else if (options$hypothesis=="groupOneGreater" && options$samplingModel=="jointMultinomial") {
										
					#ch.result = BayesFactor::posterior(BF, iterations = 10000)
					theta <- as.data.frame(ch.result)
				
					odds.ratio<-(theta[,1]*theta[,4])/(theta[,2]*theta[,3])
					logOR<-log(odds.ratio)
					prop.consistent <- 1-mean(logOR < 0)
					bf1 <- bf1 * prop.consistent / 0.5
					lbf1 <- lbf1 + log(prop.consistent) - log(0.5)
				
				} else if (options$hypothesis=="groupTwoGreater"  && options$samplingModel=="jointMultinomial") {
			
					#ch.result = BayesFactor::posterior(BF, iterations = 10000)
					theta <- as.data.frame(ch.result)
				
					odds.ratio<-(theta[,1]*theta[,4])/(theta[,2]*theta[,3])
					logOR<-log(odds.ratio)
					prop.consistent <- mean(logOR < 0)
					bf1 <- bf1 * prop.consistent / 0.5
					lbf1 <- lbf1 + log(prop.consistent) - log(0.5)
			
				} else if (options$hypothesis=="groupOneGreater" && options$samplingModel=="independentMultinomialColumnsFixed") {
										
					#ch.result = BayesFactor::posterior(BF, iterations = 10000)
					theta <- as.data.frame(ch.result[,7:10])
					prop.consistent <- mean(theta[,1] > theta[,3])  #sum(p1.sim > p2.sim)/N.sim
					bf1 <- bf1 * prop.consistent / 0.5
					lbf1 <- lbf1 + log(prop.consistent) - log(0.5)
			
				} else if (options$hypothesis=="groupOneGreater" && options$samplingModel=="independentMultinomialRowsFixed") {
								
					#ch.result = BayesFactor::posterior(BF, iterations = 10000)
					theta <- as.data.frame(ch.result[,7:10])
					prop.consistent <- mean(theta[,1] > theta[,2]) 
					bf1 <- bf1 * prop.consistent / 0.5
					lbf1 <- lbf1 + log(prop.consistent) - log(0.5)
			
				} else if (options$hypothesis=="groupTwoGreater"  && options$samplingModel=="independentMultinomialColumnsFixed") {
				
					#ch.result = BayesFactor::posterior(BF, iterations = 10000)
					theta <- as.data.frame(ch.result[,7:10])
					prop.consistent <- mean(theta[,3] > theta[,1])
					bf1 <- bf1 * prop.consistent / 0.5
					lbf1 <- lbf1 + log(prop.consistent) - log(0.5)
				
				} else if (options$hypothesis=="groupTwoGreater"  && options$samplingModel=="independentMultinomialRowsFixed"){
								
					#ch.result = BayesFactor::posterior(BF, iterations = 10000)
					theta <- as.data.frame(ch.result[,7:10])
					prop.consistent <- mean(theta[,2] > theta[,1])
					bf1 <- bf1 * prop.consistent / 0.5
					lbf1 <- lbf1 + log(prop.consistent) - log(0.5)
				}
			
			list(BF=BF, BF10=bf1, LogBF10=lbf1, post.samples=ch.result)	}	
		})
		
		new.state <- results
		
	} else {
	
		new.state <- state
	}
		
	row[["type[BF]"]] <- bfLabel
	row[["type[N]"]] <- "N"	
						
	if (is.null(results$BF)) {
	
		row[["value[BF]"]] <- "."
		row[["value[N]"]]  <- "."

	} else if (inherits(results$BF, "try-error")) {

		row[["value[N]"]]  <- results$N
		
		if ( ! identical(dim(counts.matrix),as.integer(c(2,2))) && options$samplingModel=="hypergeometric") {
		
			row[["value[BF]"]] <- .clean(NaN)
		
			sup <- .addFootnote(footnotes, "Hypergeometric contingency tables test restricted to 2 x 2 tables")
			row[[".footnotes"]] <- list("value[BF]"=list(sup))	
		
		} else {
		
			error <- .extractErrorMessage(results$BF)
		
			sup   <- .addFootnote(footnotes, error)
			row[[".footnotes"]] <- list("value[BF]"=list(sup))
		}
	
	} else if ( ! identical(dim(counts.matrix),as.integer(c(2,2))) && options$hypothesis=="groupOneGreater") {
		
		row[["value[BF]"]] <- .clean(NaN)
		row[["value[N]"]]  <- results$N
		
		sup <- .addFootnote(footnotes, "Proportion test restricted to 2 x 2 tables")
		row[[".footnotes"]] <- list("value[BF]"=list(sup))
	
	} else if ( ! identical(dim(counts.matrix),as.integer(c(2,2))) && options$hypothesis=="groupTwoGreater") {
		
		row[["value[BF]"]] <- .clean(NaN)
		row[["value[N]"]]  <- results$N
		
		sup <- .addFootnote(footnotes, "Proportion test restricted to 2 x 2 tables")
		row[[".footnotes"]] <- list("value[BF]"=list(sup))
	
	} else {

		if (options$hypothesis=="groupOneGreater" && options$samplingModel=="poisson"){

			gp1 <- rownames(counts.matrix)[1]
			gp2 <- rownames(counts.matrix)[2]

			message <- paste("For all tests, the alternative hypothesis specifies that group <em>", gp1, "</em> is greater than group <em>", gp2, ".</em>", sep="")
			.addFootnote(footnotes, symbol="<em>Note.</em>", text=message)

		} else if (options$hypothesis=="groupTwoGreater"  && options$samplingModel=="poisson"){

			gp1 <- rownames(counts.matrix)[1]
			gp2 <- rownames(counts.matrix)[2]

			message <- paste("For all tests, the alternative hypothesis specifies that group <em>", gp1, "</em> is less than group <em>", gp2, ".</em>", sep="")
			.addFootnote(footnotes, symbol="<em>Note.</em>", text=message)			

		} else if (options$hypothesis=="groupOneGreater" && options$samplingModel=="jointMultinomial"){

			gp1 <- rownames(counts.matrix)[1]
			gp2 <- rownames(counts.matrix)[2]

			message <- paste("For all tests, the alternative hypothesis specifies that group <em>", gp1, "</em> is greater than group <em>", gp2, ".</em>", sep="")
			.addFootnote(footnotes, symbol="<em>Note.</em>", text=message)

		} else if (options$hypothesis=="groupTwoGreater"  && options$samplingModel=="jointMultinomial"){

			gp1 <- rownames(counts.matrix)[1]
			gp2 <- rownames(counts.matrix)[2]

			message <- paste("For all tests, the alternative hypothesis specifies that group <em>", gp1, "</em> is less than group <em>", gp2, ".</em>", sep="")
			.addFootnote(footnotes, symbol="<em>Note.</em>", text=message)			

		} else if (options$hypothesis=="groupOneGreater" && options$samplingModel=="independentMultinomialRowsFixed"){

			gp1 <- rownames(counts.matrix)[1]
			gp2 <- rownames(counts.matrix)[2]

			message <- paste("For all tests, the alternative hypothesis specifies that group <em>", gp1, "</em> is greater than group <em>", gp2, ".</em>", sep="")
			.addFootnote(footnotes, symbol="<em>Note.</em>", text=message)

		} else if (options$hypothesis=="groupTwoGreater"  && options$samplingModel=="independentMultinomialRowsFixed"){

			gp1 <- rownames(counts.matrix)[1]
			gp2 <- rownames(counts.matrix)[2]

			message <- paste("For all tests, the alternative hypothesis specifies that group <em>", gp1, "</em> is less than group <em>", gp2, ".</em>", sep="")
			.addFootnote(footnotes, symbol="<em>Note.</em>", text=message)			

		} else if (options$hypothesis=="groupOneGreater" && options$samplingModel=="independentMultinomialColumnsFixed") {

			gp1 <- colnames(counts.matrix)[1]
			gp2 <- colnames(counts.matrix)[2]

			message <- paste("For all tests, the alternative hypothesis specifies that group <em>", gp1, "</em> is greater than group <em>", gp2, ".</em>", sep="")
			.addFootnote(footnotes, symbol="<em>Note.</em>", text=message)

		} else if (options$hypothesis=="groupTwoGreater"  && options$samplingModel=="independentMultinomialColumnsFixed") {

			gp1 <- colnames(counts.matrix)[1]
			gp2 <- colnames(counts.matrix)[2]

			message <- paste("For all tests, the alternative hypothesis specifies that group <em>", gp1, "</em> is less than group <em>", gp2, ".</em>", sep="")
			.addFootnote(footnotes, symbol="<em>Note.</em>", text=message)
		}
		
		row[["value[N]"]] <- results$N
		
		if (options$bayesFactorType == "BF10") {
		
			row[["value[BF]"]] <- .clean(results$BF$BF10)
		
		} else if (options$bayesFactorType == "BF01") {
		
			row[["value[BF]"]] <- .clean(1/results$BF$BF10)
			
		} else if (options$bayesFactorType == "LogBF10") {
		
			row[["value[BF]"]] <- .clean(results$BF$LogBF10)
		}
	}


	list(rows=list(row), state=new.state)
}

.contTablesCreateGroupMatrices <- function(dataset, rows, columns, groups, counts=NULL, rowOrderDescending=FALSE, columnOrderDescending=FALSE, populate) {

	# this creates count matrices for each of the groups

	matrices <- list()

	if (is.null(groups)) {

		if (populate == FALSE) {
		
			row.levels <- c(".", ". ")
			col.levels <- c(".", ". ")
			
			if (rows != "")
				row.levels <- base::levels(dataset[[ .v(rows) ]])
			if (columns != "")
				col.levels <- base::levels(dataset[[ .v(columns) ]])
			
			ss.matrix <- base::matrix(0, nrow=length(row.levels), ncol=length(col.levels), dimnames=list(row.levels, col.levels))

		} else if (is.null(counts)) {

			ss.dataset <- base::subset(dataset, select=.v(c(rows, columns)))
			ss.table   <- base::table(ss.dataset)
			ss.matrix  <- base::matrix(ss.table, nrow=dim(ss.table)[1], ncol=dim(ss.table)[2], dimnames=dimnames(ss.table))
			
		} else {
		
			ss.dataset <- base::subset(dataset, select=.v(c(rows, columns, counts)))
			ss.matrix  <- base::tapply(ss.dataset[[ .v(counts) ]], list(ss.dataset[[ .v(rows) ]], ss.dataset[[ .v(columns) ]]), base::sum)
			ss.matrix[is.na(ss.matrix)] <- 0
		}
		
		if (rowOrderDescending) {
			ss.matrix <- base::apply(ss.matrix, 2, base::rev)
		} else {
			ss.matrix <- ss.matrix
		}
		
		if (columnOrderDescending) {
			ss.matrix <- ss.matrix[ , ncol(ss.matrix):1]
		} else {
			ss.matrix <- ss.matrix
		}
		
		ss.matrix[base::is.na(ss.matrix)] <- 0
		
		matrices[[1]] <- ss.matrix
	
	} else {
		
		for (group in groups) {
		
			group <- group[group != ""]

			if (populate == FALSE) {
			
				ss.dataset <- dataset

			} else if (length(group) == 0) {
			
				ss.dataset <- base::subset(dataset, select=.v(c(rows, columns, counts)))
			
			} else if (populate) {

				ss.filter.string <- base::paste(.v(names(group)), "==\"", group, "\"", sep="", collapse="&")
				ss.expression <- base::parse(text=ss.filter.string)
				ss.dataset	  <- base::subset(dataset, select=.v(c(rows, columns, counts)), subset=eval(ss.expression))
			}
			
			if (populate == FALSE) {
		
				row.levels <- c(".", ". ")
				col.levels <- c(".", ". ")
			
				if (rows != "")
					row.levels <- base::levels(dataset[[ .v(rows) ]])
				if (columns != "")
					col.levels <- base::levels(dataset[[ .v(columns) ]])
			
				ss.matrix <- base::matrix(0, nrow=length(row.levels), ncol=length(col.levels), dimnames=list(row.levels, col.levels))

			} else if (is.null(counts)) {

				ss.table  <- base::table(ss.dataset)
				ss.matrix <- base::matrix(ss.table, nrow=dim(ss.table)[1], ncol=dim(ss.table)[2], dimnames=dimnames(ss.table))
			
			} else {
		
				ss.matrix <- base::tapply(ss.dataset[[ .v(counts) ]], list(ss.dataset[[ .v(rows) ]], ss.dataset[[ .v(columns) ]]), base::sum)
			}
			
			ss.matrix[base::is.na(ss.matrix)] <- 0
			
			if (rowOrderDescending) {
				ss.matrix <- base::apply(ss.matrix, 2, base::rev)
			} else {
				ss.matrix <- ss.matrix
			}
			
			if (columnOrderDescending) {
				ss.matrix <- ss.matrix[ , ncol(ss.matrix):1]
			} else {
				ss.matrix <- ss.matrix
			}

			matrices[[length(matrices)+1]] <- ss.matrix
		}
	}
	
	matrices
}

.contTableBayesianCreateCountsRows <- function(var.name, counts.matrix, options, populate, group, last.rows) {

	rows <- list()
	row.expected <- list()
	row.row.proportions <- list()
	row.col.proportions <- list()
	row.proportions <- list()
	
	if (populate) {
	
		expected.matrix <- try({

			stats::chisq.test(counts.matrix, correct=FALSE)$expected
		})

		if (inherits(expected.matrix, "try-error")) {

			expected.matrix <- counts.matrix
			expected.matrix[,] <- "&nbsp;"
		}
		
		row.proportions.matrix <- try({

			base::prop.table(counts.matrix, 1)
		})

		if (inherits(row.proportions.matrix, "try-error")) {

			row.proportions.matrix <- counts.matrix
			row.proportions.matrix[,] <- "&nbsp;"
		}
		
		col.proportions.matrix <- try({

			base::prop.table(counts.matrix, 2)
		})

		if (inherits(col.proportions.matrix, "try-error")) {

			col.proportions.matrix <- counts.matrix
			col.proportions.matrix[,] <- "&nbsp;"
		}
		
		proportions.matrix <- try({

			base::prop.table(counts.matrix, margin = NULL)
		})

		if (inherits(proportions.matrix, "try-error")) {

			proportions.matrix <- counts.matrix
			proportions.matrix[,] <- "&nbsp;"
		}
		
	} else {
	
		expected.matrix <- counts.matrix
		row.proportions.matrix <- counts.matrix
		col.proportions.matrix <- counts.matrix
		proportions.matrix <- counts.matrix
	}

	for (i in 1:dim(counts.matrix)[[1]]) {
	
		row <- list()
		if (is.list(last.rows) && i <= length(last.rows))
			row <- last.rows[[i]]

		if (populate) {

			counts <- as.list(counts.matrix[i,])
			names(counts) <- base::paste(names(counts),"[counts]", sep="")
			counts[["type[counts]"]] <- "Count"
			counts[["total[counts]"]] <- base::sum(counts.matrix[i,])
			
			row[names(counts)] <- counts
		}
		
		expected <- as.list(expected.matrix[i,])
		expected.names <- paste(names(expected),"[expected]",  sep="")
		expected.names <- c(expected.names, "type[expected]", "total[expected]")

		if (options$countsExpected) {
		
			expected[[length(expected)+1]] <- "Expected Count"
			
			if (class(expected.matrix[1,1]) == "character") {
				expected[[length(expected)+1]] <- ""
			} else {
				expected[[length(expected)+1]] <- base::sum(expected.matrix[i,])
			}
			
			names(expected) <- paste(expected.names)
			
			row <- c(row, expected)
		
		} else {
		
			# remove the expected counts cells
			row <- row[(names(row) %in% expected.names) == FALSE]
		}
		
		if (options$percentagesRow) {
		
			row.row.proportions[["type[row.proportions]"]] <- " % within row"

			row.proportions <- as.list(row.proportions.matrix[i,])
			row.proportions <- .clean(row.proportions)
			names(row.proportions) <- paste(names(row.proportions),"[row.proportions]",  sep="")
			
			if (class(row.proportions.matrix[1,1]) == "character") {
				row.proportions[["total[row.proportions]"]] <- ""
			} else {
				row.proportions[["total[row.proportions]"]] <- .clean(base::sum(row.proportions.matrix[i,]))
			}
		
			row.proportions <- c(row.row.proportions, row.proportions)
			row <- c(row, row.proportions)
		}
		
		if (options$percentagesColumn) {
		
			row.col.proportions[["type[col.proportions]"]] <- " % within column"

			col.proportions <- as.list(col.proportions.matrix[i,])
			col.proportions <- .clean(col.proportions)
			names(col.proportions) <- paste(names(col.proportions),"[col.proportions]",  sep="")
			
			if (class(col.proportions.matrix[1,1]) == "character") {
				col.proportions[["total[col.proportions]"]] <- ""
			} else {
				
				row.sum <- base::margin.table(counts.matrix, 1)
				row.prop <- as.list( base::prop.table(row.sum))
				row.prop <- .clean(row.prop)
				col.proportions[["total[col.proportions]"]] <- row.prop[[i]]
			}
		
			col.proportions <- c(row.col.proportions, col.proportions)
			row <- c(row, col.proportions)
		}
		
		if (options$percentagesTotal) {
		
			row.proportions[["type[proportions]"]] <- " % of Total"

			proportions <- as.list(proportions.matrix[i,])
			proportions <- .clean(proportions)
			names(proportions) <- paste(names(proportions),"[proportions]",  sep="")
			
			if (class(proportions.matrix[1,1]) == "character") {
				proportions[["total[proportions]"]] <- ""
			} else {
				proportions[["total[proportions]"]] <- .clean(base::sum(proportions.matrix[i,]))
			}
		
			proportions <- c(row.proportions, proportions)
			row <- c(row, proportions)
		}
		
		row[[var.name]] <- dimnames(counts.matrix)[[1]][i]
		
		for (layer in names(group)) {
		
			level <- group[[layer]]
			
			if (level == "") {

				row[[layer]] <- "Total"
							
			} else {
			
				row[[layer]] <- level
			}
		}

		if (i == 1 && options$countsExpected == FALSE && options$percentagesRow == FALSE && options$percentagesCol == FALSE && options$percentagesTotal == FALSE) {

			row[[".isNewGroup"]] <- TRUE
		}
		
		rows[[length(rows)+1]] <- row
	}
	
	
	row <- list()
	if (is.list(last.rows) && length(last.rows) > 0)
		row <- last.rows[[length(last.rows)]]

	if (populate) {

		counts <- as.list(apply(counts.matrix, 2, base::sum))
		names(counts) <- base::paste(names(counts),"[counts]", sep="")
		counts[["type[counts]"]] <- "Count"
		counts[["total[counts]"]] <- base::sum(counts.matrix)

		row[names(counts)] <- counts
		
		if (options$countsExpected) {
		
			if (class(expected.matrix[1,1]) == "character") {
				expected <- expected.matrix[1,]
			} else {
				expected <- apply(expected.matrix, 2, base::sum)
			}

			expected <- as.list(expected)
			names(expected) <- paste(names(expected),"[expected]", sep="")

			if (class(expected.matrix[1,1]) == "character") {
				expected[["total[expected]"]] <- ""
			} else {
				expected[["total[expected]"]] <- base::sum(expected.matrix)
			}
			
			expected <- c(row.expected, expected)
			
			row <- c(row,  expected)
		}
		
		if (options$percentagesRow) {
		
			if (class(row.proportions.matrix[1,1]) == "character") {
				row.proportions <- row.proportions.matrix[1,]
			} else {
				m <- base::margin.table(counts.matrix, 2)
				rowproportion <- base::prop.table(m)
			}

			row.proportions <- as.list(rowproportion)
			row.proportions <- .clean(row.proportions)
			names(row.proportions) <- paste(names(row.proportions),"[row.proportions]", sep="")

			if (class(row.proportions.matrix[1,1]) == "character") {
				row.proportions[["total[row.proportions]"]] <- ""
			} else {
				row.proportions[["total[row.proportions]"]] <- .clean(base::sum(rowproportion))
			}
			
			row.proportions <- c(row.row.proportions, row.proportions)
			
			row <- c(row,  row.proportions)
		}
		
		if (options$percentagesColumn) {
		
			if (class(col.proportions.matrix[1,1]) == "character") {
				col.proportions <- col.proportions.matrix[1,]
			} else {
				colproportion <- apply(col.proportions.matrix, 2, base::sum)
			}

			col.proportions <- as.list(colproportion)
			col.proportions <- .clean(col.proportions)
			names(col.proportions) <- paste(names(col.proportions),"[col.proportions]", sep="")

			if (class(row.proportions.matrix[1,1]) == "character") {
				col.proportions[["total[col.proportions]"]] <- ""
			} else {
				row.sum <- base::margin.table(counts.matrix, 1)
				row.prop <- base::prop.table(row.sum) 
				col.proportions[["total[col.proportions]"]] <- .clean(base::sum(row.prop))
			}
			
			col.proportions <- c(row.col.proportions, col.proportions)
			
			row <- c(row,  col.proportions)
		}
		
		
		if (options$percentagesTotal) {
		
			if (class(proportions.matrix[1,1]) == "character") {
				proportions <- proportions.matrix[1,]
			} else {
				proportions <- apply(proportions.matrix, 2, base::sum)
			}

			proportions <- as.list(proportions)
			proportions <- .clean(proportions)
			names(proportions) <- paste(names(proportions),"[proportions]", sep="")

			if (class(proportions.matrix[1,1]) == "character") {
				proportions[["total[proportions]"]] <- ""
			} else {
				proportions[["total[proportions]"]] <- .clean(base::sum(proportions.matrix))
			}
			
			proportions <- c(row.proportions, proportions)
			
			row <- c(row,  proportions)
		}
		
	}
	
	row[[var.name]] <- "Total"
	if (options$countsExpected == FALSE && options$percentagesRow == FALSE && options$percentagesCol == FALSE && options$percentagesTotal == FALSE)
		row[[".isNewGroup"]] <- TRUE
	
	for (layer in names(group)) {
	
		level <- group[[layer]]

		if (level == "") {

			row[[layer]] <- "Total"
						
		} else {
		
			row[[layer]] <- level
		}
	}

	rows[[length(rows)+1]] <- row

	rows
}

.contTablesBayesianCreateOddsRatioRows <- function(var.name, counts.matrix, footnotes, options, populate, group, bf.result, state, state.options, status) {

	row <- list()
	
	for (layer in names(group)) {
	
		level <- group[[layer]]
		
		if (level == "") {

			row[[layer]] <- "Total"
			
		} else {
		
			row[[layer]] <- level
		}
	}
	
	row[["type[oddsRatio]"]] <- "Odds ratio"
	
	result <- NULL
	complete <- TRUE
	
	if ( ! identical(dim(counts.matrix), as.integer(c(2,2)))) {

		row[["value[oddsRatio]"]] <- .clean(NaN)
		row[["low[oddsRatio]"]] <- ""
		row[["up[oddsRatio]"]] <-  ""

		sup <- .addFootnote(footnotes, "Odds ratio restricted to 2 x 2 tables")
		row[[".footnotes"]] <- list("value[oddsRatio]"=list(sup))

	} else if ( options$samplingModel == "hypergeometric") {

		row[["value[oddsRatio]"]] <- .clean(NaN)
		row[["low[oddsRatio]"]] <- ""
		row[["up[oddsRatio]"]] <-  ""

		sup <- .addFootnote(footnotes, "Odd ratio for this model not yet implemented")
		row[[".footnotes"]] <- list("value[oddsRatio]"=list(sup))
			
	} else if (is.null(state) && populate == FALSE) {
	
		row[["value[oddsRatio]"]] <- "."
		complete <- FALSE
	
	} else {
	
		if (is.null(state) == FALSE) {
		
			result <- state
		
		} else if (populate) {

			if (inherits(bf.result$BF, "try-error")) {
			
				result <- bf.result$BF
				
			} else {

				result <- try({
			
					BF <- bf.result$BF$BF
					ch.result<-bf.result$BF$post.samples

					if (options$samplingModel == "poisson") {
				
						#ch.result  <- BayesFactor::posterior(BF, iterations = 10000)
						lambda     <- as.data.frame(ch.result)
						odds.ratio.samples <- (lambda[,1]*lambda[,4])/(lambda[,2]*lambda[,3])
	
					} else if (options$samplingModel == "jointMultinomial") {
	
						#ch.result  <- BayesFactor::posterior(BF, iterations = 10000)
						theta      <- as.data.frame(ch.result)
						odds.ratio.samples <- (theta[,1]*theta[,4])/(theta[,2]*theta[,3])
		
					} else if (options$samplingModel == "independentMultinomialRowsFixed") {
	
						#ch.result  <- BayesFactor::posterior(BF, iterations = 10000)
						theta      <- as.data.frame(ch.result[,7:10])
						odds.ratio.samples <- (theta[,1]*theta[,4])/(theta[,2]*theta[,3])
		
					} else if (options$samplingModel == "independentMultinomialColumnsFixed") {
	
						#ch.result  <- BayesFactor::posterior(BF, iterations = 10000)
						theta      <- as.data.frame(ch.result[,7:10])
						odds.ratio.samples <- (theta[,1]*theta[,4])/(theta[,2]*theta[,3])
					
					} else {
				
						stop("wtf!")
					}
				
					log.odds.ratio.samples <- log(odds.ratio.samples)
					log.odds.ratio.median <- stats::median(log.odds.ratio.samples)
					sig    <- options$oddsRatioCredibleIntervalInterval
					alpha  <- (1 - sig) / 2
					lower  <- unname(stats::quantile(log.odds.ratio.samples, p = alpha))
					upper  <- unname(stats::quantile(log.odds.ratio.samples, p = (1-alpha)))
				
					list(log.odds.ratio.samples=log.odds.ratio.samples, BF=BF, median=log.odds.ratio.median, lower.ci=lower, upper.ci=upper)
				})
			}
		}
		
		if (inherits(result, "try-error")) {

			row[["value[oddsRatio]"]] <- .clean(NaN)
			error <- .extractErrorMessage(result)
			sup   <- .addFootnote(footnotes, error)
			row[[".footnotes"]] <- list("value[oddsRatio]"=list(sup))

		} else  {

			row[["value[oddsRatio]"]] <- result$median
			row[["low[oddsRatio]"]]   <- result$lower
			row[["up[oddsRatio]"]]    <- result$upper
		}
	
	}
	
	list(rows=list(row), state=result, complete=complete)
}

.contTablesBayesianCreateCramerVRows <- function(var.name, counts.matrix, footnotes, options, populate, group, bf.result, state, state.options, status) {

	row <- list()
	
	for (layer in names(group)) {
	
		level <- group[[layer]]
		
		if (level == "") {

			row[[layer]] <- "Total"
			
		} else {
		
			row[[layer]] <- level
		}
	}
	
	row[["type[CramerV]"]] <- "Cramer's V"
	
	result <- NULL
	complete <- TRUE
	
	if ( options$samplingModel == "hypergeometric") {

		row[["value[CramerV]"]] <- .clean(NaN)
		row[["low[CramerV]"]] <- ""
		row[["up[CramerV]"]] <-  ""

		sup <- .addFootnote(footnotes, "Cramer's V for this model not yet implemented")
		row[[".footnotes"]] <- list("value[CramerV]"=list(sup))
			
	} else if (is.null(state) && populate == FALSE) {
	
		row[["value[CramerV]"]] <- "."
		complete <- FALSE
	
	} else {
	
		if (is.null(state) == FALSE) {
		
			result <- state
		
		} else if (populate) {

			if (inherits(bf.result$BF, "try-error")) {
			
				result <- bf.result$BF
				
			} else {

				result <- try({
			
					BF <- bf.result$BF$BF
					ch.result<-bf.result$BF$post.samples
					d <- dim(counts.matrix)
					I <- d[1]
					J <- d[2]
					k <- min(I,J)
					N <- sum(counts.matrix)
					yr <- rowSums(counts.matrix)
					yc <- colSums(counts.matrix)

					if (options$samplingModel == "poisson") {
				
						#ch.result <- BayesFactor::posterior(BF, iterations = 10000)
						lambda <- as.data.frame(ch.result)
						theta0 <- apply(lambda,1,function(x) matrix(x,I))
						sumlambda <- apply(lambda, 1, sum)
						chi2 <- apply(theta0,2,function(data) chisq.test(matrix(data,I))$statistic)
						Phi.Poisson <- sqrt(chi2/(sumlambda*(k-1)))
						Cramer <- Phi.Poisson
	
					} else if (options$samplingModel == "jointMultinomial") {
	
						#ch.result <- BayesFactor::posterior(BF, iterations = 10000)
						theta  <- as.data.frame(ch.result)
						
						theta0 <- apply(theta, 1, function(x) matrix(x,I))
						chi2 <- apply(theta0 * N, 2, function(data) chisq.test(matrix(data,I))$statistic)
						Phi.jointMulti <- sqrt(chi2/(N*(k-1)))
						Cramer <- Phi.jointMulti
		
					} else if (options$samplingModel == "independentMultinomialRowsFixed") {
	
						#ch.result <- BayesFactor::posterior(BF, iterations = 10000)
						index <- grep(pattern="omega", x=colnames(ch.result))
						theta <- as.data.frame(ch.result[,index])
						theta0 <- apply(theta, 1, function(x) matrix(x,I))
						chi2 <- apply(theta0 * yr, 2, function(data) chisq.test(matrix(data,I))$statistic)
						Phi.indepMulti <- sqrt(chi2/(N*(k-1)))
						Cramer <- Phi.indepMulti
		
					} else if (options$samplingModel == "independentMultinomialColumnsFixed") {
	
						#ch.result <- BayesFactor::posterior(BF, iterations = 10000)
						index <- grep(pattern="omega", x=colnames(ch.result))
						theta <- as.data.frame(ch.result[,index])
						theta0 <- apply(theta, 1, function(x) matrix(x,J,byrow=TRUE))
						chi2 <- apply(theta0 * yc, 2, function(data) chisq.test(matrix(data,J))$statistic)
						Phi.indepMulti <- sqrt(chi2/(N*(k-1)))
						Cramer <- Phi.indepMulti
					
					} else {
				
						stop("wtf!")
					}
				
					CramersV.samples <-Cramer
					CramersV.median <- stats::median(CramersV.samples)
					sig <- options$effectSizeCredibleIntervalInterval
					alpha <- (1 - sig) / 2
					lower <- unname(stats::quantile(Cramer, p = alpha))
					upper <- unname(stats::quantile(Cramer, p = (1-alpha)))
				
					list(CramersV.samples=CramersV.samples, BF=BF, CVmedian=CramersV.median, CV.lower.ci=lower, CV.upper.ci=upper)
				})
			}
		}
		
		if (inherits(result, "try-error")) {

			row[["value[CramerV]"]] <- .clean(NaN)

			error <- .extractErrorMessage(result)

			sup   <- .addFootnote(footnotes, error)
			row[[".footnotes"]] <- list("value[CramerV]"=list(sup))

		} else  {

			row[["value[CramerV]"]] <- result$CVmedian
			row[["low[CramerV]"]]   <- result$CV.lower
			row[["up[CramerV]"]]    <- result$CV.upper
		}
	
	}
	
	list(rows=list(row), state=result, complete=complete)
}


.contTablesBayesianPlotPosterior <- function(
	samples,
	CI,
	medianSamples,
	BF,
	oneSided = FALSE,
	iterations = 10000,
	lwd = 2,
	cexPoints = 1.5,
	cexAxis = 1.2,
	cexYlab = 1.5,
	cexXlab = 1.5,
	cexTextBF = 1.4,
	cexCI = 1.1,
	cexLegend = 1.2,
	lwdAxis = 1.2,
	addInformation = FALSE,
	dontPlotData =FALSE,
	selectedCI = options$oddsRatioCredibleIntervalInterval,
	options) {
	
	if (addInformation) {
	
		par(mar= c(5.6, 5, 7, 4) + 0.1, las=1)
		
	} else {
	
		par(mar= c(5.6, 5, 4, 4) + 0.1, las=1)
	}
	
	if (dontPlotData) {
	
		plot(1, type='n', xlim=0:1, ylim=0:1, bty='n', axes=FALSE, xlab="", ylab="")
		
		axis(1, at=0:1, labels=FALSE, cex.axis=cexAxis, lwd=lwdAxis, xlab="")
		axis(2, at=0:1, labels=FALSE, cex.axis=cexAxis, lwd=lwdAxis, ylab="")
		
		mtext(text = "Density", side = 2, las=0, cex = cexYlab, line= 3.25)
		mtext("Log odds ratio", side = 1, cex = cexXlab, line= 2.5)
	
		return()
	}


	BF10 <- BF
	BF01 <- 1 / BF10

	# fit density estimator
	fit.posterior <-  logspline::logspline(samples)
	
	# density function posterior
	dposterior <- function(x, oneSided= oneSided, samples= samples){
	
		return(logspline::dlogspline(x, fit.posterior))
	}
	
	# set limits plot
	xlim <- vector("numeric", 2)
	
	
	if (oneSided == FALSE) {
	
		stretch <- 1.2
		xlim[1] <- quantile(samples, probs = 0.002)
		xlim[2] <- quantile(samples, probs = 0.998)
	
	} else if (oneSided == "right") {
	
		stretch <- 1
		xlim[1] <- 0
		xlim[2] <- max(2, quantile(samples, probs = 0.998))
		
	} else if (oneSided == "left") {
	
		stretch <- 1
		xlim[1] <- min(-2, quantile(samples, probs = 0.002))
		xlim[2] <- 0
	}

	xticks <- pretty(xlim)
	ylim <- vector("numeric", 2)
	
	ylim[1] <- 0
	ylim[2] <- stretch * max(.dposteriorOR(seq(min(xticks), max(xticks),length.out = 10000), mean(samples), sd(samples), oneSided= oneSided))
	
	# calculate position of "nice" tick marks and create labels
	#xticks <- pretty(xlim)
	yticks <- pretty(ylim)
	xlabels <- formatC(xticks, 1, format= "f")
	ylabels <- formatC(yticks, 1, format= "f")
	
	# compute 95% credible interval & median:
	
	CIlow <- CI[1]
	CIhigh <- CI[2]
	medianPosterior <- medianSamples
	
	z<-density(samples) 
	

	plot(0,0, xlim= range(xticks), ylim= c(0, range(yticks)[2]), ylab= "", xlab="", type= "n", axes= FALSE)
		
	lines(seq(min(xticks), max(xticks),length.out = 10000), .dposteriorOR(seq(min(xticks), max(xticks),length.out = 10000), mean(samples), sd(samples), oneSided=oneSided), lwd= lwd)
	
	if ( oneSided == "right" || oneSided == "left")
		lines(rep(0, 2), c(0, .dposteriorOR(0, mean(samples), sd(samples), oneSided=oneSided)), lwd = lwd)
	
	#lines(seq(min(xticks), max(xticks),length.out = 10000),posteriorLine, lwd= lwd)
	#lines(seq(min(xticks), max(xticks),length.out = 10000),dnorm(seq(min(xticks), max(xticks),length.out = 10000), mean(samples), sd(samples)),lwd= lwd)
	#points(z$x,z$y,type="l", col= "green")	

	axis(1, at= xticks, labels = xlabels, cex.axis= cexAxis, lwd= lwdAxis)
	axis(2, at= yticks, labels= ylabels, cex.axis= cexAxis, lwd= lwdAxis)
	
	
	if (nchar(ylabels[length(ylabels)]) > 4) {
		
		mtext(text = "Density", side = 2, las=0, cex = cexYlab, line= 4)
	} else if (nchar(ylabels[length(ylabels)]) == 4) {
		
		mtext(text = "Density", side = 2, las=0, cex = cexYlab, line= 3.25)
	} else if (nchar(ylabels[length(ylabels)]) < 4) {
		
		mtext(text = "Density", side = 2, las=0, cex = cexYlab, line= 2.85)
	}
	
	mtext("Log odds ratio", side = 1, cex = cexXlab, line= 2.5)	
	
	
	# credible interval
	dmax <- optimize(function(x).dposteriorOR(x, mean(samples), sd(samples), oneSided=oneSided), interval= range(xticks), maximum = TRUE)$objective # get maximum density
	
	# enable plotting in margin
	par(xpd=TRUE)
	
	yCI <- grconvertY(dmax, "user", "ndc") + 0.04
	yCI <- grconvertY(yCI, "ndc", "user")
	
	if (oneSided == FALSE)
		arrows(CIlow, yCI , CIhigh, yCI, angle = 90, code = 3, length= 0.1, lwd= lwd)
	
	medianText <- formatC(medianPosterior, digits= 3, format="f")
	
	
	if (addInformation) {
		
		# display BF10 value
		offsetTopPart <- 0.06
		
		yy <- grconvertY(0.75 + offsetTopPart, "ndc", "user")
		yy2 <- grconvertY(0.806 + offsetTopPart, "ndc", "user")
		
		xx <- min(xticks)
		
		if (BF10 >= 1000000 | BF01 >= 1000000) {
			BF10t <- formatC(BF10,3, format = "e")
			BF01t <- formatC(BF01,3, format = "e")
		}
		
		if (BF10 < 1000000 & BF01 < 1000000) {
			BF10t <- formatC(BF10,3, format = "f")
			BF01t <- formatC(BF01,3, format = "f")
		}
		
		if (oneSided == FALSE) {
			
			text(xx, yy2, bquote(BF[10]==.(BF10t)), cex= cexTextBF, pos = 4)
			text(xx, yy, bquote(BF[0][1]==.(BF01t)), cex= cexTextBF, pos = 4)
		}
		
		if (oneSided == "right") {
			
			text(xx, yy2, bquote(BF["+"][0]==.(BF10t)), cex= cexTextBF, pos = 4)
			text(xx, yy, bquote(BF[0]["+"]==.(BF01t)), cex= cexTextBF, pos = 4)
		}
		
		if (oneSided == "left") {
			
			text(xx, yy2, bquote(BF["-"][0]==.(BF10t)), cex= cexTextBF, pos = 4)
			text(xx, yy, bquote(BF[0]["-"]==.(BF01t)), cex= cexTextBF, pos = 4)
		}
		
		yy <- grconvertY(0.756 + offsetTopPart, "ndc", "user")
		yy2 <- grconvertY(0.812 + offsetTopPart, "ndc", "user")
		
		CIwidth <- selectedCI * 100
		CInumber <- paste(CIwidth, "% CI: [", sep="")
		CIText <- paste(CInumber,  bquote(.(formatC(CIlow,3, format="f"))), ", ",  bquote(.(formatC(CIhigh,3, format="f"))), "]", sep="")
		medianLegendText <- paste("median Log OR =", medianText)
				
		if (oneSided == FALSE) {
		
			text(max(xticks) , yy2, medianLegendText, cex= 1.1, pos= 2)
			text(max(xticks) , yy, CIText, cex= 1.1, pos= 2)
		}
		
		# probability wheel
		if (max(nchar(BF10t), nchar(BF01t)) <= 4) {
			xx <- grconvertX(0.44, "ndc", "user")
		}
		
		if (max(nchar(BF10t), nchar(BF01t)) == 5) {
			xx <- grconvertX(0.44 +  0.001* 5, "ndc", "user")
		}
		
		if (max(nchar(BF10t), nchar(BF01t)) == 6) {
			xx <- grconvertX(0.44 + 0.001* 6, "ndc", "user") 
		}
		
		if (max(nchar(BF10t), nchar(BF01t)) == 7) {
			xx <- grconvertX(0.44 + 0.002* max(nchar(BF10t), nchar(BF01t)), "ndc", "user") 
		}
		
		if (max(nchar(BF10t), nchar(BF01t)) == 8) {
			xx <- grconvertX(0.44 + 0.003* max(nchar(BF10t), nchar(BF01t)), "ndc", "user") 
		}
		
		if (max(nchar(BF10t), nchar(BF01t)) > 8) {
			xx <- grconvertX(0.44 + 0.005* max(nchar(BF10t), nchar(BF01t)), "ndc", "user") 
		}
		
		yy <- grconvertY(0.788 + offsetTopPart, "ndc", "user")
		
		
		# make sure that colored area is centered
		radius <- 0.06 * diff(range(xticks))
		A <- radius^2 * pi
		alpha <- 2 / (BF01 + 1) * A / radius^2
		startpos <- pi/2 - alpha/2
		
		# draw probability wheel
		plotrix::floating.pie(xx, yy,c(BF10, 1),radius= radius, col=c("darkred", "white"), lwd=2,startpos = startpos)
		
		yy <- grconvertY(0.865 + offsetTopPart, "ndc", "user")
		yy2 <- grconvertY(0.708 + offsetTopPart, "ndc", "user")
		
		if (oneSided == FALSE) {
			
			text(xx, yy, "data|H1", cex= cexCI)
			text(xx, yy2, "data|H0", cex= cexCI)
		}
		
		if (oneSided == "right") {
			
			text(xx, yy, "data|H+", cex= cexCI)
			text(xx, yy2, "data|H0", cex= cexCI)
		}
		
		if (oneSided == "left") {
			
			text(xx, yy, "data|H-", cex= cexCI)
			text(xx, yy2, "data|H0", cex= cexCI)
		}
		
		# add legend
		CIText <- paste("95% CI: [",  bquote(.(formatC(CIlow,3, format="f"))), " ; ",  bquote(.(formatC(CIhigh,3, format="f"))), "]", sep="")
		
		medianLegendText <- paste("median =", medianText)
	}
	
	mostPosterior <- mean(samples > mean(range(xticks)))
}

.dposteriorOR <- function(logOR, mean, sd, oneSided) {
	
	if (oneSided == FALSE) {
		
		dnorm(logOR, mean, sd)
		
	} else if (oneSided == "right") {
		
		ifelse (logOR >= 0,  dnorm(logOR, mean, sd) / pnorm(0, mean, sd, lower.tail=FALSE), 0 )
		
	} else if (oneSided == "left") {
		
		ifelse (logOR <= 0,  dnorm(logOR, mean, sd) / pnorm(0, mean, sd, lower.tail=TRUE), 0 )
	}
}

##############################################
