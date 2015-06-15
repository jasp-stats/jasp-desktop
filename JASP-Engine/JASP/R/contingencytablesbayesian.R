

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
	meta[[1]] <- list(name="title", type="title")
	meta[[2]] <- list(name="tables", type="tables")
	meta[[3]] <- list(name="plots", type="images")
	
	results[[".meta"]] <- meta
	
	results[["title"]] <- "Bayesian Contingency Tables"
	
	### CROSS TABS

	cont.tables <- list()
	plots     <- list()
	
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

	for (i in seq_along(analyses)) {
	
		analysis <- analyses[[i]]
		
		if ("results" %in% names(old.state) && i <= length(old.state$results)) {
			last.results <- old.state$results[[i]]
		} else {
			last.results <- NULL
		}
	
		res <- .contTablesBayesian(dataset, options, populate=FALSE, analysis, last.results)
		
		for (table in res$tables) {

			cont.tables[[next.table.index]] <- table
			next.table.index <- next.table.index + 1
		}
		
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
			results[["tables"]] <- cont.tables
			results[["plots"]] <- plots
		
			callback(results)
		
			analysis <- analyses[[i]]
		
			if ("results" %in% names(old.state) && i <= length(old.state$results)) {
				last.results <- old.state$results[[i]]
			} else {
				last.results <- NULL
			}
		
			res <- .contTablesBayesian(dataset, options, populate=TRUE, analysis, last.results)
		
			for (table in res$tables) {
			
				cont.tables[[next.table.index]] <- table
				next.table.index <- next.table.index + 1
			}
			
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
	
		results[["tables"]] <- cont.tables
		
	}
	
	results[["tables"]] <- cont.tables
	results[["plots"]] <- plots

	if (perform == "run") {
	
		list(results=results, status="complete", state=new.state, keep=keep)
		
	} else {
	
		status <- ifelse(complete, "complete", "inited")
		list(results=results, status=status, state=new.state, keep=keep)
	}
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

	tables[[length(tables)+1]] <- res$table
	new.state$counts <- res$state
	status <- res$status
	complete <- complete && res$complete
	

	res <- .contTablesBayesianCreateTestsTable(dataset, analysis, group.matrices, groups, footnotes, options, populate, tests.state, old.options, status)

	tables[[length(tables)+1]] <- res$table
	new.state$tests <- res$state
	status <- res$status
	complete <- complete && res$complete
	
		
	res <- .contTablesBayesianCreateOddsRatioTable(dataset, analysis, group.matrices, groups, footnotes, options, populate, odds.ratio.state, old.options, status)

	tables[[length(tables)+1]] <- res$table
	plots <- c(plots, res$plots)
	keep  <- res$keep
	
	new.state$odds.ratio <- res$state
	complete <- complete && res$complete
	
	new.state$options <- options
	
	if (length(options$rows) == 0 || length(options$columns) == 0)
		complete <- TRUE
	
	list(tables=tables, plots=plots, keep=keep, state=new.state, complete=complete)
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
	
		count.matrix <- counts.matrices[[i]]
		
		if ( ! is.null(groups)) {
		
			group <- groups[[i]]
			
		} else {
		
			group <- NULL
		}
		
		rows.state <- NULL
		if (is.null(state) == FALSE && i <= length(state$rows))
			rows.state <- state$rows[[i]]
	
		next.rows <- .contTableBayesianCreateCountsRows(analysis$rows, count.matrix, options, populate, group, rows.state)
		
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
	
		count.matrix <- counts.matrices[[i]]
		
		if ( ! is.null(groups)) {
		
			group <- groups[[i]]
			
		} else {
		
			group <- NULL
		}
		
		rows.state <- NULL
		if (i <= length(state))
			rows.state <- state[[i]]
		
		res <- .contTablesBayesianCreateTestsRows(analysis$rows, count.matrix, footnotes, options, populate, group, rows.state)

		rows <- c(rows, res$rows)
		new.state[[length(new.state)+1]] <- res$state
	}
	
	complete <- populate || is.null(state) == FALSE || length(options$rows) == 0 || length(options$columns) == 0
	
	table[["data"]] <- rows
	table[["footnotes"]] <- as.list(footnotes)
	
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


.contTablesBayesianCreateOddsRatioTable <- function(dataset, analysis, counts.matrices, groups, footnotes, options, populate, state, state.options, status) {

	if (options$oddsRatio == FALSE && options$plotPosteriorOddsRatio == FALSE)
		return(list(table=NULL, plots=NULL, state=NULL, complete=TRUE))

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
		
	fields[[length(fields)+1]] <- list(name="value[oddsRatio]", title="Odds ratio", type="number", format="sf:4;dp:3")
	fields[[length(fields)+1]] <- list(name="low[oddsRatio]", title="Lower", overTitle=ci.label, type="number", format="dp:3")
	fields[[length(fields)+1]] <- list(name="up[oddsRatio]",  title="Upper", overTitle=ci.label, type="number", format="dp:3")
	
	schema <- list(fields=fields)
	
	table[["schema"]] <- schema



	rows  <- list()
	plots <- list()
	keep  <- list()
	new.state <- list()
	footnotes <- .newFootnotes()
	
	next.is.new.group <- TRUE
	complete <- TRUE
	
	for (i in 1:length(counts.matrices)) {
	
		count.matrix <- counts.matrices[[i]]
		
		if ( ! is.null(groups)) {
		
			group <- groups[[i]]
			
		} else {
		
			group <- NULL
		}
		
		rows.state <- NULL
		if (i <= length(state))
			rows.state <- state[[i]]

		res <- .contTablesBayesianCreateOddsRatioRows(analysis$rows, count.matrix, footnotes, options, populate, group, rows.state, state.options, status)

		next.rows <- res$rows
		next.plot <- res$plot
		complete <- complete && res$complete

		if (next.is.new.group) {
		
			next.rows[[1]][[".isNewGroup"]] <- TRUE
			next.is.new.group <- FALSE
			
		} else if ( ! is.null(group) && group[[length(group)]] == "") {
		
			next.rows[[1]][[".isNewGroup"]] <- TRUE
			next.is.new.group <- TRUE
		}
		
		rows <- c(rows, next.rows)
		plots[[length(plots)+1]] <- next.plot
		
		keep <- c(keep, res$keep)
		
		new.state[[length(new.state)+1]] <- res$state
	}
	
	complete <- complete && (populate || is.null(state) == FALSE || length(options$rows) == 0 || length(options$columns) == 0)
	
	table[["data"]] <- rows
	table[["footnotes"]] <- as.list(footnotes)
	
	if (complete)
		table[["status"]] <- "complete"
	
	if (status$error) {
	
		table[["status"]] <- "complete"
		table[["error"]] <- list(errorType="badData")
	}
	
	if (options$oddsRatio == FALSE)
		table <- NULL

	list(table=table, plots=plots, keep=keep, state=new.state, complete=complete, status=status)
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
	
		if (options$bayesFactorType == "BF10") {
			bfLabel <- "BF\u2081\u2080 Poisson"
		} else if (options$bayesFactorType == "BF01") {
			bfLabel <- "BF\u2080\u2081 Poisson"
		} else if (options$bayesFactorType == "LogBF10") {
			bfLabel <- " Log\u2009(\u2009BF\u2081\u2080\u2009) Poisson"		
		}
		
		sampleType <- "poisson"
		fixedMargin <- NULL
		
	} else if (options$samplingModel == "jointMultinomial") {
	
		if (options$bayesFactorType == "BF10") {
			bfLabel <- "BF\u2081\u2080 joint multinomial"
		} else if (options$bayesFactorType == "BF01") {
			bfLabel <- "BF\u2080\u2081 joint multinomial"
		} else if (options$bayesFactorType == "LogBF10") {
			bfLabel <- "Log\u2009(\u2009BF\u2081\u2080\u2009) joint multinomial"
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
				bfLabel <-	"Log\u2009(\u2009BF\u2081\u2080\u2009) independent multinomial"
			}
						 
		} else if(options$hypothesis =="groupTwoGreater") { 
		
			if (options$bayesFactorType == "BF10"){
				bfLabel <- "BF\u208B\u2080 independent multinomial"
			} else if (options$bayesFactorType == "BF01"){
				bfLabel <- "BF\u2080\u208B independent multinomial"
			} else if (options$bayesFactorType == "LogBF10") {
				bfLabel <-"Log\u2009(\u2009BF\u2081\u2080\u2009) independent multinomial"
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
				bfLabel <-"Log\u2009(\u2009BF\u2081\u2080\u2009) independent multinomial"
			}
							 
		} else if (options$hypothesis=="groupTwoGreater"){
			
			if (options$bayesFactorType == "BF10"){
				bfLabel <- "BF\u208B\u2080 independent multinomial"
			
			} else if (options$bayesFactorType == "BF01"){
				bfLabel <- "BF\u2080\u208B independent multinomial"
				
			} else if (options$bayesFactorType == "LogBF10") {
				bfLabel <-"Log\u2009(\u2009BF\u2081\u2080\u2009) independent multinomial"
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
			
			if (options$hypothesis=="groupOneGreater" && options$samplingModel=="independentMultinomialColumnsFixed") {
										
				ch.result = BayesFactor::posterior(BF, iterations = 10000)
				theta <- as.data.frame(ch.result[,7:10])
				prop.consistent <- mean(theta[,1] > theta[,3])  #sum(p1.sim > p2.sim)/N.sim
				bf1 <- bf1 * prop.consistent / 0.5
				lbf1 <- lbf1 + log(prop.consistent) - log(0.5)
			
			} else if (options$hypothesis=="groupOneGreater" && options$samplingModel=="independentMultinomialRowsFixed") {
								
				ch.result = BayesFactor::posterior(BF, iterations = 10000)
				theta <- as.data.frame(ch.result[,7:10])
				prop.consistent <- mean(theta[,1] > theta[,2]) 
				bf1 <- bf1 * prop.consistent / 0.5
				lbf1 <- lbf1 + log(prop.consistent) - log(0.5)
			
			} else if (options$hypothesis=="groupTwoGreater"  && options$samplingModel=="independentMultinomialColumnsFixed") {
				
				ch.result = BayesFactor::posterior(BF, iterations = 10000)
				theta <- as.data.frame(ch.result[,7:10])
				prop.consistent <- mean(theta[,3] > theta[,1]) 
				bf1 <- bf1 * prop.consistent / 0.5
				lbf1 <- lbf1 + log(prop.consistent) - log(0.5)
				
			} else if (options$hypothesis=="groupTwoGreater"  && options$samplingModel=="independentMultinomialRowsFixed"){
								
				ch.result = BayesFactor::posterior(BF, iterations = 10000)
				theta <- as.data.frame(ch.result[,7:10])
				prop.consistent <- mean(theta[,2] > theta[,1])
				bf1 <- bf1 * prop.consistent / 0.5
				lbf1 <- lbf1 + log(prop.consistent) - log(0.5)
			}
			
			list(BF=BF, BF10=bf1, LogBF10=lbf1)		
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

	} else if (class(results$BF) == "try-error") {

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

		 if (options$hypothesis=="groupOneGreater" && options$samplingModel=="independentMultinomialRowsFixed"){

			gp1 <- rownames(counts.matrix)[1]
			gp2 <- rownames(counts.matrix)[2]

			message <- paste("All tests, hypothesis is group <em>", gp1, "</em> greater than group <em>", "<em>", gp2, "</em>", sep="")
			.addFootnote(footnotes, symbol="<em>Note.</em>", text=message)

		} else if (options$hypothesis=="groupTwoGreater"  && options$samplingModel=="independentMultinomialRowsFixed"){

			gp1 <- rownames(counts.matrix)[1]
			gp2 <- rownames(counts.matrix)[2]

			message <- paste("All tests, hypothesis is group <em>", gp1, "</em> less than group <em>", gp2, "</em>", sep="")
			.addFootnote(footnotes, symbol="<em>Note.</em>", text=message)			

		} else if (options$hypothesis=="groupOneGreater" && options$samplingModel=="independentMultinomialColumnsFixed") {

			gp1 <- colnames(counts.matrix)[1]
			gp2 <- colnames(counts.matrix)[2]

			message <- paste("All tests, hypothesis is group <em>", gp1, "</em> greater than group <em>", gp2, "</em>", sep="")
			.addFootnote(footnotes, symbol="<em>Note.</em>", text=message)

		} else if (options$hypothesis=="groupTwoGreater"  && options$samplingModel=="independentMultinomialColumnsFixed") {

			gp1 <- colnames(counts.matrix)[1]
			gp2 <- colnames(counts.matrix)[2]

			message <- paste("All tests, hypothesis is group <em>", gp1, "</em> less than group <em>", gp2, "</em>", sep="")
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

		if (class(expected.matrix) == "try-error") {

			expected.matrix <- counts.matrix
			expected.matrix[,] <- "&nbsp;"
		}
		
		row.proportions.matrix <- try({

			base::prop.table(counts.matrix, 1)
		})

		if (class(row.proportions.matrix) == "try-error") {

			row.proportions.matrix <- counts.matrix
			row.proportions.matrix[,] <- "&nbsp;"
		}
		
		col.proportions.matrix <- try({

			base::prop.table(counts.matrix, 2)
		})

		if (class(col.proportions.matrix) == "try-error") {

			col.proportions.matrix <- counts.matrix
			col.proportions.matrix[,] <- "&nbsp;"
		}
		
		proportions.matrix <- try({

			base::prop.table(counts.matrix, margin = NULL)
		})

		if (class(proportions.matrix) == "try-error") {

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

.contTablesBayesianCreateOddsRatioRows <- function(var.name, counts.matrix, footnotes, options, populate, group, state, state.options, status) {

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

			result <- try({

				if (options$samplingModel == "poisson") {
				
					sampleType <- "poisson"
					BF         <- BayesFactor::contingencyTableBF(counts.matrix, sampleType, priorConcentration=options$priorConcentration)
					ch.result  <- BayesFactor::posterior(BF, iterations = 10000)
					lambda     <- as.data.frame(ch.result)
					odds.ratio.samples <- (lambda[,1]*lambda[,4])/(lambda[,2]*lambda[,3])
	
				} else if (options$samplingModel == "jointMultinomial") {
	
					sampleType <- "jointMulti"
					BF         <- BayesFactor::contingencyTableBF(counts.matrix, sampleType, priorConcentration=options$priorConcentration)
					ch.result  <- BayesFactor::posterior(BF, iterations = 10000)
					theta      <- as.data.frame(ch.result)
					odds.ratio.samples <- (theta[,1]*theta[,4])/(theta[,2]*theta[,3])
		
				} else if (options$samplingModel == "independentMultinomialRowsFixed") {
	
					sampleType <- "indepMulti"
					BF         <- BayesFactor::contingencyTableBF(counts.matrix, sampleType, priorConcentration=options$priorConcentration, fixedMargin = "rows")
					ch.result  <- BayesFactor::posterior(BF, iterations = 10000)
					theta      <- as.data.frame(ch.result[,7:10])
					odds.ratio.samples <- (theta[,1]*theta[,4])/(theta[,2]*theta[,3])
		
				} else if (options$samplingModel == "independentMultinomialColumnsFixed") {
	
					sampleType <- "indepMulti"
					BF         <- BayesFactor::contingencyTableBF(counts.matrix, sampleType, priorConcentration=options$priorConcentration, fixedMargin = "cols")
					ch.result  <- BayesFactor::posterior(BF, iterations = 10000)
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
		
		if (class(result) == "try-error") {

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
	
	res <- .contTablesBayesianPlotOddsRatio(var.name, counts.matrix, options, populate, group, result, state.options, status)

	result$keep <- res$keep
	complete    <- complete && res$complete
	
	list(rows=list(row), plot=res$plot, state=result, keep=res$keep, complete=complete)
}


.contTablesBayesianPlotOddsRatio <- function(var.name, counts.matrix, options, populate, group, result, state.options, status) {

	if (options$plotPosteriorOddsRatio == FALSE || identical(dim(counts.matrix), as.integer(c(2,2))) == FALSE)
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
		base::identical(state.options$priorConcentration, options$priorConcentration) == FALSE) {
		
		result$keep <- NULL
	}
	
	odds.ratio.plot  <- list()
	keep <- NULL
	
    group[group == ""] <- "Total"
    
	
	if (length(group) == 0) {
	
		odds.ratio.plot[["title"]] <- "Odds ratio"
		
	} else if (length(group) > 0) {
		
		layerLevels <- paste(names(group),"=", group)
		layerLevels <- gsub(pattern = " = Total", layerLevels, replacement = "")

		plotTitle <- paste(layerLevels, collapse="; ")
		odds.ratio.plot[["title"]] <- plotTitle
	}
	
	width  <- 530
	height <- 400
	
	odds.ratio.plot[["width"]]  <- width
	odds.ratio.plot[["height"]] <- height
	
	if (is.null(result) == FALSE && is.null(result$keep) == FALSE) {
	
		odds.ratio.plot[["data"]] <- result$keep
		keep <- result$keep
		
		return(list(plot=odds.ratio.plot, keep=keep, complete=TRUE))
	}
	
	if (populate) {
	
		if (is.null(result) || class(result) == "try-error") {
	
			message <- .extractErrorMessage(result)
			status <- list(error=TRUE, errorMessage=message)
		
		} else if (options$samplingModel == "hypergeometric") {
		
			status <- list(error=TRUE, errorMessage="Odds ratio for this model not yet implemented")
		
		} else {
		
			BF10 <- BayesFactor::extractBF(result$BF)[[1]]
		
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
	
		image <- .beginSaveImage(width, height)

		p <- try(silent=TRUE, expr={
			
			if (options$samplingModel=="independentMultinomialColumnsFixed" || options$samplingModel=="independentMultinomialRowsFixed") {
			
				if (options$hypothesis=="groupTwoGreater") {
				
					oneSided <- "left"
			
				} else if (options$hypothesis=="groupOneGreater") {
				
					oneSided <- "right"
			
				} else {
				
					oneSided <- FALSE
				}
				
			} else {
			
				oneSided <- FALSE
			}
			
			.plotPosterior.cont.tables(samples=result$log.odds.ratio.samples, CI=c(result$lower.ci, result$upper.ci), medianSamples=result$median, BF=BF10, selectedCI=options$oddsRatioCredibleIntervalInterval,
					addInformation=options$plotPosteriorOddsRatioAdditionalInfo, oneSided=oneSided, options=options)
		})
		
		plot.data <- .endSaveImage(image)

		if (class(p) != "try-error") {

			keep <- plot.data
	
		} else {
			
			errorMessage <- .extractErrorMessage(p)
			
			if (errorMessage == "not enough data") {
			
				errorMessage <- "Plotting is not possible: The Bayes factor is too small"
					
			} else if (errorMessage == "'from' cannot be NA, NaN or infinite") {
			
				errorMessage <- "Plotting is not possible: The Bayes factor is too small"
			}
			
			status <- list(error=TRUE, errorMessage=errorMessage)
		}	
	
	}

	if (populate && status$error == FALSE) {
	
		odds.ratio.plot[["data"]] <- plot.data
		odds.ratio.plot[["status"]] <- "complete"
		
		complete <- TRUE
	
	} else {
	
		image <- .beginSaveImage(width, height)
		.plotPosterior.cont.tables(dontPlotData=TRUE,addInformation=options$plotPosteriorOddsRatioAdditionalInfo)
		odds.ratio.plot[["data"]] <- .endSaveImage(image)

		if (status$error) {
		
			message <- status$errorMessage
			odds.ratio.plot[["error"]]  <- list(error="badData", errorMessage=paste("Plotting is not possible:", message))
			odds.ratio.plot[["status"]] <- "complete"
			complete <- TRUE
			
		} else {
		
			complete <- FALSE
		}
	}
	
	list(plot=odds.ratio.plot, keep=keep, complete=complete)
}

.plotPosterior.cont.tables <- function(samples, CI, medianSamples, BF, oneSided= FALSE, iterations= 10000, lwd= 2, cexPoints= 1.5,
 cexAxis= 1.2, cexYlab= 1.5, cexXlab= 1.5, cexTextBF= 1.4, cexCI= 1.1, cexLegend= 1.2, lwdAxis= 1.2, addInformation= FALSE, dontPlotData=FALSE, selectedCI= options$oddsRatioCredibleIntervalInterval, options) {
	
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
		mtext("log(odds ratio)", side = 1, cex = cexXlab, line= 2.5)
	
		return()
	}

	BF10 <- BF
	BF01 <- 1 / BF10

	# fit denisty estimator
	fit.posterior <-  logspline::logspline(samples)
	
	# density function posterior
	dposterior <- function(x, oneSided= oneSided, samples= samples){
	
		return(logspline::dlogspline(x, fit.posterior))
	}
	
	
	# set limits plot
	xlim <- vector("numeric", 2)
	
	xlim[1] <- min(-2, quantile(samples, probs = 0.01)[[1]])
	xlim[2] <- max(2, quantile(samples, probs = 0.99)[[1]])
	stretch <- 1.2	

	
	ylim <- vector("numeric", 2)
	
	ylim[1] <- 0
	ylim[2] <- stretch * max(dposterior(x= samples, oneSided= oneSided, samples=samples))
	
	# calculate position of "nice" tick marks and create labels
	xticks <- pretty(xlim)
	yticks <- pretty(ylim)
	xlabels <- formatC(xticks, 1, format= "f")
	ylabels <- formatC(yticks, 1, format= "f")
	
	# compute 95% credible interval & median:
	
	CIlow <- CI[1]
	CIhigh <- CI[2]
	medianPosterior <- medianSamples
	
	
	posteriorLine <- dposterior(x= seq(min(xticks), max(xticks),length.out = 1000), oneSided = oneSided, samples=samples)
	
	xlim <- c(min(CIlow,range(xticks)[1]), max(range(xticks)[2], CIhigh))
	
	plot(1,1, xlim= xlim, ylim= range(yticks), ylab= "", xlab="", type= "n", axes= FALSE)
	
	lines(seq(min(xticks), max(xticks),length.out = 1000),posteriorLine, lwd= lwd)
		
	axis(1, at= xticks, labels = xlabels, cex.axis= cexAxis, lwd= lwdAxis)
	axis(2, at= yticks, labels= ylabels, , cex.axis= cexAxis, lwd= lwdAxis)
	
	
	if (nchar(ylabels[length(ylabels)]) > 4) {
		
		mtext(text = "Density", side = 2, las=0, cex = cexYlab, line= 4)
	} else if (nchar(ylabels[length(ylabels)]) == 4) {
		
		mtext(text = "Density", side = 2, las=0, cex = cexYlab, line= 3.25)
	} else if (nchar(ylabels[length(ylabels)]) < 4) {
		
		mtext(text = "Density", side = 2, las=0, cex = cexYlab, line= 2.85)
	}
	
	mtext("Log(odds ratio)", side = 1, cex = cexXlab, line= 2.5)	
	
	
	# credible interval
	dmax <- optimize(function(x)dposterior(x,oneSided= oneSided, samples=samples), interval= range(xticks), maximum = TRUE)$objective # get maximum density
	
	# enable plotting in margin
	par(xpd=TRUE)
	
	yCI <- grconvertY(dmax, "user", "ndc") + 0.04
	yCI <- grconvertY(yCI, "ndc", "user")
	
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
		medianLegendText <- paste("median =", medianText)
		
		text(max(xticks) , yy2, medianLegendText, cex= 1.1, pos= 2)
		text(max(xticks) , yy, CIText, cex= 1.1, pos= 2)
		
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

