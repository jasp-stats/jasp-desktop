
.crosstabBayesian <- function(dataset, options, perform, analysis) {

	# analysis is a list of the form :
	# list(columns="var.name", rows="var.name", "Layer 1"="var.name", etc...)
	
	counts.var <- options$counts
	if (counts.var == "")
		counts.var <- NULL
	
	all.vars <- c(unlist(analysis), counts.var)

	dataset <- subset(dataset, select=.v(all.vars))
	
	
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
	

	tables <- list()


    ### SETUP COLUMNS COMMON TO BOTH TABLES

	fields <- list()
	
	if (length(analysis) >= 3) {
	
		for (j in length(analysis):3)
			fields[[length(fields)+1]] <- list(name=analysis[[j]], type="string", combine=TRUE)
	}
	

	
	### SETUP COUNTS TABLE SCHEMA

	counts.table <- list()
	
	counts.table[["title"]] <- "Bayesian Crosstabs"
	
	counts.fields <- fields
	
	counts.fields[[length(counts.fields)+1]] <- list(name=analysis$rows, type="string", combine=TRUE)

	lvls <- c()
	if (is.factor(dataset[[ .v(analysis$columns) ]] )) {

		lvls <- base::levels(dataset[[ .v(analysis$columns) ]])

	} else if (perform == "run") {
	
		lvls <- base::unique(dataset[[ .v(analysis$columns) ]])
	}
	
	for (column.name in lvls) {

		private.name <- base::paste(".", column.name, sep="")
		counts.fields[[length(counts.fields)+1]] <- list(name=private.name, title=column.name, type="number")
	}

	schema <- list(fields=counts.fields)

	counts.table[["schema"]] <- schema
	
	
	

	### SETUP TESTS TABLE SCHEMA

	tests.table <- list()
	
	tests.table[["title"]] <- "Bayesian Crosstabs Tests"
	
	tests.fields <- fields
	
	tests.fields[[length(tests.fields)+1]] <- list(name="type[N]", title="", type="string")
	tests.fields[[length(tests.fields)+1]] <- list(name="value[N]", title="Value", type="integer")

	if (options$oddsRatio) {
	
		tests.fields[[length(tests.fields)+1]] <- list(name="type[oddsRatio]", title="", type="string")
		tests.fields[[length(tests.fields)+1]] <- list(name="value[oddsRatio]", title="Value", type="number", format="sf:4;dp:3")
	}
	
	tests.fields[[length(tests.fields)+1]] <- list(name="type[BF]", title="", type="string")
	tests.fields[[length(tests.fields)+1]] <- list(name="value[BF]", title="Value", type="number", format="sf:4;dp:3")

	schema <- list(fields=tests.fields)
	
	tests.table[["schema"]] <- schema
	
	


	# POPULATE TABLES

	# create count matrices for each group

	group.matrices <- .crosstabsCreateGroupMatrices(dataset, .v(analysis$rows), .v(analysis$columns), groups, .v(counts.var))
	
	counts.rows <- list()
	tests.rows <- list()
	
	tests.footnotes <- .newFootnotes()

	for (i in 1:length(group.matrices)) {
	
		group.matrix <- group.matrices[[i]]
		
		if ( ! is.null(groups)) {
		
			group <- groups[[i]]
			
		} else {
		
			group <- NULL
		}
	
		next.rows <- .crosstabsCreateCountsRows(analysis$rows, group.matrix, options, perform, group)
		counts.rows <- c(counts.rows, next.rows)
		
		next.rows <- .crosstabsBayesianCreateTestsRows(analysis$rows, group.matrix, tests.footnotes, options, perform, group)
		tests.rows <- c(tests.rows, next.rows)
	}

	counts.table[["data"]] <- counts.rows
	
	tests.table[["data"]] <- tests.rows
	tests.table[["footnotes"]] <- as.list(tests.footnotes)

	tables[[1]] <- counts.table
	tables[[2]] <- tests.table
	
	tables
}

.crosstabsBayesianCreateTestsRows <- function(var.name, counts.matrix, footnotes, options, perform, group=NULL) {

	row <- list()
	
	for (layer in names(group)) {
	
		level <- group[[layer]]
		
		if (level == "") {

			row[[layer]] <- "Total"
						
		} else {
		
			row[[layer]] <- level
		}
	}
	
	row[["type[N]"]] <- "N"

	if (perform == "run") {
	
		row[["value[N]"]] <- base::sum(counts.matrix)
		
	} else {
	
		row[["value[N]"]] <- "."
	}
	
	
	if (options$oddsRatio) {
	
		row[["type[oddsRatio]"]] <- "Odds ratio"

		if (perform == "run") {
		
			chi.result <- try({

				chi.result <- stats::chisq.test(counts.matrix)
			})
			
			if (class(chi.result) == "try-error") {

				row[["value[oddsRatio]"]] <- .clean(NaN)
				
				error <- .extractErrorMessage(chi.result)
				
				if (error == "at least one entry of 'x' must be positive")
					error <- "\u03A7\u00B2 could not be calculated, contains no observations"
				
				sup   <- .addFootnote(footnotes, error)
				row[[".footnotes"]] <- list("value[oddsRatio]"=list(sup))
			
			} else if (is.na(chi.result$statistic)) {
			
				row[["value[oddsRatio]"]] <- .clean(NaN)
			
				sup <- .addFootnote(footnotes, "\u03A7\u00B2 could not be calculated")
				row[[".footnotes"]] <- list("value[oddsRatio]"=list(sup))
			
			} else {
			
				row[["value[oddsRatio]"]] <- unname(chi.result$statistic)
			}
			
		} else {
		
			row[["value[oddsRatio]"]] <- "."
		}
		
	}
	
	
	
	if (options$samplingModel=="poisson") {
	
		bfLabel <- "BF Poisson"
		sampleType <- "poisson"
		fixedMargin <- NULL
		
	} else if (options$samplingModel=="jointMultinomial") {
	
		bfLabel <- "BF Joint Multinomial"
		sampleType <- "jointMulti"
		fixedMargin <- NULL
	
	} else if (options$samplingModel=="independentMultinomialRowsFixed") {

		bfLabel <- "BF Joint Multinomial"	
		sampleType <- "indepMulti"
		fixedMargin <- "rows"
	
	} else if (options$samplingModel=="independentMultinomialColumnsFixed") {
	
		bfLabel <- "BF Joint Multinomial"	
		sampleType <- "indepMulti"
		fixedMargin <- "cols"
	
	} else if (options$samplingModel=="hypergeometric") {

		bfLabel <- "BF Hypergeometric"
		sampleType <- "hypergeom"
		fixedMargin <- NULL
		
	} else {
	
		stop("wtf?")
	}
	

	row[["type[BF]"]] <- bfLabel

	if (perform == "run") {
	
		BF <- try({

			BF <- BayesFactor::contingencyTableBF(counts.matrix, sampleType=sampleType, fixedMargin=fixedMargin)
		})
		
		if (class(BF) == "try-error") {

			row[["value[BF]"]] <- .clean(NaN)
			
			error <- .extractErrorMessage(BF)
			
			sup   <- .addFootnote(footnotes, error)
			row[[".footnotes"]] <- list("value[BF]"=list(sup))
		
		} else {
		
			row[["value[BF]"]] <- unname(BF@bayesFactor$bf)
		}
		
	} else {
	
		row[["value[BF]"]] <- "."
	}

	list(row)
}

CrosstabsBayesian <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {

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
			dataset <- .readDataSetToEnd(columns.as.factor=factors, columns.as.numeric=counts.var)
		} else {
			dataset <- .readDataSetHeader(columns.as.factor=factors, columns.as.numeric=counts.var)
		}
	}

	results <- list()
	
	### META

	meta <- list()
	meta[[1]] <- list(name="crosstabs", type="tables")
	
	results[[".meta"]] <- meta
	
	
	### CROSS TABS
	
	crosstabs <- list()

	if (length(options$rows) > 0 && length(options$columns) > 0)
	{
		rows <- as.vector(options$rows, "character")
		columns <- as.vector(options$columns, "character")

		analyses <- data.frame("columns"=columns, stringsAsFactors=FALSE)
		analyses <- cbind(analyses, "rows"=rep(rows, each=dim(analyses)[1]), stringsAsFactors=FALSE)
		
		for (layer in options$layers)
		{
			layer.vars <- as.vector(layer$variables, "character")
			analyses <- cbind(analyses, rep(layer.vars, each=dim(analyses)[1]), stringsAsFactors=FALSE)
			names(analyses)[dim(analyses)[2]] <- layer$name
		}
		
		analyses <- .dataFrameToRowList(analyses)

		for (analysis in analyses)
		{
			tables <- .crosstabBayesian(dataset, options, perform, analysis)
			for (table in tables)
				crosstabs[[length(crosstabs)+1]] <- table				
		}
	
	} else {
	
		crosstabs[[1]] <- list(title = "Bayesian Crosstabs", cases = list(), schema = list(fields=list()))
	}

	results[["crosstabs"]] <- crosstabs

	results
}

