
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

	} else  {
	
		lvls <- base::unique(dataset[[ .v(analysis$columns) ]])
	}
	
	counts.fp <- FALSE  # whether the counts are float point or not; changes formatting
	
	if (is.null(counts.var) == FALSE) {

		counts <- dataset[[ .v(counts.var) ]]
		if (identical(counts, as.integer(counts)) == FALSE)  # are the counts floating point?
			counts.fp <- TRUE
	}
	
	for (column.name in lvls) {

		private.name <- base::paste(column.name,"[counts]", sep="")
		
		if (counts.fp || options$countsExpected) {
		
			counts.fields[[length(counts.fields)+1]] <- list(name=private.name, title=column.name, type="number", format="sf:4;dp:2")
		
		} else {
		
			counts.fields[[length(counts.fields)+1]] <- list(name=private.name, title=column.name, type="integer")
		}
		
		if (options$countsExpected) {
		
			private.name <- base::paste(column.name,"[expected]", sep="")
			counts.fields[[length(counts.fields)+1]] <- list(name=private.name, title=column.name, type="number", format="sf:4;dp:2")
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
	

	### SETUP TESTS TABLE SCHEMA

	tests.table <- list()
	
	tests.table[["title"]] <- "Bayesian Crosstabs Tests"
	
	
	tests.fields <- fields
	
	tests.fields[[length(tests.fields)+1]] <- list(name="type[BF]", title="", type="string")
	tests.fields[[length(tests.fields)+1]] <- list(name="value[BF]", title="Value", type="number", format="sf:4;dp:3")
	
	tests.fields[[length(tests.fields)+1]] <- list(name="type[N]", title="", type="string")
	tests.fields[[length(tests.fields)+1]] <- list(name="value[N]", title="Value", type="integer")

	#if (options$oddsRatio) {
	
	#	tests.fields[[length(tests.fields)+1]] <- list(name="type[oddsRatio]", title="", type="string")
	#	tests.fields[[length(tests.fields)+1]] <- list(name="value[oddsRatio]", title="Value", type="number", format="sf:4;dp:3")
	#}
	
	

	schema <- list(fields=tests.fields)
	
	tests.table[["schema"]] <- schema
	
	##### Ordinal Table
	if (options$oddsRatio || options$oddsRatioCredibleInterval) {
		
		oddsratio.table <- list()
		
		oddsratio.table[["title"]] <- "Odds ratio"
		
		oddsratio.fields <- fields
			
		#oddsratio.fields[[length(oddsratio.fields)+1]] <- list(name="type[oddsRatio]", title="", type="string")
		oddsratio.fields[[length(oddsratio.fields)+1]] <- list(name="value[oddsRatio]", title="Odds ratio", type="number", format="sf:4;dp:3")
		#oddsratio.fields[[length(oddsratio.fields)+1]] <- list(name="Sigma[oddsRatio]", title="std. error", type="number", format="dp:3")
		
		oddsratio.fields[[length(oddsratio.fields)+1]] <- list(name="low[oddsRatio]", title="Lower CI", type="number", format="dp:3")
		oddsratio.fields[[length(oddsratio.fields)+1]] <- list(name="up[oddsRatio]",  title="Upper CI", type="number", format="dp:3")
		
		schema <- list(fields=oddsratio.fields)
		
		oddsratio.table[["schema"]] <- schema
	}
	
	status <- list(error=FALSE)
	if (is.null(counts.var) == FALSE) {

		counts <- dataset[[ .v(counts.var) ]]
		
		if (any(counts < 0) || any(is.infinite(counts)))
			status <- list(error=TRUE, errorMessage="Counts may not contain negative numbers or infinite number")
	}


	# POPULATE TABLES

	# create count matrices for each group

	group.matrices <- .crosstabsCreateGroupMatrices(dataset, .v(analysis$rows), .v(analysis$columns), groups, .v(counts.var))
	
	counts.rows <- list()
	tests.rows <- list()
	oddsratio.rows <- list()
	
	tests.footnotes <- .newFootnotes()
	oddsratio.footnotes <- .newFootnotes()

	for (i in 1:length(group.matrices)) {
	
		group.matrix <- group.matrices[[i]]
		
		if ( ! is.null(groups)) {
		
			group <- groups[[i]]
			
		} else {
		
			group <- NULL
		}
	
		next.rows <- .crosstabsCreateCountsRows(analysis$rows, group.matrix, options, perform, group, status)
		counts.rows <- c(counts.rows, next.rows)
		
		next.rows <- .crosstabsBayesianCreateTestsRows(analysis$rows, group.matrix, tests.footnotes, options, perform, group, status)
		tests.rows <- c(tests.rows, next.rows)
		
		next.rows <- .crosstabsBayesianCreateoddratioRows(analysis$rows, group.matrix, oddsratio.footnotes, options, perform, group, status)
		oddsratio.rows <- c(oddsratio.rows, next.rows)
	}

	counts.table[["data"]] <- counts.rows
	if (status$error)
		counts.table[["error"]] <- list(errorType="badData", errorMessage=status$errorMessage)

	tables[[1]] <- counts.table
	
	tests.table[["data"]] <- tests.rows
	tests.table[["footnotes"]] <- as.list(tests.footnotes)
	if (status$error)
		tests.table[["error"]] <- list(errorType="badData")
		
	tables[[2]] <- tests.table
	
	if (options$oddsRatio || options$oddsRatioCredibleInterval) {
		oddsratio.table[["data"]] <- oddsratio.rows 
		oddsratio.table[["footnotes"]] <- as.list(oddsratio.footnotes)
	
		if (status$error)
			oddsratio.table[["error"]] <- list(errorType="badData")
			
		tables[[3]] <- oddsratio.table
	}
	
	tables
}

.crosstabsBayesianCreateTestsRows <- function(var.name, counts.matrix, footnotes, options, perform, group, status) {

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

		if (perform == "run" && status$error == FALSE) {
	
		row[["value[N]"]] <- base::sum(counts.matrix)
		
	} else {
	
		row[["value[N]"]] <- "."
	}
	
		
	if (options$samplingModel=="poisson") {
	
		bfLabel <- "BF\u2081\u2080 Poisson"
		sampleType <- "poisson"
		fixedMargin <- NULL
		
	} else if (options$samplingModel=="jointMultinomial") {
	
		bfLabel <- "BF\u2081\u2080 joint multinomial"
		sampleType <- "jointMulti"
		fixedMargin <- NULL
		
	} else if (options$samplingModel=="independentMultinomialRowsFixed") {

		bfLabel <- "BF\u2081\u2080 independent multinomial"	
		sampleType <- "indepMulti"
		fixedMargin <- "rows"
		
	} else if (options$samplingModel=="independentMultinomialColumnsFixed") {
	
		bfLabel <- "BF\u2081\u2080 independent multinomial"	
		sampleType <- "indepMulti"
		fixedMargin <- "cols"
		
	} else if (options$samplingModel=="hypergeometric") {

		bfLabel <- "BF\u2081\u2080 hypergeometric"
		sampleType <- "hypergeom"
		fixedMargin <- NULL
		
	} else {
	
		stop("wtf?")
	}
	

	row[["type[BF]"]] <- bfLabel

	if (perform == "run" && status$error == FALSE) {
	
		BF <- try({

			BF <- BayesFactor::contingencyTableBF(counts.matrix, sampleType=sampleType, priorConcentration=options$priorConcentration, fixedMargin=fixedMargin)
		})
		
		if (class(BF) == "try-error") {

			row[["value[BF]"]] <- .clean(NaN)
			
			error <- .extractErrorMessage(BF)
			
			sup   <- .addFootnote(footnotes, error)
			row[[".footnotes"]] <- list("value[BF]"=list(sup))
		
		} else {
		
			row[["value[BF]"]] <- exp(as.numeric(BF@bayesFactor$bf))
		}
		
	} else {
	
		row[["value[BF]"]] <- "."
	}	

	list(row)
}

.crosstabsBayesianCreateoddratioRows <- function(var.name, counts.matrix, footnotes, options, perform, group, status) { 

	row <- list()
	
	for (layer in names(group)) {
	
		level <- group[[layer]]
		
		if (level == "") {

			row[[layer]] <- "Total"
			row[[".isNewGroup"]] <- TRUE
						
		} else {
		
			row[[layer]] <- level
		}
	}
	
	if (options$oddsRatio) {

		

		row[["type[oddsRatio]"]] <- "Odds ratio"

		if (perform == "run" && status$error == FALSE) {
		
			if ( options$samplingModel== "hypergeometric") {

				row[["value[oddsRatio]"]] <- .clean(NaN)
				row[["low[oddsRatio]"]] <- ""
				row[["up[oddsRatio]"]] <-  ""
			
				sup <- .addFootnote(footnotes, "Odd ratio for this model not yet implemented")
				row[[".footnotes"]] <- list("value[oddsRatio]"=list(sup))
			
			} else {

				OR <- try({
	
					if(options$samplingModel== "poisson"){
						sampleType <- "poisson"
						BF <- BayesFactor::contingencyTableBF(counts.matrix, sampleType, priorConcentration=options$priorConcentration)
						chi.result <- BayesFactor::posterior(BF, iterations = 10000)
						lambda<-as.data.frame(chi.result,col.names=c("lambda11","lambda21","lambda12","lambda22"))
						odds.ratio<-(lambda[,1]*lambda[,4])/(lambda[,2]*lambda[,3])
			
					} else if (options$samplingModel== "jointMultinomial"){
			
						sampleType <- "jointMulti"
						BF <- BayesFactor::contingencyTableBF(counts.matrix, sampleType, priorConcentration=options$priorConcentration)
						chi.result <- BayesFactor::posterior(BF, iterations = 10000)
						theta <- as.data.frame(chi.result,col.names=c("theta11","theta21","theta12","theta22"))
						odds.ratio<-(theta[,1]*theta[,4])/(theta[,2]*theta[,3])
				
					} else if (options$samplingModel== "independentMultinomialRowsFixed"){
			
						sampleType <- "indepMulti"
						BF <- BayesFactor::contingencyTableBF(counts.matrix, sampleType, priorConcentration=options$priorConcentration, fixedMargin = "rows")
						chi.result <- BayesFactor::posterior(BF, iterations = 10000)
						theta <- as.data.frame(chi.result[,7:10],col.names=c("theta11","theta21","theta12","theta22"))
						odds.ratio<-(theta[,1]*theta[,4])/(theta[,2]*theta[,3])
				
					} else if (options$samplingModel== "independentMultinomialColumnsFixed"){
			
						sampleType <- "indepMulti"
						BF <- BayesFactor::contingencyTableBF(counts.matrix, sampleType, priorConcentration=options$priorConcentration, fixedMargin = "cols")
						chi.result <- BayesFactor::posterior(BF, iterations = 10000)
						theta <- as.data.frame(chi.result[,7:10],col.names=c("theta11","theta21","theta12","theta22"))
						odds.ratio<-(theta[,1]*theta[,4])/(theta[,2]*theta[,3])
				
					} 
			
				})
						
				logOR<-log(odds.ratio)
				z<-stats::density(logOR)
				x.mode <- z$x[i.mode <- which.max(z$y)]
				Sig <- options$oddsRatioCredibleIntervalInterval
				alpha <- (1 - Sig)/2
				x0 <- unname(stats::quantile(logOR, p = alpha))
				x1 <- unname(stats::quantile(logOR, p = (1-alpha)))
				

	
				if (class(OR) == "try-error") {
		
					row[["value[oddsRatio]"]] <- .clean(NaN)
		
					error <- .extractErrorMessage(BF)
		
					sup   <- .addFootnote(footnotes, error)
					row[[".footnotes"]] <- list("value[oddsRatio]"=list(sup))
		
				} else  {
		
					row[["value[oddsRatio]"]] <- exp(x.mode)
					row[["low[oddsRatio]"]] <- exp(x0)
					row[["up[oddsRatio]"]] <- exp(x1)
				}
			}
		}
		
	} else {

		row[["value[oddsRatio]"]] <- "."
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

