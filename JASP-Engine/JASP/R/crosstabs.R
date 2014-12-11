
.crosstab <- function(dataset, options, perform, analysis) {

	# analysis is a list of the form :
	# list(columns="var.name", rows="var.name", "Layer 1"="var.name", etc...)
	
	dataset <- subset(dataset, select=.v(unlist(analysis)))
	
	
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
	
	counts.table[["title"]] <- "Crosstabs"
	
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
	
	tests.table[["title"]] <- "Crosstabs Tests"
	
	tests.fields <- fields
	
	tests.fields[[length(tests.fields)+1]] <- list(name="type[N]", title="", type="string")
	tests.fields[[length(tests.fields)+1]] <- list(name="value[N]", title="Value", type="integer")

	if (options$chiSquare) {
	
		tests.fields[[length(tests.fields)+1]] <- list(name="type[chiSquare]", title="", type="string")
		tests.fields[[length(tests.fields)+1]] <- list(name="value[chiSquare]", title="Value", type="number", format="sf:4;dp:3")
	}
	
	schema <- list(fields=tests.fields)
	
	tests.table[["schema"]] <- schema
	
	


	# POPULATE TABLES

	# create count matrices for each group

	group.matrices <- .crosstabsCreateGroupMatrices(dataset, .v(analysis$rows), .v(analysis$columns), groups)
	
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
		
		next.rows <- .crosstabsCreateTestsRows(analysis$rows, group.matrix, tests.footnotes, options, perform, group)
		tests.rows <- c(tests.rows, next.rows)
	}

	counts.table[["data"]] <- counts.rows
	
	tests.table[["data"]] <- tests.rows
	tests.table[["footnotes"]] <- as.list(tests.footnotes)

	tables[[1]] <- counts.table
	tables[[2]] <- tests.table
	
	tables
}

.crosstabsCreateTestsRows <- function(var.name, counts.matrix, footnotes, options, perform, group=NULL) {

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
	
	
	if (options$chiSquare) {
	
		row[["type[chiSquare]"]] <- "\u03A7\u00B2"

		if (perform == "run") {
		
			chi.result <- try({

				chi.result <- stats::chisq.test(counts.matrix)
			})
			
			if (class(chi.result) == "try-error") {

				row[["value[chiSquare]"]] <- .clean(NaN)
				
				error <- .extractErrorMessage(chi.result)
				
				if (error == "at least one entry of 'x' must be positive")
					error <- "\u03A7\u00B2 could not be calculated, contains no observations"
				
				sup   <- .addFootnote(footnotes, error)
				row[[".footnotes"]] <- list("value[chiSquare]"=list(sup))
			
			} else if (is.na(chi.result$statistic)) {
			
				row[["value[chiSquare]"]] <- .clean(NaN)
			
				sup <- .addFootnote(footnotes, "\u03A7\u00B2 could not be calculated")
				row[[".footnotes"]] <- list("value[chiSquare]"=list(sup))
			
			} else {
			
				row[["value[chiSquare]"]] <- unname(chi.result$statistic)
			}
			
		} else {
		
			row[["value[chiSquare]"]] <- "."
		}
		
	}

	list(row)
}

.crosstabsCreateCountsRows <- function(var.name, counts.matrix, options, perform, group=NULL) {

	rows <- list()

	for (i in 1:dim(counts.matrix)[[1]]) {
	
		if (perform == "run") {

			row <- as.list(counts.matrix[i,])
			names(row) <- paste(".", names(row), sep="")
		
		} else {
		
			row <- list()
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

		if (i == 1) {

			row[[".isNewGroup"]] <- TRUE
		}
		
		rows[[length(rows)+1]] <- row
	}
	
	if (perform == "run") {

		row <- apply(counts.matrix, 2, base::sum)
		row <- as.list(row)
		names(row) <- paste(".", names(row), sep="")
		
	} else {
	
		row <- list()
	}
	
	row[[var.name]] <- "Total"
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

.crosstabsCreateGroupMatrices <- function(dataset, rows, columns, groups, counts=NULL) {

	# this creates count matrices for each of the groups

	matrices <- list()

	if (is.null(groups)) {

		ss.dataset <- base::subset(dataset, select=c(rows, columns))	
		ss.table <- base::table(ss.dataset)
		ss.matrix <- base::matrix(ss.table, nrow=dim(ss.table)[1], ncol=dim(ss.table)[2], dimnames=dimnames(ss.table))
		
		matrices[[1]] <- ss.matrix
	
	} else {
	
		for (group in groups) {
		
			group <- group[group != ""]

			if (length(group) == 0) {
			
				ss.dataset <- base::subset(dataset, select=c(rows, columns))
			
			} else {

				ss.filter.string <- base::paste(.v(names(group)), "==\"", group, "\"", sep="", collapse="&")
				ss.expression    <- base::parse(text=ss.filter.string)
				ss.dataset       <- base::subset(dataset, select=c(rows, columns), subset=eval(ss.expression))
			}
			
			ss.table  <- base::table(ss.dataset)
			ss.matrix <- base::matrix(ss.table, nrow=dim(ss.table)[1], ncol=dim(ss.table)[2], dimnames=dimnames(ss.table))

			matrices[[length(matrices)+1]] <- ss.matrix
		}
	}
	
	matrices
}

Crosstabs <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {

	layer.variables <- c()

	for (layer in options$layers)
		layer.variables <- c(layer.variables, unlist(layer$variables))

	all.variables = c(unlist(options$rows), unlist(options$columns), layer.variables)

	if (is.null(dataset))
	{
		if (perform == "run") {
			dataset <- .readDataSetToEnd(columns.as.factor=all.variables)
		} else {
			dataset <- .readDataSetHeader(columns.as.factor=all.variables)
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
			tables <- .crosstab(dataset, options, perform, analysis)
			for (table in tables)
				crosstabs[[length(crosstabs)+1]] <- table				
		}
	
	} else {
	
		crosstabs[[1]] <- list(title = "Crosstabs", cases = list(), schema = list(fields=list()))
	}

	results[["crosstabs"]] <- crosstabs

	results
}

