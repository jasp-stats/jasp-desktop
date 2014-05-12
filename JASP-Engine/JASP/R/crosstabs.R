
.crosstab <- function(dataset, perform, analysis) {

	# analysis is a list of the form :
	# list(columns="var.name", rows="var.name", "Layer 1"="var.name", etc...)

	a.factor.contains.no.levels <- FALSE

	lvls <- c()
	if (is.factor( dataset[[ .v(analysis$rows) ]] )) {
	
		lvls <- levels(dataset[[ .v(analysis$rows) ]])
		
	} else if (perform == "run") {
	
		lvls <- unique(dataset[[ .v(analysis$rows) ]])
	}
	
	if (length(lvls) == 0) {
	
		lvls = c(".")
		a.factor.contains.no.levels <- TRUE
	}
		
	cases <- data.frame(lvls, stringsAsFactors=FALSE)
	names(cases) <- analysis$rows
	
	if (length(analysis) > 2)
	{
		for (j in 3:length(analysis))
		{
			var.name <- analysis[[j]]
		
			lvls <- c()
			if (is.factor(dataset[[ .v(var.name) ]])) {
			
				lvls <- levels(dataset[[ .v(var.name) ]])
			}
			else if (perform == "run") {
			
				lvls <- unique(dataset[[ .v(var.name) ]] )
			}
			
			if (length(lvls) == 0) {
			
				lvls = c(".")
				a.factor.contains.no.levels <- TRUE
			}
			
			cases <- cbind(rep(lvls, each=dim(cases)[1]), cases, stringsAsFactors=FALSE)
			names(cases)[1] <- var.name
		}
	}
	
	cases <- .dataFrameToRowList(cases)

	tables <- list()

	table <- list()
	
	table[["title"]] <- "Crosstabs"

	fields <- list()
	
	if (length(analysis) > 2) {
	
		for (j in length(analysis):3)
			fields[[length(fields)+1]] <- list(name=analysis[[j]], type="string", combine=TRUE)
	}
				
	fields[[length(fields)+1]] <- list(name=analysis$rows, type="string")
	
	lvls <- c()
	if (is.factor(dataset[[ .v(analysis$columns) ]] )) {

		lvls <- levels(dataset[[ .v(analysis$columns) ]])

	} else if (perform == "run") {
	
		lvls <- unique(dataset[[ .v(analysis$columns) ]])
	}
	
	if (length(lvls) == 0) {
	
		lvls <- c(".")
		a.factor.contains.no.levels <- TRUE
	}
	
	for (column.name in lvls) {

		private.name <- paste(".", column.name, sep="")
		fields[[length(fields)+1]] <- list(name=private.name, title=column.name, type="number")
	}
	
	if (perform == "run" && a.factor.contains.no.levels == FALSE) {

		for (i in .indices(cases)) {

			caze <- cases[[i]]
			
			ss.filter.string <- paste(.v(names(caze)), "==\"", caze, "\"", sep="", collapse="&")
			ss.expression <- parse(text=ss.filter.string)
			
			ss <- subset(dataset, select=.v(analysis$columns), subset=eval(ss.expression))
			
			t <- base::table(ss)

			for (n in names(t)) {
			
				private.name <- paste(".", n, sep="")
				caze[[private.name]] <- unname(t[n])
			}

			cases[[i]] <- caze
		}
	
	}
	
	schema <- list(fields=fields)

	table[["schema"]] <- schema
	table[["data"]] <- cases

	tables[[1]] <- table
	
	tables
}

Crosstabs <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {

	layer.variables <- c()

	for (layer in options$layers)
		layer.variables <- c(layer.variables, unlist(layer$variables))

	all.variables = c(unlist(options$rows), unlist(options$columns), layer.variables)

	if (is.null(dataset))
	{
		if (perform == "run") {
			dataset <- read.dataset.to.end(columns.as.factor=all.variables)
		} else {
			dataset <- read.dataset.header(columns.as.factor=all.variables)
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
			tables <- .crosstab(dataset, perform, analysis)
			for (table in tables)
				crosstabs[[length(crosstabs)+1]] <- table				
		}
	
	} else {
	
		crosstabs[[1]] <- list(title = "Crosstabs", cases = list(), schema = list(fields=list()))
	}

	results[["crosstabs"]] <- crosstabs

	results
}

