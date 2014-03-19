
Crosstabs <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {

	if (is.null(dataset))
	{
		if (perform == "run") {
			dataset <- read.dataset.to.end()
		} else {
			dataset <- read.dataset.header()
		}
	}

	results <- list()
	
	crosstabs <- list()

	if (length(options$rows) > 0 && length(options$columns) > 0 && perform == "run")
	{
		rows <- as.vector(options$rows, "character")
		columns <- as.vector(options$columns, "character")

		analyses <- data.frame(".columns"=columns, stringsAsFactors=FALSE)
		analyses <- cbind(analyses, ".rows"=rep(rows, each=dim(analyses)[1]), stringsAsFactors=FALSE)
		
		for (layer in options$layers)
		{
			layer.vars <- as.vector(layer$variables, "character")
			analyses <- cbind(analyses, rep(layer.vars, each=dim(analyses)[1]), stringsAsFactors=FALSE)
			names(analyses)[dim(analyses)[2]] <- layer$name
		}
		
		analyses <- .dataFrameToRowList(analyses)

		for (analysis in analyses)
		{
			lvls <- levels(dataset[[ analysis$.rows ]])
			if (is.null(lvls))
				lvls <- levels(factor(dataset[[ analysis$.rows ]]))
				
			cases <- data.frame(".rows"=lvls, stringsAsFactors=FALSE)
			
			if (length(analysis) > 2)
			{
				for (j in 3:length(analysis))
				{
					var.name <- analysis[[j]]
				
					lvls <- levels(dataset[[ var.name ]])
					if (is.null(lvls))
						lvls <- levels(factor(dataset[[ var.name ]]))
					
					cases <- cbind(rep(lvls, each=dim(cases)[1]), cases, stringsAsFactors=FALSE)
					names(cases)[dim(cases)[1]] <- var.name
				}
			}
			
			cases <- .dataFrameToRowList(cases, discard.column.names=TRUE)

			table <- list()
			
			table[["title"]] <- "Crosstabs"

			table[["cases"]] <- cases
	
			fields <- list(
				list(id=analysis$.rows))
			
			if (length(analysis) > 2)
			{
				for (j in 3:length(analysis))
					fields[[length(fields)+1]] <- list(id=analysis[[j]])
			}
		
			schema <- list(fields=fields)
	
			table[["schema"]] <- schema			
			
			crosstabs[[length(crosstabs)+1]] <- table				
		}
	

	
	} else {
	
		crosstabs[[1]] <- list(title = "Crosstabs", cases = list(), schema = list(fields=list()))
	}

	results[["crosstabs"]] <- crosstabs

	results
}

