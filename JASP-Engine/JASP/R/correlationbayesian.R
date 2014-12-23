
CorrelationBayesian <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {

	if (is.null(dataset))
	{
		if (perform == "run") {
		
			if (options$missingValues == "excludeListwise") {
		
				dataset <- .readDataSetToEnd(columns.as.numeric=options$variables, exclude.na.listwise=options$variables)
				
			} else {

				dataset <- .readDataSetToEnd(columns.as.numeric=options$variables)
			}
			
		} else {
		
			dataset <- .readDataSetHeader(columns.as.numeric=options$variables)
		}
	}

	results <- list()
	
	
	meta <- list(
		list(name="correlations", type="table"))
	
	results[[".meta"]] <- meta
	
	results[["correlations"]] <- .correlationTableBayesian(dataset, perform, variables=options$variables)

	results
}

.correlationTableBayesian <- function(dataset, perform, variables) {
	
	correlation.table <- list()

	correlation.table[["title"]] <- "Correlation Table"
	
	fields <- list(list(name=".variable", title="", type="string"))
	rows <- list()
	v.c <- length(variables)  # variable count
	
	if (v.c > 0) {

		column.names <- c()
			
		for (variable.name in variables) {
		
			column.names[[length(column.names)+1]] <- variable.name
			fields[[length(fields)+1]] <- list(name=variable.name, title=variable.name, type="number", format="dp:3")
		}
		
		for (i in 1:v.c) {

			row <- list()
			row.footnotes <- list()
			
			variable.name <- variables[[i]]
			
			for (j in .seqx(1, i-1)) {         # fill in blanks in table
			
				variable.2.name <- variables[[j]]
				row[[variable.2.name]] <- ""
			}
			
			row[[".variable"]] <- variable.name

			row[[variable.name]] <- "\u2014" # em-dash

			for (j in .seqx(i+1, v.c)) {

				variable.2.name <- variables[[j]]
	
				if (perform == "run") {

					result <- cor.test(dataset[[ .v(variable.name) ]], dataset[[ .v(variable.2.name) ]], method="pearson", alternative="two.sided")
					
					row[[variable.2.name]] <- .clean(unname(result$estimate))
				
				} else {
			
					row[[variable.2.name]] <- "."
				}
			}
	
			rows[[i]] <- row
		}
	}
	
	schema <- list(fields=fields)
	
	correlation.table[["schema"]] <- schema
	correlation.table[["data"]] <- rows
	
	correlation.table
}
