
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
	
	results[["correlations"]] <- .correlationTableBayesian(dataset, perform,
		variables=options$variables, pearson=options$pearson,
		kendallsTauB=options$kendallsTauB, spearman=options$spearman,
		hypothesis=options$hypothesis, reportSignificance=options$reportSignificance,
		flagSignificant=options$flagSignificant,
		meansAndStdDev=options$meansAndStdDev, crossProducts=options$crossProducts)

	results
}

.correlationTableBayesian <- function(dataset, perform, variables=c(), pearson=TRUE, kendallsTauB=FALSE,
	spearman=FALSE, hypothesis="correlated", reportSignificance=FALSE,
	flagSignificant=FALSE, meansAndStdDev=FALSE, crossProducts=FALSE) {
	
	correlation.table <- list()

	tests <- c()
	if (pearson)
		tests <- c(tests, "pearson")
	if (spearman)
		tests <- c(tests, "spearman")
	if (kendallsTauB)
		tests <- c(tests, "kendall")

	if (length(tests) != 1) {
	
		correlation.table[["title"]] <- "Correlation Table"
	}
	else if (pearson) {
	
		correlation.table[["title"]] <- "Pearson Correlations"
	}
	else if (spearman) {
	
		correlation.table[["title"]] <- "Spearman Correlations"
	}
	else if (kendallsTauB) {
	
		correlation.table[["title"]] <- "Kendall's Tau"
	}
	else {
	
		correlation.table[["title"]] <- "Correlation Table"
	}
	
	fields <- list(list(name=".variable", title="", type="string"))
	rows <- list()
	v.c <- length(variables)
	
	if (v.c > 0) {
			
		test.names <- list(pearson="Pearson's R", spearman="Spearman's Rho", kendall="Kendall's Tau B")

		column.names <- c()

		for (test in tests) {
		
			if (length(tests) > 1 || reportSignificance) {
			
				column.name <- paste(".test[", test, "]", sep="")
				column.names[[length(column.names)+1]] <- column.name
				fields[[length(fields)+1]] <- list(name=column.name, title="", type="string")
			}
				
			for (variable.name in variables) {
			
				column.name <- paste(variable.name, "[", test, "]", sep="")
				column.names[[length(column.names)+1]] <- column.name
				fields[[length(fields)+1]] <- list(name=column.name, title=variable.name, type="number", format="dp:3")
			}
			
			if (reportSignificance) {
			
				column.name <- paste(".test[", test, "-p]", sep="")
				column.names[[length(column.names)+1]] <- column.name
				fields[[length(fields)+1]] <- list(name=column.name, title="", type="string")

				for (variable.name in variables) {
			
					column.name <- paste(variable.name, "[", test, "-p]", sep="")
					column.names[[length(column.names)+1]] <- column.name
					fields[[length(fields)+1]] <- list(name=column.name, title=variable.name, type="number", format="dp:3;p:.001")
				}								
			}
		}
		
		for (i in 1:v.c) {

			row <- list()
			row.footnotes <- list()
			
			variable.name <- variables[[i]]
			
			for (test in tests) {
			
				p.values <- list()

				if (length(tests) > 1 || reportSignificance)
					row[[length(row)+1]] <- test.names[[test]]
					
				if (reportSignificance)
					p.values[[length(p.values)+1]] <- "p-value"
			
				for (j in .seqx(1, i-1)) {
				
					row[[length(row)+1]] <- ""
					p.values[[length(p.values)+1]] <- ""
				}

				row[[length(row)+1]] <- "\u2014" # em-dash
				p.values[[length(p.values)+1]] <- ""

				for (j in .seqx(i+1, v.c)) {

					variable.2.name <- variables[[j]]
		
					if (perform == "run") {
				
						if (hypothesis == "correlated") {

							result   <- cor.test(dataset[[ .v(variable.name) ]], dataset[[ .v(variable.2.name) ]], method=test, alternative="two.sided")
							estimate <- as.numeric(result$estimate)

							p.value  <- as.numeric(result$p.value)
							
						}
						else {
						
							result1 <- cor.test(dataset[[ .v(variable.name) ]], dataset[[ .v(variable.2.name) ]], method=test, alternative="less")
							result2 <- cor.test(dataset[[ .v(variable.name) ]], dataset[[ .v(variable.2.name) ]], method=test, alternative="greater")
							estimate <- as.numeric(result1$estimate)

							p.value  <- min(as.numeric(result1$p.value), as.numeric(result2$p.value))
						}
						
						row[[length(row)+1]] <- .clean(estimate)
						
						if (flagSignificant && is.na(p.value) == FALSE && p.value < .05) {
						
							column.name <- paste(variable.2.name, "[", test, "]", sep="")
							row.footnotes[[column.name]] <- list(0)
						}
						
						if (reportSignificance)
							p.values[[length(p.values)+1]] <- p.value
					
					} else {
				
						row[[length(row)+1]] <- "."
						p.values[[length(p.values)+1]] <- "."
						
					}
				}
				
				if (reportSignificance) {
				
					for (p.value in p.values)
						row[[length(row)+1]] <- p.value
				}
			
			}
	
			names(row) <- column.names
			row[[".variable"]] <- variable.name
			
			if (length(row.footnotes) > 0)
				row[[".footnotes"]] <- row.footnotes
	
			rows[[i]] <- row
		}
	}
	
	schema <- list(fields=fields)
	
	correlation.table[["schema"]] <- schema
	correlation.table[["data"]] <- rows
	
	if (flagSignificant) {
	
		if (hypothesis == "correlated") {
		
			correlation.table[["footnotes"]] <- list(list(symbol="*", text="p < .05"))
			
		} else {
		
			correlation.table[["footnotes"]] <- list(list(symbol="*", text="p < .05, one tailed"))
		}
	}
	
	correlation.table
}
