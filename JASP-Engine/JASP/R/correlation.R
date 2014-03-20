
Correlation <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {

	if (is.null(dataset))
	{
		if (perform == "run") {
			dataset <- read.dataset.to.end()
		} else {
			dataset <- read.dataset.header()
		}
	}

	results <- list()
	
	correlations <- list()

	tests <- c()
	if (options$pearson)
		tests <- c(tests, "pearson")
	if (options$spearman)
		tests <- c(tests, "spearman")
	if (options$kendallsTauB)
		tests <- c(tests, "kendall")

	if (length(tests) != 1) {
	
		correlations[["title"]] <- "Correlation Table"
	}
	else if (options$pearson) {
	
		correlations[["title"]] <- "Pearson Correlations"
	}
	else if (options$spearman) {
	
		correlations[["title"]] <- "Spearman Correlations"
	}
	else if (options$kendallsTauB) {
	
		correlations[["title"]] <- "Kendall's Tau"
	}
	else {
	
		correlations[["title"]] <- "Correlation Table"
	}
	
	fields <- list(list(name=".variable", title="", type="string"))
	rows <- list()
	v.c <- length(options$variables)
	
	if (v.c > 0) {
			
		test.names <- list(pearson="Pearson's R", spearman="Spearman's Rho", kendall="Kendall's Tau B")

		column.names <- c()

		for (test in tests) {
		
			if (length(tests) > 1 || options$reportSignificance) {
			
				column.name <- paste(".test[", test, "]", sep="")
				column.names[[length(column.names)+1]] <- column.name
				fields[[length(fields)+1]] <- list(name=column.name, title="", type="string")
			}
				
			for (variable.name in options$variables) {
			
				column.name <- paste(variable.name, "[", test, "]", sep="")
				column.names[[length(column.names)+1]] <- column.name
				fields[[length(fields)+1]] <- list(name=column.name, title=variable.name, type="number", format="dp:3")
			}
			
			if (options$reportSignificance) {
			
				column.name <- paste(".test[", test, "-p]", sep="")
				column.names[[length(column.names)+1]] <- column.name
				fields[[length(fields)+1]] <- list(name=column.name, title="", type="string")

				for (variable.name in options$variables) {
			
					column.name <- paste(variable.name, "[", test, "-p]", sep="")
					column.names[[length(column.names)+1]] <- column.name
					fields[[length(fields)+1]] <- list(name=column.name, title=variable.name, type="number", format="dp:3;p:.001")
				}								
			}
		}
		
		for (i in 1:v.c) {

			row <- list()
			row.footnotes <- list()
			
			variable.name <- options$variables[[i]]
			
			for (test in tests) {
			
				p.values <- list()

				if (length(tests) > 1 || options$reportSignificance)
					row[[length(row)+1]] <- test.names[[test]]
					
				if (options$reportSignificance)
					p.values[[length(p.values)+1]] <- "p-value"
			
				for (j in .seqx(1, i-1)) {
				
					row[[length(row)+1]] <- ""
					p.values[[length(p.values)+1]] <- ""
				}

				row[[length(row)+1]] <- "-"
				p.values[[length(p.values)+1]] <- ""

				for (j in .seqx(i+1, v.c)) {

					variable.2.name <- options$variables[[j]]
		
					if (perform == "run") {
				
						if (options$tails == "twoTailed") {

							result   <- cor.test(dataset[[variable.name]], dataset[[variable.2.name]], method=test, alternative="two.sided")
							estimate <- as.numeric(result$estimate)

							p.value  <- as.numeric(result$p.value)
							
						}
						else {
						
							result1 <- cor.test(dataset[[variable.name]], dataset[[variable.2.name]], method=test, alternative="less")
							result2 <- cor.test(dataset[[variable.name]], dataset[[variable.2.name]], method=test, alternative="greater")
							estimate <- as.numeric(result1$estimate)

							p.value  <- min(as.numeric(result1$p.value), as.numeric(result2$p.value))
						}
						
						row[[length(row)+1]] <- .clean(estimate)
						
						if (options$flagSignificant && is.na(p.value) == FALSE && p.value < .05) {
						
							column.name <- paste(variable.2.name, "[", test, "]", sep="")
							row.footnotes[[column.name]] <- list("*")
						}
						
						if (options$reportSignificance)
							p.values[[length(p.values)+1]] <- p.value
					
					} else {
				
						row[[length(row)+1]] <- "."
						p.values[[length(p.values)+1]] <- "."
						
					}
				}
				
				if (options$reportSignificance) {
				
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
	
	correlations[["schema"]] <- schema
	correlations[["data"]] <- rows
	
	if (options$flagSignificant) {
	
		if (options$tails == "twoTailed") {
		
			correlations[["footnotes"]] <- list(list(symbol="*", text="p < .05"))
			
		} else {
		
			correlations[["footnotes"]] <- list(list(symbol="*", text="p < .05, one tailed"))
		}
	}
	
	results[["correlations"]] <- correlations

	results
}

