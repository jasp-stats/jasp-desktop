
TTestBayesianPairedSamples <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {

	all.variables <- unique(unlist(options$pairs))
	all.variables <- all.variables[all.variables != ""]

	if (is.null(dataset))
	{
		if (perform == "run") {
		
			if (options$missingValues == "excludeListwise") {
		
				dataset <- .readDataSetToEnd(columns.as.numeric=all.variables, exclude.na.listwise=all.variables)
			
			} else {
		
				dataset <- .readDataSetToEnd(columns.as.numeric=all.variables)
			}
			
		} else {
		
			dataset <- .readDataSetHeader(columns.as.numeric=all.variables)
		}
	}

	results <- list()
	
	
	meta <- list()
	
	meta[[1]] <- list(name="ttest", type="table")
	meta[[2]] <- list(name="inequalityOfVariances", type="table")
	meta[[3]] <- list(name="descriptives", type="table")
	
	results[[".meta"]] <- meta
	
	

	ttest <- list()

	ttest[["title"]] <- "Bayesian Paired Samples T-Test"
	
	bf.type <- options$bayesFactorType
	
	if (bf.type == "BF10") {
		bf.title <- "BF\u2081\u2080"
	} else {
		bf.title <- "BF\u2080\u2081"
	}

	fields <- list(
		list(name=".variable1", type="string", title=""),
		list(name=".separator", type="string", title=""),
		list(name=".variable2", type="string", title=""),
		list(name="BF", type="number", format="sf:4;dp:3", title=bf.title),
		list(name="error", type="number", format="sf:4;dp:3", title="error %"))

	ttest[["schema"]] <- list(fields=fields)

	ttest.rows <- list()
	footnotes <- .newFootnotes()
	
	for (pair in options$pairs)
	{
		if (pair[[1]] == "" || pair[[2]] == "") {
		
			p1 <- ifelse(pair[[1]] != "", pair[[1]], "...") 
			p2 <- ifelse(pair[[2]] != "", pair[[2]], "...")
		
			result <- list(.variable1=p1, .separator="-", .variable2=p2, BF="", error="")
			
		} else {

			if (perform == "run") {

				result <- try (silent = TRUE, expr = {

					subDataSet <- subset(dataset, select=c(.v(pair[[1]]), .v(pair[[2]])) )
					subDataSet <- na.omit(subDataSet)
			
					c1 <- subDataSet[[ .v(pair[[1]]) ]]
					c2 <- subDataSet[[ .v(pair[[2]]) ]]
	
					r <- BayesFactor::ttestBF(c1, c2, paired = TRUE, r=options$priorWidth)
					
					bf.raw <- exp(as.numeric(r@bayesFactor$bf))
					if (bf.type == "BF01")
						bf.raw <- 1 / bf.raw
			
					BF <- .clean(bf.raw)
					error <- .clean(as.numeric(r@bayesFactor$error))
			
					list(.variable1=pair[[1]], .separator="-", .variable2=pair[[2]], BF=BF, error=error)
			
				})
		
				if (class(result) == "try-error") {
				
					errorMessage <- .extractErrorMessage(result)
					
					if (errorMessage == "x or y must not contain missing or infinite values.") {
				
						errorMessage <- paste("BayesFactor is undefined - one or both of the variables contain infinity")
					
					#} else if (errorMessage == "data are essentially constant") {
					#				
					#	errorMessage <- paste("BayesFactor is undefined - the sample contains all the same value (the variance is zero)")
					#
					} else if (errorMessage == "Insufficient sample size for t analysis." || errorMessage == "not enough observations") {
					
						errorMessage <- "BayesFactor is undefined - one or both of the variables has too few observations (possibly only after missing values are excluded)"	
					}

					index <- .addFootnote(footnotes, errorMessage)

					result <- list(.variable1=pair[[1]], .separator="-", .variable2=pair[[2]], BF=.clean(NaN), error="", .footnotes=list(BF=list(index)))
				}
			
			} else {
			
				result <- list(.variable1=pair[[1]], .separator="-", .variable2=pair[[2]], BF=".", error=".")
			}
			
		}
		
		ttest.rows[[length(ttest.rows)+1]] <- result
		
		ttest[["data"]] <- ttest.rows
		ttest[["footnotes"]] <- as.list(footnotes)

	}

	if (options$descriptives) {
	
		descriptives <- list()

		descriptives[["title"]] <- "Descriptives"

		fields <- list(
			list(name=".variable", type="string", title=""),
			list(name="N", type="integer"),
			list(name="mean", type="number", format="sf:4"),
			list(name="sd", type="number", format="dp:4;p:.001"),
			list(name="SE", type="number", format="dp:4;p:.001"))

		descriptives[["schema"]] <- list(fields=fields)

		if (perform == "run") {
		
			descriptives.results <- list()
			
			variables <- NULL
			
			for (pair in options$pairs) {	

				for (i in 1:2) {

					result <- try (silent = TRUE, expr = {
						
						n <- .clean(as.numeric(length(dataset[[ .v(pair[[i]]) ]])))
						m <- .clean(as.numeric(mean(dataset[[ .v(pair[[i]]) ]], na.rm = TRUE)))
						std <- .clean(as.numeric(sd(dataset[[ .v(pair[[i]]) ]], na.rm = TRUE)))
						if(is.numeric(std)){
							se <- .clean(as.numeric(std/sqrt(n)))}
						else
							se <- .clean(NaN)
										
						list(.variable=pair[[i]], N=n, mean=m, sd=std, SE=se)
					})
					
					if (class(result) == "try-error") {
					
						result <- list(.variable=pair[[i]], N="", mean="", sd="", SE="")
					}
					
					if(is.na(match(pair[[i]],variables))){
						descriptives.results[[length(descriptives.results)+1]] <- result
						variables <- c(variables,pair[[i]])
					}				
				}
			}
			descriptives[["data"]] <- descriptives.results

		}
		results[["descriptives"]] <- descriptives
	}
	
	results[["ttest"]] <- ttest
	
		
	results
}

