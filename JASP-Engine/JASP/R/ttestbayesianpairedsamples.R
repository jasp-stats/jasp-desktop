
TTestBayesianPairedSamples <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {

	all.variables <- unique(unlist(options$pairs))
	all.variables <- all.variables[all.variables != ""]

	if (is.null(dataset))
	{
		if (perform == "run") {
			dataset <- read.dataset.to.end(columns.as.numeric=all.variables)
		} else {
			dataset <- read.dataset.header(columns.as.numeric=all.variables)
		}
	}

	results <- list()

	ttest <- list()

	ttest[["title"]] <- "Bayesian Paired Samples T-Test"

	fields <- list(
		list(name=".variable1", type="string", title=""),
		list(name=".separator", type="string", title=""),
		list(name=".variable2", type="string", title=""),
		list(name="BF", type="number", format="sf:4", title="BF<sub>10</sub>"),
		list(name="error", type="number", format="sf:4"))

	ttest[["schema"]] <- list(fields=fields)

	ttest.results <- list()
	
	for (pair in options$pairs)
	{
		if (pair[[1]] == "" || pair[[2]] == "") {
		
			p1 <- ifelse(pair[[1]] != "", pair[[1]], "...") 
			p2 <- ifelse(pair[[2]] != "", pair[[2]], "...")
		
			result <- list(.variable1=p1, .separator="-", .variable2=p2, BF="", error="")
			
		} else {

			if (perform == "run") {

				result <- try (silent = TRUE, expr = {
			
					c1 <- dataset[[ .v(pair[[1]]) ]]
					c2 <- dataset[[ .v(pair[[2]]) ]]
	
					r <- BayesFactor::ttestBF(c1, c2, paired = TRUE)
			
					BF <- .clean(exp(as.numeric(r@bayesFactor$bf)))
					error <- .clean(as.numeric(r@bayesFactor$error))
			
					list(.variable1=pair[[1]], .separator="-", .variable2=pair[[2]], BF=BF, error=error)
			
				})
		
				if (class(result) == "try-error") {
			
					result <- list(.variable1=pair[[1]], .separator="-", .variable2=pair[[2]], BF="", error="")
				}
			
			} else {
			
				result <- list(.variable1=pair[[1]], .separator="-", .variable2=pair[[2]], BF=".", error=".")
			}
			
		}
		
		ttest.results[[length(ttest.results)+1]] <- result
		
		ttest[["data"]] <- ttest.results

	}

	if (options$descriptives) {
	
		descriptives <- list()

		descriptives[["title"]] <- "Descriptives"

		fields <- list(
			list(name=".variable", type="string", title=""),
			list(name="N", type="number", format="sf:4"),
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
							se <- "NaN"
										
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

