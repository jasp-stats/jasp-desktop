
TTestPairedSamples <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {

	if (is.null(dataset))
	{
		if (perform == "run") {
			dataset <- read.dataset.to.end()
		} else {
			dataset <- read.dataset.header()
		}
	}

	results <- list()

	ttest <- list()

	ttest[["title"]] <- "Paired Samples T-Test"

	fields <- list(
		list(name=".variable1", type="string", title=""),
		list(name=".separator", type="string", title=""),
		list(name=".variable2", type="string", title=""),
		list(name="t", type="number", format="sf:4"),
		list(name="df", type="number", format="sf:4"),
		list(name="p", type="number", format="dp:4;p:.001"))

	if(options$meanDifference){
		fields[[length(fields)+1]] <- list(name="mean difference", type="number", format="sf:4")
	}
	
	if(options$effectSize){
		fields[[length(fields)+1]] <- list(name="Cohen's d", type="number", format="sf:4")
	}
	
	if(options$confidenceInterval){
		fields[[length(fields)+1]] <- list(name="lower", type="number", format="sf:4")
		fields[[length(fields)+1]] <- list(name="upper", type="number", format="sf:4")
	}

	ttest[["schema"]] <- list(fields=fields)

	if (perform == "run")
	{
		ttest.results <- list()
		
		for (pair in options$pairs)
		{
			result <- try (silent = TRUE, expr = {
				
				c1 <- dataset[[ .v(pair[[1]]) ]]
				c2 <- dataset[[ .v(pair[[2]]) ]]
				
				ci <- options$confidenceIntervalInterval
				
				if (options$tails == "twoTailed")
					tail <- "two.sided"
				if (options$tails == "oneTailedGreaterThan")
					tail <- "greater"
				if (options$tails == "oneTailedLessThan")
					tail <- "less"
		
				r <- t.test(c1, c2, paired = TRUE, conf.level = ci, alternative = tail)
				
				t  <- .clean(as.numeric(r$statistic))
				df <- as.numeric(r$parameter)
				p  <- as.numeric(r$p.value)
				m  <- as.numeric(r$estimate)
				es <- .clean((mean(c1)-mean(c2))/(sqrt((sd(c1)^2+sd(c2)^2)/2)))
				
				ci.l <- as.numeric(r$conf.int[1])
				ci.u <- as.numeric(r$conf.int[2])
				
				if (options$tails == "oneTailedGreaterThan")
					ci.u = .clean(Inf)
				if (options$tails == "oneTailedLessThan")
					ci.l = .clean(-Inf)
				
				r <- list(.variable1=pair[[1]], .separator="-", .variable2=pair[[2]], t=t, df=df, p=p)
				
				if (options$meanDifference) {
				
					r[["mean difference"]] <- m
				}
				
				if (options$effectSize) {
				
					r[["Cohen's d"]] <- es
				}
				
				if(options$confidenceInterval) {
				
					r[["lower"]] <- ci.l
					r[["upper"]] <- ci.u
				}
				
				r
			})
			
			if (class(result) == "try-error"){
				result <- list(.variable1=pair[[1]], .separator="-", .variable2=pair[[2]], t="", df="", p="")
				
				if(options$meanDifference){
					result[["mean difference"]] <- ""
				}
				
				if(options$effectSize){
					result[["effect size"]] <- ""
				}
				
				if(options$confidenceInterval){
					result[["lower"]] <- ""
					result[["upper"]] <- ""
				}
			}
			
			ttest.results[[length(ttest.results)+1]] <- result
		}
		
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

