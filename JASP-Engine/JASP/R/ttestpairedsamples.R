
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

	cases <- list()

	for (pair in options$pairs)
	{
		if (pair[[1]] == "")
			pair[[1]] <- "..."
		if (pair[[2]] == "")
			pair[[2]] <- "..."
			
		cases[[length(cases)+1]] <- paste(pair[[1]], "-", pair[[2]])
	}

	ttest[["cases"]] <- cases

	fields <- list(
		list(id="t", type="number", format="sf:4"),
		list(id="df", type="number", format="sf:4"),
		list(id="p", type="number", format="dp:4;p:.001"))

	if(options$meanDifference){
		fields[[length(fields)+1]] <- list(id="mean difference", type="number", format="sf:4")
	}
	
	if(options$effectSize){
		fields[[length(fields)+1]] <- list(id="Cohen's d", type="number", format="sf:4")
	}
	
	if(options$confidenceInterval){
		fields[[length(fields)+1]] <- list(id="lower", type="number", format="sf:4")
		fields[[length(fields)+1]] <- list(id="upper", type="number", format="sf:4")
	}

	ttest[["schema"]] <- list(fields=fields)

	if (perform == "run")
	{
		ttest.results <- list()
		
		for (pair in options$pairs)
		{
			result <- try (silent = TRUE, expr = {
				
				c1 <- dataset[[ pair[[1]] ]]
				c2 <- dataset[[ pair[[2]] ]]
				
				ci <- options$confidenceIntervalInterval
				
				if(options$tails=="twoTailed"){
					tail <- "two.sided"}
				if(options$tails=="oneTailedGreaterThan"){
					tail <- "greater"}
				if(options$tails=="oneTailedLessThan"){
					tail <- "less"}
		
				r <- t.test(c1, c2, paired = TRUE, conf.level = ci, alternative = tail)
				r2 <- t.test(c1, c2, paired = TRUE, conf.level = ci, alternative = "two.sided")
				
				t <- .clean(as.numeric(r$statistic))
				df <- as.numeric(r$parameter)
				p <- as.numeric(r$p.value)
				m <- as.numeric(r$estimate)
				es <- .clean((mean(c1)-mean(c2))/(sqrt((sd(c1)^2+sd(c2)^2)/2)))
				ci.l <- as.numeric(r2$conf.int[1])
				ci.u <- as.numeric(r2$conf.int[2])
				
				r <- list(t=t, df=df, p=p)
				if(options$meanDifference){
					r[["mean difference"]] <- m
				}
				
				if(options$effectSize){
					r[["Cohen's d"]] <- es
				}
				
				if(options$confidenceInterval){
					r[["lower"]] <- ci.l
					r[["upper"]] <- ci.u
				}
				r
			})
			
			if (class(result) == "try-error"){
				result <- list(t="", df="", p="")
				
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

	if(options$descriptives){
	
		descriptives <- list()

		descriptives[["title"]] <- "Descriptives"

		cases <- list()
			
		for (pair in options$pairs)
		{
			if (pair[[1]] == "")
				pair[[1]] <- "..."
			if (pair[[2]] == "")
				pair[[2]] <- "..."

			for(i in 1:2){
				if(is.na(match(pair[[i]],cases))){
					cases[[length(cases)+1]] <- pair[[i]]
				}			
			}
		}

		descriptives[["cases"]] <- cases

		fields <- list(
			list(id="N", type="number", format="sf:4"),
			list(id="mean", type="number", format="sf:4"),
			list(id="sd", type="number", format="dp:4;p:.001"),
			list(id="SE", type="number", format="dp:4;p:.001"))

		descriptives[["schema"]] <- list(fields=fields)

		if (perform == "run")
		{
			descriptives.results <- list()
			
			variables <- NULL
			
			for (pair in options$pairs)
			{	
				for (i in 1:2){
					result <- try (silent = TRUE, expr = {
						
						n <- .clean(as.numeric(length(dataset[[ pair[[i]] ]])))
						m <- .clean(as.numeric(mean(dataset[[ pair[[i]] ]], na.rm = TRUE)))
						std <- .clean(as.numeric(sd(dataset[[ pair[[i]] ]], na.rm = TRUE)))
						if(is.numeric(std)){
							se <- .clean(as.numeric(std/sqrt(n)))}
						else
							se <- "NaN"
										
						list(N=n, mean=m, sd=std, SE=se)
					})
					
					if (class(result) == "try-error"){
						result <- list(N="", mean="", sd="", SE="")
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

