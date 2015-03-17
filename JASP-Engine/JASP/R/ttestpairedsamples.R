
TTestPairedSamples <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {

	all.variables <- unique(unlist(options$pairs))
	all.variables <- all.variables[all.variables != ""]

	if (is.null(dataset))
	{
		if (perform == "run") {
		
			if (options$missingValues == "excludeListwise") {
			
				dataset <- .readDataSetToEnd(columns.as.numeric=all.variables, exclude.na.listwise=all.variables)
			}
			else {
			
				dataset <- .readDataSetToEnd(columns.as.numeric=all.variables)
			}
			
		} else {
		
			dataset <- .readDataSetHeader(columns.as.numeric=all.variables)
		}
	}

	results <- list()
	
	
	meta <- list()
	
	meta[[1]] <- list(name="title", type="title")
	meta[[2]] <- list(name="ttest", type="table")
	meta[[3]] <- list(name="descriptives", type="table")
	meta[[4]] <- list(name="normalityTests", type="table")
	
	results[[".meta"]] <- meta
	results[["title"]] <- "T-Test"
	

	ttest <- list()
	
	ttest[["title"]] <- "Paired Samples T-Test"

	fields <- list(
		list(name="v1",  type="string", title=""),
		list(name="sep", type="separator", title=""),
		list(name="v2",  type="string", title=""),
		list(name="t",   type="number", format="sf:4;dp:3"),
		list(name="df",  type="integer"),
		list(name="p",   type="number", format="dp:3;p:.001"))

	if(options$meanDifference){
		fields[[length(fields)+1]] <- list(name="md", title="Mean Difference", type="number", format="sf:4;dp:3")
		fields[[length(fields) + 1]] <- list(name="sed", title="SE Difference", type="number", format="sf:4;dp:3")	
	}
	
	if(options$effectSize){
		fields[[length(fields)+1]] <- list(name="Cohen's d", type="number", format="sf:4;dp:3")
	}
	
	if (options$confidenceInterval) {
	
		interval <- 100 * options$confidenceIntervalInterval
		title    <- paste(interval, "% Confidence Interval", sep="")
	
		fields[[length(fields)+1]] <- list(name="lowerCI", type="number", format="sf:4;dp:3", title=title, combineHeaders=TRUE)
		fields[[length(fields)+1]] <- list(name="upperCI", type="number", format="sf:4;dp:3", title=title, combineHeaders=TRUE)
	}

	ttest[["schema"]] <- list(fields=fields)

	ttest.results <- list()
	
	footnotes <- .newFootnotes()
	
	if (options$hypothesis == "groupOneGreater") {

		message <- paste("All tests, hypothesis is group one greater than group two", sep="")
		.addFootnote(footnotes, symbol="<em>Note.</em>", text=message)

	} else if (options$hypothesis == "groupTwoGreater") {

		message <- paste("All tests, hypothesis is group one less than group two", sep="")
		.addFootnote(footnotes, symbol="<em>Note.</em>", text=message)
	}

	for (pair in options$pairs)
	{
		row <- list(v1=pair[[1]], sep="-", v2=pair[[2]])
		
		if (perform == "run") {
		
			row.footnotes <- NULL
		
			if (pair[[1]] != "" && pair[[2]] != "") {

				c1 <- dataset[[ .v(pair[[1]]) ]]
				c2 <- dataset[[ .v(pair[[2]]) ]]
			
				ci <- options$confidenceIntervalInterval
			
				if (options$hypothesis == "groupsNotEqual")
					tail <- "two.sided"
				if (options$hypothesis == "groupOneGreater")
					tail <- "greater"
				if (options$hypothesis == "groupTwoGreater")
					tail <- "less"
	
				r <- try( { t.test(c1, c2, paired = TRUE, conf.level = ci, alternative = tail) } )
				
				if (class(r) != "try-error" && is.na(r$statistic) == FALSE) {
			
					t  <- .clean(as.numeric(r$statistic))
					df <- as.numeric(r$parameter)
					p  <- as.numeric(r$p.value)
					m  <- as.numeric(r$estimate)
					sed <- .clean(sd(c1-c2, na.rm = TRUE)/length(na.omit(c1-c2)))
					
					es <- .clean((mean(c1)-mean(c2))/(sqrt((sd(c1)^2+sd(c2)^2)/2)))
			        
					ci.l <- as.numeric(r$conf.int[1])
					ci.u <- as.numeric(r$conf.int[2])
			
					if (options$hypothesis == "groupOneGreater")
						ci.u <- .clean(Inf)
					if (options$hypothesis == "groupTwoGreater")
						ci.l <- .clean(-Inf)
						
				} else {
				
					if (class(r) != "try-error") {
					
						errorMessage <- "could not be calculated"
						
					} else {
				
						errorMessage <- .extractErrorMessage(r)
					}
					
					if (errorMessage == "missing value where TRUE/FALSE needed") {
					
						errorMessage <- "t-statistic is undefined - one or both of the variables contain infinity"
						
					} else if (errorMessage == "data are essentially constant") {
					
						errorMessage <- "t-statistic is undefined - one or both of the variables contain all the same value (zero variance)"
						
					} else if (errorMessage == "not enough 'x' observations") {
					
						errorMessage <- "t-statistic is undefined - one or both of the variables contain only one value"
						
					} else {
					
						errorMessage <- paste("t statistic is undefined - ", errorMessage)
					}
					
					index <- .addFootnote(footnotes, errorMessage)
				
					t  <- .clean(NaN)
					df <- ""
					p  <- ""
					m  <- ""
					sed <- ""
					es <- ""
			
					ci.l <- ""
					ci.u <- ""
					
					row.footnotes <- list(t=list(index))
				}
				
			}
			else {
			
				t  <- ""
				df <- ""
				p  <- ""
				m  <- ""
				sed <- ""
				es <- ""
			
				ci.l <- ""
				ci.u <- ""
			}
			
			row[["t"]]  <- t
			row[["df"]] <- df
			row[["p"]]  <- p
			
			if (options$meanDifference) {
			
				row[["md"]] <- m
				row[["sed"]] <- sed
			}
			
			if (options$effectSize) {
			
				row[["Cohen's d"]] <- es
			}
			
			if(options$confidenceInterval) {
			
				row[["lowerCI"]] <- ci.l
				row[["upperCI"]] <- ci.u
			}
			
			row[[".footnotes"]] <- row.footnotes
		}
		
		ttest.results[[length(ttest.results)+1]] <- row
	}
	
	ttest[["data"]] <- ttest.results
	
	ttest[["footnotes"]] <- as.list(footnotes)
	
	results[["ttest"]] <- ttest

	if (options$descriptives) {
	
		descriptives <- list()

		descriptives[["title"]] <- "Descriptives"

		fields <- list(
			list(name="v",    title="",     type="string"),
			list(name="N",                  type="integer"),
			list(name="mean", title="Mean", type="number", format="sf:4;dp:3"),
			list(name="sd",   title="SD",   type="number", format="sf:4;dp:3"),
			list(name="se",   title="SE",   type="number", format="sf:4;dp:3"))

		descriptives[["schema"]] <- list(fields=fields)
		
		descriptives.results <- list()
		
		desc.vars <- unique(unlist(options$pairs))
		desc.vars <- desc.vars[desc.vars != ""]
		
		for (var in desc.vars) {
		
			row <- list(v=var)

			if (perform == "run") {
				
				n   <- .clean(as.numeric(length(dataset[[ .v(var) ]])))
				m   <- .clean(as.numeric(  mean(dataset[[ .v(var) ]], na.rm = TRUE)))
				std <- .clean(as.numeric(    sd(dataset[[ .v(var) ]], na.rm = TRUE)))
				
				if (is.numeric(std)) {
					se <- .clean(as.numeric(std/sqrt(n)))}
				else
					se <- .clean(NaN)
					
				row[["N"]] <- n
				row[["mean"]] <- m
				row[["sd"]] <- std
				row[["se"]] <- se
			
			}
			
			descriptives.results[[length(descriptives.results)+1]] <- row
		}
		
		descriptives[["data"]] <- descriptives.results

		results[["descriptives"]] <- descriptives
	}
	
	if (options$normalityTests) {
	
	    normalityTests <- list()
	
		normalityTests[["title"]] <- "Test of Normality (Shapiro-Wilk)"
		normalityTests[["cases"]] <- I(options$variables)

		fields <- list(
			list(name="v1",  type="string", title=""),
		    list(name="sep", type="separator", title=""),
		    list(name="v2",  type="string", title=""),
			list(name="W", title="W", type="number",   format="sf:4;dp:3"),
			list(name="p", title="p", type="number", format="dp:3;p:.001"))

		normalityTests[["schema"]] <- list(fields=fields)
		normalityTests.results <- list()
		
		pairs <- options$pairs
		if (length(pairs) == 0) {
		    pairs[[1]] <- list(".", ".")
		}

		for (pair in pairs) {
		    			
			if (perform == "run" && length(options$pairs) > 0 && pair[[1]] != pair[[2]]) {
                    
                c1 <- dataset[[ .v(pair[[1]]) ]]
				c2 <- dataset[[ .v(pair[[2]]) ]]
                
				data <- na.omit(c1 - c2)

				if (class(data) != "factor") {
                    
                    r <- stats::shapiro.test(data)
                    
					W <- .clean(as.numeric(r$statistic))
					p <- .clean(r$p.value)
					
					if(length(normalityTests.results) == 0) {
			            newGroup <- TRUE   
			        } else {				
				        newGroup <- FALSE
			        }
					
					result <- list(v1=pair[[1]], sep="-", v2=pair[[2]], "W" = W, "p" = p, ".isNewGroup" = newGroup)
					
				} else {
			        
			        if(length(normalityTests.results) == 0) {
			            newGroup <- TRUE   
			        } else {				
				        newGroup <- FALSE
			        }
					
					result <- list(v1=pair[[1]], sep="-", v2=pair[[2]], "W" = "", "p" = "", ".isNewGroup" = newGroup)
					
				}
			
			} else {
			
			    if(length(normalityTests.results) == 0) {
			        newGroup <- TRUE   
			    } else {				
				    newGroup <- FALSE
			    }
			
				result <- list(v1=pair[[1]], sep="-", v2=pair[[2]], "W" = ".", p = ".", ".isNewGroup" = newGroup)			
			
			}
			
			normalityTests.results[[length(normalityTests.results)+1]] <- result
		}
		
		normalityTests[["data"]] <- normalityTests.results
		
		results[["normalityTests"]] <- normalityTests
	}
		
	results
}

