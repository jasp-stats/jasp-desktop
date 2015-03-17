TTestIndependentSamples <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {

	dependents <- unlist(options$variables)
	
	grouping   <- options$groupingVariable
	if (grouping == "")
		grouping <- NULL

	if (is.null(dataset))
	{
		if (perform == "run") {
		
			if (options$missingValues == "excludeListwise") {
		
				dataset <- .readDataSetToEnd(columns.as.numeric=dependents, columns.as.factor=grouping, exclude.na.listwise=dependents)
			
			} else {
		
				dataset <- .readDataSetToEnd(columns.as.numeric=dependents, columns.as.factor=grouping)
			}
		

		} else {
		
			dataset <- .readDataSetHeader(columns.as.numeric=dependents, columns.as.factor=grouping)
		}
	}

	results <- list()
	

	#### META
	
	meta <- list()
	
	meta[[1]] <- list(name="title", type="title")
	meta[[2]] <- list(name="ttest", type="table")
	meta[[3]] <- list(name="inequalityOfVariances", type="table")
	meta[[4]] <- list(name="descriptives", type="table")
	meta[[5]] <- list(name="normalityTests", type="table")
	
	results[[".meta"]] <- meta
	results[["title"]] <- "T-Test"
	

	results[["ttest"]] <- .ttestIndependentSamplesTTest(dataset, options, perform)
	results[["descriptives"]] <- .ttestIndependentSamplesDescriptives(dataset, options, perform)
	results[["inequalityOfVariances"]] <- .ttestIndependentSamplesInequalityOfVariances(dataset, options, perform)
	results[["normalityTests"]] <- .ttestNormalityTests(dataset, options, perform)

	results
}

.ttestNormalityTests <- function(dataset, options, perform) {

	if (options$normalityTests == FALSE)
		return(NULL)
		
    normalityTests <- list()

    normalityTests[["title"]] <- "Test of Normality (Shapiro-Wilk)"

    fields <- list(
        list(name="dep",  type="string", title="", combine=TRUE),
        list(name="lev",  type="string", title=""),
        list(name="W", title="W", type="number",   format="sf:4;dp:3"),
        list(name="p", title="p", type="number", format="dp:3;p:.001"))

    normalityTests[["schema"]] <- list(fields=fields)
    
    footnotes <- .newFootnotes()
    .addFootnote(footnotes, symbol="<em>Note.</em>", text="Significant results indicate a deviation from normality")
    
    normalityTests.results <- list()
    
	variables <- options$variables
	if (length(variables) == 0)
		variables = "."

    for (variable in variables) {
        
        factor <- options$groupingVariable
        levels <- levels(dataset[[ .v(factor) ]])
        
        for(level in levels) {
                    
            if (perform == "run" && length(options$variables) > 0) {
                
                data <- na.omit(dataset[[.v(variable)]][dataset[[.v(factor)]] == level])
                
                if (class(data) != "factor") {
                
                    r <- stats::shapiro.test(data)
                
                    W <- .clean(as.numeric(r$statistic))
                    p <- .clean(r$p.value)
                
                    if(level == levels[1]) {
                        newGroup <- TRUE   
                    } else {				
                        newGroup <- FALSE
                    }
                
                    result <- list("dep"=variable, "lev"=level, "W" = W, "p" = p, ".isNewGroup" = newGroup)
                
                } else {
                
                    if(level == levels[1]) {
                        newGroup <- TRUE   
                    } else {				
                        newGroup <- FALSE
                    }
                
                    result <- list("dep"=variable, "lev"=level, "W" = "", "p" = "", ".isNewGroup" = newGroup)
                
                }
        
            } else {
        
                if(level == levels[1]) {
                    newGroup <- TRUE   
                } else {				
                    newGroup <- FALSE
                }
        
                result <- list("dep"=variable, "lev"=level, "W" = ".", "p" = ".", ".isNewGroup" = newGroup)
        
            }
        
            normalityTests.results[[length(normalityTests.results)+1]] <- result
        }
    }
    
    normalityTests[["data"]] <- normalityTests.results
    
    normalityTests[["footnotes"]] <- as.list(footnotes)
    
    normalityTests
}


.ttestIndependentSamplesDescriptives <- function(dataset, options, perform) {

	if (options$descriptives == FALSE)
		return(NULL)
		
	descriptives <- list()

	descriptives[["title"]] <- "Group Descriptives"
	
	fields <- list(
		list(name="variable", title="", type="string", combine=TRUE),
		list(name="group",    title="Group", type="string"),
		list(name="N",        title="N", type="number"),
		list(name="mean",     title="Mean", type="number", format="sf:4;dp:3"),
		list(name="sd",       title="SD", type="number", format="sf:4;dp:3"),
		list(name="se",       title="SE", type="number", format="sf:4;dp:3"))

	descriptives[["schema"]] <- list(fields=fields)
	
	data <- list()
	
	variables <- options[["variables"]]
	if (length(variables) == 0)
		variables <- "."
	
	for (variable in variables) {
	
		data[[length(data)+1]] <- list(variable=variable, .isNewGroup=TRUE)
		data[[length(data)+1]] <- list(variable=variable)
	}
	
	if (perform == "run" && options$groupingVariable != "") {
	
		levels <- base::levels(dataset[[ .v(options$groupingVariable) ]])
		
		if (length(levels) != 2) {
		
			descriptives[["error"]] <- list(errorType="badData")
			
		} else {
		
			rowNo <- 1
			groupingData <- dataset[[ .v(options$groupingVariable) ]]
		
			for (variable in options[["variables"]]) {
		
				for (i in 1:2) {
			
					level <- levels[i]
					variableData <- dataset[[ .v(variable) ]]
				
					groupData <- variableData[groupingData == level]
					groupDataOm <- na.omit(groupData)

					if (class(groupDataOm) != "factor") {

						n <- .clean(length(groupDataOm))
						mean <- .clean(mean(groupDataOm))
						stdDeviation <- .clean(sd(groupDataOm))
						stdErrorMean <- .clean(sd(groupDataOm) / sqrt(length(groupDataOm)))
					
						result <- list(variable=variable, group=level, N=n, mean=mean, "sd"=stdDeviation,
							"se"=stdErrorMean)						
							
					} else {
				
						n <- .clean(length(groupDataOm))
						result <- list(variable=variable, group="", N=n, mean="", "sd"="", "se"="")
					}
					
					if (i == 1)
						result[[".isNewGroup"]] <- TRUE
				
					data[[rowNo]] <- result
					rowNo <- rowNo + 1
				}
			}
		}
		
		descriptives[["status"]] <- "complete"
	}
	
	descriptives[["data"]] <- data

	descriptives
}

.ttestIndependentSamplesTTest <- function(dataset, options, perform) {

	ttest <- list()

	ttest[["title"]] <- "Independent Samples T-Test"

	fields <- list(
		list(name="v", title="", type="string", combine=TRUE))
	
	if (options$equalityOfVariances == "reportBoth")	{
	
		fields[[length(fields)+1]] <- list(name="variances", title="Variances", type="string")
	}
	
	fields[[length(fields)+1]] <- list(name="t", type="number", format="sf:4;dp:3")
	fields[[length(fields)+1]] <- list(name="df", type="number", format="sf:4;dp:3")
	fields[[length(fields)+1]] <- list(name="p", type="number", format="dp:3;p:.001")

	if (options$meanDifference) {
	
		fields[[length(fields) + 1]] <- list(name="md",  title="Mean Difference", type="number", format="sf:4;dp:3")
		fields[[length(fields) + 1]] <- list(name="sed", title="SE Difference", type="number", format="sf:4;dp:3")	
	}
	
	if(options$effectSize){
		fields[[length(fields) + 1]] <- list(name="d", title="Cohen's d", type="number", format="sf:4;dp:3")
	}
	
	if (options$confidenceInterval) {

		interval <- 100 * options$confidenceIntervalInterval
		title    <- paste(interval, "% Confidence Interval", sep="")
	
		fields[[length(fields) + 1]] <- list(name="lowerCI", type="number", format="sf:4;dp:3", title=title, combineHeaders=TRUE)
		fields[[length(fields) + 1]] <- list(name="upperCI", type="number", format="sf:4;dp:3", title=title, combineHeaders=TRUE)
		
	}
		
	ttest[["schema"]] <- list(fields=fields)
	
	ttest.rows <- list()
	
	variables <- options[["variables"]]
	if (length(variables) == 0)
		variables <- "."
	
	if (options$equalityOfVariances == "reportBoth") {

		for (variable in variables) {

			ttest.rows[[length(ttest.rows)+1]] <- list(v=variable, variances="assumed equal", .isNewGroup=TRUE)
			ttest.rows[[length(ttest.rows)+1]] <- list(v=variable, variances="no assumption")
		}
		
	} else {
	
		for (variable in variables) {

			ttest.rows[[length(ttest.rows)+1]] <- list(v=variable)
		}
	}
	
	
	footnotes <- .newFootnotes()

	if (options$equalityOfVariances == "assumeEqual")
		.addFootnote(footnotes, symbol="<em>Note.</em>", text="All tests, variances of groups assumed equal")
	if (options$equalityOfVariances == "noAssumption")
		.addFootnote(footnotes, symbol="<em>Note.</em>", text="All tests, variances of groups not assumed equal")
	
	if (perform == "run" && length(options$variables) != 0 && options$groupingVariable != "") {

		levels <- base::levels(dataset[[ .v(options$groupingVariable) ]])
		
		if (length(levels) != 2) {
		
			ttest[["error"]] <- list(errorType="badData", errorMessage="The Grouping Variable must have 2 levels")
			
		} else {
		
			if (options$hypothesis == "groupOneGreater") {

				message <- paste("All tests, hypothesis is group <em>", levels[1], "</em> greater than group <em>", levels[2], "</em>", sep="")
				.addFootnote(footnotes, symbol="<em>Note.</em>", text=message)
				
				testType <- "greater"
		
			} else if (options$hypothesis == "groupTwoGreater") {
	
				message <- paste("All tests, hypothesis is group <em>", levels[1], "</em> less than group <em>", levels[2], "</em>", sep="")
				.addFootnote(footnotes, symbol="<em>Note.</em>", text=message)

				testType <- "less"
				
			} else {
		
				testType <- "two.sided"
			}
		
			if (options$equalityOfVariances == "assumeEqual") {
		
				assume <- c(TRUE)
				assumption=c("assumed equal", NA)
			
			} else if (options$equalityOfVariances == "noAssumption") {
		
				assume <- c(FALSE)
				assumption=c("no assumption", NA)
			
			} else {  # assuming both
		
				assume <- c(TRUE, FALSE)
				assumption=c("assumed equal", "no assumption")
			}
		
			groupingData <- dataset[[ .v(options$groupingVariable) ]]
			rowNo <- 1
			
			for (variable in options[["variables"]]) {

				variableData <- dataset[[ .v(variable) ]]
			
				for (i in .indices(assume)) {
				
					result <- try (silent=FALSE, expr= {
					
						row.footnotes <- NULL

						assume.equal = assume[i]
						
						f <- as.formula(paste(.v(variable), "~", .v(options$groupingVariable)))

						r <- t.test(f, data=dataset, alternative=testType, var.equal=assume.equal, conf.level=options$confidenceIntervalInterval)

						if (assume.equal) {
					
							levene <- car::leveneTest(variableData, groupingData, "mean")
							
							if ( ! is.na(levene[1,3]) && levene[1,3] < .05) {
						
								foot.index <- .addFootnote(footnotes, "Levene's test is significant (p < .05), suggesting a violation of the equal variance assumption")
								row.footnotes <- list(p=list(foot.index))
							}
						}
					
						t <- as.numeric(r$statistic)
						df <- as.numeric(r$parameter)
						p <- as.numeric(r$p.value)
						m <- as.numeric(r$estimate[1]) - as.numeric(r$estimate[2])
						
						ns <- tapply(dataset[[.v(variable)]], dataset[[.v(options$groupingVariable)]], function(x)length(na.omit(x)))
						ms <- tapply(dataset[[.v(variable)]], dataset[[.v(options$groupingVariable)]], mean, na.rm = TRUE)
						sds <- tapply(dataset[[.v(variable)]], dataset[[.v(options$groupingVariable)]], sd, na.rm = TRUE)
												
						sdPooled <- sqrt(((ns[1]-1)*sds[1]^2 + (ns[2]-1)*sds[2]^2) / (ns[1] + ns[2] - 2))
						d <- as.numeric((ms[1] - ms[2]) / sdPooled)
						
						sed <- .clean(as.numeric(sqrt(sds[1]^2/ns[1] + sds[2]^2/ns[2])))
						ciLow <- .clean(r$conf.int[1])
						ciUp <- .clean(r$conf.int[2])
											
#						if (testType == "two.sided") {
#					
#							sed <- .clean()  # beckward approach - getting spread of CI and deviding by critical value for t
#						
#						} else if (testType == "less") {
#					
#							sed <- ""# .clean((ciUp - m) / (qt(options$confidenceIntervalInterval,r$parameter)))
#						
#						} else {
#					
#							sed <- ""# .clean((m - ciLow) / (qt(options$confidenceIntervalInterval,r$parameter)))
#						}
					
						list(v=variable, variances=assumption[i], t=t, df=df, p=p, md=m, d=d,
							 lowerCI=ciLow, upperCI=ciUp, sed=sed, .footnotes=row.footnotes)
						
					})

					if (class(result) == "try-error") {
					
						errorMessage <- .extractErrorMessage(result)
						
						if (errorMessage == "missing value where TRUE/FALSE needed" && any(is.finite(variableData) == FALSE)) {
						
							errorMessage <- "t-statistic is undefined - the dependent variable contains infinity"
							
						} else if (errorMessage == "grouping factor must have exactly 2 levels") {
						
							# We know that the grouping factor *does* have two levels, because we've checked this earlier on
							# This error means that all of one factor has been excluded because of missing values in the dependent
							
							errorMessage <- "t-statistic is undefined - the grouping variable contains less than two levels once missing values in the dependent are excluded"
							
						} else if (errorMessage == "data are essentially constant") {
						
							errorMessage <- "t-statistic is undefined - one or both levels of the dependent contains all the same value (zero variance)"
							
						} else if (errorMessage == "not enough observations") {
					
							errorMessage <- "t-statistic is undefined - one or both levels of the dependent contain too few observations"
						}
						
						index <- .addFootnote(footnotes, errorMessage)
				
						result <- list(v=variable, variances=assumption[i], t="NaN", df="", p="", md="",
								lowerCI="", upperCI="", sed="", .footnotes=list(t=list(index)))
					}
					
					if (i == 1 && options$equalityOfVariances == "reportBoth")
						result[[".isNewGroup"]] <- TRUE
				
					ttest.rows[[rowNo]] <- result
					rowNo <- rowNo + 1
				}
			}
		
			ttest[["footnotes"]] <- as.list(footnotes)
		}
		
	}
	
	ttest[["data"]] <- ttest.rows
	
	ttest

}

.ttestIndependentSamplesInequalityOfVariances <- function(dataset, options, perform) {

	if (options$testUnequalVariances == FALSE)
		return(NULL)
		
	levenes <- list()
	footnotes <- .newFootnotes()

	levenes[["title"]] <- "Test of Inequality of Variances (Levene's)"
	
	fields <- list(
		list(name="variable", title="", type="string"),
		list(name="F", type="number", format="sf:4;dp:3"),
		list(name="df", type="integer"),
		list(name="p", type="number", format="dp:3;p:.001"))

	levenes[["schema"]] <- list(fields=fields)
	
	data <- list()
	
	variables <- options[["variables"]]
	if (length(variables) == 0)
		variables <- "."
	
	for (variable in variables)
		data[[length(data)+1]] <- list(variable=variable)
	
	if (perform == "run" && options$groupingVariable != "") {
	
		levels <- base::levels(dataset[[ .v(options$groupingVariable) ]])
		
		if (length(levels) != 2) {
		
			levenes[["error"]] <- list(errorType="badData")
			
		} else {
	
			rowNo <- 1

			for (variable in options[["variables"]]) {
		
				result <- try (silent=TRUE, expr= {

					levene <- car::leveneTest(dataset[[ .v(variable) ]], dataset[[ .v(options$groupingVariable) ]], "mean")

					F  <- .clean(levene[1,"F value"])
					df <- .clean(levene[1,"Df"])
					p  <- .clean(levene[1,"Pr(>F)"])
		
					row <- list(variable=variable, F=F, df=df, p=p)
					
					if (is.na(levene[1,"F value"])) {
					
						index <- .addFootnote(footnotes, "F-statistic could not be calculated")
						row[[".footnotes"]] <- list(F=list(index))
					}
					
					row
				})

				if (class(result) == "try-error")
					result <- list(variable=variable, F="", df="", p="")
	
				data[[rowNo]] <- result
				rowNo <- rowNo + 1
			}
		}
	
	}
	
	levenes[["data"]] <- data
	levenes[["footnotes"]] <- as.list(footnotes)

	levenes
}
