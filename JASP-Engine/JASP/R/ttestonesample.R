
TTestOneSample <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {

	all.variables <- unlist(options$variables)

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
		
	} else {
	
		if (options$missingValues == "excludeListwise") {
		
			dataset <- .vdf(dataset, columns.as.numeric=all.variables, exclude.na.listwise=all.variables)
		}
		else {
		
			dataset <- .vdf(dataset, columns.as.numeric=all.variables)
		}
	}

	results <- list()
	
	
	
	meta <- list()
	
	meta[[1]] <- list(name="title", type="title")
	meta[[2]] <- list(name="ttest", type="table")
	meta[[3]] <- list(name="descriptives", type="table")
	meta[[4]] <- list(name="normalityTests", type="table")
	meta[[5]] <- list(name="headerDescriptivesPlots", type="h1")
	meta[[6]] <- list(name="descriptivesPlots", type="images")
	
	results[[".meta"]] <- meta
	results[["title"]] <- "T-Test"
	

	ttest <- list()
	descriptives <- list()
	
	ttest[["title"]] <- "One Sample T-Test"

	fields <- list(
		list(name="v", type="string", title=""),
		list(name="t", type="number", format="sf:4;dp:3"),
		list(name="df", type="number", format="sf:4;dp:3"),
		list(name="p", type="number", format="dp:3;p:.001"))
		
	if (options$meanDifference) {
	
		fields[[length(fields) + 1]] <- list(name="Mean Difference", type="number", format="sf:4;dp:3")
	}
	
	if(options$effectSize){
		fields[[length(fields) + 1]] <- list(name="d", title="Cohen's d", type="number", format="sf:4;dp:3")
	}
	
	if (options$confidenceInterval) {
	
		interval <- 100 * options$confidenceIntervalInterval
		title    <- paste(interval, "% Confidence Interval", sep="")

		fields[[length(fields) + 1]] <- list(name="lowerCI", type="number", format="sf:4;dp:3", title="Lower", overTitle=title)
		fields[[length(fields) + 1]] <- list(name="upperCI", type="number", format="sf:4;dp:3", title="Upper", overTitle=title)
	}
	
	ttest[["schema"]] <- list(fields=fields)
	
	footnotes <- .newFootnotes()
	
	if (options$hypothesis == "greaterThanTestValue") {

		message <- paste("All tests, hypothesis is population mean is greater than ", options$testValue, sep="")
		.addFootnote(footnotes, symbol="<em>Note.</em>", text=message)
		
		testType <- "greater"

	} else if (options$hypothesis == "lessThanTestValue") {

		message <- paste("All tests, hypothesis is population mean is less than ", options$testValue, sep="")
		.addFootnote(footnotes, symbol="<em>Note.</em>", text=message)
		
		testType <- "less"
		
	} else {

		if (options$testValue != 0) {
		
			message <- paste("All tests, hypothesis is population mean is different to ", options$testValue, sep="")
			.addFootnote(footnotes, symbol="<em>Note.</em>", text=message)
		}
	
		testType <- "two.sided"	
	}
	
	ttest.rows <- list()
	
	variables <- options[["variables"]]
	if (length(variables) == 0)
		variables = "."
	
	for (variable in variables) {

		if (perform == "run" && length(options$variables) > 0) {

			result <- try (silent = TRUE, expr = {
			
				r <- t.test(dataset[[ .v(variable) ]], alternative=testType, mu=options$testValue,
							conf.level=options$confidenceIntervalInterval)
			
				t  <- as.numeric(r$statistic)
				df <- as.numeric(r$parameter)
				p  <- as.numeric(r$p.value)
				m  <- as.numeric(r$estimate - r$null.value)
				d <- .clean((mean(dataset[[ .v(variable) ]]) - options$testValue) / sd(dataset[[ .v(variable) ]]))
				ciLow <- .clean(as.numeric(r$conf.int[1]))
				ciUp  <- .clean(as.numeric(r$conf.int[2]))
				
				if (is.na(t))
					stop("data are essentially constant")
			
				list(v=variable, t=t, df=df, p=p, "Mean Difference"=m, "d"=d, "lowerCI"=ciLow, "upperCI"=ciUp)
			})
				
			if (class(result) == "try-error") {
			
				errorMessage <- .extractErrorMessage(result)
						
				if (errorMessage == "missing value where TRUE/FALSE needed") {
				
					errorMessage <- paste("t-statistic is undefined - the sample contains infinity")
					
				} else if (errorMessage == "data are essentially constant") {
				
					errorMessage <- paste("t-statistic is undefined - the sample contains all the same value (zero variance)")
				
				} else if (errorMessage == "not enough 'x' observations") {
					
					errorMessage <- "t-statistic is undefined - sample contains only one value"
						
				}
				
				index <- .addFootnote(footnotes, errorMessage)
		
				result <- list(v=variable, t=.clean(NaN), df="", p="", "Mean Difference"="", "lowerCI"="", "upperCI"="", .footnotes=list(t=list(index)))
			}
			
		} else {

			result <- list(v=variable, t=".", df=".", p=".", "Mean Difference"=".", "d"=".", "lowerCI"=".", "upperCI"=".")
		
		}
		
		ttest.rows[[length(ttest.rows)+1]] <- result
	}
	
	ttest[["data"]] <- ttest.rows
	ttest[["footnotes"]] <- as.list(footnotes)
	
	

	if (options$descriptives) {
	
		descriptives[["title"]] <- "Descriptives"
		descriptives[["cases"]] <- I(options$variables)

		fields <- list(
			list(name="v",    title="",   type="string"),
			list(name="N",    title="N",  type="number",   format="sf:4;dp:3"),
			list(name="mean", title="Mean", type="number", format="sf:4;dp:3"),
			list(name="sd",   title="SD", type="number",   format="sf:4;dp:3"),
			list(name="se",   title="SE", type="number",   format="sf:4;dp:3"))

		descriptives[["schema"]] <- list(fields=fields)
		descriptives.results <- list()
		
		variables <- options[["variables"]]
		if (length(variables) == 0)
			variables = "."

		for (variable in variables) {
			
			if (perform == "run" && length(options[["variables"]]) > 0) {

				data <- na.omit(dataset[[ .v(variable) ]])

				if (class(data) != "factor") {

					n    <- .clean(length(data))
					mean <- .clean(mean(data))
					stdDeviation <- .clean(sd(data))
					stdErrorMean <- .clean(sd(data)/sqrt(length(data)))
					
					if(length(descriptives.results) == 0) {
			            newGroup <- TRUE   
			        } else {				
				        newGroup <- FALSE
			        }

					result <- list(v=variable, N=n, mean=mean, sd=stdDeviation, se=stdErrorMean, ".isNewGroup" = newGroup)
				} else {
				
				    if(length(descriptives.results) == 0) {
			            newGroup <- TRUE   
			        } else {				
				        newGroup <- FALSE
			        }
			
					n <- .clean(length(data))
					result <- list(v=variable, N=n, mean="", sd="", se="", ".isNewGroup" = newGroup)
				}
			
			} else {
			
			    if(length(descriptives.results) == 0) {
			        newGroup <- TRUE   
			    } else {				
				    newGroup <- FALSE
			    }
			
				result <- list(v=variable, N=".", mean=".", sd= ".", se=".", ".isNewGroup" = newGroup)			
			
			}
			
			descriptives.results[[length(descriptives.results)+1]] <- result
		}
		
		descriptives[["data"]] <- descriptives.results
	}
	
	if (options$normalityTests) {
	
	    normalityTests <- list()
	
		normalityTests[["title"]] <- "Test of Normality (Shapiro-Wilk)"
		normalityTests[["cases"]] <- I(options$variables)

		fields <- list(
			list(name="v", title="", type="string"),
			list(name="W", title="W", type="number",   format="sf:4;dp:3"),
			list(name="p", title="p", type="number", format="dp:3;p:.001"))

		normalityTests[["schema"]] <- list(fields=fields)
		
		footnotes <- .newFootnotes()
        .addFootnote(footnotes, symbol="<em>Note.</em>", text="Significant results indicate a deviation from normality")
		
		normalityTests.results <- list()
		
		variables <- options[["variables"]]
		if (length(variables) == 0)
			variables = "."

		for (variable in variables) {
			
			if (perform == "run" && length(options[["variables"]]) > 0) {

				data <- na.omit(dataset[[ .v(variable) ]])
				
				row.footnotes <- NULL
                error <- FALSE
                
                if(length(data) < 3) {
                
                    foot.index <- .addFootnote(footnotes, "Too few datapoints (N < 3) to compute statistic reliably")
					row.footnotes <- list(W=list(foot.index), p=list(foot.index))
					error <- TRUE
					
                } else if(length(data) > 5000) {
                
                    foot.index <- .addFootnote(footnotes, "Too many datapoints (N > 5000) to compute statistic reliably")
					row.footnotes <- list(W=list(foot.index), p=list(foot.index))
					error <- TRUE
					
                }

				if (!error) {
                    
                    r <- stats::shapiro.test(data)
                    
					W <- .clean(as.numeric(r$statistic))
					p <- .clean(r$p.value)
					
					if(length(normalityTests.results) == 0) {
			            newGroup <- TRUE   
			        } else {				
				        newGroup <- FALSE
			        }
					
					result <- list("v" = variable, "W" = W, "p" = p, ".isNewGroup" = newGroup)
					
				} else {
			        
			        if(length(normalityTests.results) == 0) {
			            newGroup <- TRUE   
			        } else {				
				        newGroup <- FALSE
			        }
					
					result <- list("v" = variable, "W" = "NaN", "p" = "NaN", ".isNewGroup" = newGroup, .footnotes=row.footnotes)
					
				}
			
			} else {
			
			    if(length(normalityTests.results) == 0) {
			        newGroup <- TRUE   
			    } else {				
				    newGroup <- FALSE
			    }
			
				result <- list("v" = variable, "W" = ".", p = ".", ".isNewGroup" = newGroup)			
			
			}
			
			normalityTests.results[[length(normalityTests.results)+1]] <- result
		}
		
		normalityTests[["data"]] <- normalityTests.results
		
		normalityTests[["footnotes"]] <- as.list(footnotes)
		
		results[["normalityTests"]] <- normalityTests
	}
	
	
	
	results[["ttest"]] <- ttest
	
	if (options$descriptives)
		results[["descriptives"]] <- descriptives
	
	
	if (options$descriptivesPlots) {
		
		if (length(options$variables) > 1) {
			results[["headerDescriptivesPlots"]] <-  "Descriptives Plots"
		} else {
			results[["headerDescriptivesPlots"]] <-  "Descriptives Plot"
		}
		
		results[["descriptivesPlots"]] <- .oneSampleTTestDescriptivesPlot(dataset, options, perform)
	}

	
	results
}

.oneSampleTTestDescriptivesPlot <- function(dataset, options, perform) {

	descriptivesPlotList <- list()

	base_breaks_y <- function(x, options){
	
		values <- c(options$testValue, x[,"dependent"]-x[,"ci"],x[,"dependent"]+x[,"ci"])
		ci.pos <- c(min(values), max(values))
		b <- pretty(ci.pos)
		d <- data.frame(x=-Inf, xend=-Inf, y=min(b), yend=max(b))
		list(ggplot2::geom_segment(data=d, ggplot2::aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE, size = 1),
			ggplot2::scale_y_continuous(breaks=c(min(b), options$testValue, max(b))))
	}

	for (i in .indices(options$variables)) {

		var <- options$variables[[i]]
		
		descriptivesPlot <- list()
	
		descriptivesPlot[["title"]] <- ""
		descriptivesPlot[["width"]] <- options$plotWidth
		descriptivesPlot[["height"]] <- options$plotHeight
		descriptivesPlot[["custom"]] <- list(width="plotWidth", height="plotHeight")

		if (perform == "run" && var != "") {
												
			dataSubset <- data.frame("dependent" = dataset[[.v(var)]], "groupingVariable" = rep(var,length(dataset[[.v(var)]])))
												
			summaryStat <- .summarySE(dataSubset, measurevar = "dependent", groupvars = "groupingVariable",
						   			  conf.interval = options$descriptivesPlotsConfidenceInterval, na.rm = TRUE, .drop = FALSE)
						
			testValue <- data.frame("testValue" = options$testValue)
									
			pd <- ggplot2::position_dodge(.2)
				
			p <- ggplot2::ggplot(summaryStat, ggplot2::aes(x=groupingVariable, y=dependent, group=1)) +
				 ggplot2::geom_errorbar(ggplot2::aes(ymin=ciLower, ymax=ciUpper), colour="black", width=.2, position=pd) +
				 ggplot2::geom_line(position=pd, size = .7) + 
				 ggplot2::geom_point(position=pd, size=4) +
				 ggplot2::geom_hline(data = testValue, ggplot2::aes(yintercept=testValue), linetype="dashed") +
				 ggplot2::ylab(NULL) +
				 ggplot2::xlab(NULL) +
				 ggplot2::theme_bw() +
				 ggplot2::theme(panel.grid.minor=ggplot2::element_blank(), plot.title = ggplot2::element_text(size=18),
					panel.grid.major=ggplot2::element_blank(),
					axis.title.x = ggplot2::element_text(size=18,vjust=-.2), axis.title.y = ggplot2::element_text(size=18,vjust=-1),
					axis.text.x = ggplot2::element_text(size=15), axis.text.y = ggplot2::element_text(size=15),
					panel.background = ggplot2::element_rect(fill = 'transparent', colour = NA),
					plot.background = ggplot2::element_rect(fill = 'transparent', colour = NA),
					legend.background = ggplot2::element_rect(fill = 'transparent', colour = NA),
					panel.border = ggplot2::element_blank(), axis.line = ggplot2::element_blank(),
					legend.key = ggplot2::element_blank(),
					legend.title = ggplot2::element_text(size=12),
					legend.text = ggplot2::element_text(size = 12),
					axis.ticks = ggplot2::element_line(size = 0.5),
					axis.ticks.margin = grid::unit(1,"mm"),
					axis.ticks.length = grid::unit(3, "mm"),
					plot.margin = grid::unit(c(.5,0,.5,.5), "cm")) +
				 base_breaks_y(summaryStat, options)
						
			image <- .beginSaveImage(options$plotWidth, options$plotHeight)
			print(p)
			content <- .endSaveImage(image)

			descriptivesPlot[["data"]] <- content
	
		} else {
	
			descriptivesPlot[["data"]] <- ""
			
		}
		
		descriptivesPlotList[[i]] <- descriptivesPlot
	}

	descriptivesPlotList
}
