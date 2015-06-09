
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
	meta[[5]] <- list(name="headerIntervalPlots", type="h1")
	meta[[6]] <- list(name="intervalPlots", type="images")
	
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
	
		fields[[length(fields)+1]] <- list(name="lowerCI", type="number", format="sf:4;dp:3", title="Lower", overTitle=title)
		fields[[length(fields)+1]] <- list(name="upperCI", type="number", format="sf:4;dp:3", title="Upper", overTitle=title)
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
					sed <- .clean(sd(c1-c2, na.rm = TRUE)/sqrt(length(na.omit(c1-c2))))
					
					es <- .clean((mean(c1)-mean(c2))/(sqrt(sd(c1)^2+sd(c2)^2-2*cov(c1, c2))))
			        
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
	
	if (length(ttest.results) == 0)
		ttest.results[[1]] <- list()
	
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
		
		footnotes <- .newFootnotes()
        .addFootnote(footnotes, symbol="<em>Note.</em>", text="Significant results indicate a deviation from normality")
		
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
					
					result <- list(v1=pair[[1]], sep="-", v2=pair[[2]], "W" = W, "p" = p, ".isNewGroup" = newGroup)
					
				} else {
			        
			        if(length(normalityTests.results) == 0) {
			            newGroup <- TRUE   
			        } else {				
				        newGroup <- FALSE
			        }
					
					result <- list(v1=pair[[1]], sep="-", v2=pair[[2]], "W" = "NaN", "p" = "NaN", ".isNewGroup" = newGroup, .footnotes=row.footnotes)
					
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
		
		normalityTests[["footnotes"]] <- as.list(footnotes)
		
		results[["normalityTests"]] <- normalityTests
	}
	
	if (options$intervalPlots) {
		
		if (length(options$pairs) > 1) {
			results[["headerIntervalPlots"]] <-  "Interval Plots"
		} else {
			results[["headerIntervalPlots"]] <-  "Interval Plot"
		}
		
		results[["intervalPlots"]] <- .pairedSamplesTTestIntervalPlot(dataset, options, perform)
	}
		
	results
}

.pairedSamplesTTestIntervalPlot <- function(dataset, options, perform) {

	intervalPlotList <- list()
		
	base_breaks_x <- function(x){
		b <- unique(as.numeric(x))
		d <- data.frame(y=-Inf, yend=-Inf, x=min(b), xend=max(b))
		list(ggplot2::geom_segment(data=d, ggplot2::aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE, size = 1))
	}

	base_breaks_y <- function(x){
		ci.pos <- c(x[,"dependent"]-x[,"ci"],x[,"dependent"]+x[,"ci"])
		b <- pretty(ci.pos)
		d <- data.frame(x=-Inf, xend=-Inf, y=min(b), yend=max(b))
		list(ggplot2::geom_segment(data=d, ggplot2::aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE, size = 1),
			ggplot2::scale_y_continuous(breaks=c(min(b),max(b))))
	}

	for (i in .indices(options$pairs)) {

		pair <- options$pairs[[i]]
		
		intervalPlot <- list()
	
		intervalPlot[["title"]] <- ""
		intervalPlot[["width"]] <- options$plotWidth
		intervalPlot[["height"]] <- options$plotHeight
		intervalPlot[["custom"]] <- list(width="plotWidth", height="plotHeight")

		if (perform == "run" && pair[[1]] != "" && pair[[2]] != "") {
	
			c1 <- dataset[[ .v(pair[[1]]) ]]
			c2 <- dataset[[ .v(pair[[2]]) ]]
	
			data <- data.frame("id" = rep(1:length(c1),2), "dependent" = c(c1, c2), 
							   "groupingVariable" = c(rep(paste(pair[[1]],".1", sep=""), length(c1)), rep(paste(pair[[2]],".2", sep=""), length(c2))))
			
			print(data)
			
			summaryStat <- .summarySEwithin(data, measurevar = "dependent", withinvars = "groupingVariable", idvar = "id", 
						   					conf.interval = options$intervalIntervalPlots, na.rm = TRUE, .drop = FALSE)
			
			pd <- ggplot2::position_dodge(.2)
				
			p <- ggplot2::ggplot(summaryStat, ggplot2::aes(x=groupingVariable, y=dependent, group=1)) +
				 ggplot2::geom_errorbar(ggplot2::aes(ymin=ciLower, ymax=ciUpper), colour="black", width=.2, position=pd) +
				 ggplot2::geom_line(position=pd, size = .7) + 
				 ggplot2::geom_point(position=pd, size=4) +
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
				 base_breaks_y(summaryStat) +
				 base_breaks_x(summaryStat$groupingVariable) +
				 ggplot2::scale_x_discrete(labels=c(pair[[1]], pair[[2]]))
						
			image <- .beginSaveImage(options$plotWidth, options$plotHeight)
			print(p)
			content <- .endSaveImage(image)

			intervalPlot[["data"]] <- content
	
		} else {
	
			intervalPlot[["data"]] <- ""
			
		}
		
		intervalPlotList[[i]] <- intervalPlot
	}

	intervalPlotList
}


