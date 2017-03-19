#
# Copyright (C) 2013-2015 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

# options is a list with the following names:
# "variables": data.frame thingie vfc
# "pearson": TRUE/FALSE 
# "kendallsTauB": TRUE/FALSE
# "spearman": TRUE/FALSE
# "hypothesis": "correlated", "correlatedPositively", "correlatedNegatively"
# "reportBayesFactors": TRUE/FALSE
# "flagSupported": TRUE/FALSE
# "credibleIntervals": TRUE/FALSE
# "credibleIntervalsInterval": OptionNumber(.95, 0, 1, "%"));
# "priorWidth": c(0, 2)
# "missingValues": excludePairwise, excludeListwist
# "bayesFactorType": BF10/BF01/LogBF10
# "plotCorrelationMatrix: TRUE/FALSE
# "plotDensitiesForVariables": TRUE/FALSE
# "plotPosteriors": TRUE/FALSE
#
CorrelationBayesian <- function(dataset=NULL, options, perform="run",
								callback=function(...) 0, ...) {
	
	state <- .retrieveState()
	
	diff <- NULL
	
	if (!is.null(state)) {
		
		diff <- .diff(options, state$options)
		
	}
	
	# dataset is data.frame
	# options is a list
	#
	if (is.null(dataset)) {
		if (perform == "run" || options$missingValues == "excludeListwise") {
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
	meta <- list()
	meta[[1]] <- list(name="title", type="title")
	meta[[2]] <- list(name="correlations", type="table")
	meta[[3]] <- list(name="plot", type="image")
	
	results[[".meta"]] <- meta
	results[["title"]] <- "Bayesian Correlation Matrix"
	correlationTableOutput <-
		.correlationTableBayesian(dataset, perform, variables=options$variables,
								  pearson=options$pearson,
								  kendallsTauB=options$kendallsTauB,
								  spearman=options$spearman,
								  hypothesis=options$hypothesis,
								  reportBayesFactors=options$reportBayesFactors,
								  flagSupported=options$flagSupported,
								  credibleIntervals=options$credibleIntervals,
								  credibleIntervalsInterval=options$credibleIntervalsInterval,
								  priorWidth=options$priorWidth,
								  bayesFactorType=options$bayesFactorType,
								  missingValues=options$missingValues, state, diff) 
	
	tableVariables <- correlationTableOutput$variables
	tableTests <- correlationTableOutput$tests
	tableRows <- correlationTableOutput$rows
	
	results[["correlations"]] <- correlationTableOutput$correlationTable
	
	
	if (!is.null(state) && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$credibleIntervals == FALSE && diff$credibleIntervalsInterval == FALSE
																										&& diff$hypothesis == FALSE && diff$kendallsTauB == FALSE && diff$missingValues == FALSE && diff$pearson == FALSE && diff$plotCorrelationMatrix == FALSE
																										&& diff$plotDensitiesForVariables == FALSE && diff$plotPosteriors == FALSE && diff$spearman == FALSE && diff$variables == FALSE && diff$priorWidth == FALSE)))) {
		
		results[["plot"]] <- state$correlationPlot
		
	} else {
		
		results[["plot"]] <- .correlationMatrixPlotBayesian(dataset, perform, options, hypothesis=options$hypothesis)
	}
	
	keep <- results[["plot"]]$data
	
	if (perform == "init") {
		if (length(options$variables) < 2) {
			results <- list(results=results, status="complete", keep=keep)
			return(results)
		} else {
			results <- list(results=results, status="inited", state=state, keep=keep)
			return(results)
		}
	} else {
		
		
		statedBFValuesExcludeListwise <- state$bfValuesExcludeListwise
		statedFootnotesExcludeListwise <- state$footnotesExcludeListwise
		
		
		return(list(results=results, status="complete", 
					state=list(options=options, 
							   results=results, 
							   tableVariables=tableVariables,
							   tableTests=tableTests,
							   tableRows=tableRows,
							   rValuesExcludePairwise=correlationTableOutput$rValuesExcludePairwise,
							   bfValuesExcludePairwise=correlationTableOutput$bfValuesExcludePairwise,
							   footnotesExcludePairwise=correlationTableOutput$footnotesExcludePairwise,
							   bfValuesExcludeListwise=correlationTableOutput$bfValuesExcludeListwise,
							   footnotesExcludeListwise=correlationTableOutput$footnotesExcludeListwise,
							   correlationPlot=results$plot), keep=keep))
	}
}
# "variables": data.frame thingie vfc
# "pearson": TRUE/FALSE
# "kendallsTauB": TRUE/FALSE
# "spearman": TRUE/FALSE
# "hypothesis": "correlated", "correlatedPositively", "correlatedNegatively"
# "reportBayesFactors": TRUE/FALSE
# "flagSupported": TRUE/FALSE
# "credibleIntervals": TRUE/FALSE
# "credibleIntervalsInterval": OptionNumber(.95, 0, 1, "%"));
# "priorWidth": c(0.5, Inf)
# "bayesFactorType": BF10/BF01
.correlationTableBayesian <- function(dataset, perform, variables, pearson=TRUE,
									  kendallsTauB=FALSE, spearman=FALSE,
									  hypothesis="correlated",
									  reportBayesFactors=TRUE,
									  flagSupported=FALSE,
									  credibleIntervals=FALSE,
									  credibleIntervalsInterval=0.95,
									  priorWidth=priorWidth,
									  bayesFactorType=bayesFactorType,
									  missingValues="excludePairwise", 
									  state, diff) {
	# TODO: check for all arguments in particular meansAndStdDev,
	# hypothesis="correlated"
	#
	#
	correlation.table <- list()
	if(pearson & kendallsTauB){
	  correlation.table[["citation"]] <- list(
		  "Ly, A., Verhagen, A. J. & Wagenmakers, E.-J. (2015). Harold Jeffreys's Default Bayes Factor Hypothesis Tests: Explanation, Extension, and Application in Psychology. Manuscript submitted for publication.\n \nvan Doorn, J.B., Ly, A., Marsman, M. & Wagenmakers, E.-J. (2016). Bayesian Inference for Kendall’s Rank Correlation Coefficient. Manuscript submitted for publication."
	  )} else if(pearson){
	    correlation.table[["citation"]] <- list(
	      "Ly, A., Verhagen, A. J. & Wagenmakers, E.-J. (2015). Harold Jeffreys's Default Bayes Factor Hypothesis Tests: Explanation, Extension, and Application in Psychology. Manuscript submitted for publication."
    )} else if(kendallsTauB){
	    correlation.table[["citation"]] <- list(
	    "van Doorn, J.B., Ly, A., Marsman, M. & Wagenmakers, E.-J. (2016). Bayesian Inference for Kendall’s Rank Correlation Coefficient. Manuscript submitted for publication."
	  )}
	
	if (perform == "init") {
		if (length(variables) < 2)
			variables <- c(variables, "...")
		if (length(variables) < 2)
			variables <- c(variables, "... ")
	}
	if (hypothesis == "correlated") {
		if (bayesFactorType=="BF10"){
			bf.title <- "BF\u2081\u2080"
		} else if (bayesFactorType == "BF01") {
			bf.title <- "BF\u2080\u2081"
		} else if (bayesFactorType=="LogBF10"){
			bf.title <- "log(BF\u2081\u2080)"
		}
	} else if (hypothesis == "correlatedPositively") {
		if (bayesFactorType == "BF10"){
			bf.title <- "BF\u208A\u2080"
		} else if (bayesFactorType == "BF01") {
			bf.title <- "BF\u2080\u208A"
		} else if (bayesFactorType=="LogBF10"){
			bf.title <- "log(BF\u208A\u2080)"
		}
	} else if (hypothesis == "correlatedNegatively"){
		if (bayesFactorType == "BF10"){
			bf.title <- "BF\u208B\u2080"
		} else if (bayesFactorType == "BF01") {
			bf.title <- "BF\u2080\u208B"
		} else if (bayesFactorType=="LogBF10"){
			bf.title <- "log(BF\u208B\u2080)"
		}
	}
	# Note: test contains the tests that are performed
	tests <- c()
	if (pearson)
		tests <- c(tests, "pearson")
	if (spearman)
		tests <- c(tests, "spearman")
	if (kendallsTauB)
		tests <- c(tests, "kendall")
	# Note: Naming of the table
	if (length(tests) != 1) {
		correlation.table[["title"]] <- paste("Bayesian Correlation Table")
	} else if (pearson) {
		correlation.table[["title"]] <- paste("Bayesian Pearson Correlations")
	} else if (spearman) {
		correlation.table[["title"]] <- paste("Bayesian Spearman Correlations")
	} else if (kendallsTauB) {
		correlation.table[["title"]] <- paste("Bayesian Kendall's Tau")
	} else {
		correlation.table[["title"]] <- paste("Bayesian Correlation Table")
	}
	
	# Note: Describe column names to the returned object
	fields <- list(list(name=".variable", title="", type="string"))
	rows <- list()
	
	# Note: create footnote function
	footnotes <- .newFootnotes()
	
	if (flagSupported || reportBayesFactors) {
		if (hypothesis == "correlatedPositively") {
			.addFootnote(footnotes, "For all tests, the alternative hypothesis specifies that the correlation is positive.", symbol="<i>Note</i>.")
		} else if (hypothesis == "correlatedNegatively") {
			.addFootnote(footnotes, "For all tests, the alternative hypothesis specifies that the correlation is negative.", symbol="<i>Note</i>.")
		}
	}
	
	if (flagSupported) {
		if (bayesFactorType=="LogBF10"){
			.addFootnote(footnotes, paste(bf.title, " > log(10), ** , ", bf.title, " > log(30), *** ", bf.title, " > log(100)"), symbol="*")
		} else {
			.addFootnote(footnotes, paste(bf.title, " > 10, ** , ", bf.title, " > 30, *** ", bf.title, " > 100"), symbol="*")
		}
	}
	
	# Note: Go over each variable
	v.c <- length(variables)
	
	# State:
	#
	if (!is.null(state)){
		# Retrieve from state, I could have done this directly on state$footnotes etc, 
		# but then I have to check whether it is initialised and whether I can write to it
		# for instance, in the writing to bfValuesList
		#
		# Pairwise based on names
		rValuesListExcludePairwise <- state$rValuesExcludePairwise
		bfValuesListExcludePairwise <- state$bfValuesExcludePairwise
		footnotesListExcludePairwise <- state$footnotesExcludePairwise
		
		# Pairwise based on n and r
		bfValuesListExcludeListwise <- state$bfValuesExcludeListwise
		footnotesListExcludeListwise <- state$footnotesExcludeListwise
	} else {
		# Initialise if there is no state
		temp <- list()
		for (levels in 1:3){
			temp <- list(temp)
			
			if (levels==2){
				# Structure is [[variable.name]][[column.name]]
				rValuesListExcludePairwise <- temp
				footnotesListExcludePairwise <- temp
				
				# Not necessary to consider the names only need the ns
				# Structure is [[n.label]][[r.label]]
				footnotesListExcludeListwise <- temp
			} else if (levels==3){
				# Structure is [[prior.label]][[variable.name]][[column.name]]
				bfValuesListExcludePairwise <- temp 
				
				# Not necessary to consider the names only need the rs
				# Structure is [[prior.label]][[n.label]][[r.label]]
				bfValuesListExcludeListwise <- temp
			}
		}
	}
	
	
	prior.label <- as.character(round(priorWidth, 5))
	
	if (v.c > 0) {
		# Note: There are variables: 
		test.names <- list(pearson="Pearson's r", spearman="Spearman's rho", kendall="Kendall's tau")
		column.names <- c()
		for (test in tests) {
			# Note: create columns per test
			if (length(tests) > 1 || reportBayesFactors) {
				column.name <- paste(".test[", test, "]", sep="")
				column.names[[length(column.names)+1]] <- column.name
				fields[[length(fields)+1]] <- list(name=column.name, title="", type="string")
			}
			
			# Note: create columns per test variable 
			for (variable.name in variables) {
				column.name <- paste(variable.name, "[", test, "]", sep="")
				column.names[[length(column.names)+1]] <- column.name
				fields[[length(fields)+1]] <- list(name=column.name, title=variable.name, type="number", format="dp:3")
			}
			
			# Note: create column for bfs
			if (reportBayesFactors) {
				column.name <- paste(".test[", test, "Bf]", sep="")
				column.names[[length(column.names)+1]] <- column.name
				fields[[length(fields)+1]] <- list(name=column.name, title=bf.title, type="string")
				for (variable.name in variables) {
					column.name <- paste(variable.name, "[", test, "Bf]", sep="")
					column.names[[length(column.names)+1]] <- column.name
					fields[[length(fields)+1]] <- list(name=column.name, title=variable.name, type="number", format="sf:4;dp:3")
				}
			}
		}
		
		for (i in 1:v.c) {
			# Note: Create row given a column
			row <- list()
			row.footnotes <- list()
			
			variable.name <- variables[[i]]
			
			for (test in tests) {
				# Note: Create test row given a column
				bayes.factors <- list()
				
				if (length(tests) > 1 || reportBayesFactors)
					# Note: Create test name for each test row given a column
					row[[length(row)+1]] <- test.names[[test]]
				
				if (reportBayesFactors)
					# Note: Create bf row each test given each column (variable)
					bayes.factors[[length(bayes.factors)+1]] <- bf.title
				
				for (j in .seqx(1, i-1)) {
					# Note: Fill in blanks
					row[[length(row)+1]] <- ""
					bayes.factors[[length(bayes.factors)+1]] <- ""
				}
				
				row[[length(row)+1]] <- "\u2014" # em-dash # Note: Fill in blanks
				bayes.factors[[length(bayes.factors)+1]] <- "\u2014"
				
				for (j in .seqx(i+1, v.c)) {
					# Note: fill in blanks in table upper left-hand off diaganols
					variable.2.name <- variables[[j]]
					column.name <- paste(variable.2.name, "[", test, "]", sep="")
					
					
					# State retrieval block ----
					resultProcessing <- FALSE
					# This switches whether we report something or, 
					# useful for when we retrieve data from state and when we perform, 
					# when perform!=run we don't do this part. However, doesn't work leads to less rows
					# resultsReporting <- FALSE
					# 
					# Try to retrieve bfs, if no retrival, then either calculate or init
					#
					retrievalFailure <- TRUE
					recalculateCIs <- FALSE
					
					if (missingValues=="excludePairwise"){
						retrievedBFs <- bfValuesListExcludePairwise[[prior.label]][[variable.name]][[column.name]]
						
						if (!is.null(retrievedBFs)){
							# State: Retrieval
							some.r <- unlist(rValuesListExcludePairwise[[variable.name]][[column.name]])
							all.bfs <- retrievedBFs
							
							retrievedFootnote <- footnotesListExcludePairwise[[variable.name]][[column.name]]
							
							if (!is.null(retrievedFootnote)){
								some.footnote <- unlist(retrievedFootnote)
								index <- .addFootnote(footnotes, some.footnote)
								row.footnotes[[column.name]] <- c(row.footnotes[[column.name]], list(index))
							}
							
							retrievalFailure <- FALSE
							resultProcessing <- TRUE
							
							# CIs check:
							#
							if (credibleIntervals){
								calculatedCIValues <- all.bfs$ciValues
								
								if (!is.na(all.bfs$bf10) && !(credibleIntervalsInterval %in% calculatedCIValues)){
									# put this in for the next time
									# Output prepare
									new.ci.label <- as.character(round(credibleIntervalsInterval, 5))
									calculatedCIValues <- c(calculatedCIValues, credibleIntervalsInterval)
									ciList <- all.bfs$CIs
									
									# Calculate
									newCIList <- .credibleIntervals(alpha=all.bfs$betaA, beta=all.bfs$betaB, credibleIntervalsInterval)[[2]]
									
									# Store in State
									ciList[[new.ci.label]] <- newCIList
									all.bfs$CIs <- ciList
									all.bfs$ciValues <- calculatedCIValues
									bfValuesListExcludePairwise[[prior.label]][[variable.name]][[column.name]] <- all.bfs
								}	
							}
							#
							# Close succesfull retrieval of bf
						} else {
							retrievalFailure <- TRUE
						}
					} else if (missingValues=="excludeListwise"){
						# Load: data
						#
						v1 <- dataset[[ .v(variable.name) ]]
						v2 <- dataset[[ .v(variable.2.name) ]]
						
						if (!is.null(v1) && !is.null(v2)){
							# Note: Data: PREPARE
							#
							some.n <- length(v1)
							some.r <- cor(v1, v2)
							
							# For stateRetrieval
							n.label <- as.character(round(some.r, 5))
							r.label <- as.character(round(some.r, 5))
							
							# Try state retrieval
							#
							retrievedBFs <- bfValuesListExcludeListwise[[prior.label]][[n.label]][[r.label]]
							
							if (!is.null(retrievedBFs)){
								# State: Retrieval
								some.r <- some.r
								all.bfs <- retrievedBFs
								
								retrievedFootnote <- footnotesListExcludeListwise[[n.label]][[r.label]]
								
								if (!is.null(retrievedFootnote)){
									some.footnote <- unlist(retrievedFootnote)
									index <- .addFootnote(footnotes, some.footnote)
									row.footnotes[[column.name]] <- c(row.footnotes[[column.name]], list(index))
								}
								
								retrievalFailure <- FALSE
								resultProcessing <- TRUE
								
								# CIs check:
								#
								if (credibleIntervals){
									calculatedCIValues <- all.bfs$ciValues
									
									if (!is.na(all.bfs$bf10) && !(credibleIntervalsInterval %in% calculatedCIValues)){
										# put this in for the next time
										# Output prepare
										new.ci.label <- as.character(round(credibleIntervalsInterval, 5))
										calculatedCIValues <- c(calculatedCIValues, credibleIntervalsInterval)
										ciList <- all.bfs$CIs
										
										# Calculate
										newCIList <- .credibleIntervals(alpha=all.bfs$betaA, beta=all.bfs$betaB, credibleIntervalsInterval)[[2]]
										
										# Store in State
										ciList[[new.ci.label]] <- newCIList
										all.bfs$CIs <- ciList
										all.bfs$ciValues <- calculatedCIValues
										bfValuesListExcludeListwise[[prior.label]][[n.label]][[r.label]] <- all.bfs
									}	
								}
								#
								# Close succesfull retrieval of bf
							} else {
								retrievalFailure <- TRUE
							}
						} else {
							# hence, no data
							retrievalFailure <- TRUE
						}
					} # Close state retrieval block ---- 
					
					# No retrieval: if perform==run, run the analysis ---
					if (retrievalFailure) {
						# Results cannot be retrieved from state
						
						if (perform == "run") {
							# Note: Data screening 
							#
							v1 <- dataset[[ .v(variable.name) ]]
							v2 <- dataset[[ .v(variable.2.name) ]]
							
							if (missingValues=="excludePairwise"){
								pairwise.excluded.data <- .excludePairwiseCorData(v1, v2)
								v1 <- pairwise.excluded.data$v1
								v2 <- pairwise.excluded.data$v2
							}
							
							# Note: Data: PREPARE
							#
							some.r <- cor(v1, v2)
							some.n <- length(v1)
							if(test == "kendall"){some.r <- cor(v1,v2,method = "kendall")}
							
							# Initialise output
							all.bfs <- list(bf10=NA, bfPlus0=NA, bfMin0=NA)
							method.number <- 1
							
							# Note: Data and bfs check [start]
							if (is.na(some.r) || some.n <= 1) {
								# Note: Data: NOT ok, 
								# 		bf10: can't
								if (some.n <= 1){
									some.footnote <- "Pearson's sample correlation coefficient r is undefined -- too few observations"
									index <- .addFootnote(footnotes, some.footnote)
								} else if (base::any(base::is.infinite(v1)) || base::any(base::is.infinite(v2))) {
									some.footnote <- "Pearson's sample correlation coefficient r is undefined -- one or more variables contain infinity"
									index <- .addFootnote(footnotes, some.footnote)
								} else {
									some.footnote <- "Pearson's sample correlation coefficient r is undefined -- one or more variables do not vary"
									index <- .addFootnote(footnotes, some.footnote)
								}
								
								some.r <- NaN
								
								# Store in state
								#
								if (missingValues=="excludePairwise"){
									footnotesListExcludePairwise[[variable.name]][[column.name]] <- list(some.footnote)
								} else if (missingValues=="excludeListwise"){
									# TODO: these should be defined above in the retrieval block
									n.label <- as.character(round(some.n))
									r.label <- as.character(round(some.r))
									footnotesListExcludeListwise[[n.label]][[r.label]] <- list(some.footnote)
								}
								
								#row.footnotes[[variable.2.name]] <- c(row.footnotes[[variable.name]], list(index))
								row.footnotes[[column.name]] <- c(row.footnotes[[column.name]], list(index))
								
								if (some.n==1){
									all.bfs$bf10 <- 1
								}
							} else {
								# Data: OK
								# Try: bfs
								
								while (any(is.na(c(all.bfs$bf10, all.bfs$bfPlus0, all.bfs$bfMin0))) && method.number <=3){
									# Note: Try all normal methods
									if (credibleIntervals==TRUE){
										all.bfs <- .bfCorrieKernel(n=some.n, r=some.r, kappa=priorWidth, method=method.number, ciValue=credibleIntervalsInterval)
									}
									# Calculated at standard level 0.95
									all.bfs <- .bfCorrieKernel(n=some.n, r=some.r, kappa=priorWidth, method=method.number)
									method.number <- method.number + 1
								}
								
								if (any(is.na(c(all.bfs$bf10, all.bfs$bfPlus0, all.bfs$bfMin0)))){
									# Note: all normal methods FAILED. Use Jeffreys approximation
									# NO CI INTERVALS FOR credibleIntervalsInterval
									all.bfs <- .bfCorrieKernel(n=some.n, r=some.r, kappa=priorWidth, method="jeffreysApprox")
								}
							  if(test == "kendall"){
							    all.bfs <- .bfCorrieKernelKendallTau(n=some.n, tau=some.r, kappa = priorWidth, var = 1)
							  }
							}
							
							# Store in State
							#
							if (missingValues=="excludePairwise"){
								rValuesListExcludePairwise[[variable.name]][[column.name]] <- list(some.r)
								bfValuesListExcludePairwise[[prior.label]][[variable.name]][[column.name]] <- all.bfs
							} else if (missingValues=="excludeListwise"){
								bfValuesListExcludeListwise[[prior.label]][[n.label]][[r.label]] <- all.bfs
							}
							resultProcessing <- TRUE
						}
					}
						
					# Processing and reporting ---
					if (resultProcessing){
						# Note: Result reporting: sample r
						# TODO: also for other kappas and find place to report the credible interval
						
						report.r <- some.r
						row[[length(row)+1]] <- .clean(report.r)
						
						
						# Note: Result processing: bf
						# 
						if (hypothesis == "correlated") {
							report.bf <- all.bfs$bf10
							
							if (bayesFactorType == "BF01"){
								report.bf <- 1/report.bf
							}
						} else if (hypothesis == "correlatedPositively"){
							# TODO: Still need to implement this for general rho0, rather than rho0=0
							
							report.bf <- all.bfs$bfPlus0
							
							if (bayesFactorType == "BF01"){
								report.bf <- 1/report.bf
							}
						} else if (hypothesis == "correlatedNegatively") {
							report.bf <- all.bfs$bfMin0
							
							if (bayesFactorType == "BF01"){
								report.bf <- 1/report.bf
							}
						} 
						
						# Note: Flagging at the data [report]
						if (flagSupported && is.na(report.bf) == FALSE) {
							if (report.bf > 100) {
								row.footnotes[[column.name]] <- list("***")
							} else if (report.bf > 30) {
								row.footnotes[[column.name]] <- list("**")
							} else if (report.bf > 10) {
								row.footnotes[[column.name]] <- list("*")
							}
						}
						
						# Note: Flagging and report bfs [report]
						if (reportBayesFactors) {
							if (bayesFactorType == "LogBF10") {
								report.bf <- base::log10(report.bf)
							}
							
							bayes.factors[[length(bayes.factors)+1]] <- .clean(report.bf)
						}
					} # Close: Results reporting from 1 retrieved or 2 performed
					
					
					# No retrieval and no performance:
					#
					if (retrievalFailure && perform!="run"){
						# No data retrieved from state and nothing run
						row[[length(row)+1]] <- "."
						bayes.factors[[length(bayes.factors)+1]] <- "."
					} # Close: Results reporting from initialisation
				} # Close loop: each column
				if (reportBayesFactors) {
					for (bf in bayes.factors){
						row[[length(row)+1]] <- bf
					}
				}
			} # Close loop: each test	
			names(row) <- column.names
			row[[".variable"]] <- variable.name
			if (length(row.footnotes) > 0)
				row[[".footnotes"]] <- row.footnotes
			rows[[i]] <- row
		} # Note: close each variable
	}
	schema <- list(fields=fields)
	correlation.table[["schema"]] <- schema
	correlation.table[["data"]] <- rows
	correlation.table[["footnotes"]] <- as.list(footnotes)
	
	return(list(correlationTable=correlation.table, 
				test=tests, 
				variables=variables, 
				rows=rows, 
				rValuesExcludePairwise=rValuesListExcludePairwise,
				bfValuesExcludePairwise=bfValuesListExcludePairwise,
				footnotesExcludePairwise=footnotesListExcludePairwise,
				bfValuesExcludeListwise=bfValuesListExcludeListwise,
				footnotesExcludeListwise=footnotesListExcludeListwise))
}
## Help functions ------------------------------------------------------------
# 0.1 Prior specification Pearson's Rho
.excludePairwiseCorData <- function(v1, v2){
	# To exclude the data pairwise
	#
	screened.data <- list(v1=v1, v2=v2)
	
	remove.index.1 <- which(is.na(v1))
	remove.index.2 <- which(is.na(v2))
	remove.index <- unique(c(remove.index.1, remove.index.2))
	if (length(remove.index) > 0){
		screened.data$v1 <- v1[-(remove.index)]
		screened.data$v2 <- v2[-(remove.index)]
	}
	
	return(screened.data)
}

.stretchedBeta <- function(rho, alpha, beta){
	result <- 1/2*dbeta((rho+1)/2, alpha, beta)
	return(result)
}


.priorRho <- function(rho, kappa=1) {
	.stretchedBeta(rho, 1/kappa, 1/kappa)
}

.priorRhoPlus <- function(rho, kappa=1) {
	non.negative.index <- rho >=0
	less.than.one.index <- rho <=1
	value.index <- as.logical(non.negative.index*less.than.one.index)
	result <- rho*0
	result[value.index] <- 2*.priorRho(rho[value.index], kappa)
	return(result)
}

.priorRhoMin <- function(rho, kappa=1) {
	negative.index <- rho <=0
	greater.than.min.one.index <- rho >= -1
	value.index <- as.logical(negative.index*greater.than.min.one.index)
	result <- rho*0
	result[value.index] <- 2*.priorRho(rho[value.index], kappa)
	return(result)
}
# 0.2 Prior specification Kendall's Tau
.scaledBetaTau <- function(tau, alpha=1, beta=1){
  result <-   ((pi*2^(-2*alpha))/beta(alpha,alpha))  * cos((pi*tau)/2)^(2*alpha-1)
  return(result)
}

.priorTau <- function(tau, kappa){
  .scaledBetaTau(tau, alpha = (1/kappa), beta = (1/kappa))
}

.priorTauPlus <- function(tau, kappa=1) {
  non.negative.index <- tau >=0
  less.than.one.index <- tau <=1
  value.index <- as.logical(non.negative.index*less.than.one.index)
  result <- tau*0
  result[value.index] <- 2*.priorTau(tau[value.index], kappa)
  return(result)
}

.priorTauMin <- function(tau, kappa=1) {
  negative.index <- tau <=0
  greater.than.min.one.index <- tau >= -1
  value.index <- as.logical(negative.index*greater.than.min.one.index)
  result <- tau*0
  result[value.index] <- 2*.priorTau(tau[value.index], kappa)
  return(result)
}

# 1.0. Built-up for likelihood functions
.aFunction <- function(n, r, rho) {
	#hyper.term <- Re(hypergeo::hypergeo(((n-1)/2), ((n-1)/2), (1/2), (r*rho)^2))
	hyper.term <- Re(hypergeo::genhypergeo(U=c((n-1)/2, (n-1)/2), L=(1/2), z=(r*rho)^2))
	result <- (1-rho^2)^((n-1)/2)*hyper.term
	return(result)
}

.bFunction <- function(n, r, rho) {
	#hyper.term.1 <- Re(hypergeo::hypergeo((n/2), (n/2), (1/2), (r*rho)^2))
	#hyper.term.2 <- Re(hypergeo::hypergeo((n/2), (n/2), (-1/2), (r*rho)^2))
	#hyper.term.1 <- Re(hypergeo::genhypergeo(U=c(n/2, n/2), L=(1/2), z=(r*rho)^2))
	#hyper.term.2 <- Re(hypergeo::genhypergeo(U=c(n/2, n/2), L=(-1/2), z=(r*rho)^2))
	#result <- 2^(-1)*(1-rho^2)^((n-1)/2)*exp(log.term)*
	#	((1-2*n*(r*rho)^2)/(r*rho)*hyper.term.1-(1-(r*rho)^2)/(r*rho)*hyper.term.2)
	#
	hyper.term <- Re(hypergeo::genhypergeo(U=c(n/2, n/2), L=(3/2), z=(r*rho)^2))
	log.term <- 2*(lgamma(n/2)-lgamma((n-1)/2))+((n-1)/2)*log(1-rho^2)
	result <- 2*r*rho*exp(log.term)*hyper.term
	return(result)
}

.hFunction <- function(n, r, rho) {
	result <- .aFunction(n, r, rho) + .bFunction(n, r, rho)
	return(result)
}


.jeffreysApproxH <- function(n, r, rho) {	
	result <- ((1 - rho^(2))^(0.5*(n - 1)))/((1 - rho*r)^(n - 1 - 0.5))
	return(result)
}

# 1.1 Explicit marginal likelihood functions
.m0MarginalLikelihood <- function(s, t, n) {
	log.term <- 2*lgamma(0.5*(n-1))
	result <- 1/4*n^(0.5*(1-2*n))*pi^(1-n)*(s*t)^(1-n)*exp(log.term)
	return(result)
}

.m1MarginalLikelihoodNoRho <- function(s, t, n, r, rho) {
	return(.m0MarginalLikelihood(s, t, n)*
		   	(.aFunction(n, r, rho)+.bFunction(n, r, rho)))
}

#
# 2.1 Two-sided main Bayes factor ----------------------------------------------
.bf10Exact <- function(n, r, kappa=1) {
	# Ly et al 2015
	# This is the exact result with symmetric beta prior on rho
	# with parameter alpha. If kappa = 1 then uniform prior on rho
	#
	#
	if (n <= 2){
		return(1)
	} else if (any(is.na(r))){
		return(NaN)
	}
	# TODO: use which
	check.r <- abs(r) >= 1 # check whether |r| >= 1
	if (kappa >= 1 && n > 2 && check.r) {
		return(Inf)
	}
	#log.hyper.term <- log(hypergeo::hypergeo(((n-1)/2), ((n-1)/2), ((n+2/kappa)/2), r^2))
	log.hyper.term <- log(hypergeo::genhypergeo(U=c((n-1)/2, (n-1)/2), L=((n+2/kappa)/2), z=r^2))
	log.result <- log(2^(1-2/kappa))+0.5*log(pi)-lbeta(1/kappa, 1/kappa)+
		lgamma((n+2/kappa-1)/2)-lgamma((n+2/kappa)/2)+log.hyper.term
	real.result <- exp(Re(log.result))
	#return(realResult)
	return(real.result)
}

# 2.2 Two-sided secondairy Bayes factor
.bf10JeffreysIntegrate <- function(n, r, kappa=1) {
	# Jeffreys' test for whether a correlation is zero or not
	# Jeffreys (1961), pp. 289-292
	# This is the exact result, see EJ
	##
	if (n <= 2){
		return(1)
	} else if ( any(is.na(r)) ){
		return(NaN)
	}
	
	# TODO: use which
	if (n > 2 && abs(r)==1) {
		return(Inf)
	}
	hyper.term <- Re(hypergeo::genhypergeo(U=c((2*n-3)/4, (2*n-1)/4), L=(n+2/kappa)/2, z=r^2))
	log.term <- lgamma((n+2/kappa-1)/2)-lgamma((n+2/kappa)/2)-lbeta(1/kappa, 1/kappa)
	result <- sqrt(pi)*2^(1-2/kappa)*exp(log.term)*hyper.term
	return(result)
}

# 2.3 Two-sided third Bayes factor
.bfCorNumerical <- function(n, r, kappa=1, lowerRho=-1, upperRho=1) {
	# Numerically integrate Jeffreys approximation of the likelihood
	integrand <- function(rho){.jeffreysApproxH(n, r, rho)*.priorRho(rho, kappa)}
	some.integral <- try(integrate(integrand, lowerRho, upperRho))
	
	if (is(some.integral, "try-error")) {
		return(NA)
	}
	
	if (some.integral$message=="OK"){
		return(some.integral$value)
	} else {
		return(NA)
	}
}

.bf10Numerical <- function(n, r, kappa=1, lowerRho=-1, upperRho=1) {
	# Jeffreys' test for whether a correlation is zero or not
	# Jeffreys (1961), pp. 289-292
	# This is a numerical approximation for .bf10JeffreysIntegrate,
	# when it explodes
	# #
	# TODO: 1. check for n=1, n=2, as r is then undefined
	# 2. check for r=1, r=-1
	#
	# TODO: REMOVE ALL NUMERICAL STUFF
	if ( any(is.na(r)) ){
		return(NA)
	}
	# TODO: use which
	if (n > 2 && abs(r)==1) {
		return(Inf)
	}
	
	
	# TODO: be very careful here, might integrate over non-finite function
	jeffreysNumericalIntegrate <- .bfCorNumerical(n, r, kappa, lowerRho=-1, upperRho=1)
	
	if (is.na(jeffreysNumericalIntegrate) || jeffreysNumericalIntegrate < 0){
		return(NA)
	} else if (jeffreysNumericalIntegrate >= 0){
		# jeffreys numerical integrate success
		return(jeffreysNumericalIntegrate)
	} else {
		# NO IDEA, EVERYTHING FAILED :(
		return(NA)
	}
	return(jeffreysNumericalIntegrate)
}

# 2.4 The Marsman MH sampler 

.logTarget <- function(z, n, r, kappa){
	# z is Fisher's transformation for r, but also use it for rho
	# The Fisher z transform and the log (likelihood*prior*Jacobian) of the tranformation
	(0.5*(n - 1))*log(1 - tanh(z)^2) - (n - 1 - 0.5)*log(1 - tanh(z)*r)+log(1-tanh(z)^2)/kappa
}

.logProposal <- function(z, n, r){
	# z is Fisher's transformation for r, but also use it for rho
	# The log sampling distribution as per Fisher approximation, however, swtiched the role of r and rho 
	# in the mean. This is reasonable as the sampling distribution of r looks a bit like that of rho (the real one)
	# Hence, this is normal distribution
	# A Rabbi and a Priest buy a car together and it's being stored at the Priest's house. One day the 
	# Rabbi goes over to use the car and he sees him sprinkling water on it. The Rabbi asked, ''What are 
	# you doing?'' The Priest responded, ''I'm blessing the car.'' So the Rabbi said ''Okay, since we're 
	# doing that....'' and takes out a hacksaw and cuts two inches off the tail pipe.
	-(n-3)/2*(z-atanh(r))^2
}


.metropolisOneStep <- function (rhoCurrent, n, r, kappa=1) {
	# 
	zCurrent <- atanh(rhoCurrent)
	zCandidate <- rnorm(1, mean=atanh(r), sd=1/sqrt(n-3))
	
	candidateAcceptance <- .logTarget(zCandidate, n, r, kappa)+.logProposal(zCurrent, n, r)-
		(.logTarget(zCurrent, n, r, kappa)+.logProposal(zCandidate, n, r))
	
	if (log(runif (1)) <= candidateAcceptance) {
		rhoCandidate <- tanh(zCandidate)
		return(rhoCandidate)
	} else {
		return (rhoCurrent)
	}
}

.marsmanMHSampler <- function(n, r, kappa=1, nIters=50000){
	rhoMetropolisChain <- NULL
	yTemp <- r
	
	for (iter in 1:nIters) {
		yTemp <- .metropolisOneStep(yTemp, n, r, kappa)
		rhoMetropolisChain[iter] <- yTemp
	}
	
	acceptanceRate <- length (unique (rhoMetropolisChain)) / nIters
	
	metropolisVar <- var(rhoMetropolisChain)/2^2
	metropolisMean <- mean((rhoMetropolisChain+1)/2)
	
	mhFit <- .betaParameterEstimates(metropolisMean, metropolisVar)
	mhFit$acceptanceRate <- acceptanceRate
	return(mhFit)
}

# 2.5. Two-sided fourth Bayes factor
.bf10JeffreysApprox <- function(n, r) {
	#Jeffreys' test for whether a correlation is zero or not
	#Jeffreys (1961), pp. 291 Eq. 14
	#
	if (n <= 2){
		return(1)
	} else if ( any(is.na(r)) ){
		return(NA)
	}
	# TODO: use which
	if (n > 2 && abs(r)==1) {
		return(Inf)
	}
	result <- ((2*n-3)/pi)^(.5)*(1-r^2)^((n-4)/2)
	return(1/result)
}

# 3.0 One-sided preparation ----------------------------------------------------
# For .bfPlus0Exact
# For .bfPlus0Exact
.mPlusExact <- function(n, r, kappa=1){
	# Ly et al 2015
	# This is the contribution of one-sided test
	#
	#
	# TODO: 1. check for n=1, n=2, as r is then undefined
	# 2. check for r=1, r=-1
	#
	#hyper.term.1 <- Re(hypergeo::genhypergeo(U=c(1, (n+2)/2, (n+2)/2),
	#										 L=c(1/2, (n+2/kappa+3)/2), z=r^2))
	#hyper.term.2 <- Re(hypergeo::genhypergeo(U=c(1, (n+2)/2, (n+2)/2),
	#										 L=c(3/2, (n+2/kappa+1)/2), z=r^2))
	#hyper.term.3 <- Re(hypergeo::genhypergeo(U=c(1, (n+2)/2, (n+2)/2),
	#										 L=c(3/2, (n+2/kappa+3)/2), z=r^2))
	#sum.term <- -((n*r)^2*hyper.term.1-n^2*(n+2/kappa+1)*hyper.term.2+2*n^3*r^2*
	#			  	hyper.term.3+(2*n^2-2/kappa*(1-2*n)+n-1))
	#product.term <- (2^(1-2/kappa)*r)/((n+2/kappa-1)*(n+2/kappa+1))
	#log.term <- 2*lgamma(n/2)-2*lgamma((n+1)/2)-lbeta(1/kappa, 1/kappa)
	#result <- product.term*exp(log.term)*sum.term
	
	hyper.term <- Re(hypergeo::genhypergeo(U=c(1, n/2, n/2), L=c(3/2, (2+kappa*(n+1))/(2*kappa)), z=r^2))
	log.term <- 2*(lgamma(n/2)-lgamma((n-1)/2))-lbeta(1/kappa, 1/kappa)
	result <- 2^((3*kappa-2)/kappa)*kappa*r/(2+(n-1)*kappa)*exp(log.term)*hyper.term
	return(result)
}

# For .bfPlus0EJeffreysIntegrate
.mPlusJeffreysIntegrate <- function(n, r, kappa=1){
	# Ly et al 2015
	# This is the exact result with symmetric beta prior on rho
	# This is the contribution of one-sided test
	#
	#	
	hyper.term <- Re(hypergeo::genhypergeo(U=c(1, (2*n-1)/4, (2*n+1)/4),
										   L=c(3/2, (n+1+2/kappa)/2), z=r^2))
	log.term <- -lbeta(1/kappa, 1/kappa)
	result <- 2^(1-2/kappa)*r*(2*n-3)/(n+2/kappa-1)*exp(log.term)*hyper.term
	return(result)
}

.bfPlus0Numerical <- function(n, r, kappa=1, lowerRho=0, upperRho=1){
	# Ly et al 2015
	# This is a numerical approximation
	# with parameter kappa. If kappa = 1 then uniform prior on rho
	# bf positive vs null
	#
	# Ly et al 2015
	# This is the contribution of one-sided test
	#
	#
	if ( any(is.na(r)) ){
		return(NA)
	}
	if (kappa >= 1 && n > 2 && r>=1) {
		return(Inf)
	} else if (kappa >= 1 && n > 2 && r<=-1){
		return(0)
	}
	
	my.numerical.Jeffreys <- .bfCorNumerical(n, r, kappa, lowerRho, upperRho)
	# TODO: be very careful here, might integrate over non-finite function
	# in particular with the exact h function. 
	#
	if (!is.na(my.numerical.Jeffreys) && my.numerical.Jeffreys >= 0){
		# Note: Numerical Jeffreys okay
		return(my.numerical.Jeffreys)
	} else if (is.na(my.numerical.Jeffreys) || my.numerical.Jeffreys < 0){
		# All numerical failed
		return(NaN)
	} 
	return (NaN)
}

.bfPlus0JeffreysIntegrate <- function(n, r, kappa=1){
	# Ly et al 2015
	# This is the exact result with symmetric beta prior on rho
	#	
	if ( any(is.na(r)) ){
		return(NaN)
	}
	if (n > 2 && r>=1) {
		return(Inf)
	} else if (n > 2 && r<=-1){
		return(0)
	}
	
	bf10 <- .bf10JeffreysIntegrate(n, r, kappa)
	m.plus <- .mPlusJeffreysIntegrate(n, r, kappa)
	
	if (is.na(bf10) || is.na(m.plus)){
		return(NA)
	}
	
	result <- bf10+m.plus	
	return(result)
}

## Suit:
.bfCorrieKernel <- function(n, r, kappa=1, method="exact", ciValue=0.95){
	# The idea is incremental when it comes to method numbers, if 1 doesn't work then go down. 
	# In particular, when 
	#	methodNumber=1 we use the exact result Ly et al (2015)
	#	methodNumber=2 we use the semi-exact result, based on approximation of the likelihood JeffreysExact, see Wagenmakers et al (2015) bathing
	#	methodNumber=3 we use a numerical approximation of Jeffreys approximation likleihood
	#	methodNumber=4 we use the Marsman sampler and report a posterior beta fit summarised by betaA, betaB
	# 	methodNumber=5 we use the Jeffreys approximation 
	#   methodNumber=6 we use the Jeffreys BF the asymptotic approximation of the BF
	#
	# Ex. 	if we want confidence intervals and the bfs are based on method 1, 
	#		then we can find betaA, betaB based on .posteriorMean and .posteriorVariance
	# 		if this doens't work we use the Marsman sampler
	# Ex.	if we retrieve bfs from methodNumber 4, we know that we already have betaA and betaB, 
	#		so we do not need to try .posteriorMean and .posteriorVariance 
	# Ex.	if we retrieve bfs from methodNumber 6, we don't know anything about the posterior, so no report at all
	#
	tempList <- list(vector())
	output <- list(n=n, r=r, bf10=NA, bfPlus0=NA, bfMin0=NA, methodNumber=NA, betaA=NA, betaB=NA, 
				   twoSidedTooPeaked=FALSE, plusSidedTooPeaked=FALSE, minSidedTooPeaked=FALSE, 
				   CIs=tempList, ciValues=ciValue, acceptanceRate=1)
	# Note: If log(bf10) then use beta fit for the one sided bfs
	# The 24 is based on n=430, r=-0.3500786
	hyperGeoOverFlowThreshold <- 24

	# Note: Data check
	#
	if (any(is.na(r)) ){
		output$methodNumber <- 6
		output$twoSidedTooPeaked <- TRUE 
		output$plusSidedTooPeaked <- TRUE 
		output$minSidedTooPeaked <- TRUE
		return(output)
	}
	
	# Note: Data: OK
	# "No" prior, alternative model is the same as the null model
	# TODO: however this bound of 0.002 is arbitrarily chosen. I should choose this based on a trade off
	# between r and n, but it doesn't really matter. 
	if (kappa <= 0.002){
		output$bf10 <- 1
		output$bfPlus0 <- 1
		output$bfMin0 <- 1
		output$methodNumber <- 6
		return(output)
	}
	
	check.r <- abs(r) >= 1 # check whether |r| >= 1
	if (n <= 2 || kappa==0){
		output$bf10 <- 1
	} else if (kappa >= 1 && n > 2 && check.r) {
		output$bf10 <- Inf
		output$twoSidedTooPeaked <- TRUE 
		output$plusSidedTooPeaked <- TRUE 
		output$minSidedTooPeaked <- TRUE
		if (r > 0){
			output$bfPlus0 <- Inf
			output$bfMin0 <- 0
		} else if (r <= 0){
			output$bfPlus0 <- 0
			output$bfMin0 <- Inf
		}
		output$methodNumber <- 6
		return(output)
	}
	
	# Note: Different methods
	#
	if (method=="exact" || method==1){
		output$methodNumber <- 1
		output$bf10 <- .bf10Exact(n, r, kappa)
		
		# Note: bf10: CHECK
		if (is.na(output$bf10) || output$bf10 < 0){
			output$bf10 <- NA
			
			# Posterior not interesting
			output$twoSidedTooPeaked <- TRUE 
			output$plusSidedTooPeaked <- TRUE 
			output$minSidedTooPeaked <- TRUE
			return(output)
		}
		
		# Note: bf10: EXTREME
		if (base::is.infinite(output$bf10)){
			# Note: Check extreme
			if (r >= 0){
				output$bfPlus0 <- Inf
				output$bfMin0 <- 0
			} else if (r < 0){
				output$bfPlus0 <- 0
				output$bfMin0 <- Inf
			}
			# Posterior is too peaked
			output$twoSidedTooPeaked <- TRUE 
			output$plusSidedTooPeaked <- TRUE 
			output$minSidedTooPeaked <- TRUE
			return(output)
		} else if (!base::is.infinite(output$bf10)){
			# Calculate beta fit
			someFit <- .posteriorBetaParameters(n, r, kappa)
			# Store output
			output$betaA <- someFit$betaA
			output$betaB <- someFit$betaB
			
			if (log(output$bf10) > hyperGeoOverFlowThreshold){
				# Avoid overflow in hypergeo
				# Recalculate BF10
				savageDickyNumerator <- .priorRho(0, kappa)
				savageDickyDenominator <- .stretchedBeta(0, someFit$betaA, someFit$betaB)
				#
				output$bf10 <- savageDickyNumerator/savageDickyDenominator
				
				if (base::is.infinite(output$bf10)){
					# Note: Check extreme
					if (r >= 0){
						output$bfPlus0 <- Inf
						output$bfMin0 <- 0
					} else if (r < 0){
						output$bfPlus0 <- 0
						output$bfMin0 <- Inf
					}
					# Posterior is too peaked
					output$twoSidedTooPeaked <- TRUE 
					output$plusSidedTooPeaked <- TRUE 
					output$minSidedTooPeaked <- TRUE
					return(output)
				} else if (!base::is.infinite(output$bf10)){
					# Note: bfPlus0, bfMin0: PREPARE
					leftProportion <- pbeta(1/2, someFit$betaA, someFit$betaB)
					
					if (!is.na(leftProportion) && leftProportion <1){
						output$bfMin0 <- 2*output$bf10*leftProportion
						output$bfPlus0 <- 2*output$bf10*(1-leftProportion)	
					} else {
						rightProportion <- pbeta(1/2, someFit$betaA, someFit$betaB, lower.tail=FALSE)
						output$bfMin0 <- 2*output$bf10*(1-rightProportion)
						output$bfPlus0 <- 2*output$bf10*rightProportion
					}
				}
			} else {
				# Note: bfPlus0, bfMin0: PREPARE
				output$bfPlus0 <- output$bf10 + .mPlusExact(n, r, kappa)
				output$bfMin0 <- output$bf10 + .mPlusExact(n, -r, kappa)
			}
		}
	} else if (method=="jeffreysIntegrate" || method==2){
		output$methodNumber <- 2
		output$bf10 <- .bf10JeffreysIntegrate(n, r, kappa)
		
		# Note: bf10: CHECK
		if (is.na(output$bf10) || output$bf10 < 0){
			output$bf10 <- NA
			
			# Posterior not interesting
			output$twoSidedTooPeaked <- TRUE 
			output$plusSidedTooPeaked <- TRUE 
			output$minSidedTooPeaked <- TRUE
			return(output)
		}
		
		# Note: bf10: EXTREME
		if (base::is.infinite(output$bf10)){
			# Note: Check extreme
			if (r >= 0){
				output$bfPlus0 <- Inf
				output$bfMin0 <- 0
			} else if (r < 0){
				output$bfPlus0 <- 0
				output$bfMin0 <- Inf
			}
			# Posterior too peaked
			output$twoSidedTooPeaked <- TRUE 
			output$plusSidedTooPeaked <- TRUE 
			output$minSidedTooPeaked <- TRUE
			
			return(output)
		} else if (!base::is.infinite(output$bf10)){
			# Calculate beta fit
			someFit <- .posteriorBetaParameters(n, r, kappa)
			# Store output
			output$betaA <- someFit$betaA
			output$betaB <- someFit$betaB
			
			if (log(output$bf10) > hyperGeoOverFlowThreshold){
				# To avoid overflow in the hypergeometric function
				# Recalculate BF10
				savageDickyNumerator <- .priorRho(0, kappa)
				savageDickyDenominator <- .stretchedBeta(0, someFit$betaA, someFit$betaB)
				#
				output$bf10 <- savageDickyNumerator/savageDickyDenominator
				
				if (base::is.infinite(output$bf10)){
					# Note: Check extreme
					if (r >= 0){
						output$bfPlus0 <- Inf
						output$bfMin0 <- 0
					} else if (r < 0){
						output$bfPlus0 <- 0
						output$bfMin0 <- Inf
					}
					# Posterior is too peaked
					output$twoSidedTooPeaked <- TRUE 
					output$plusSidedTooPeaked <- TRUE 
					output$minSidedTooPeaked <- TRUE
					return(output)
				} else if (!base::is.infinite(output$bf10)){
					# Note: bfPlus0, bfMin0: PREPARE
					leftProportion <- pbeta(1/2, someFit$betaA, someFit$betaB)
					
					if (!is.na(leftProportion) && leftProportion <1){
						output$bfMin0 <- 2*output$bf10*leftProportion
						output$bfPlus0 <- 2*output$bf10*(1-leftProportion)	
					} else {
						rightProportion <- pbeta(1/2, someFit$betaA, someFit$betaB, lower.tail=FALSE)
						output$bfMin0 <- 2*output$bf10*(1-rightProportion)
						output$bfPlus0 <- 2*output$bf10*rightProportion
					}
				}
			} else {
				# Note: bfPlus0, bfMin0: PREPARE
				output$bfPlus0 <- output$bf10 + .mPlusJeffreysIntegrate(n, r, kappa)
				output$bfMin0 <- output$bf10 + .mPlusJeffreysIntegrate(n, -r, kappa)
			}
		}
	} else if (method=="numerical" || method==3){
		output$methodNumber <- 3
		output$bf10 <- .bf10Numerical(n, r, kappa, lowerRho=-1, upperRho=1)
		
		# Note: bf10: CHECK
		if (is.na(output$bf10) || output$bf10 < 0){
			output$bf10 <- NA
			
			# Posterior not interesting
			output$twoSidedTooPeaked <- TRUE 
			output$plusSidedTooPeaked <- TRUE 
			output$minSidedTooPeaked <- TRUE
			return(output)
		}
		
		# Note: bf10: EXTREME
		if (base::is.infinite(output$bf10)){
			# Note: Check extreme
			if (r >= 0){
				output$bfPlus0 <- Inf
				output$bfMin0 <- 0
			} else if (r < 0){
				output$bfPlus0 <- 0
				output$bfMin0 <- Inf
			}
			
			# Posterior too peaked
			output$twoSidedTooPeaked <- TRUE 
			output$plusSidedTooPeaked <- TRUE 
			output$minSidedTooPeaked <- TRUE
			return(output)
		} else if (!base::is.infinite(output$bf10)){
			# Calculate beta fit
			someFit <- .posteriorBetaParameters(n, r, kappa)
			# Store output
			output$betaA <- someFit$betaA
			output$betaB <- someFit$betaB
			
			if (log(output$bf10) > hyperGeoOverFlowThreshold){
				# To avoid overflow in the numerical integration
				# Recalculate BF10
				savageDickyNumerator <- .priorRho(0, kappa)
				savageDickyDenominator <- .stretchedBeta(0, someFit$betaA, someFit$betaB)
				#
				output$bf10 <- savageDickyNumerator/savageDickyDenominator
				
				if (base::is.infinite(output$bf10)){
					# Note: Check extreme
					if (r >= 0){
						output$bfPlus0 <- Inf
						output$bfMin0 <- 0
					} else if (r < 0){
						output$bfPlus0 <- 0
						output$bfMin0 <- Inf
					}
					# Posterior is too peaked
					output$twoSidedTooPeaked <- TRUE 
					output$plusSidedTooPeaked <- TRUE 
					output$minSidedTooPeaked <- TRUE
					return(output)
				} else if (!base::is.infinite(output$bf10)){
					# Note: bfPlus0, bfMin0: PREPARE
					leftProportion <- pbeta(1/2, someFit$betaA, someFit$betaB)
					
					if (!is.na(leftProportion) && leftProportion <1){
						output$bfMin0 <- 2*output$bf10*leftProportion
						output$bfPlus0 <- 2*output$bf10*(1-leftProportion)	
					} else {
						rightProportion <- pbeta(1/2, someFit$betaA, someFit$betaB, lower.tail=FALSE)
						output$bfMin0 <- 2*output$bf10*(1-rightProportion)
						output$bfPlus0 <- 2*output$bf10*rightProportion
					}
				}
			} else {
				# Note: bfPlus0, bfMin0: PREPARE
				output$bfPlus0 <- .bfPlus0Numerical(n, r, kappa, lowerRho=0, upperRho=1)
				output$bfMin0 <- .bfPlus0Numerical(n, r, kappa, lowerRho=-1, upperRho=0)
			}
		}
	} else if  (method=="metropolisHastings" || method==4){
		# We use the Marsman Sampler (c) here based on posterior model fit. 
		output$methodNumber <- 4
		marsmanResult <- .marsmanMHSampler(n,r,kappa)
		
		# Calculate bf10
		savageDickyNumerator <- .priorRho(0, kappa)
		savageDickyDenominator <- .stretchedBeta(0, marsmanResult$betaA, marsmanResult$betaB)
		
		# Save output
		output$bf10 <- savageDickyNumerator/savageDickyDenominator
		output$betaA <- marsmanResult$betaA
		output$betaB <- marsmanResult$betaB
		output$acceptanceRate <- marsmanResult$acceptanceRate
		
		# Note: bf10: CHECK
		if (is.na(output$bf10) || output$bf10 < 0){
			output$bf10 <- NA
			
			# Posterior not interesting
			output$twoSidedTooPeaked <- TRUE 
			output$plusSidedTooPeaked <- TRUE 
			output$minSidedTooPeaked <- TRUE
			return(output)
		}
		
		# Note: bf10: EXTREME
		if (base::is.infinite(output$bf10)){
			# Note: Check extreme
			if (r >= 0){
				output$bfPlus0 <- Inf
				output$bfMin0 <- 0
			} else if (r < 0){
				output$bfPlus0 <- 0
				output$bfMin0 <- Inf
			}
			# Posterior is too peaked
			output$twoSidedTooPeaked <- TRUE 
			output$plusSidedTooPeaked <- TRUE 
			output$minSidedTooPeaked <- TRUE
			return(output)
		} else if (!base::is.infinite(output$bf10)){
			# Note: bfPlus0, bfMin0: PREPARE
			leftProportion <- pbeta(1/2, marsmanResult$betaA, marsmanResult$betaB)
			
			if (!is.na(leftProportion) && leftProportion <1){
				output$bfMin0 <- 2*output$bf10*leftProportion
				output$bfPlus0 <- 2*output$bf10*(1-leftProportion)	
			} else {
				rightProportion <- pbeta(1/2, marsmanResult$betaA, marsmanResult$betaB, lower.tail=FALSE)
				output$bfMin0 <- 2*output$bf10*(1-rightProportion)
				output$bfPlus0 <- 2*output$bf10*rightProportion
			}
		}
	} else if (method=="jeffreysApprox" || method==5){
		output$methodNumber <- 5
		output$bf10 <- .bf10JeffreysApprox(n, r)
		
		# Note: bf10: CHECK
		if (is.na(output$bf10) || output$bf10 < 0){
			output$bf10 <- NA
			
			# Posterior not interesting
			output$twoSidedTooPeaked <- TRUE 
			output$plusSidedTooPeaked <- TRUE 
			output$minSidedTooPeaked <- TRUE
			return(output)
		}
		
		# Note: bf10: EXTREME
		if (base::is.infinite(output$bf10)){
			# Note: Check extreme
			if (r >= 0){
				output$bfPlus0 <- Inf
				output$bfMin0 <- 0
			} else if (r < 0){
				output$bfPlus0 <- 0
				output$bfMin0 <- Inf
			}
			
			# Posterior too peaked
			output$twoSidedTooPeaked <- TRUE 
			output$plusSidedTooPeaked <- TRUE 
			output$minSidedTooPeaked <- TRUE
			return(output)
		}
		return(output)
	}
	
	# Note: Calculate credible intervals
	if (!is.na(output$betaA) && !is.na(output$betaB)){
		output$CIs <- .credibleIntervals(output$betaA, output$betaB, ciValue)
	}
	
	# Note: bfPlus0, bfMin0: CHECK
	if (any(is.na(c(output$bfPlus0, output$bfMin0)))){
		# Note: bfPlus0, bfMin0: NOT ok
		# 	if one is NA, then both are NA
		output$bfPlus0 <- NA
		output$bfMin0 <- NA
		
		# Posterior not interesting 
		output$plusSidedTooPeaked <- TRUE 
		output$minSidedTooPeaked <- TRUE
		return(output)
	} else if (any(c(output$bfPlus0, output$bfMin0)==0)){
		# Note: bfPlus0, bfMin0: EXTREME
		# 	if one is extreme, so is the other
		if (output$bfPlus0==0){
			output$bfPlus0 <- 0
			output$bfMin0 <- Inf
		} else if (output$bfMin0==0){
			output$bfPlus0 <- Inf
			output$bfMin0 <- 0
		}
		
		# Posterior too peaked
		output$plusSidedTooPeaked <- TRUE 
		output$minSidedTooPeaked <- TRUE
		return(output)
	} 
	
	
	# Note: bfPlus0, bfMin0: CHECK COHERENCE:
	if (output$bfPlus0 > 1 && output$bfMin0 > 1 || any(c(output$bfPlus0, output$bfMin0)<0)){
		if (r>0){
			# Note: Data: OK, 
			# 		bf10: OK. 
			#		bfPlus0: OK
			#		bfMin0: NOT ok 
			# 
			# bfMin0 is bigger than one due to overflow: bfMin0 = 2*bf10 - bfPlus0. 
			# Example: 2*1.2.... 10^ 24 - 2.... 10^24 = 1... 10^12 (due to round off)
			#
			output$bfMin0 <- 10^(-317) 
			output$bfPlus0 <- 2*output$bf10 - output$bfMin0
		} else if (r < 0) {
			# Note: Data: OK, 
			# 		bf10: OK. 
			#		bfPlus0: NOT ok
			#		bfMin0: OK
			output$bfPlus0 <- 10^(-317) 
			output$bfMin0 <- 2*output$bf10 - output$bfPlus0
		}
	}
	return(output)
}

.postDensKendallTau <- function(delta,Tstar,n,kappa=1,var=var,test="two-sided"){ 
  if(test == "two-sided"){priorDens <- .priorTau(delta,kappa)
  } else if(test == "positive"){priorDens <- .priorTauPlus(delta,kappa)
  } else if(test == "negative"){priorDens <- .priorTauMin(delta,kappa)}
  priorDens <- .priorTau(delta,kappa)
  dens <- dnorm(Tstar,(1.5*delta*sqrt(n)),sd=sqrt(var))* priorDens
  return(dens)
}
.posteriorTau <- function(delta,kentau,n,kappa=1,var=1,test="two-sided"){
  Tstar <- (kentau * ((n*(n-1))/2))/sqrt(n*(n-1)*(2*n+5)/18)
  if(test == "two-sided"){lims <- c(-1,1)
  } else if(test == "positive"){lims <- c(0,1)
  } else if(test == "negative"){lims <- c(-1,0)}
  logicalCensor <- (delta >= lims[1] & delta <= lims[2])
  dens <- logicalCensor*.postDensKendallTau(delta,Tstar,n,kappa,var,test=test)/
    integrate(function(delta){.postDensKendallTau(delta,Tstar,n,kappa,var,test=test)},lims[1],lims[2])$value
} 

.bfCorrieKernelKendallTau <- function(tau, n, kappa=1, var=1, ciValue=0.95){ 
  tempList <- list(vector())
  output <- list(n=n, r=tau, bf10=NA, bfPlus0=NA, bfMin0=NA, methodNumber=NA, betaA=NA, betaB=NA, 
                 twoSidedTooPeaked=FALSE, plusSidedTooPeaked=FALSE, minSidedTooPeaked=FALSE, 
                 CIs=tempList, ciValues=ciValue, acceptanceRate=1)
  if (any(is.na(tau)) ){
    output$methodNumber <- 6
    output$twoSidedTooPeaked <- TRUE 
    output$plusSidedTooPeaked <- TRUE 
    output$minSidedTooPeaked <- TRUE
    return(output)
  }
  if (kappa <= 0.002){
    output$bf10 <- 1
    output$bfPlus0 <- 1
    output$bfMin0 <- 1
    output$methodNumber <- 6
    return(output)
  }
  
  output$bf10 <- .priorTau(0,kappa)/.posteriorTau(0,tau,n,kappa=kappa,var=var,test="two-sided")
  output$bfPlus0 <- .priorTauPlus(0,kappa)/.posteriorTau(0,tau,n,kappa=kappa,var=var,test="positive")
  output$bfMin0 <- .priorTauMin(0,kappa)/.posteriorTau(0,tau,n,kappa=kappa,var=var,test="negative")
  output$methodNumber <- NA
  return(output)
}


# Replication Bayes factors
# 
.bfR0Josine <- function(nOri, rOri, nRep, rRep, kappa=1){
	# Verhagen & Wagenmakers 2014
	integrand <- function(rho){.hFunction(nRep, rRep, rho)*.hFunction(nOri, rOri, rho)*.priorRho(rho, kappa)}
	logNumerator <- log(integrate(integrand, -1, 1)$value)
	logDenominator <- log(.bf10Exact(n=nOri, r=rOri, kappa))
	
	logBfR0 <- logNumerator-logDenominator
	return(exp(logBfR0))
}


# 4.0 Posteriors to graph TODO: we have to think about this, different
# results, thus, also switching of the illustrations?
#


# 4.1 Two-sided
.posteriorRho <- function(n, r, rho, kappa=1){
	if (!is.na(r) && !r==0){
		return(1/.bf10Exact(n, r, kappa)*.hFunction(n, r, rho)*.priorRho(rho, kappa))
	} else if (!is.na(r) && r==0){
		return(1/.bf10JeffreysIntegrate(n, r, kappa)*.jeffreysApproxH(n, r, rho)*.priorRho(rho, kappa))
	}	
}

.posteriorRhoPlus <- function(n, r, rho, kappa=1){
	if (!is.na(r) && !r==0){
		return(1/.bfCorrieKernel(n, r, kappa, method="exact")$bfPlus0*.hFunction(n, r, rho)*.priorRhoPlus(rho, kappa))
	} else if (!is.na(r) && r==0){
		return(1/.bfCorrieKernel(n, r, kappa, method="jeffreysIntegrate")$bfPlus0*.jeffreysApproxH(n, r, rho)*.priorRhoPlus(rho, kappa))
	}	
}

.posteriorRhoMin <- function(n, r, rho, kappa=1){
	if (!is.na(r) && !r==0){.approximatePosteriorRho
		return(1/.bfCorrieKernel(n, r, kappa, method="exact")$bfMin0*.hFunction(n, r, rho)*.priorRhoMin(rho, kappa))
	} else if (!is.na(r) && r==0){
		return(1/.bfCorrieKernel(n, r, kappa, method="jeffreysIntegrate")$bfMin0*.jeffreysApproxH(n, r, rho)*.priorRhoMin(rho, kappa))
	}	
	
}


.approximatePosteriorRho <- function(rho, n, r) {
  1/(1-rho^2)*dnorm(atanh(rho), mean=atanh(r), sd=1/sqrt(n))
}

.approximatePosteriorRhoPlus <- function(rho, n, r) {
  (.approximatePosteriorRho(rho,n,r) * (rho>0)) / (pnorm(0,mean=atanh(r),sd=1/sqrt(n),lower.tail = FALSE))
}

.approximatePosteriorRhoMin <- function(rho, n, r) {
    (.approximatePosteriorRho(rho,n,r) * (rho<0)) / (pnorm(0,mean=atanh(r),sd=1/sqrt(n)))
}
  


# 4.2 
.posteriorMean <- function(n, r, kappa=1){
	# Posterior mean of the .bf10Exact
	#	That is, (rho+1)/2, thus, on 0,1 scale to estimate a, b in a beta distribution
	#
	# TODO: add safeguard for large n as then hyper.term.1/hyper.term.2 is almost 1
	# 	and also for log.term almost being 1 (it works okay if I cut off the hyper.term.1 
	# 	with three terms and hyper.term.2 with three terms and then divide them, though, 
	#	this is rather bad as a formal procedure due to the fact that it violates the 
	#	definition of products of sum sequences. Though it yields a good approximation.
	#
	# 	if (abs(r) < 0.5 && n <= 200){
	# 		log.term <- 2*(lgamma(n/2)-lgamma((n-1)/2))
	# 		hyper.term.1 <- Re(hypergeo::hypergeo((n/2), (n/2), ((n+2/kappa+2)/2), r^2))
	# 		hyper.term.2 <- Re(hypergeo::hypergeo(((n-1)/2), ((n-1)/2), ((n+2/kappa)/2), r^2))
	# 		
	# 		some.factor <- exp(log.term)*hyper.term.1/hyper.term.2
	# 	} else {
	# 		# TODO: a linear approximation, needs proof
	# 		some.factor <- n/2
	# 	}
	#
	#log.term <- 2*(lgamma(n/2)-lgamma((n-1)/2))
	#
	# Note: interestingly, it breaks down from n=199 to n=200 when using hypergeo
	# that is:
	#
	# 	.posteriorMean(200, 0.8) yields -2.600069e+26
	# 	.posteriorMean(199, 0.8) yields 0.7948551
	#
	#hyper.term.1 <- Re(hypergeo::hypergeo((n/2), (n/2), ((n+2/kappa+2)/2), r^2))
	#hyper.term.2 <- Re(hypergeo::hypergeo(((n-1)/2), ((n-1)/2), ((n+2/kappa)/2), r^2))
	
	# Note: interestingly, it breaks down from n=199 to n=200 when using the integral form f15.3.1
	# that is:
	#
	# 	.posteriorMean(339, 0.8) yields 0.796992
	# 	.posteriorMean(340, 0.8) yields Inf
	# 
	# 		In hypergeo::f15.3.1((n/2), (n/2), ((n + 2 / kappa + 2)/2), r^2) :
	#			value out of range in 'gammafn'
	#
	#
	#hyper.term.1 <- Re(hypergeo::f15.3.1((n/2), (n/2), ((n+2/kappa+2)/2), r^2))
	#hyper.term.2 <- Re(hypergeo::f15.3.1(((n-1)/2), ((n-1)/2), ((n+2/kappa)/2), r^2))
	#
	# Note: interestingly, the continued fraction solution goes haywire, that is:
	#
	#
	#	.posteriorMean(n=67, 0.8) yielding 0.8526101  (a peak)
	#	.posteriorMean(n=299, 0.8) yielding -1.179415
	#
	#hyper.term.1 <- Re(hypergeo::hypergeo_contfrac((n/2), (n/2), ((n+2/kappa+2)/2), r^2))
	#hyper.term.2 <- Re(hypergeo::hypergeo_contfrac(((n-1)/2), ((n-1)/2), ((n+2/kappa)/2), r^2))
	#
	#hyper.term.1 <- Re(hypergeo::genhypergeo(U=c(n/2, n/2), L=c((n+2/kappa+2)/2), z=r^2))
	#hyper.term.2 <- Re(hypergeo::genhypergeo(U=c((n-1)/2, (n-1)/2), L=c((n+2/kappa)/2), z=r^2))
	#
	#some.factor <- exp(log.term)*hyper.term.1/hyper.term.2
	#
	#result <- 2*r/(n+2/kappa)*some.factor
	#return(result)
	
	if (n <= 2){
		return(NaN)
	} else if (any(is.na(r))){
		return(NaN)
	}
	# TODO: use which
	check.r <- abs(r) >= 1 # check whether |r| >= 1
	
	if (kappa >= 1 && n > 2 && check.r) {
		return(r)
	}
	#log.hyper.term <- log(hypergeo::hypergeo(((n-1)/2), ((n-1)/2), ((n+2/kappa)/2), r^2))
	hyper.term.1 <- Re(hypergeo::genhypergeo(U=c(n/2, n/2), L=c((2+(n+2)*kappa)/(2*kappa)), z=r^2))
	hyper.term.2 <- Re(hypergeo::genhypergeo(U=c((n-1)/2, (n-1)/2), L=c((2+n*kappa)/(2*kappa)), z=r^2))
	
	log.result <- 2*(lgamma(n/2)-lgamma((n-1)/2))
	result <- (2*kappa*r)/(2+n*kappa)*exp(log.result)*hyper.term.1/hyper.term.2
	
	if (is.na(result) || abs(result) > 1){
		return(r)
	} else {
		return(result)
	}
}


.posteriorSecondMoment <- function(n, r, kappa=1){
	#
	#
	if (n <= 2){
		return(NaN)
	} else if (any(is.na(r))){
		return(NaN)
	}
	# TODO: use which
	check.r <- abs(r) >= 1 # check whether |r| >= 1
	
	if (kappa >= 1 && n > 2 && check.r) {
		return(r)
	}
	#log.hyper.term <- log(hypergeo::hypergeo(((n-1)/2), ((n-1)/2), ((n+2/kappa)/2), r^2))
	hyper.term.1 <- Re(hypergeo::genhypergeo(U=c(3/2, (n-1)/2, (n-1)/2), 
											 L=c(1/2, (2+(n+2)*kappa)/(2*kappa)), z=r^2))
	hyper.term.2 <- Re(hypergeo::genhypergeo(U=c((n-1)/2, (n-1)/2), L=c((2+n*kappa)/(2*kappa)), z=r^2))
	
	result <- kappa/(n*kappa+2)*hyper.term.1/hyper.term.2
	
	if (is.na(result) || result <= 0){
		return(NA)
	} else {
		return(result)
	}
}

.posteriorVariance <- function(n, r, kappa=1){
	# Posterior mean of the .bf10Exact
	#	That is, (rho+1)/2, thus, on 0,1 scale to estimate a, b in a beta distribution
	#
	# TODO: add safeguard for large n as then hyper.term.1/hyper.term.2 is almost 1
	# 	and also for log.term almost being 1
	#
	# 	.posteriorVariance(199, 0.8) yields 6808.702
	# 	
	#
	#hyper.term.3 <- Re(hypergeo::genhypergeo(U=c((n-1)/2, (n+1)/2),L=(n+2/kappa)/2, z=r^2))
	#hyper.term.4 <- Re(hypergeo::genhypergeo(U=c((n-1)/2, (n-1)/2), L=(n+2/kappa)/2, z=r^2))
	# result.0 <- 1/((r+2/kappa*r)^2)*(
	#(n-1)*(n+2/kappa-1)-
	#	(2/kappa+1)*(n-2)*r^2+
	#	(n-1)*(n+2/kappa-1)*(r^2-1)*hyper.term.3/hyper.term.4)
	#result <- .posteriorSecondMoment(n,r,kappa)-(.posteriorMean(n,r,kappa))^2
	
	result <- .posteriorSecondMoment(n,r,kappa)-(.posteriorMean(n,r,kappa))^2
	
	if (is.na(result) | result <= 0){
		return(NA)
	} else{
		return(result)
	}
}

.betaParameterEstimates <- function(someMean, someVar){
	# someMean \in (0, 1)
	# TODO: think about someMean = 0
	some.a <- someMean*(someMean*(1-someMean)/someVar-1)
	some.b <- (1-someMean)*(someMean*(1-someMean)/someVar-1)
	
	result <- list(betaA=some.a, betaB=some.b)
	return(result)
}

.posteriorBetaParameters <- function(n, r, kappa=1){
	some.mu <- try((.posteriorMean(n, r, kappa)+1)/2)
	some.var <- try(.posteriorVariance(n, r, kappa)/2^2)
	
	if (is(some.mu, "try-error") || is(some.var, "try-error") || is.na(some.mu) || is.na(some.var)){
		# TODO: Before doing this try the MH sampler
		return(list(betaA=NA, betaB=NA))
	} else {
		return(.betaParameterEstimates(some.mu, some.var))
	}
}
# 
# .posteriorAParameter <- function(n, r, alpha=1){
# 	# Method of moments estimate for a beta distribution
# 	# First scale back to means on the (0, 1) domain
# 	
# 	
# 	myA <- .betaAParameterFit(some.mu, some.var)
# 	return(myA)
# }
# 
# .posteriorBParameter <- function(n, r, alpha=1){
# 	# Method of moments estimate for a beta distribution
# 	# First scale back to means on the (0, 1) domain
# 	some.mu <- (.posteriorMean(n, r, alpha)+1)/2
# 	some.var <- .posteriorVariance(n, r, alpha)/2^2
# 	
# 	myB <- (1-some.mu)*(some.mu*(1-some.mu)/some.var-1)
# 	return(myB)
# }

.credibleIntervals <- function(alpha, beta, ciValue){
	output <- list(betaA=alpha, betaB=beta, twoSidedCI=NA, minSidedCI=NA, plusSidedCI=NA)
	typeOne <- 1-ciValue
	excessLevel <- typeOne/2
	# Two sided:
	if (is.na(alpha) || is.na(beta)){
		return(output)
	} else {
		lowerCIZeroOne <- try(qbeta(excessLevel, alpha, beta))
		medianCIZeroOne <- try(qbeta(1/2, alpha, beta))
		upperCIZeroOne <- try(qbeta(1-excessLevel, alpha, beta))
		if (is(lowerCIZeroOne, "try-error") || is(medianCIZeroOne, "try-error") || is(upperCIZeroOne, "try-error")) {
			return(output)
		} else {
			lowerCI <- 2*lowerCIZeroOne-1
			medianCI <- 2*medianCIZeroOne-1
			upperCI <- 2*upperCIZeroOne-1
		}
	}
	output$twoSidedCI <- c(lowerCI, medianCI, upperCI)
	# One sided:
	output$minSidedCI <- .minSidedCredibleInterval(alpha, beta, ciValue)
	# The problem is symmetric
	temp  <- .minSidedCredibleInterval(beta, alpha, ciValue)
	output$plusSidedCI <- c(-temp[3], -temp[2], -temp[1])
	wrapOutput <- list(list())
	ci.label <- as.character(round(ciValue, 5))
	wrapOutput[[ci.label]] <- output
	return(wrapOutput)
}

.minSidedCredibleInterval <- function(alpha, beta, ciValue){
	output <- NA
	typeOne <- 1-ciValue
	excessLevel <- typeOne/2
	if (is.na(alpha) || is.na(beta)){
		return(output)
	} else {
		leftArea <- pbeta(1/2, alpha, beta)
		lowerCIZeroOne <- try(qbeta(excessLevel*leftArea, alpha, beta))
		medianCIZeroOne <- try(qbeta(leftArea/2, alpha, beta))
		upperCIZeroOne <- try(qbeta((1-excessLevel)*leftArea, alpha, beta))
		if (is(lowerCIZeroOne, "try-error") || is(medianCIZeroOne, "try-error") || is(upperCIZeroOne, "try-error")) {
			return(output)
		} else {
			lowerCI <- 2*lowerCIZeroOne-1
			medianCI <- 2*medianCIZeroOne-1
			upperCI <- 2*upperCIZeroOne-1
		}
	}
	output <- c(lowerCI, medianCI, upperCI)
	return(output)
}

#}
# 
# 
# .rhoQuantile <- function(n, r, kappa=1, ciPercentage=.95){
# 	# Fitting parameters
# 	beta.fit <- try(.posteriorBetaParameters(n, r, kappa))
# 	
# 	if (is(beta.fit, "try-error") || is.na(beta.fit$alpha) || is.na(beta.fit$beta)) {
# 		return(c(NA, r, NA))
# 	}
# 	
# 	# Output median
# 	some.median <- 2*qbeta(.5, beta.fit$alpha, beta.fit$beta)-1
# 	
# 	# Calculate CI
# 	type.one <- 1-ciPercentage
# 	
# 	left.CI <- try(2*qbeta(type.one/2, beta.fit$alpha, beta.fit$beta)-1)
# 	right.CI <- try(2*qbeta((1-type.one/2), beta.fit$alpha, beta.fit$beta)-1)
# 	
# 	# TODO: This actually doesn't override left.CI or rigthCI even if they are try-errors
# 	if ( is(left.CI, "try-error") || is(right.CI, "try-error") || is.na(left.CI) || is.na(right.CI) ){
# 		return(c(NA, r, NA))
# 	} else {
# 		return(c(left.CI, some.median, right.CI))
# 	}
# }
#------------------------------------------------- Matrix Plot -------------------------------------------------#

### empty posterior Plot with error message ###
.displayErrorPosterior <- function(errorMessage=NULL, xticks, xlabels, xlim, cexText=1.6, cexAxis= 1.2, cexYlab= 1.5, cexXlab= 1.28, lwdAxis= 1.2) {
	
	plot(1, 1, xlim= xlim, ylim= 0:1, ylab= "", xlab="", type= "n", axes= FALSE)
	
	text(0, .5, errorMessage, cex=cexText)
	
	axis(1, at= xticks, labels = xlabels, cex.axis= cexAxis, lwd= lwdAxis)
	axis(2, at = c(0, .5, 1), pos= range(xticks)- 0.08*diff(range(xticks)), labels = c("", "Density", ""), lwd.ticks=0, cex.axis= 1.7, mgp= c(3, 0.7, 0), las=0)
	
	mtext(expression(rho), side = 1, cex = cexXlab, line= 2.5)
	
}

#### Plotting Function for posterior ####
.plotPosterior.BayesianCorrelationMatrix <- function(x, y, kappa=1, oneSided= FALSE, addInformation= FALSE, drawCI= FALSE, lwd= 2, cexPoints= 1.5, cexAxis= 1.2, 
                                                     cexYlab= 1.5, cexXlab= 1.28, cexTextBF= 1.4, cexCI= 1.1, cexLegend= 1.2, lwdAxis= 1.2, addRho= TRUE, addTau= FALSE) {
	
	tooPeaked <- FALSE
	screenedData <- .excludePairwiseCorData(x, y)
	
	x <- screenedData$v1
	y <- screenedData$v2
	
	r <- cor(x, y)
	tau <- cor(x,y,method="kendall")
	n <- length(x)
	# set limits plot
	xlim <- c(-1, 1)
	
	if (oneSided == FALSE) {
		stretch <- 1.2
	}
	
	if (oneSided == "right") {
		stretch <- 1.32
	}
	
	if (oneSided == "left") {
		stretch <- 1.32
	}
	
	# calculate position of "nice" tick marks and create labels
	xticks <- seq(-1.0, 1.0, 0.25)
	xlabels <- c("-1", "-0.75", "-0.5", "-0.25", "0", "0.25", "0.5", "0.75", "1")
	
	rho <- seq(-0.99, 0.99, length.out = 1000)
	
	someFit <- .posteriorBetaParameters(n=n, r=r, kappa=kappa)
	betaA <- someFit$betaA
	betaB <- someFit$betaB
	
	betaApproximation <- FALSE
	
	if (oneSided == FALSE) {
	
		priorLine <- .priorRho(rho=rho, kappa=kappa)
		posteriorLine <- .posteriorRho(rho=rho, n=n, r=r, kappa=kappa)
		try(silent=TRUE, expr = {
		  numIntegrate <- integrate(function(x){.posteriorRho(x, n=n, r=r, kappa=kappa)},lower = -1,upper=1)$value
		  if(round(numIntegrate,digits=2) != 1){posteriorLine <- .approximatePosteriorRho(rho = rho, n = n, r = r)}
		  })
		posteriorLineTau <- .posteriorTau(delta=rho, kentau=tau, n=n, kappa=kappa, var=1, test="two-sided")
		legendPosition <- c("topright","topleft")[ (which(posteriorLine == max(posteriorLine))>500)+1 ]
		
		if (sum(is.na(posteriorLine)) > 1 || any(posteriorLine < 0) || any(is.infinite(posteriorLine))) {
			
			betaApproximation <- TRUE
			
			if (any(is.na(c(betaA, betaB))))
				tooPeaked <- TRUE
			
			posteriorLine <- .stretchedBeta(alpha=betaA, beta=betaB, rho=rho)
			
			if (sum(is.na(posteriorLine)) > 1 || any(posteriorLine < 0) || any(is.infinite(posteriorLine)))
				tooPeaked <- TRUE
		}
	
	} else if (oneSided == "right") {
	
		priorLine <- .priorRhoPlus(rho=rho, kappa=kappa)
		posteriorLine <- .posteriorRhoPlus(rho=rho, n=n, r=r, kappa= kappa)
		try(silent=TRUE, expr = {
		  numIntegrate <- integrate(function(x){.posteriorRhoPlus(x, n=n, r=r, kappa=kappa)},lower = -1,upper=1)$value
		  if(round(numIntegrate,digits=2) != 1){posteriorLine <- .approximatePosteriorRhoPlus(rho = rho, n = n, r = r)}
		})
		posteriorLineTau <- .posteriorTau(delta=rho, kentau=tau, n=n, kappa=kappa, var=1, test="positive")
		legendPosition <- "topleft"
		
		if (sum(is.na(posteriorLine)) > 1 || any(posteriorLine < 0) || any(is.infinite(posteriorLine))) {
		
			betaApproximation <- TRUE
			
			if (any(is.na(c(betaA, betaB))))
				tooPeaked <- TRUE
			
			posteriorLine <- .stretchedBeta(alpha=betaA, beta=betaB, rho=rho) / pbeta(1/2,  betaA, betaB, lower.tail=FALSE)
			posteriorLine[rho < 0] <- 0
			
			if (sum(is.na(posteriorLine)) > 1 || any(posteriorLine < 0) || any(is.infinite(posteriorLine)))
				tooPeaked <- TRUE
				
		}
			
	} else if (oneSided == "left") {
	
		priorLine <- .priorRhoMin(rho=rho, kappa=kappa)
		posteriorLine <- .posteriorRhoMin(rho=rho, n=n, r=r, kappa=kappa)
		try(silent=TRUE, expr = {
		  numIntegrate <- integrate(function(x){.posteriorRhoMin(x, n=n, r=r, kappa=kappa)},lower = -1,upper=1)$value
		  if(round(numIntegrate,digits=2) != 1){posteriorLine <- .approximatePosteriorRhoMin(rho = rho, n = n, r = r)}
		})
		posteriorLineTau <- .posteriorTau(delta=rho, kentau=tau, n=n, kappa=kappa, var=1, test="negative")
		legendPosition <- "topright"
		
		if (sum(is.na(posteriorLine)) > 1 || any(posteriorLine < 0) || any(is.infinite(posteriorLine))) {
		
			betaApproximation <- TRUE
			
			if (any(is.na(c(betaA, betaB))))
				tooPeaked <- TRUE
			
			posteriorLine <- .stretchedBeta(alpha=betaA, beta=betaB, rho=rho) / pbeta(1/2,  betaA, betaB, lower.tail=TRUE)
			posteriorLine[rho > 0] <- 0
			
			if (sum(is.na(posteriorLine)) > 1 || any(posteriorLine < 0) || any(is.infinite(posteriorLine)))
				tooPeaked <- TRUE
				
		}

	}
	
	dmax <- max(c(posteriorLine*addRho,posteriorLineTau*addTau))
	
	ylim <- vector("numeric", 2)
	ylim[1] <- 0
	ylim[2] <- stretch * dmax
	
	yticks <- pretty(ylim)
	
	ylim <- range(yticks)
	ylabels <- formatC(yticks, 1, format= "f")
	
	if (tooPeaked) {
	
		.displayErrorPosterior(errorMessage="Posterior is too peaked", xticks=xticks, xlabels=xlabels, xlim=xlim)
	
	} else {
	
		plot(1, 1, xlim= xlim, ylim= ylim, ylab= "", xlab="", type= "n", axes= FALSE)
		
	  if(addRho & addTau){
	    lines(rho, posteriorLine, lwd= lwd)
	    lines(rho,posteriorLineTau, lwd=lwd, lty=2)
	    xlabExpression <- "Correlation Coefficient"
	    legend(x=legendPosition[1],y=legendPosition[2], legend=c(expression(rho),expression(tau)), lty=1:2, cex=cexYlab, bty="n", lwd=lwd)
	  } else if(addRho){
	    lines(rho, posteriorLine, lwd= lwd)
	    xlabExpression <- expression(rho)
	  } else if(addTau){
	    lines(rho,posteriorLineTau, lwd=lwd)
	    xlabExpression <- expression(tau)
	  }
	  
		axis(1, at= xticks, labels = xlabels, cex.axis= cexAxis, lwd= lwdAxis)
		axis(2, at = c(ylim[1], mean(ylim), ylim[2]) , pos= range(xticks)- 0.08*diff(range(xticks)), labels = c("", "Density", ""), lwd.ticks=0, cex.axis= 1.7, mgp= c(3, 0.7, 0), las=0)
		
		mtext(xlabExpression, side = 1, cex = cexXlab, line= 2.5)
	}
	
}


#### Matrix Plot function #####
.correlationMatrixPlotBayesian <- function(dataset, perform, options, hypothesis=options$hypothesis) {
	
	if (!options$plotCorrelationMatrix)
		return()
	
	correlation.plot <- list()
	if (hypothesis == "correlated") {
		
		oneSided <- FALSE
		
	} else if (hypothesis == "correlatedPositively") {
		
		oneSided <- "right"
		
	} else if (hypothesis == "correlatedNegatively") {
		
		oneSided <- "left"
		
	}
	
	if (perform == "init") {
		
		variables <- unlist(options$variables)
		
		l <- length(variables)
		
		if (l > 1) {
			
			if (l <= 2 && (options$plotDensities || options$plotPosteriors)) {
				
				width <- 580
				height <- 580
				
			} else if (l <= 2) {
				
				width <- 400
				height <- 400
				
			} else {
				
				width <- 250 * l
				height <- 250 * l
				
			}	
			
			plot <- list()
			
			plot[["title"]] <- "Correlation Plot"
			plot[["width"]]  <- width
			plot[["height"]] <- height
			
			correlation.plot <- plot
		
		} else {
		
			correlation.plot <- NULL
		}
	}
	
	if (perform == "run" && length(options$variables) > 0) {
		
		variables <- unlist(options$variables)
		
		l <- length(variables)
		
		# check variables
		d <- vector("character", length(.v(variables)))
		sdCheck <- vector("numeric", length(.v(variables)))
		infCheck <- vector("logical", length(.v(variables)))
		
		for (i in seq_along(.v(variables))) {
			
			variable2check <- na.omit(dataset[[.v(variables)[i]]])
			d[i] <- class(variable2check)
			sdCheck[i] <- sd(variable2check) > 0
			infCheck[i] <- all(is.finite(variable2check))
		}
		
		numericCheck <- d == "numeric" | d == "integer"
		variables <- .v(variables)
		variable.statuses <- vector("list", length(variables))
		
		for (i in seq_along(variables)) {
		
			variable.statuses[[i]]$unplotable <- FALSE
			variable.statuses[[i]]$plottingError <- NULL
			
			if ( ! (numericCheck[i] && sdCheck[i] && infCheck[i])) {
				
				variable.statuses[[i]]$unplotable <- TRUE
				
				if ( ! numericCheck[i]) {
					variable.statuses[[i]]$plottingError <- "Variable is not continuous"
				} else if ( ! infCheck[i]) {
					variable.statuses[[i]]$plottingError <- "Variable contains infinity"
				} else if ( ! sdCheck[i]) {
					variable.statuses[[i]]$plottingError <- "Variable has zero variance"
				}
				
			}
		}
		
		
		if (l <= 2 && (options$plotDensities || options$plotPosteriors)) {
			
			width <- 580
			height <- 580
			
		} else if (l <= 2) {
			
			width <- 400
			height <- 400
			
		} else {
			
			width <- 250 * l
			height <- 250 * l
			
		}	
		
		correlation.plot <- list()
		
		plot <- list()
		
		plot[["title"]] <-  "Correlation Plot"
		plot[["width"]]  <- width
		plot[["height"]] <- height
		
		correlation.plot <- plot
		cexText <- 1.6
		
		if (length(variables) > 0) {
			
			p <- try(silent=FALSE, expr= {
				
				image <- .beginSaveImage(width, height)
				
				if (l == 1) {
					
					# par(mfrow= c(1,1), cex.axis= 1.3, mar= c(3, 4, 2, 1.5) + 0.1, oma= c(2, 2.2, 2, 0))
					# 
					# .plotMarginalCor(dataset[[variables[1]]]) 
					# mtext(text = .unv(variables)[1], side = 1, cex=1.9, line = 3)	
					
				} else if (l == 2 && !options$plotDensities && !options$plotPosteriors) {
					
					par(mfrow= c(1, 1), cex.axis= 1.3, mar= c(3, 4, 2, 1.5) + 0.1, oma= c(2, 2.2, 2, 0))
					
					if ( ! variable.statuses[[1]]$unplotable && ! variable.statuses[[2]]$unplotable) {
					
						maxYlab <- .plotScatter(dataset[[variables[1]]], dataset[[variables[2]]])
						distLab <- maxYlab / 1.8
						
						mtext(text = .unv(variables)[1], side = 1, cex=1.5, line = 3)
						mtext(text = .unv(variables)[2], side = 2, cex=1.5, line = distLab + 2, las=0)
					
					} else {
					
						errorMessages <- c(variable.statuses[[1]]$plottingError, variable.statuses[[2]]$plottingError)
						errorMessagePlot <- paste0("Correlation coefficient undefined:", "\n", errorMessages[1])
						.displayError(errorMessagePlot)
					}
					
				} else if (l >= 2) {
					
					if (l == 2)
						cexText <- 1.3
					
					par(mfrow= c(l,l), cex.axis= 1.3, mar= c(3, 4, 2, 1.5) + 0.1, oma= c(1, 2.2, 2, 0))
					
					for (row in seq_len(l)) {
						
						for (col in seq_len(l)) {
							
							if (row == col) {
								
								if (options$plotDensities) {
								
									if ( ! variable.statuses[[row]]$unplotable) {
										.plotMarginalCor(dataset[[variables[row]]]) # plot marginal (histogram with density estimator)
									} else {
										.displayError(variable.statuses[[row]]$plottingError, cexText=cexText)
									}
									
								} else {
								
									plot(1, type= "n", axes= FALSE, ylab="", xlab="")
								}
							}
							
							if (col > row) {
								
								if (options$plotCorrelationMatrix) {
								
									if ( ! variable.statuses[[col]]$unplotable && ! variable.statuses[[row]]$unplotable) {
										.plotScatter(dataset[[variables[col]]], dataset[[variables[row]]]) # plot scatterplot
									} else {
										errorMessages <- c(variable.statuses[[row]]$plottingError, variable.statuses[[col]]$plottingError)
										errorMessagePlot <- paste0("Correlation coefficient undefined:", "\n", errorMessages[1])
										.displayError(errorMessagePlot, cexText=cexText)
									}
									
								} else {
								
									plot(1, type= "n", axes= FALSE, ylab="", xlab="")
								}
							}
							
							if (col < row) {
								
								if (options$plotPosteriors) {
								
									if ( ! variable.statuses[[col]]$unplotable && ! variable.statuses[[row]]$unplotable) {
										.plotPosterior.BayesianCorrelationMatrix(dataset[[variables[col]]], dataset[[variables[row]]], oneSided=oneSided, kappa=options$priorWidth, addRho= options$pearson, addTau=options$kendallsTauB)
									} else {
										errorMessages <- c(variable.statuses[[row]]$plottingError, variable.statuses[[col]]$plottingError)
										errorMessagePlot <- paste0("Correlation coefficient undefined:", "\n", errorMessages[1])
										.displayError(errorMessagePlot, cexText=cexText)
									}
									
								} else {
								
									plot(1, type= "n", axes= FALSE, ylab="", xlab="")
								}
							}
						}
					}
				}
				
				
				if (l > 2 || ((l == 2 && options$plotDensities) || (l == 2 && options$plotPosteriors))) {
					
					textpos <- seq(1/(l*2), (l*2-1)/(l*2), 2/(l*2))
					
					if (!options$plotDensities && !options$plotPosteriors) {
						
						for (t in seq_along(textpos)) {
							
							mtext(text = .unv(variables)[t], side = 3, outer = TRUE, at= textpos[t], cex=1.5, line= -0.8)
							
							if (t < length(textpos)) {
								mtext(text = .unv(variables)[t], side = 2, outer = TRUE, at= rev(textpos)[t], cex=1.5, line= -0.1, las= 0)
							}
						}
						
					} else {
						
						for (t in seq_along(textpos)) {
							
							mtext(text = .unv(variables)[t], side = 3, outer = TRUE, at= textpos[t], cex=1.5, line= -0.8)
							mtext(text = .unv(variables)[t], side = 2, outer = TRUE, at= rev(textpos)[t], cex=1.5, line= -0.1, las= 0)
						}
					}
				}
				
				content <- .endSaveImage(image)
				
				plot <- correlation.plot
				plot[["data"]]  <- content
			})
			
			if (class(p) == "try-error") {
				
				errorMessage <- .extractErrorMessage(p)
				plot[["error"]] <- list(error="badData", errorMessage= paste("Plotting is not possible:", errorMessage))
			}
			
			correlation.plot <- plot
			
		}
	}
	
	correlation.plot
}