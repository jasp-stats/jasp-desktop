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
# "credibleInterval": TRUE/FALSE
# "ciValue": OptionNumber(.95, 0, 1, "%"));
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
								  credibleInterval=options$credibleInterval,
								  ciValue=options$ciValue,
								  priorWidth=options$priorWidth,
								  bayesFactorType=options$bayesFactorType,
								  missingValues=options$missingValues, state, diff) 
	
	tableVariables <- correlationTableOutput$variables
	tableTests <- correlationTableOutput$tests
	tableRows <- correlationTableOutput$rows
	
	results[["correlations"]] <- correlationTableOutput$correlationTable
	
	
	if (!is.null(state) && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$credibleInterval == FALSE && diff$ciValue == FALSE
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
		return(list(results=results, status="complete", 
					state=list(options=options, 
							   results=results, 
							   tableVariables=tableVariables,
							   tableTests=tableTests,
							   tableRows=tableRows,
							   memoryExcludePairwise=correlationTableOutput$memoryExcludePairwise,
							   memoryExcludeListwise=correlationTableOutput$memoryExcludeListwise,
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
# "credibleInterval": TRUE/FALSE
# "ciValue": OptionNumber(.95, 0, 1, "%"));
# "priorWidth": c(0.5, Inf)
# "bayesFactorType": BF10/BF01
.correlationTableBayesian <- function(dataset, perform, variables, pearson=TRUE,
									  kendallsTauB=FALSE, spearman=FALSE,
									  hypothesis="correlated",
									  reportBayesFactors=TRUE,
									  flagSupported=FALSE,
									  credibleInterval=FALSE,
									  ciValue=0.95,
									  priorWidth=priorWidth,
									  bayesFactorType=bayesFactorType,
									  missingValues="excludePairwise", 
									  state, diff) {
	# Note: This is the default failed bfObject for wrong data
    #
    failedBfObject <- list(n=NaN, r=NaN, stat=NA, bf10=NaN, bfPlus0=NA, bfPlus0=NA, bfMin0=NA, ciValue=ciValue, ci=list())
    
	correlationTable <- list()
	if (pearson & kendallsTauB) {
	    correlationTable[["citation"]] <- list(
	        "Ly, A., Verhagen, A. J. & Wagenmakers, E.-J. (2015). Harold Jeffreys's Default Bayes Factor Hypothesis Tests: Explanation, Extension, and Application in Psychology. Manuscript submitted for publication.",
	        "van Doorn, J.B., Ly, A., Marsman, M. & Wagenmakers, E.-J. (2016). Bayesian Inference for Kendall’s Rank Correlation Coefficient. Manuscript submitted for publication."
	    )
	} else if (pearson) {
	    correlationTable[["citation"]] <- list(
	      "Ly, A., Verhagen, A. J. & Wagenmakers, E.-J. (2015). Harold Jeffreys's Default Bayes Factor Hypothesis Tests: Explanation, Extension, and Application in Psychology. Manuscript submitted for publication."
	    )
	} else if (kendallsTauB) {
	    correlationTable[["citation"]] <- list(
	    "van Doorn, J.B., Ly, A., Marsman, M. & Wagenmakers, E.-J. (2016). Bayesian Inference for Kendall’s Rank Correlation Coefficient. Manuscript submitted for publication."
	  )
	}
	
	
	# MarkUp: General: Choose BF type to report
	#
	if (hypothesis == "correlated") {
		if (bayesFactorType=="BF10") {
			bfTitle <- "BF\u2081\u2080"
		} else if (bayesFactorType == "BF01") {
			bfTitle <- "BF\u2080\u2081"
		} else if (bayesFactorType=="LogBF10") {
			bfTitle <- "log(BF\u2081\u2080)"
		}
	} else if (hypothesis == "correlatedPositively") {
		if (bayesFactorType == "BF10") {
			bfTitle <- "BF\u208A\u2080"
		} else if (bayesFactorType == "BF01") {
			bfTitle <- "BF\u2080\u208A"
		} else if (bayesFactorType=="LogBF10") {
			bfTitle <- "log(BF\u208A\u2080)"
		}
	} else if (hypothesis == "correlatedNegatively") {
		if (bayesFactorType == "BF10") {
			bfTitle <- "BF\u208B\u2080"
		} else if (bayesFactorType == "BF01") {
			bfTitle <- "BF\u2080\u208B"
		} else if (bayesFactorType=="LogBF10") {
			bfTitle <- "log(BF\u208B\u2080)"
		}
	}
	
	# MarkUp: General: Make table and the column folding here
	#
	# Note: test contains the tests that are performed
	tests <- c()
	if (pearson)
		tests <- c(tests, "pearson")
	if (spearman)
		tests <- c(tests, "spearman")
	if (kendallsTauB)
		tests <- c(tests, "kendall")
	
	# MarkUp: General: Assign name to table
	#
	# Note: Naming of the table
	if (length(tests) != 1) {
		correlationTable[["title"]] <- paste("Bayesian Correlation Table")
	} else if (pearson) {
		correlationTable[["title"]] <- paste("Bayesian Pearson Correlations")
	} else if (spearman) {
		correlationTable[["title"]] <- paste("Bayesian Spearman Correlations")
	} else if (kendallsTauB) {
		correlationTable[["title"]] <- paste("Bayesian Kendall's Tau")
	} else {
		correlationTable[["title"]] <- paste("Bayesian Correlation Table")
	}
	
	# MarkUp: General: initialise column names
	#
	# Note: Describe column names to the returned object
	fields <- list(list(name=".variable", title="", type="string"))
	allRows <- list()
	
	# MarkUp: General: footnote for the whole table
	#
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
		if (bayesFactorType=="LogBF10") {
			.addFootnote(footnotes, paste(bfTitle, " > log(10), ** , ", bfTitle, " > log(30), *** ", bfTitle, " > log(100)"), symbol="*")
		} else {
			.addFootnote(footnotes, paste(bfTitle, " > 10, ** , ", bfTitle, " > 30, *** ", bfTitle, " > 100"), symbol="*")
		}
	}
	
	# State: Processing
	#
	if (!is.null(state)) {
		# Retrieve from state, 
		# Pairwise based on names
		memoryExcludePairwise <- state$memoryExcludePairwise
		
		# Listwise based on n and r
		memoryExcludeListwise <- state$memoryExcludeListwise
	} else {
		# State is null
	    # Initialise: the hierarchical structure of the state object
		temp <- list()
		
		# Structure is [[priorLabel]][[variableName]][[columnName]] then bfObject
		# TODO (Johnny): add [[test]] in front of it
		memoryExcludePairwise <- list(list(list(list())))
		
		# Structure is [[priorLabel]][[variableName]][[columnName]] then bfObject
		# TODO (Johnny): add [[test]] in front of it
		memoryExcludeListwise <- list(list(list(list())))
	}
	
	
	priorLabel <- as.character(round(priorWidth, 5))
	
	numberOfVariables <- length(variables)
	
	if (perform == "init") {
	    if (numberOfVariables == 0) {
	        variables <- c(variables, "...", "... ")
	    } else if (numberOfVariables == 1) {
	        variables <- c(variables, "... ")
	    }
	}
	
	# Update number of variables, hence, from here it's always >= 2, thus, outputting a table of 2 by 2
	#
	numberOfVariables <- length(variables)
	
	
	# Filling in goes row wise and fill in each column with an entry
	#
	if (numberOfVariables > 0) {
	    # MarkUp: Per row (variable): Create column names so the table can find it later. 
	    #
		testNames <- list(pearson="Pearson's r", spearman="Spearman's rho", kendall="Kendall's tau")
		columnNames <- c()
		
		# MarkUp: Per row (variable): Create names for the rows below and create identifiers for each row
		# 
		# Column folding: create multiple colums to be folded into rows later 
		# #   Nest it by tests and if reportBayesFactors=TRUE then add bf title for each test
		#
		for (test in tests) {
			# Note: create columns per test
			if (length(tests) > 1 || reportBayesFactors || credibleInterval) {
			    # Note: Column folding construction using ".test[", test, "]"
				columnName <- paste(".test[", test, "]", sep="")
				columnNames[[length(columnNames)+1]] <- columnName
				fields[[length(fields)+1]] <- list(name=columnName, title="", type="string")
			}
			
			# Note: create columns per test variable 
			for (variableName in variables) {
				columnName <- paste(variableName, "[", test, "]", sep="")
				columnNames[[length(columnNames)+1]] <- columnName
				fields[[length(fields)+1]] <- list(name=columnName, title=variableName, type="number", format="dp:3")
			}
			
			# Note: create column for bfs
			if (isTRUE(reportBayesFactors)) {
				columnName <- paste(".test[", test, "Bf]", sep="")
				columnNames[[length(columnNames)+1]] <- columnName
				fields[[length(fields)+1]] <- list(name=columnName, title=bfTitle, type="string")
				for (variableName in variables) {
					columnName <- paste(variableName, "[", test, "Bf]", sep="")
					columnNames[[length(columnNames)+1]] <- columnName
					fields[[length(fields)+1]] <- list(name=columnName, title=variableName, type="number", format="sf:4;dp:3")
				}
			}
		    
		    # Note: create column for bfs
		    if (isTRUE(credibleInterval)) {
		        columnName <- paste(".test[", test, "-upperCI]", sep="")
		        columnNames[[length(columnNames)+1]] <- columnName
		        fields[[length(fields)+1]] <- list(name=columnName, title="", type="string")
		        
		        for (variableName in variables) {
		            columnName <- paste(variableName, "[", test, "-upperCI]", sep="")
		            columnNames[[length(columnNames)+1]] <- columnName
		            fields[[length(fields)+1]] <- list(name=columnName, title=variableName, type="number", format="dp:3")
		        }
		        
		        columnName <- paste(".test[", test, "-lowerCI]", sep="")
		        columnNames[[length(columnNames)+1]] <- columnName
		        fields[[length(fields)+1]] <- list(name=columnName, title="", type="string")
		        
		        for (variableName in variables) {
		            columnName <- paste(variableName, "[", test, "-lowerCI]", sep="")
		            columnNames[[length(columnNames)+1]] <- columnName
		            fields[[length(fields)+1]] <- list(name=columnName, title=variableName, type="number", format="dp:3")
		        }
		    }
		}
		
		for (i in 1:numberOfVariables) {
		    # MarkUp: Per row (variable): Initialise a row and the footnote per row
		    #
			# Note: Create row given a column
		    # This row will be filled in from left to right
			row <- list()
			rowFootnotes <- list()
			
			variableName <- variables[[i]]
			
			# MarkUp: Per row (variable): Column folding: create multiple rows per variable by folding the columns
			#
			for (test in tests) {
			    # MarkUp: Per row (variable): Per test: Create a list of Bayes factors 
			    #   This list of Bayes factors will be filled in from left to right. 
			    #   Say i = 5 and numberOfVariables = 8, then first 
			    #       1. fill in blanks between for variable 1, 2, 3, 4. 
			    #       2. Fill in dash for variable 5
			    #       3. Retrieve BFs from state, or compute them
			    #       4. Fill in the bf from bayesFactorsList in the right row
			    #           thus, bayesFactorsList should contain the bfs for 6,7,8 and is of length 3
			    #
			    # Make a list of bayesFactors for variable1Name compared to all subsequent variable2Name
			    bayesFactorsList <- list()
			    upperCiList <- list()
			    lowerCiList <- list()
			    
				if (length(tests) > 1 || reportBayesFactors || credibleInterval) {
				    # Note: Create test name for each test row given a column
				    row[[length(row)+1]] <- testNames[[test]]
				}
					
				if (isTRUE(reportBayesFactors)) {
				    # Note: Create bf row each test given each column (variable)
				    bayesFactorsList[[length(bayesFactorsList)+1]] <- bfTitle
				}
				
				if (isTRUE(credibleInterval)) {
				    upperCiList[[length(upperCiList)+1]] <- paste("Upper ", 100 * ciValue, "% CI", sep="")
				    lowerCiList[[length(lowerCiList)+1]] <- paste("Lower ", 100 * ciValue, "% CI", sep="")
				}
				
				# Reporting: For this row, create blanks for everything before the current variable i
				#   this yields an empty lower triangle. Left down is empty
				#
				for (j in .seqx(1, i-1)) {
					# Note: Fill in blanks
					row[[length(row)+1]] <- ""
					bayesFactorsList[[length(bayesFactorsList)+1]] <- ""
					upperCiList[[length(upperCiList)+1]] <- ""
					lowerCiList[[length(lowerCiList)+1]] <- ""
				}
				
			    # Note: fill in em-dash when comparing variable1Name with variable1Name
				row[[length(row)+1]] <- "\u2014" # em-dash # Note: Fill in blanks
				bayesFactorsList[[length(bayesFactorsList)+1]] <- "\u2014"
				upperCiList[[length(upperCiList)+1]] <- "\u2014"
				lowerCiList[[length(lowerCiList)+1]] <- "\u2014"
				
				# Reporting: For each next variable find the values
				#
				for (j in .seqx(i+1, numberOfVariables)) {
					# Note: fill in blanks in table upper right-hand off diaganols
					variable2Name <- variables[[j]]
					columnName <- paste(variable2Name, "[", test, "]", sep="")
					
					
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
					
					if (missingValues=="excludePairwise") {
					    # Try variableName then columnName
					    bfObject <- memoryExcludePairwise[[priorLabel]][[variableName]][[columnName]]
						
					    if (is.null(bfObject)) {
					        # Try variableName then columnName
					        bfObject <- memoryExcludePairwise[[priorLabel]][[columnName]][[variableName]]
					    }
					    
						if (!is.null(bfObject)) {
							# State: retrieved
							#
							retrievalFailure <- FALSE
							resultProcessing <- TRUE
							
							# CIs check:
							#
							if (isTRUE(credibleInterval)) {
							    if (ciValue != bfObject$ciValue) {
							        bfObject$ciValue <- ciValue
							        
							        if (test=="pearson"){
							            bfObject$ci <- .computePearsonCredibleInterval(alpha=bfObject$betaA, beta=bfObject$betaB, bfObject$ciValue)
							        } else if (test=="kendall"){
							            bfObject$ci <- .computeKendallCredibleInterval(n=bfObject$n, tauObs=bfObject$stat, kappa=priorWidth, ciValue=bfObject$ciValue)
							        }
							        
							        
							        # Store back into state
							        memoryExcludePairwise[[priorLabel]][[variableName]][[columnName]] <- bfObject
							    }
							}
							#
							# Close succesfull retrieval of bf
						} else {
							retrievalFailure <- TRUE
						}
					} else if (missingValues=="excludeListwise") {
						# Check data
						#
					    errors <- .hasErrors(dataset, perform = perform, message = 'short', type = c('observations','variance', 'infinity'),
					                         all.target = c(variableName, variable2Name), observations.amount = '< 2')
					    
					    if (!identical(errors, FALSE)) {
					        # Note: Data: NOT ok, 
					        # bf10: can't compute
					        bfObject <- failedBfObject
					        bfObject$footnote <- errors$message
					        
					        retrievalFailure <- FALSE
					        resultProcessing <- TRUE
					    } else {
					        # Data is good, now try to retrieve bfObject
					        #
					        v1 <- dataset[[ .v(variableName) ]]
					        v2 <- dataset[[ .v(variable2Name) ]]
					        
					        if (!is.null(v1) && !is.null(v2)) {
					            # Note: Data: PREPARE
					            # 
					            nObs <- length(v1)
					            rObs <- cor(v1, v2, method=test)
					            
					            # For stateRetrieval
					            nLabel <- as.character(round(nObs, 5))
					            rLabel <- as.character(round(rObs, 5))
					            
					            # Try state retrieval
					            #
					            bfObject <- memoryExcludeListwise[[priorLabel]][[nLabel]][[rLabel]]
					            
					            if (!is.null(bfObject)) {
					                # State: Retrieved
					                
					                retrievalFailure <- FALSE
					                resultProcessing <- TRUE
					                
					                # CIs if user changed ciValue, then recompute and store again
					                #
					                if (isTRUE(credibleInterval)) {
					                    if (ciValue != bfObject$ciValue) {
					                        bfObject$ciValue <- ciValue
					                        
					                        if (test=="pearson"){
					                            bfObject$ci <- .computePearsonCredibleInterval(alpha=bfObject$betaA, beta=bfObject$betaB, bfObject$ciValue)
					                        } else if (test=="kendall"){
					                            bfObject$ci <- .computeKendallCredibleInterval(n=bfObject$n, tauObs=bfObject$tauObs, kappa=bfObject$kappa, ciValue=bfObject$ciValue)
					                        }
					                        
					                        # Store back into state with new ciValue
					                        memoryExcludeListwise[[priorLabel]][[nLabel]][[rLabel]] <- bfObject
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
					    }
					} # Close state retrieval block ---- 
					
					# State: No retrieval: if perform==run, run the analysis ---
					# Compute: 
					if (isTRUE(retrievalFailure)) {
						# Results cannot be retrieved from state
						
						if (perform == "run") {
							# Note: Data screening 
							#
						    errors <- .hasErrors(dataset, perform = perform, message = 'short', type = c('observations','variance', 'infinity'),
						                         all.target = c(variableName, variable2Name), observations.amount = '< 2')
						    
						    # 
						    # if (missingValues=="excludePairwise"){
						    #     subDataSet <- subset(dataset, select=c(.v(variableName), .v(variable2Name)) )
						    #     subDataSet <- na.omit(subDataSet)
						    #     
						    #     errors <- .hasErrors(dataset=subDataSet, perform = perform, message = 'short', type = c('observations','variance', 'infinity'),
						    #                          all.target = c(variableName, variable2Name), observations.amount = '< 2')
						    # } else if (missingValues=="excludeListwise"){
						    #     errors <- .hasErrors(dataset, perform = perform, message = 'short', type = c('observations','variance', 'infinity'),
						    #                          all.target = c(variableName, variable2Name), observations.amount = '< 2')
						    # }
						    # 
						    
							if (!identical(errors, FALSE)) {
							    # Note: Data: NOT ok, 
								# 		bf10: can't
							    bfObject <- failedBfObject
							    bfObject$footnote <- errors$message
							} else {
								# Data: OK
								# Try: Calculte bfs
							    
							    if (missingValues=="excludePairwise"){
							        subDataSet <- subset(dataset, select=c(.v(variableName), .v(variable2Name)) )
							        subDataSet <- na.omit(subDataSet)
							        
							        v1 <- subDataSet[[ .v(variableName) ]]
							        v2 <- subDataSet[[ .v(variable2Name) ]]
							    } else {
							        v1 <- dataset[[ .v(variableName) ]]
							        v2 <- dataset[[ .v(variable2Name) ]]
							    }
							    
							    nObs <- length(v1)
							    rObs <- cor(v1, v2, method=test)
							    
							    if (test == "pearson") {
							        # TODO: perhaps call this .bfCorrelation(, method=..), where method="pearson", "kendall" or "spearman"
							        bfObject <- .bfPearsonCorrelation(n=nObs, r=rObs, kappa=priorWidth, ciValue=ciValue)
							    } else if (test == "kendall") {
							        # TODO (Johnny): now by default var=1
							        bfObject <- .bfKendallTau(n=nObs, tauObs=rObs, kappa=priorWidth, ciValue=ciValue)
							    } else if (test == "spearman") {
							        # TODO (Johnny)
							        # 
							    }
							}
							
							# Store in State
							#
							if (missingValues=="excludePairwise") {
								memoryExcludePairwise[[priorLabel]][[variableName]][[columnName]] <- bfObject
							} else if (missingValues=="excludeListwise") {
								memoryExcludeListwise[[priorLabel]][[nLabel]][[rLabel]] <- bfObject
							}
							resultProcessing <- TRUE
						} # Close perform == "run"
					} # Close retrievalFailure==TRUE
						
					# Processing and reporting --- Thus, bfObject exists
					# 
					if (isTRUE(resultProcessing)) {
						# Note: Result reporting: sample r
					    
					    # Report r value or tauObs
					    row[[length(row)+1]] <- .clean(bfObject$stat)
					    
					    retrievedFootnote <- bfObject$footnote
					    
					    if (!is.null(retrievedFootnote)){
					        tempList <- .addFootnote(footnotes, retrievedFootnote)
					        rowFootnotes[[columnName]] <- c(rowFootnotes[[columnName]], list(tempList))
					    }
					    
												# MarkUp per variable: 
						# Note: Result processing: decide which BF to report
						# 
						if (hypothesis == "correlated") {
							reportBf <- bfObject$bf10
							reportLowerCi <- bfObject$ci$twoSided[1]
							reportUpperCi <- bfObject$ci$twoSided[3]
							
							if (bayesFactorType == "BF01") {
								reportBf <- 1/reportBf
							}
						} else if (hypothesis == "correlatedPositively") {
							# TODO: Still need to implement this for general rho0, rather than rho0=0
							reportBf <- bfObject$bfPlus0
							reportLowerCi <- bfObject$ci$plusSided[1]
							reportUpperCi <- bfObject$ci$plusSided[3]
							
							if (bayesFactorType == "BF01") {
								reportBf <- 1/reportBf
							}
						} else if (hypothesis == "correlatedNegatively") {
							reportBf <- bfObject$bfMin0
							reportLowerCi <- bfObject$ci$minSided[1]
							reportUpperCi <- bfObject$ci$minSided[3]
							
							if (bayesFactorType == "BF01") {
								reportBf <- 1/reportBf
							}
						} 
						
						# MarkUp: Per row (variable): add footnote per row
						#
						# Note: Flagging at the data [report]
						if (isTRUE(flagSupported) && is.na(reportBf) == FALSE) {
							if (reportBf > 100) {
								rowFootnotes[[columnName]] <- list("***")
							} else if (reportBf > 30) {
								rowFootnotes[[columnName]] <- list("**")
							} else if (reportBf > 10) {
								rowFootnotes[[columnName]] <- list("*")
							}
						}
						
						# Note: Flagging and report bfs [report]
						if (isTRUE(reportBayesFactors)) {
							if (bayesFactorType == "LogBF10") {
								reportBf <- base::log10(reportBf)
							}
							
							bayesFactorsList[[length(bayesFactorsList)+1]] <- .clean(reportBf)
						}
						
						if (isTRUE(credibleInterval)) {
						    upperCiList[[length(upperCiList)+1]] <- .clean(reportUpperCi)
						    lowerCiList[[length(lowerCiList)+1]] <- .clean(reportLowerCi)
						}
					} # Close: Results reporting from 1 retrieved or 2 performed
					
					# Note: No retrieval and no performance. Thus fill in full stops
					#
					if (retrievalFailure && perform!="run") {
						# No data retrieved from state and nothing run
						row[[length(row)+1]] <- "."
						bayesFactorsList[[length(bayesFactorsList)+1]] <- "."
						upperCiList[[length(upperCiList)+1]] <- "."
						lowerCiList[[length(lowerCiList)+1]] <- "."
					} # Close: Results reporting from initialisation
				} # Close loop: each column
				#
				# Hence, we now have all the information to fill in the rest of the table 
				
				# Reporting: Take each bf in the list i+1, i+2, .. numberOfVariables and fill in
				#
				if (isTRUE(reportBayesFactors)) {
					for (bf in bayesFactorsList) {
						row[[length(row)+1]] <- bf
					}
				}
				
				if (isTRUE(credibleInterval)) {
				    for (upperCi in upperCiList) {
				        row[[length(row)+1]] <- upperCi
				    }
				    
				    for (lowerCi in lowerCiList) {
				        row[[length(row)+1]] <- lowerCi
				    }
				}
			} # Close loop: each test	
			
			# MarkUp: Per row (variable) Design 
			names(row) <- columnNames
			row[[".variable"]] <- variableName
			if (length(rowFootnotes) > 0)
				row[[".footnotes"]] <- rowFootnotes
			allRows[[i]] <- row
		} # Note: close what goes each row 
	} # Close: the naming and initialisation of structure of each row
	
	# Give the column names to 
	schema <- list(fields=fields)
	correlationTable[["schema"]] <- schema
	correlationTable[["data"]] <- allRows
	correlationTable[["footnotes"]] <- as.list(footnotes)
	
	return(list(correlationTable=correlationTable, 
				test=tests, 
				variables=variables, 
				rows=allRows, 
				memoryExcludePairwise=memoryExcludePairwise,
				memoryExcludeListwise=memoryExcludeListwise))
}
## Help functions ------------------------------------------------------------
# 0.1 Prior specification Pearson's Rho
.excludePairwiseCorData <- function(v1, v2) {
	# To exclude the data pairwise
	#
	screenedData <- list(v1=v1, v2=v2)
	
	removeIndex1 <- which(is.na(v1))
	removeIndex2 <- which(is.na(v2))
	removeIndex <- unique(c(removeIndex1, removeIndex2))
	if (length(removeIndex) > 0) {
		screenedData$v1 <- v1[-(removeIndex)]
		screenedData$v2 <- v2[-(removeIndex)]
	}
	
	return(screenedData)
}

.stretchedBeta <- function(rho, alpha, beta) {
	result <- 1/2*dbeta((rho+1)/2, alpha, beta)
	return(result)
}


.priorRho <- function(rho, kappa=1) {
	.stretchedBeta(rho, 1/kappa, 1/kappa)
}

.priorRhoPlus <- function(rho, kappa=1) {
	nonNegativeIndex <- rho >=0
	lessThanOneIndex <- rho <=1
	valueIndex <- as.logical(nonNegativeIndex*lessThanOneIndex)
	result <- rho*0
	result[valueIndex] <- 2*.priorRho(rho[valueIndex], kappa)
	return(result)
}

.priorRhoMin <- function(rho, kappa=1) {
	negativeIndex <- rho <=0
	greaterThanMinOneIndex <- rho >= -1
	valueIndex <- as.logical(negativeIndex*greaterThanMinOneIndex)
	result <- rho*0
	result[valueIndex] <- 2*.priorRho(rho[valueIndex], kappa)
	return(result)
}
# 0.2 Prior specification Kendall's Tau
.scaledBetaTau <- function(tauPop, alpha=1, beta=1) {
  result <- ((pi*2^(-2*alpha))/beta(alpha,alpha))  * cos((pi*tauPop)/2)^(2*alpha-1)
  return(result)
}

.priorTau <- function(tauPop, kappa) {
  .scaledBetaTau(tauPop, alpha = (1/kappa), beta = (1/kappa))
}

.priorTauPlus <- function(tauPop, kappa=1) {
  nonNegativeIndex <- tauPop >=0
  lessThanOneIndex <- tauPop <=1
  valueIndex <- as.logical(nonNegativeIndex*lessThanOneIndex)
  result <- tauPop*0
  result[valueIndex] <- 2*.priorTau(tauPop[valueIndex], kappa)
  return(result)
}

.priorTauMin <- function(tauPop, kappa=1) {
  negativeIndex <- tauPop <=0
  greaterThanMinOneIndex <- tauPop >= -1
  valueIndex <- as.logical(negativeIndex*greaterThanMinOneIndex)
  result <- tauPop*0
  result[valueIndex] <- 2*.priorTau(tauPop[valueIndex], kappa)
  return(result)
}

# 1.0. Built-up for likelihood functions
.aFunction <- function(n, r, rho) {
	#hyperTerm <- Re(hypergeo::hypergeo(((n-1)/2), ((n-1)/2), (1/2), (r*rho)^2))
	hyperTerm <- Re(hypergeo::genhypergeo(U=c((n-1)/2, (n-1)/2), L=(1/2), z=(r*rho)^2))
	result <- (1-rho^2)^((n-1)/2)*hyperTerm
	return(result)
}

.bFunction <- function(n, r, rho) {
	#hyperTerm1 <- Re(hypergeo::hypergeo((n/2), (n/2), (1/2), (r*rho)^2))
	#hyperTerm2 <- Re(hypergeo::hypergeo((n/2), (n/2), (-1/2), (r*rho)^2))
	#hyperTerm1 <- Re(hypergeo::genhypergeo(U=c(n/2, n/2), L=(1/2), z=(r*rho)^2))
	#hyperTerm2 <- Re(hypergeo::genhypergeo(U=c(n/2, n/2), L=(-1/2), z=(r*rho)^2))
	#result <- 2^(-1)*(1-rho^2)^((n-1)/2)*exp(logTerm)*
	#	((1-2*n*(r*rho)^2)/(r*rho)*hyperTerm1-(1-(r*rho)^2)/(r*rho)*hyperTerm2)
	#
	hyperTerm <- Re(hypergeo::genhypergeo(U=c(n/2, n/2), L=(3/2), z=(r*rho)^2))
	logTerm <- 2*(lgamma(n/2)-lgamma((n-1)/2))+((n-1)/2)*log(1-rho^2)
	result <- 2*r*rho*exp(logTerm)*hyperTerm
	return(result)
}

.hFunction <- function(n, r, rho) {
	result <- .aFunction(n=n, r=r, rho) + .bFunction(n=n, r=r, rho)
	return(result)
}

.hFunctionCombined <- function(nOri, rOri, nRep, rRep, rho) {
    result <- .hFunction(n=nOri, r=rOri, rho)*.hFunction(n=nRep, r=rRep, rho) 
    return(result)
}

.hFunctionCombinedTwoSided <- function(nOri, rOri, nRep, rRep, rho) {
    result <- .aFunction(n=nOri, r=rOri, rho)*.aFunction(n=nRep, r=rRep, rho) +
        .bFunction(n=nOri, r=rOri, rho)*.bFunction(n=nRep, r=rRep, rho)
    return(result)
}

.hJeffreysApprox <- function(n, r, rho) {	
	result <- ((1 - rho^(2))^(0.5*(n - 1)))/((1 - rho*r)^(n - 1 - 0.5))
	return(result)
}

# 1.1 Explicit marginal likelihood functions
.m0MarginalLikelihood <- function(s, t, n) {
	logTerm <- 2*lgamma(0.5*(n-1))
	result <- 1/4*n^(0.5*(1-2*n))*pi^(1-n)*(s*t)^(1-n)*exp(logTerm)
	return(result)
}

.m1MarginalLikelihoodNoRho <- function(s, t, n, r, rho) {
	return(.m0MarginalLikelihood(s, t, n)*
		   	(.aFunction(n=n, r=r, rho)+.bFunction(n=n, r=r, rho)))
}

#
# 2.1 Two-sided main Bayes factor ----------------------------------------------
.bf10Exact <- function(n, r, kappa=1) {
	# Ly et al 2015
	# This is the exact result with symmetric beta prior on rho
	# with parameter alpha. If kappa = 1 then uniform prior on rho
	#
	#
	if (n <= 2) {
		return(1)
	} else if (any(is.na(r))) {
		return(NA)
	}
	# TODO: use or vectorise with an apply function
    checkR <- abs(r) >= 1 # check whether |r| >= 1
	if (kappa >= 1 && n > 2 && checkR) {
		return(Inf)
	}
	#logHyperTerm <- log(hypergeo::hypergeo(((n-1)/2), ((n-1)/2), ((n+2/kappa)/2), r^2))
    logHyperTerm <- log(hypergeo::genhypergeo(U=c((n-1)/2, (n-1)/2), L=((n+2/kappa)/2), z=r^2))
    logResult <- log(2^(1-2/kappa))+0.5*log(pi)-lbeta(1/kappa, 1/kappa)+
		lgamma((n+2/kappa-1)/2)-lgamma((n+2/kappa)/2)+logHyperTerm
    realResult <- exp(Re(logResult))
	
    if (realResult < 0) {
        return(NA)
    }
    
    # Fail
    return(realResult)
}

# 2.2 Two-sided secondairy Bayes factor
.bf10JeffreysIntegrate <- function(n, r, kappa=1) {
	# Jeffreys' test for whether a correlation is zero or not
	# Jeffreys (1961), pp. 289-292
	# This is the exact result, see EJ
	##
	if (n <= 2) {
		return(1)
	} else if ( any(is.na(r)) ) {
		return(NA)
	}
	
	# TODO: use which
	if (n > 2 && abs(r)==1) {
		return(Inf)
	}
  
    hyperTerm <- Re(hypergeo::genhypergeo(U=c((2*n-3)/4, (2*n-1)/4), L=(n+2/kappa)/2, z=r^2))
    logTerm <- lgamma((n+2/kappa-1)/2)-lgamma((n+2/kappa)/2)-lbeta(1/kappa, 1/kappa)
    result <- sqrt(pi)*2^(1-2/kappa)*exp(logTerm)*hyperTerm
	
    if (result < 0) {
        return(NA)
    }
    return(result)
}

# 2.3 Two-sided third Bayes factor
# .bfCorNumerical <- function(n, r, kappa=1, lowerRho=-1, upperRho=1) {
# 	# Numerically integrate Jeffreys approximation of the likelihood
# 	integrand <- function(rho) {.hJeffreysApprox(n=n, r=r, rho)*.priorRho(rho, kappa)}
# 	someIntegral <- try(silent=TRUE, integrate(integrand, lowerRho, upperRho))
# 	
# 	if (isTryError(someIntegral)) {
# 		return(NA)
# 	}
# 	
# 	if (someIntegral$message=="OK") {
# 		return(someIntegral$value)
# 	} else {
# 		return(NA)
# 	}
# }
# 
# .bf10Numerical <- function(n, r, kappa=1, lowerRho=-1, upperRho=1) {
# 	# Jeffreys' test for whether a correlation is zero or not
# 	# Jeffreys (1961), pp. 289-292
# 	# This is a numerical approximation for .bf10JeffreysIntegrate,
# 	# when it explodes
# 	# #
# 	# TODO: 1. check for n=1, n=2, as r is then undefined
# 	# 2. check for r=1, r=-1
# 	#
# 	# TODO: REMOVE ALL NUMERICAL STUFF
# 	if ( any(is.na(r)) ) {
# 		return(NA)
# 	}
# 	# TODO: use which
# 	if (n > 2 && abs(r)==1) {
# 		return(Inf)
# 	}
# 	
# 	
# 	# TODO: be very careful here, might integrate over non-finite function
# 	jeffreysNumericalIntegrate <- .bfCorNumerical(n=n, r=r, kappa, lowerRho=-1, upperRho=1)
# 	
# 	if (is.na(jeffreysNumericalIntegrate) || jeffreysNumericalIntegrate < 0) {
# 		return(NA)
# 	} else if (jeffreysNumericalIntegrate >= 0) {
# 		# jeffreys numerical integrate success
# 		return(jeffreysNumericalIntegrate)
# 	} else {
# 		# NO IDEA, EVERYTHING FAILED :(
# 		return(NA)
# 	}
# 	return(jeffreysNumericalIntegrate)
# }

# 2.3 Savage-Dickey beta approximation
.bfSavageDickeyBetaData <- function(n, r, kappa=1, rho0=0) {
    # Savage-Dickey based on a beta approximation
    #
    #
    fit <- .posteriorBetaParameters(n=n, r=r, kappa=kappa)
    return(.bfSavageDickeyBeta(a=fit$betaA, b=fit$betaB, kappa=kappa, rho0=rho0))
}

.bfSavageDickeyOneSidedAdapt <- function(bf10, a, b, kappa=1, rho0=0) {
    return(.bfSavageDickeyBeta(bf10=bf10, a=a, b=b, kappa))
}


.bfSavageDickeyBeta <- function(a, b, kappa=1, rho0=0, bf10=NULL) {
    # Savage-Dickey based on a beta approximation
    # Default failure for infinite bf10
    # Depending on bf10 define the result list
    #
    #
    
    if (is.null(bf10)) {
        result <- list(bf10=NA, bfPlus0=NA, bfMin0=NA, betaA=a, betaB=b)
        
        savageDickeyNumerator <- .priorRho(rho0, kappa)
        savageDickeyDenominator <- .stretchedBeta(rho0, alpha=a, beta=b)
        bf10 <- try(savageDickeyNumerator/savageDickeyDenominator)
        
        if (isTryError(bf10)) {
            # NAs
            return(result)
        } else {
            result$bf10 <- bf10
        }
    } else {
        result <- list(bf10=bf10, bfPlus0=NA, bfMin0=NA, betaA=a, betaB=b)
    }
    
    if (is.na(bf10)) {
        # Failure
        return(result)
    } 
    
    if (bf10 < 0) {
        # Total failure it's true
        return(result)
    }
    
    if (is.infinite(bf10)) {
        # .bfCorrieKernel gives the default values for the one sided ones
        #
        result$bf10 <- Inf
        return(result)
    } 
    
    if (is.finite(bf10)) {
        # bf10 is finite, now calculate one-sided stuff
        #
        
        result$bf10 <- bf10
        result$twoSidedTooPeaked <- FALSE
        
        leftProportion <- stats::pbeta(1/2, shape1=a, shape2=b)
        
        if (is.na(leftProportion)) {
            result <- utils::modifyList(result, list(bfPlus0=NA, bfMin0=NA, minSidedTooPeaked=TRUE, plusSidedTooPeaked=TRUE))
            return(result)
        }
        
        if (leftProportion > 0 && leftProportion < 1) {
            result$plusSidedTooPeaked <- FALSE
            result$minSidedTooPeaked <- FALSE
            
            result$bfMin0 <- 2*bf10*leftProportion
            result$bfPlus0 <- 2*bf10*(1-leftProportion)	
        } else {
            rightProportion <- stats::pbeta(1/2, shape1=a, shape2=b, lower.tail=FALSE)
            
            if (!is.na(rightProportion) && rightProportion < 1) {
                result$plusSidedTooPeaked <- FALSE
                result$minSidedTooPeaked <- FALSE
                
                result$bfMin0 <- 2*bf10*(1-rightProportion)
                result$bfPlus0 <- 2*bf10*rightProportion
            } else if (leftProportion >= 1) {
                result$bfMin0 <- 2*bf10
                result$bfPlus0 <- 0
            } else if (rightProportion >= 1) {
                result$bfMin0 <- 0
                result$bfPlus0 <- 2*bf10
            } else {
                # TODO: either leftProportion <= 0 or rightProportion < = 0
                # this shouldn't happen, but actually I have no idea
                return(result)
            }
        }
    }
    return(result)
}

# 2.4 The Marsman MH sampler 

.logTarget <- function(z, n, r, kappa) {
	# z is Fisher's transformation for r, but also use it for rho
	# The Fisher z transform and the log (likelihood*prior*Jacobian) of the tranformation
	(0.5*(n - 1))*log(1 - tanh(z)^2) - (n - 1 - 0.5)*log(1 - tanh(z)*r)+log(1-tanh(z)^2)/kappa
}

.logProposal <- function(z, n, r) {
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
	
	candidateAcceptance <- .logTarget(zCandidate, n=n, r=r, kappa)+.logProposal(zCurrent, n, r)-
		(.logTarget(zCurrent, n=n, r=r, kappa)+.logProposal(zCandidate, n, r))
	
	if (log(runif (1)) <= candidateAcceptance) {
		rhoCandidate <- tanh(zCandidate)
		return(rhoCandidate)
	} else {
		return (rhoCurrent)
	}
}

.marsmanMHSampler <- function(n, r, kappa=1, nIters=50000) {
	rhoMetropolisChain <- NULL
	yTemp <- r
	
	for (iter in 1:nIters) {
		yTemp <- .metropolisOneStep(yTemp, n=n, r=r, kappa)
		rhoMetropolisChain[iter] <- yTemp
	}
	
	acceptanceRate <- length (unique (rhoMetropolisChain)) / nIters
	
	metropolisVar <- var(rhoMetropolisChain)/2^2
	metropolisMean <- mean((rhoMetropolisChain+1)/2)
	
	mhFit <- .betaParameterEstimates(metropolisMean, metropolisVar)
	mhFit$acceptanceRate <- acceptanceRate
	return(mhFit)
}

# 2.5. Two-sided asymptotic approximation of the Bayes factor 
.bf10JeffreysApprox <- function(n, r) {
	#Jeffreys' test for whether a correlation is zero or not
	#Jeffreys (1961), pp. 291 Eq. 14
	#
    result <- list(bf10=NA, bfPlus0=NA, bfMin0=NA)
    
	if (n <= 2) {
		return(1)
	} else if ( any(is.na(r)) ) {
		return(NA)
	}
	# TODO: use which
	if (n > 2 && abs(r)==1) {
		return(Inf)
	}
	
    bf01 <- ((2*n-3)/pi)^(.5)*(1-r^2)^((n-4)/2)
    bf10 <- 1/bf01
    
    if (bf10 < 0) {
        return(result)
    }
    
    if (is.na(bf10)) {
        return(result)
    }
    
    if (is.finite(bf10)) {
        result$bf10 <- bf10 
        result$twoSidedTooPeaked <- FALSE
        
        return(result)
    }
	
    if (is.infinite(bf10)) {
        # Note: Check extreme
        if (r >= 0) {
            result$bfPlus0 <- Inf
            result$bfMin0 <- 0
        } else if (r < 0) {
            result$bfPlus0 <- 0
            result$bfMin0 <- Inf
        }
        
        # Posterior too peaked
        result$twoSidedTooPeaked <- TRUE 
        result$plusSidedTooPeaked <- TRUE 
        result$minSidedTooPeaked <- TRUE
        return(result)
    }
}

# 3.0 One-sided preparation ----------------------------------------------------
# For .bfPlus0Exact
# For .bfPlus0Exact
.mPlusExact <- function(n, r, kappa=1) {
	# Ly et al 2015
	# This is the contribution of one-sided test
	#
	#
	# TODO: 1. check for n=1, n=2, as r is then undefined
	# 2. check for r=1, r=-1
	#
	#hyperTerm1 <- Re(hypergeo::genhypergeo(U=c(1, (n+2)/2, (n+2)/2),
	#										 L=c(1/2, (n+2/kappa+3)/2), z=r^2))
	#hyperTerm2 <- Re(hypergeo::genhypergeo(U=c(1, (n+2)/2, (n+2)/2),
	#										 L=c(3/2, (n+2/kappa+1)/2), z=r^2))
	#hyperTerm3 <- Re(hypergeo::genhypergeo(U=c(1, (n+2)/2, (n+2)/2),
	#										 L=c(3/2, (n+2/kappa+3)/2), z=r^2))
	#sum.term <- -((n*r)^2*hyperTerm1-n^2*(n+2/kappa+1)*hyperTerm2+2*n^3*r^2*
	#			  	hyperTerm3+(2*n^2-2/kappa*(1-2*n)+n-1))
	#product.term <- (2^(1-2/kappa)*r)/((n+2/kappa-1)*(n+2/kappa+1))
	#logTerm <- 2*lgamma(n/2)-2*lgamma((n+1)/2)-lbeta(1/kappa, 1/kappa)
	#result <- product.term*exp(logTerm)*sum.term
    
	hyperTerm <- Re(hypergeo::genhypergeo(U=c(1, n/2, n/2), L=c(3/2, (2+kappa*(n+1))/(2*kappa)), z=r^2))
	logTerm <- 2*(lgamma(n/2)-lgamma((n-1)/2))-lbeta(1/kappa, 1/kappa)
	result <- 2^((3*kappa-2)/kappa)*kappa*r/(2+(n-1)*kappa)*exp(logTerm)*hyperTerm
	return(result)
}

# For .bfPlus0EJeffreysIntegrate
.mPlusJeffreysIntegrate <- function(n, r, kappa=1) {
	# Ly et al 2015
	# This is the exact result with symmetric beta prior on rho
	# This is the contribution of one-sided test
	#
	#	
	hyperTerm <- Re(hypergeo::genhypergeo(U=c(1, (2*n-1)/4, (2*n+1)/4),
										   L=c(3/2, (n+1+2/kappa)/2), z=r^2))
	logTerm <- -lbeta(1/kappa, 1/kappa)
	result <- 2^(1-2/kappa)*r*(2*n-3)/(n+2/kappa-1)*exp(logTerm)*hyperTerm
	return(result)
}

# 
# .bfPlus0Numerical <- function(n, r, kappa=1, lowerRho=0, upperRho=1) {
# 	# Ly et al 2015
# 	# This is a numerical approximation
# 	# with parameter kappa. If kappa = 1 then uniform prior on rho
# 	# bf positive vs null
# 	#
# 	# Ly et al 2015
# 	# This is the contribution of one-sided test
# 	#
# 	#
# 	if ( any(is.na(r)) ) {
# 		return(NA)
# 	}
# 	if (kappa >= 1 && n > 2 && r>=1) {
# 		return(Inf)
# 	} else if (kappa >= 1 && n > 2 && r<=-1) {
# 		return(0)
# 	}
# 	
# 	my.numerical.Jeffreys <- .bfCorNumerical(n, r, kappa, lowerRho, upperRho)
# 	# TODO: be very careful here, might integrate over non-finite function
# 	# in particular with the exact h function. 
# 	#
# 	if (!is.na(my.numerical.Jeffreys) && my.numerical.Jeffreys >= 0) {
# 		# Note: Numerical Jeffreys okay
# 		return(my.numerical.Jeffreys)
# 	} else if (is.na(my.numerical.Jeffreys) || my.numerical.Jeffreys < 0) {
# 		# All numerical failed
# 		return(NA)
# 	} 
# 	return (NA)
# }

## Suit:
.bfHypergeo <- function(n, r, kappa=1, methodNumber=1, hyperGeoOverFlowThreshold=24) {
    # Outputs: 
    #   list of bfs and the beta fits based on the exact form of the reduced likelihood,
    #   see Ly, Marsman and Wagenmakers (2017) "Analytic Posteriors for Pearson’s Correlation Coefficient".  
    #
    # This is the exact result with symmetric beta prior on rho
    # with parameter alpha. If kappa = 1 then uniform prior on rho
    #
    
    result <- list(bf10=NA, bfPlus0=NA, bfMin0=NA)
    tempList <- .posteriorBetaParameters(n=n, r=r, kappa=kappa)
    result <- utils::modifyList(result, tempList)
    
    bf10 <- switch(methodNumber, 
                   try(silent=TRUE, .bf10Exact(n=n, r=r, kappa=kappa)), 
                   try(silent=TRUE, .bf10JeffreysIntegrate(n=n, r=r, kappa))
    )
    
    if (isTryError(bf10)) {
        # all NAs
        return(result)
    } 
    
    if (is.na(bf10)) {
        # all NAs
        return(result)
    } 
    
    if (bf10 <0) {
        # Total failure it's true
        return(result)
    }
    
    if (is.finite(bf10)) {
        # Store
        result$bf10 <- bf10 
        result$twoSidedTooPeaked <- FALSE
        
        if (log(bf10) < hyperGeoOverFlowThreshold) {
            # No overflow, can use exact result
            switch(methodNumber, 
                   {
                       bfPlus0 <- try(silent=TRUE, bf10 + .mPlusExact(n=n, r=r, kappa))
                       bfMin0 <- try(silent=TRUE, bf10 + .mPlusExact(n, -r, kappa))
                   }, 
                   {
                       bfPlus0 <- try(silent=TRUE, bf10 + .mPlusJeffreysIntegrate(n=n, r=r, kappa=kappa))
                       bfMin0 <- try(silent=TRUE, bf10 + .mPlusJeffreysIntegrate(n=n, r=-r, kappa=kappa))	    
                   }
            )
            
            if (is.finite(bfPlus0) && is.finite(bfMin0)) {
                tempList <- list(bfPlus0=bfPlus0, bfMin0=bfMin0, plusSidedTooPeaked=FALSE, minSidedTooPeaked=FALSE) 
                result <- utils::modifyList(result, tempList)
                return(result)
            }
        }
        
        # bf10 either overflow, or exact one-sided bfs failed: 
        # but bf10 is finite, so try Savage-Dickey adaptation for one-sided bfs
        tempList <- .bfSavageDickeyOneSidedAdapt(bf10, a=result$betaA, b=result$betaB, kappa=kappa)
        result <- utils::modifyList(result, tempList)
        return(result)
    }
    
    if (is.infinite(bf10)) {
        # Note: Information consistency check is done at a higher level
        #  For information consistent data it actually outputs the NA list
        #
        # Exact bf10 is infinite, perhaps due to hypergeometric, try numerical integrate
        twoSidedIntegrand <- switch(methodNumber, {
            function(x){.hFunction(n=n, r=r, x)*.priorRho(x, kappa=kappa)} }, {
            function(x){.hJeffreysApprox(n=n, r=r, x)*.priorRho(x, kappa=kappa)}
        })
        
        
        bf10 <- try(silent=TRUE, exp=integrate(twoSidedIntegrand, -1, 1)$value)
        result$bf10 <- bf10
        
        if (is.infinite(result$bf10)) {
            # .bfCorrieKernel gives the default values for the one sided ones
            #
            return(result)
        } 
        
        if (is.finite(bf10)) {
            # Numerical integrated bf10 is finite
            result$twoSidedTooPeaked <- FALSE
            
            if (methodNumber==1) {
                plusSidedIntegrand <- function(x){.hFunction(n=n, r=r, x)*.priorRhoPlus(x, kappa=kappa)}
                minSidedIntegrand <- function(x){.hFunction(n=n, r=r, x)*.priorRhoMin(x, kappa=kappa)}
            }
            
            if (methodNumber==2) {
                plusSidedIntegrand <- function(x){.hJeffreysApprox(n=n, r=r, x)*.priorRhoPlus(x, kappa=kappa)}
                minSidedIntegrand <- function(x){.hJeffreysApprox(n=n, r=r, x)*.priorRhoMin(x, kappa=kappa)}	    
            }
            
            bfPlus0 <- try(silent=TRUE, exp=integrate(plusSidedIntegrand, 0, 1)$value)
            bfMin0 <- try(silent=TRUE, exp=integrate(minSidedIntegrand, -1, 0)$value)
            
            if (is.finite(bfPlus0) && is.finite(bfMin0)) {
                tempList <- list(bf10=bf10, bfMin0=bfMin0, bfPlus0=bfPlus0, plusSidedTooPeaked=FALSE, minSidedTooPeaked=FALSE)
                result <- utils::modifyList(result, tempList)
                return(result)
            }
            
            # Numerical procedure failed, but bf10 is finite, try savage-dickey adapt
            tempList <- .bfSavageDickeyOneSidedAdapt(bf10, a=result$betaA, b=result$betaB, kappa=kappa)
            result <- utils::modifyList(result, tempList)
        }
        
        # numerical bf10 is infinite
        #
        
        
        # Numerical bf10 is not finite nor infinite
        # All NAs
        return(result)
    }
    # Exact bf10 is not finite, nor infinite, nor NA, nor try-error No clue what happened here
    # All NAs
    return(result)
}

.bfCorrieKernel <- function(n, r, kappa=1, method="exact", ciValue=0.95, hyperGeoOverFlowThreshold=24) {
    # The idea is incremental when it comes to method numbers, if 1 doesn't work then go down. 
    # In particular, when 
    #	methodNumber=1: exact result Ly et al (2015)
    #	methodNumber=2: semi-exact result, based on approximation of the likelihood JeffreysExact, see Wagenmakers et al (2015) bathing
    #	methodNumber=3: Savage Dickey beta approximation 
    #	methodNumber=4: Marsman's IMH sampler and report a posterior beta fit summarised by betaA, betaB
    #   methodNumber=5: Jeffreys asymptotic approximation of the BF 
    #   methodNumber=6: invalid data, or point prior
    #   
    #   hyperGeoOverFlowThreshold=25 implies that if log(bf10) > 24 that we use Savage-Dickey adaptation
    #   for the one-sided bfs. 
    #
    # Ex. 	if we want confidence intervals and the bfs are based on method 1, 
    #		then we can find betaA, betaB based on .posteriorBetaParameters
    # 		if this doens't work we use the Marsman sampler
    # Ex.	if we retrieve bfs from methodNumber 4, we know that we already have betaA and betaB, 
    #		so we do not need to try .posteriorMean and .posteriorVariance 
    # Ex.	if we retrieve bfs from methodNumber 6, we don't know anything about the posterior, so no report at all
    #
    
    tempList <- list()
    # result <- list(n=n, r=r, kappa=kappa, bf10=NA, bfPlus0=NA, bfMin0=NA, methodNumber=NA, betaA=NA, betaB=NA, 
    # 			   twoSidedTooPeaked=FALSE, plusSidedTooPeaked=FALSE, minSidedTooPeaked=FALSE, 
    # 			   ci=tempList, ciValue=ciValue, acceptanceRate=1)
    result <- list(n=n, r=r, kappa=kappa, bf10=NULL, bfPlus0=NULL, bfMin0=NULL, methodNumber=NULL, betaA=NULL, betaB=NULL, 
                   twoSidedTooPeaked=NULL, plusSidedTooPeaked=NULL, minSidedTooPeaked=NULL, 
                   ci=tempList, ciValue=ciValue, acceptanceRate=1)
    
    # When bf10 is na, modify result with naList
    #
    tooPeakedList <- list(twoSidedTooPeaked=TRUE, plusSidedTooPeaked=TRUE, minSidedTooPeaked=TRUE)
    
    naList <- append(list(bf10=NA, bfPlus0=NA, bfMin0=NA), tooPeakedList)
    
    # When the prior is trivial (null is alternative) or when the data is predictively matched
    #
    predictiveMatchingList <- list(bf10=1, bfPlus0=1, bfMin0=1, twoSidedTooPeaked=FALSE, plusSidedTooPeaked=FALSE, minSidedTooPeaked=FALSE, methodNumber=0)
    
    # Information consistent result
    #
    infoConsistentList <- append(list(bf10=Inf, methodNumber=0), tooPeakedList)
    
    # Note: If log(bf10) then use beta fit for the one sided bfs
    # The 24 is based on n=30, r=-0.3500786
    # hyperGeoOverFlowThreshold <- 24
    
    # Note: Data check
    #
    if (any(is.na(r)) ) {
        result$methodNumber <- 6
        result <- utils::modifyList(result, naList)
        return(result)
    }
    
    # Note: Data: OK
    # "No" prior, alternative model is the same as the null model
    # TODO: however this bound of 0.002 is arbitrarily chosen. I should choose this based on a trade off
    # between r and n, but it doesn't really matter. 
    if (kappa <= 0.002) {
        result <- utils::modifyList(result, predictiveMatchingList)
        return(result)
    }
    
    checkR <- abs(r) >= 1 # check whether |r| >= 1
    if (n <= 2 || kappa==0) {
        result <- utils::modifyList(result, predictiveMatchingList)
        return(result)
    } else if (kappa >= 1 && n > 2 && checkR) {
        if (r > 0) {
            result$bfPlus0 <- Inf
            result$bfMin0 <- 0
        } else if (r <= 0) {
            result$bfPlus0 <- 0
            result$bfMin0 <- Inf
        }
        result <- utils::modifyList(result, infoConsistentList)
        return(result)
    }
    
    # Note: Define different methods and method number
    #
    if (method=="exact" || method==1) {
        result$methodNumber <- 1
        tempList <- .bfHypergeo(n=n, r=r, kappa=kappa, methodNumber=1, hyperGeoOverFlowThreshold=hyperGeoOverFlowThreshold)
        result <- utils::modifyList(result, tempList)
    } 
    
    if (method=="jeffreysIntegrate" || method==2) {
        result$methodNumber <- 2
        tempList <- .bfHypergeo(n=n, r=r, kappa=kappa, methodNumber=2, hyperGeoOverFlowThreshold=hyperGeoOverFlowThreshold)
        result <- utils::modifyList(result, tempList)
    } 
    
    if (method=="savageDickeyBeta" || method==3) {
        result$methodNumber <- 3
        tempList <- .bfSavageDickeyBetaData(n=n, r=r, kappa=kappa)
        result <- utils::modifyList(result, tempList)
    } 
    
    if  (method=="metropolisHastings" || method==4) {
        # We use the Marsman Sampler (c) here based on posterior model fit. 
        result$methodNumber <- 4
        marsmanResult <- .marsmanMHSampler(n=n, r=r, kappa=kappa)
        result$acceptanceRate <- marsmanResult$acceptanceRate
        
        tempResult <- .bfSavageDickeyBeta(a=marsmanResult$betaA, b=marsmanResult$betaB, kappa=kappa)
        result <- utils::modifyList(result, tempResult)
    } 
    
    if (method=="jeffreysApprox" || method==5) {
        result$methodNumber <- 5
        tempResult <- .bf10JeffreysApprox(n=n, r=r)
        result <- utils::modifyList(result, tempResult)
        return(result)
    }
    
    # Note: bf10: CHECK
    if (is.na(result$bf10)) {
        # Posterior not interesting
        result <- utils::modifyList(result, naList)
        return(result)
    }
    
    if (is.infinite(result$bf10)) {
        if (r >= 0) {
            result$bfPlus0 <- Inf
            result$bfMin0 <- 0
        } else if (r < 0) {
            result$bfPlus0 <- 0
            result$bfMin0 <- Inf
        }
        
        result <- utils::modifyList(result, tooPeakedList)
        return(result)
    }
    
    # Note: Calculate credible intervals
    #
    if (!is.null(ciValue)) {
        # Note: ciValue=NULL, speeds up the calculations for sequential analysis
        result$ci <- .computePearsonCredibleInterval(alpha=result$betaA, beta=result$betaB, ciValue=result$ciValue)
    }

    # Note: bfPlus0, bfMin0: CHECK
    #
    if (any(is.na(result$bfPlus0), is.na(result$bfMin0))) {
        # Note: bfPlus0, bfMin0: NOT ok
        # 	if one is NA, then both are NA
        result$bfPlus0 <- NA
        result$bfMin0 <- NA
        
        # Posterior not interesting 
        result$plusSidedTooPeaked <- TRUE 
        result$minSidedTooPeaked <- TRUE
        return(result)
    } 
    
    if (any(c(result$bfPlus0, result$bfMin0)==0)) {
        # Note: bfPlus0, bfMin0: EXTREME
        # 	if one is extreme, so is the other
        if (result$bfPlus0==0) {
            result$bfPlus0 <- 0
            result$bfMin0 <- Inf
        } else if (result$bfMin0==0) {
            result$bfPlus0 <- Inf
            result$bfMin0 <- 0
        }
        
        # Posterior too peaked
        result$plusSidedTooPeaked <- TRUE 
        result$minSidedTooPeaked <- TRUE
        return(result)
    } 
    
    
    # Note: bfPlus0, bfMin0: CHECK COHERENCE:
    if (result$bfPlus0 > 1 && result$bfMin0 > 1 || any(c(result$bfPlus0, result$bfMin0)<0)) {
        if (r > 0) {
            # Note: Data: OK, 
            # 		bf10: OK. 
            #		bfPlus0: OK
            #		bfMin0: NOT ok 
            # 
            # bfMin0 is bigger than one due to overflow: bfMin0 = 2*bf10 - bfPlus0. 
            # Example: 2*1.2.... 10^ 24 - 2.... 10^24 = 1... 10^12 (due to round off)
            #
            result$bfMin0 <- 10^(-317) 
            result$bfPlus0 <- 2*result$bf10 - result$bfMin0
        } else if (r < 0) {
            # Note: Data: OK, 
            # 		bf10: OK. 
            #		bfPlus0: NOT ok
            #		bfMin0: OK
            result$bfPlus0 <- 10^(-317) 
            result$bfMin0 <- 2*result$bf10 - result$bfPlus0
        }
    }
    return(result)
}

.bfPearsonCorrelation <- function(n, r, kappa=1, ciValue=0.95, hyperGeoOverFlowThreshold=24) {
    # Wrapper around .bfCorrieKernel
    #
    result <- list(bf10=NA, bfPlus0=NA, bfMin0=NA)
    methodNumber <- 1
    
    while (any(is.na(c(result$bf10, result$bfPlus0, result$bfMin0)), 
               is.infinite(c(result$bf10, result$bfPlus0, result$bfMin0))) 
           && methodNumber <= 4) {
        # Note: Try all normal methods
        # 1. Exact
        # 2. semi-exact result
        # 3. Savage-Dickey beta approximation
        # 4. Marsman sampler
        
        result <- .bfCorrieKernel(n=n, r=r, kappa=kappa, method=methodNumber, ciValue=ciValue, hyperGeoOverFlowThreshold=hyperGeoOverFlowThreshold)
        #}
        methodNumber <- methodNumber+1
    }
    
    result$call <- paste0(".bfPearsonCorrelation(n=", n, ", r=", r, ", kappa=", kappa, ", ciValue=", ciValue, ", hyperGeoOverFlowThreshold=", hyperGeoOverFlowThreshold, ")")
    result$stat <- r
    return(result)
}

.postDensKendallTau <- function(n, Tstar, tauPop, kappa=1, var=1, test="two-sided") { 
    if (test == "two-sided") { 
        priorDens <- .priorTau(tauPop, kappa)
    } else if (test == "positive") { 
        priorDens <- .priorTauPlus(tauPop, kappa)
    } else if (test == "negative") { 
        priorDens <- .priorTauMin(tauPop, kappa)
    }
    
    priorDens <- .priorTau(tauPop, kappa)
    dens <- stats::dnorm(Tstar, (1.5*tauPop*sqrt(n)), sd=sqrt(var))* priorDens
    return(dens)
}

.posteriorTau <- function(n, tauObs, tauPop, kappa=1, var=1, test="two-sided") {
    Tstar <- (tauObs * ((n*(n-1))/2))/sqrt(n*(n-1)*(2*n+5)/18)
    if (test == "two-sided") {
        lims <- c(-1, 1)
    } else if (test == "positive") {
        lims <- c(0, 1)
    } else if (test == "negative") { 
        lims <- c(-1, 0)
    }
    
    logicalCensor <- (tauPop >= lims[1] & tauPop <= lims[2])
    # TODO (Johnny): Wrap this into a try statement. and do an isTryError
    dens <- logicalCensor*.postDensKendallTau(n=n, Tstar=Tstar, tauPop=tauPop, kappa=kappa, var=var, test=test)/
        integrate(function(x){.postDensKendallTau(n=n, Tstar=Tstar, tauPop=x, kappa=kappa, var=var, test=test)}, lims[1], lims[2])$value
} 


.bfKendallTau <- function(n, tauObs, kappa=1, var=1, ciValue=0.95) {
    # TODO (Johnny): Wrapper around .bfCorrieKernelKendallTau to loop over the different methods, if any
    #
    result <- list(bf10=NA, bfPlus0=NA, bfMin0=NA)
    methodNumber <- 1
    
    # # TODO (Johnny) loop number
    # while (any(is.na(c(result$bf10, result$bfPlus0, result$bfMin0)), 
    #            is.infinite(c(result$bf10, result$bfPlus0, result$bfMin0))) 
    #        && methodNumber <= 4) {
    #     # Note: Try all normal methods
    #     # 1. Exact
    #     # 2. semi-exact result
    #     # 3. Savage-Dickey beta approximation
    #     # 4. Marsman sampler
    #     
    #     result <- .bfCorrieKernel(n=n, r=r, kappa=kappa, method=methodNumber, ciValue=ciValue, hyperGeoOverFlowThreshold=hyperGeoOverFlowThreshold)
    #     #}
    #     methodNumber <- methodNumber+1
    # }
    result <- .bfCorrieKernelKendallTau(n=n, tauObs=tauObs, kappa=kappa, var=var, ciValue=ciValue)
    result$call <- paste0(".bfKendallTau(n=", n, ", tauObs=", tauObs, ", kappa=", kappa, ", var=", var, ", ciValue=", ciValue, ")")
    result$stat <- tauObs
    return(result)
}

.bfCorrieKernelKendallTau <- function(n, tauObs, kappa=1, var=1, ciValue=0.95) { 
    tempList <- list(vector())
    result <- list(n=n, tauObs=tauObs, bf10=NA, bfPlus0=NA, bfMin0=NA, methodNumber=NA, betaA=NA, betaB=NA, 
                   twoSidedTooPeaked=FALSE, plusSidedTooPeaked=FALSE, minSidedTooPeaked=FALSE, 
                   ci=tempList, ciValue=ciValue, acceptanceRate=1)
    
    predictiveMatchingList <- list(bf10=1, bfPlus0=1, bfMin0=1, twoSidedTooPeaked=FALSE, plusSidedTooPeaked=FALSE, minSidedTooPeaked=FALSE, methodNumber=0)
    
    if (kappa <= 0.002) {
        result <- utils::modifyList(result, predictiveMatchingList)
        return(result)
    }
    
    if (any(is.na(tauObs)) ) {
        result$methodNumber <- 6
        result$twoSidedTooPeaked <- TRUE 
        result$plusSidedTooPeaked <- TRUE 
        result$minSidedTooPeaked <- TRUE
        return(result)
    }
    
    checkTau <- abs(tauObs) >= 1 # check whether |tau| >= 1
    if (n <= 2 || kappa==0) {
        result <- utils::modifyList(result, predictiveMatchingList)
        return(result)
    }
    
    result$bf10 <- .priorTau(tauPop=0, kappa)/.posteriorTau(n=n, tauObs=tauObs, tauPop=0, kappa=kappa, var=var, test="two-sided")
    result$bfPlus0 <- .priorTauPlus(tauPop=0, kappa)/.posteriorTau(n=n, tauObs=tauObs, tauPop=0, kappa=kappa, var=var, test="positive")
    result$bfMin0 <- .priorTauMin(tauPop=0, kappa)/.posteriorTau(n=n, tauObs=tauObs, tauPop=0, kappa=kappa, var=var, test="negative")
    result$methodNumber <- 1
    
    # Calculate credible intervals
    if (!is.null(ciValue)) {
        result$ci <- .computeKendallCredibleInterval(n=n, tauObs=tauObs, kappa=kappa, var=var, ciValue=ciValue)
    }
    return(result)
}


# Replication Bayes factors
#

.bfCorrieRepJosine <- function(nOri, rOri, nRep, rRep, kappa=1, hyperGeoOverFlowThreshold=24) {
    result <- list(combined=list(bf10=NA, bfPlus0=NA, bfMin0=NA))
    
    methodNumber <- 1
    while (methodNumber <= 4 && any(is.na(c(result$combined$bf10, 
                                            result$combined$bfPlus0, 
                                            result$combined$bfMin0)), 
                                    is.infinite(result$combined$bf10))) {
        result <- .bfCorrieRepJosineKernel(nOri=nOri, rOri=rOri, nRep=nRep, rRep=rRep, kappa=kappa, methodNumber=methodNumber, hyperGeoOverFlowThreshold=hyperGeoOverFlowThreshold)
        methodNumber <- methodNumber+1
    }
    
    result$call <- 
        paste0(".bfCorrieRepJosine(nOri=", nOri, ", rOri=", rOri, ", nRep=", nRep, ", rRep=", rRep, ", kappa=", kappa, ", hyperGeoOverFlowThreshold=", hyperGeoOverFlowThreshold, ")")
    
    return(result)
}

.bfCorrieRepJosineKernel <- function(nOri, rOri, nRep, rRep, kappa=1, methodNumber=1, hyperGeoOverFlowThreshold=24) {
    # 
    #  Ly, A., Etz, A., Marsman, M., & Wagenmakers, E.--J. (2017) Replication Bayes factors. Manuscript in preparation
    #  Ly, A., Marsman, M., & Wagenmakers, E.-J. (2017) Analytic Posteriors for Pearson’s Correlation Coefficient. Under review
    #  Wagenmakers, E.-J., Verhagen, A. J., & Ly, A. (2016). How to quantify the evidence for the absence of a correlation. Behavior Research Methods, 48, 413-426.
    # 
    # Replication BF for the correlation
    #
    # 1:2 are based on the exact reduced likelihood functions
    # 3:4 are based on the beta approximations to the reduced likelihood functions
    #
    #	methodNumber=1: Use exact likelihood Ly, Marsman, Wagenmakers (2017)
    #	methodNumber=2: Use semi-exact result, based on approximation of the likelihood JeffreysExact, see Wagenmakers et al (2015) bathing
    #	methodNumber=3: Savage Dickey beta approximation 
    #	methodNumber=4: Marsman's IMH sampler and then Savage Dickey beta approximation
    #
    # Output is a list of the 
    #   - original data, 
    #   - rep data, 
    #   - combined inference 
    #   - replication BFs given the original 
    # 
    # 
    
    # TODO: avoid when pass through object
    oriObj <- .bfCorrieKernel(n=nOri, r=rOri, method=methodNumber, kappa=kappa)
    
    # Default is "NA" list
    result <- list(ori=oriObj, rep=list(NULL), 
                   combined=list(n=c(nOri, nRep), r=c(rOri, rRep), 
                                 repMethodNumber=methodNumber,
                                 bf10=NA, bfPlus0=NA, bfMin0=NA, 
                                 betaA=NA, betaB=NA) , 
                   repGivenOri=list(n=c(nOri, nRep), r=c(rOri, rRep), 
                                    bf10=NA, bfPlus0=NA, bfMin0=NA), 
                   repMethodNumber=methodNumber)
    
    if (is.infinite(oriObj$bf10)) {
        # No use, too big too great, it's true
        #
        return(result)
    }
    
    # Calculate beta fits of the combined likelihood
    if (kappa==1) {
        #
        # methods 3 and 4 are highly dependent on the beta fits based on kappa = 1
        if (methodNumber %in% 3:4 && any(is.na(c(oriObj$betaA, oriObj$betaB)))) {
            # Total failure, real sad
            return(result)
        }
        
        repObj <- .bfCorrieKernel(n=nRep, r=rRep, method=methodNumber, kappa=kappa)
        result$rep <- repObj
        
        if (methodNumber %in% 3:4 && any(is.na(c(repObj$betaA, repObj$betaB)))) {
            # Failed 
            return(result)
        }
        
        result$combined$betaA <- oriObj$betaA-1+repObj$betaA
        result$combined$betaB <- oriObj$betaB-1+repObj$betaB
    } else {
        # kappa \neq 1
        
        if (methodNumber %in% 1:3) {
            oriLikelihoodFit <- .posteriorBetaParameters(n=nOri, r=rOri, kappa=1)
            repLikelihoodFit <- .posteriorBetaParameters(n=nRep, r=rRep, kappa=1)
        }
        
        if (methodNumber==4) {
            oriLikelihoodFit <- .marsmanMHSampler(n=nOri, r=rOri, kappa=1)
            
            if (is.na(oriLikelihoodFit$betaA) || is.na(oriLikelihoodFit$betaB)) {
                # Total failure, it's sad
                #
                return(result)
            }
            
            repLikelihoodFit <- .marsmanMHSampler(n=nRep, r=rRep, kappa=1)
        }
        
        if (methodNumber %in% 3:4) {
            if (any(is.na(c(oriLikelihoodFit$betaA, oriLikelihoodFit$betaB, 
                            repLikelihoodFit$betaA, repLikelihoodFit$betaB)))) {
                # Failure
                return(result)
            }
        }
        # combine here
        result$combined$betaA <- oriLikelihoodFit$betaA-1+repLikelihoodFit$betaA-1+1/kappa
        result$combined$betaB <- oriLikelihoodFit$betaB-1+repLikelihoodFit$betaB-1+1/kappa
        
        
        # Here kappa not 1, but still can see what the original default bfs will do for the rep data
        repObj <- .bfCorrieKernel(n=nRep, r=rRep, method=methodNumber, kappa=kappa)
        result$rep <- repObj
    }

    if (methodNumber=="exact" || methodNumber==1) {
        twoSidedIntegrand <- function(x){.hFunctionCombinedTwoSided(nOri=nOri, rOri=rOri, nRep=nRep, rRep=rRep, x)*.priorRho(x, kappa=kappa)}
        plusSidedIntegrand <- function(x){.hFunctionCombined(nOri=nOri, rOri=rOri, nRep=nRep, rRep=rRep, x)*.priorRhoPlus(x, kappa=kappa)}
        minSidedIntegrand <- function(x){.hFunctionCombined(nOri=nOri, rOri=rOri, nRep=nRep, rRep=rRep, x)*.priorRhoMin(x, kappa=kappa)}
    } else if (methodNumber=="jeffreysIntegrate" || methodNumber==2) {
        twoSidedIntegrand <- function(x){.hJeffreysApprox(nRep, rRep, x)*.hJeffreysApprox(nOri, rOri, x)*.priorRho(x, kappa=kappa)}
        plusSidedIntegrand <- function(x){.hJeffreysApprox(nRep, rRep, x)*.hJeffreysApprox(nOri, rOri, x)*.priorRhoPlus(x, kappa=kappa)}
        minSidedIntegrand <- function(x){.hJeffreysApprox(nRep, rRep, x)*.hJeffreysApprox(nOri, rOri, x)*.priorRhoMin(x, kappa=kappa)}
    } 
    
    
    if (methodNumber %in% 1:2) {
        bf10Combined <- try(silent=TRUE, exp=integrate(twoSidedIntegrand, -1, 1)$value)
        
        if (isTryError(bf10Combined)) {
            # Total loser, can't even calculate the combined bf10
            return(result)
        }
        
        if (is.na(bf10Combined)) {
            # So sad combined bf10 not available
            result$combined$bf10 <- NA
            return(result)
        }
        
        if (is.infinite(bf10Combined)) {
            # So big, totally infinite
            #
            result$combined$bf10 <- Inf
            result$repGivenOri$bf10 <- Inf
            
            if (r >= 0) {
                result$combined$bfPlus0 <- Inf
                result$combined$bfMin0 <- 0
                
                result$repGivenOri$bfPlus0 <- Inf
                result$repGivenOri$bfMin0 <- 0
            } else if (r < 0) {
                result$combined$bfPlus0 <- 0
                result$combined$bfMin0 <- Inf
                
                result$repGivenOri$bfPlus0 <- 0
                result$repGivenOri$bfMin0 <- Inf
            }
            return(result)
        }
        
        if (is.finite(bf10Combined)) {
            # Total winner, real great, it's the best
            
            result$combined$bf10 <- bf10Combined
            result$repGivenOri$bf10 <- bf10Combined/oriObj$bf10
            
            if (log(bf10Combined) > hyperGeoOverFlowThreshold) {
                # So big like my hands, can't handle it need to adjust
                tempList <- .bfSavageDickeyOneSidedAdapt(bf10Combined, a=result$combined$betaA, b=result$combined$betaB, kappa=kappa)
                
                result$combined$bfPlus0 <- tempList$bfPlus0
                result$combined$bfMin0 <- tempList$bfMin0
            } else {
                # No overflow, thus, try numerically integrate
                #
                bfPlus0Combined <- try(silent=TRUE, exp=integrate(plusSidedIntegrand, 0, 1)$value)
                bfMin0Combined <- try(silent=TRUE, exp=integrate(minSidedIntegrand, -1, 0)$value)
                
                if (isTryError(list(bfPlus0Combined, bfMin0Combined))) {
                    # One sided failed
                    return(result)
                }
                
                if ( bfPlus0Combined < 0 || bfMin0Combined < 0) {
                    # One sided failed
                    return(result)
                }
                
                if (is.na(bfPlus0Combined) || is.na(bfMin0Combined) || 
                    is.infinite(bfPlus0Combined) || is.infinite(bfMin0Combined) || 
                    (bfPlus0Combined > 1 && bfMin0Combined > 1) || 
                    (bfPlus0Combined < 1 && bfMin0Combined < 1) ) {
                    tempList <- .bfSavageDickeyOneSidedAdapt(bf10Combined, a=result$combined$betaA, b=result$combined$betaB, kappa=kappa)
                    
                    result$combined$bfPlus0 <- tempList$bfPlus0
                    result$combined$bfMin0 <- tempList$bfMin0
                } else {
                    # All good, store numerically calculated one-sided bfs
                    
                    result$combined$bfPlus0 <- bfPlus0Combined
                    result$combined$bfMin0 <- bfMin0Combined
                }
            }
        } 
    } 
    
    
    if (methodNumber %in% 3:4) {
        # TODO:
        if (!is.na(result$combined$betaA) && !is.na(result$combined$betaB)) {
            # Use beta fit and Savage-Dickey 
            tempList <- .bfSavageDickeyBeta(a=result$combined$betaA, b=result$combined$betaB, kappa=kappa)
            result$combined$bf10 <- tempList$bf10
            result$combined$bfPlus0 <- tempList$bfPlus0
            result$combined$bfMin0 <- tempList$bfMin0
        }
    }
    
    # TODO: checks for bf10Combined, bfPlus0Combined, bfMin0Combined for zeroes and infinities
    result$repGivenOri$bf10 <- (result$combined$bf10) / (oriObj$bf10)
    result$repGivenOri$bfPlus0 <- (result$combined$bfPlus0) / (oriObj$bfPlus0)
    result$repGivenOri$bfMin0 <- (result$combined$bfMin0) / (oriObj$bfMin0)
    
    return(result)
}



# 4.0 Posteriors to graph TODO: we have to think about this, different
# results, thus, also switching of the illustrations?
#


# 4.1 Two-sided
.posteriorRho <- function(n, r, rho, kappa=1) {
	if (!is.na(r) && !r==0) {
		return(1/.bf10Exact(n=n, r=r, kappa)*.hFunction(n=n, r=r, rho)*.priorRho(rho, kappa))
	} else if (!is.na(r) && r==0) {
		return(1/.bf10JeffreysIntegrate(n=n, r=r, kappa)*.hJeffreysApprox(n=n, r=r, rho)*.priorRho(rho, kappa))
	}	
}

.posteriorRhoPlus <- function(n, r, rho, kappa=1) {
	if (!is.na(r) && !r==0) {
		return(1/.bfCorrieKernel(n=n, r=r, kappa, method="exact")$bfPlus0*.hFunction(n=n, r=r, rho)*.priorRhoPlus(rho, kappa))
	} else if (!is.na(r) && r==0) {
		return(1/.bfCorrieKernel(n=n, r=r, kappa, method="jeffreysIntegrate")$bfPlus0*.hJeffreysApprox(n=n, r=r, rho)*.priorRhoPlus(rho, kappa))
	}	
}

.posteriorRhoMin <- function(n, r, rho, kappa=1) {
	if (!is.na(r) && !r==0) { 
		return(1/.bfCorrieKernel(n=n, r=r, kappa, method="exact")$bfMin0*.hFunction(n=n, r=r, rho)*.priorRhoMin(rho, kappa))
	} else if (!is.na(r) && r==0) {
		return(1/.bfCorrieKernel(n=n, r=r, kappa, method="jeffreysIntegrate")$bfMin0*.hJeffreysApprox(n=n, r=r, rho)*.priorRhoMin(rho, kappa))
	}	
	
}


.approximatePosteriorRho <- function(rho, n, r) {
  1/(1-rho^2)*stats::dnorm(atanh(rho), mean=atanh(r), sd=1/sqrt(n))
}

.approximatePosteriorRhoPlus <- function(rho, n, r) {
  (.approximatePosteriorRho(rho,n,r) * (rho>0)) / (stats::pnorm(0,mean=atanh(r),sd=1/sqrt(n),lower.tail = FALSE))
}

.approximatePosteriorRhoMin <- function(rho, n, r) {
    (.approximatePosteriorRho(rho,n,r) * (rho<0)) / (stats::pnorm(0,mean=atanh(r),sd=1/sqrt(n)))
}
  


# 4.2 
.posteriorMean <- function(n, r, kappa=1) {
	# Posterior mean of the .bf10Exact
	#	That is, (rho+1)/2, thus, on 0,1 scale to estimate a, b in a beta distribution
	#
	# TODO: add safeguard for large n as then hyperTerm1/hyperTerm2 is almost 1
	# 	and also for logTerm almost being 1 (it works okay if I cut off the hyperTerm1 
	# 	with three terms and hyperTerm2 with three terms and then divide them, though, 
	#	this is rather bad as a formal procedure due to the fact that it violates the 
	#	definition of products of sum sequences. Though it yields a good approximation.
	#
	# 	if (abs(r) < 0.5 && n <= 200) {
	# 		logTerm <- 2*(lgamma(n/2)-lgamma((n-1)/2))
	# 		hyperTerm1 <- Re(hypergeo::hypergeo((n/2), (n/2), ((n+2/kappa+2)/2), r^2))
	# 		hyperTerm2 <- Re(hypergeo::hypergeo(((n-1)/2), ((n-1)/2), ((n+2/kappa)/2), r^2))
	# 		
	# 		some.factor <- exp(logTerm)*hyperTerm1/hyperTerm2
	# 	} else {
	# 		# TODO: a linear approximation, needs proof
	# 		some.factor <- n/2
	# 	}
	#
	#logTerm <- 2*(lgamma(n/2)-lgamma((n-1)/2))
	#
	# Note: interestingly, it breaks down from n=199 to n=200 when using hypergeo
	# that is:
	#
	# 	.posteriorMean(200, 0.8) yields -2.600069e+26
	# 	.posteriorMean(199, 0.8) yields 0.7948551
	#
	#hyperTerm1 <- Re(hypergeo::hypergeo((n/2), (n/2), ((n+2/kappa+2)/2), r^2))
	#hyperTerm2 <- Re(hypergeo::hypergeo(((n-1)/2), ((n-1)/2), ((n+2/kappa)/2), r^2))
	
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
	#hyperTerm1 <- Re(hypergeo::f15.3.1((n/2), (n/2), ((n+2/kappa+2)/2), r^2))
	#hyperTerm2 <- Re(hypergeo::f15.3.1(((n-1)/2), ((n-1)/2), ((n+2/kappa)/2), r^2))
	#
	# Note: interestingly, the continued fraction solution goes haywire, that is:
	#
	#
	#	.posteriorMean(n=67, 0.8) yielding 0.8526101  (a peak)
	#	.posteriorMean(n=299, 0.8) yielding -1.179415
	#
	#hyperTerm1 <- Re(hypergeo::hypergeo_contfrac((n/2), (n/2), ((n+2/kappa+2)/2), r^2))
	#hyperTerm2 <- Re(hypergeo::hypergeo_contfrac(((n-1)/2), ((n-1)/2), ((n+2/kappa)/2), r^2))
	#
	#hyperTerm1 <- Re(hypergeo::genhypergeo(U=c(n/2, n/2), L=c((n+2/kappa+2)/2), z=r^2))
	#hyperTerm2 <- Re(hypergeo::genhypergeo(U=c((n-1)/2, (n-1)/2), L=c((n+2/kappa)/2), z=r^2))
	#
	#some.factor <- exp(logTerm)*hyperTerm1/hyperTerm2
	#
	#result <- 2*r/(n+2/kappa)*some.factor
	#return(result)
	
	if (n <= 2) {
		return(NA)
	} else if (any(is.na(r))) {
		return(NA)
	}
	# TODO: use which
	checkR <- abs(r) >= 1 # check whether |r| >= 1
	
	if (kappa >= 1 && n > 2 && checkR) {
		return(r)
	}
	#logHyperTerm <- log(hypergeo::hypergeo(((n-1)/2), ((n-1)/2), ((n+2/kappa)/2), r^2))
	hyperTerm1 <- Re(hypergeo::genhypergeo(U=c(n/2, n/2), L=c((2+(n+2)*kappa)/(2*kappa)), z=r^2))
	hyperTerm2 <- Re(hypergeo::genhypergeo(U=c((n-1)/2, (n-1)/2), L=c((2+n*kappa)/(2*kappa)), z=r^2))
	
	logResult <- 2*(lgamma(n/2)-lgamma((n-1)/2))
	result <- (2*kappa*r)/(2+n*kappa)*exp(logResult)*hyperTerm1/hyperTerm2
	
	if (is.na(result) || abs(result) > 1) {
		return(r)
	} else {
		return(result)
	}
}


.posteriorSecondMoment <- function(n, r, kappa=1) {
	#
	#
	if (n <= 2) {
		return(NA)
	} else if (any(is.na(r))) {
		return(NA)
	}
	# TODO: use which
	checkR <- abs(r) >= 1 # check whether |r| >= 1
	
	if (kappa >= 1 && n > 2 && checkR) {
		return(r)
	}
	#log.hyper.term <- log(hypergeo::hypergeo(((n-1)/2), ((n-1)/2), ((n+2/kappa)/2), r^2))
	hyperTerm1 <- Re(hypergeo::genhypergeo(U=c(3/2, (n-1)/2, (n-1)/2), 
											 L=c(1/2, (2+(n+2)*kappa)/(2*kappa)), z=r^2))
	hyperTerm2 <- Re(hypergeo::genhypergeo(U=c((n-1)/2, (n-1)/2), L=c((2+n*kappa)/(2*kappa)), z=r^2))
	
	result <- kappa/(n*kappa+2)*hyperTerm1/hyperTerm2
	
	if (is.na(result) || result <= 0) {
		return(NA)
	} else {
		return(result)
	}
}

.posteriorVariance <- function(n, r, kappa=1) {
	# Posterior mean of the .bf10Exact
	#	That is, (rho+1)/2, thus, on 0,1 scale to estimate a, b in a beta distribution
	#
	# TODO: add safeguard for large n as then hyperTerm1/hyperTerm2 is almost 1
	# 	and also for logTerm almost being 1
	#
	# 	.posteriorVariance(199, 0.8) yields 6808.702
	# 	
	#
	#hyperTerm3 <- Re(hypergeo::genhypergeo(U=c((n-1)/2, (n+1)/2),L=(n+2/kappa)/2, z=r^2))
	#hyperTerm4 <- Re(hypergeo::genhypergeo(U=c((n-1)/2, (n-1)/2), L=(n+2/kappa)/2, z=r^2))
	# result.0 <- 1/((r+2/kappa*r)^2)*(
	#(n-1)*(n+2/kappa-1)-
	#	(2/kappa+1)*(n-2)*r^2+
	#	(n-1)*(n+2/kappa-1)*(r^2-1)*hyperTerm3/hyperTerm4)
	#result <- .posteriorSecondMoment(n,r,kappa)-(.posteriorMean(n,r,kappa))^2
	
	result <- .posteriorSecondMoment(n,r,kappa)-(.posteriorMean(n,r,kappa))^2
	
	if (is.na(result) | result <= 0) {
		return(NA)
	} else {
		return(result)
	}
}

.betaParameterEstimates <- function(someMean, someVar) {
	# someMean \in (0, 1)
	# TODO: think about someMean = 0
	some.a <- someMean*(someMean*(1-someMean)/someVar-1)
	some.b <- (1-someMean)*(someMean*(1-someMean)/someVar-1)
	
	result <- list(betaA=some.a, betaB=some.b)
	return(result)
}

.posteriorBetaParameters <- function(n, r, kappa=1) {
	some.mu <- try((.posteriorMean(n=n, r=r, kappa)+1)/2)
	some.var <- try(.posteriorVariance(n=n, r=r, kappa)/2^2)
	
	if (is(some.mu, "try-error") || is(some.var, "try-error") || is.na(some.mu) || is.na(some.var)) {
		# TODO: Before doing this try the MH sampler
		return(list(betaA=NA, betaB=NA))
	} else {
		return(.betaParameterEstimates(some.mu, some.var))
	}
}

.computePearsonCredibleInterval <- function(alpha, beta, ciValue) {
    # Compute Pearson's correlation credible interval based on a beta fit
    #
    result <- list(twoSided=NA, minSided=NA, plusSided=NA)
    
    if (is.null(ciValue)) {
        return(result)
    }
    
    if (ciValue <= 0 || ciValue >= 1) {
        # Note: Don't modify ciValue <- solve this at a higher level
        return(result)
    }
    
    typeOne <- 1-ciValue
    excessLevel <- typeOne/2
    
    if (any(is.na(c(alpha, beta)), is.infinite(c(alpha, beta)))) {
        return(result)
    } else {
        # Note: Zero one refers to the problem on the (0, 1) rather than on (-1, 1)
        lowerCIZeroOne <- try(qbeta(excessLevel, alpha, beta))
        medianCIZeroOne <- try(qbeta(1/2, alpha, beta))
        upperCIZeroOne <- try(qbeta(1-excessLevel, alpha, beta))
        
        if (isTryError(list(lowerCIZeroOne, medianCIZeroOne, upperCIZeroOne))) {
            return(result)
        } else {
            # Note: This is simply an application of the definition of the stretched beta
            lowerCI <- 2*lowerCIZeroOne-1 
            medianCI <- 2*medianCIZeroOne-1
            upperCI <- 2*upperCIZeroOne-1
        }
    }
    
    
    result$twoSided <- c(lowerCI, medianCI, upperCI)
    
    # One sided:
    result$minSided <- .computePearsonMinSidedCredibleInterval(alpha, beta, ciValue)
    # The problem is symmetric
    temp  <- .computePearsonMinSidedCredibleInterval(beta, alpha, ciValue)
    result$plusSided <- c(-temp[3], -temp[2], -temp[1])
    
    return(result)
}

.computeKendallCredibleInterval <- function(n, tauObs, kappa=1, var=1, ciValue=0.95) {
    # Compute Kendall's correlation credible interval based on a sampling
    #
    # tau is the observed Kendall's tau
    #
    result <- list(twoSided=NA, minSided=NA, plusSided=NA)
    
    if (is.null(ciValue)) {
        return(result)
    }
    
    if (ciValue <= 0 || ciValue >= 1) {
        # Note: Don't modify ciValue <- solve this at a higher level
        return(result)
    }
    
    result$twoSided <- .credibleIntervalKendallTau(n=n, tauObs=tauObs, kappa=kappa, var=var, test="two-sided", ciValue=ciValue)
    result$plusSided <- .credibleIntervalKendallTau(n=n, tauObs=tauObs, kappa=kappa, var=var, test="positive", ciValue=ciValue)
    result$minSided <- .credibleIntervalKendallTau(n=n, tauObs=tauObs, kappa=kappa, var=var, test="negative", ciValue=ciValue)
    return(result)
}

# Compute credible intervals kendalls tau
.credibleIntervalKendallTau <- function(n, tauObs, kappa=1, var=1, test="two-sided", ciValue = 0.95) {
    nSeqs <- 1000
    lowCI <- (1-ciValue)/2
    upCI <- (1+ciValue)/2
    tauPopDomain <- seq(-1, 1, length.out=(nSeqs-1))
    densVals <- .posteriorTau(n=n, tauObs=tauObs, tauPop=tauPopDomain, kappa=kappa, var=var, test=test)
    cdfVals <- cumsum((densVals[1:(nSeqs-1)] + densVals[2:nSeqs]) * 0.5 * (tauPopDomain[2]-tauPopDomain[1]))
    #densVals <- cumsum(densVals)/sum(densVals)
    lowerCI <- tauPopDomain[which(cdfVals>=lowCI)[1]]
    upperCI <- tauPopDomain[which(cdfVals>=upCI)[1]]
    median <- tauPopDomain[which(cdfVals>=0.5)[1]]
    return(c(lowerCI, median, upperCI))
}

.computePearsonMinSidedCredibleInterval <- function(alpha, beta, ciValue) {
    # Compute min sided Pearson's correlation credible interval based on a beta fit
    #
    result <- NA
    typeOne <- 1-ciValue
    excessLevel <- typeOne/2
    
    if (any(is.na(c(alpha, beta)), is.infinite(c(alpha, beta)))) {
        return(result)
    } else {
        leftArea <- pbeta(1/2, alpha, beta)
        lowerCIZeroOne <- try(qbeta(excessLevel*leftArea, alpha, beta))
        medianCIZeroOne <- try(qbeta(leftArea/2, alpha, beta))
        upperCIZeroOne <- try(qbeta((1-excessLevel)*leftArea, alpha, beta))
        
        if (isTryError(list(lowerCIZeroOne, medianCIZeroOne, upperCIZeroOne))) {
            return(result)
        } else {
            lowerCI <- 2*lowerCIZeroOne-1
            medianCI <- 2*medianCIZeroOne-1
            upperCI <- 2*upperCIZeroOne-1
        }
    }
    result <- c(lowerCI, medianCI, upperCI)
    return(result)
}

#}
# 
# 
# .rhoQuantile <- function(n=n, r=r, kappa=1, ciPercentage=.95) {
# 	# Fitting parameters
# 	beta.fit <- try(.posteriorBetaParameters(n=n, r=r, kappa))
# 	
# 	if (is(beta.fit, "try-error") || is.na(beta.fit$alpha) || is.na(beta.fit$beta)) {
# 		return(c(NA, r, NA))
# 	}
# 	
# 	# result median
# 	some.median <- 2*qbeta(.5, beta.fit$alpha, beta.fit$beta)-1
# 	
# 	# Calculate CI
# 	type.one <- 1-ciPercentage
# 	
# 	left.CI <- try(2*qbeta(type.one/2, beta.fit$alpha, beta.fit$beta)-1)
# 	right.CI <- try(2*qbeta((1-type.one/2), beta.fit$alpha, beta.fit$beta)-1)
# 	
# 	# TODO: This actually doesn't override left.CI or rigthCI even if they are try-errors
# 	if ( is(left.CI, "try-error") || is(right.CI, "try-error") || is.na(left.CI) || is.na(right.CI) ) {
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
	tau <- cor(x, y, method="kendall")
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
		    if (round(numIntegrate,digits=2) != 1) {
		        posteriorLine <- .approximatePosteriorRho(rho = rho, n = n, r = r)
		    }
		})
		
		posteriorLineTau <- .posteriorTau(n=n, tauObs=tau, tauPop=rho, kappa=kappa, var=1, test="two-sided")
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
	        if (round(numIntegrate,digits=2) != 1){posteriorLine <- .approximatePosteriorRhoPlus(rho = rho, n = n, r = r)}
	    })
		 
		posteriorLineTau <- .posteriorTau(n=n, tauObs=tau, tauPop=rho, kappa=kappa, var=1, test="positive")
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
		  if (round(numIntegrate,digits=2) != 1) {posteriorLine <- .approximatePosteriorRhoMin(rho = rho, n = n, r = r)}
		})
		
		posteriorLineTau <- .posteriorTau(n=n, tauObs=tau, tauPop=rho, kappa=kappa, var=1, test="negative")
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
		
	  if (addRho & addTau) {
	    lines(rho, posteriorLine, lwd= lwd)
	    lines(rho,posteriorLineTau, lwd=lwd, lty=2)
	    xlabExpression <- "Correlation Coefficient"
	    legend(x=legendPosition[1],y=legendPosition[2], legend=c(expression(rho),expression(tau)), lty=1:2, cex=cexYlab, bty="n", lwd=lwd)
	  } else if (addRho) {
	    lines(rho, posteriorLine, lwd= lwd)
	    xlabExpression <- expression(rho)
	  } else if (addTau) {
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
				
				# image <- .beginSaveImage(width, height)
				.plotFunc <- function() {
				
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
				}
				# content <- .endSaveImage(image)
				content <- .writeImage(width = width, height = height, plot = .plotFunc, obj = TRUE)
				
				plot <- correlation.plot
				plot[["convertible"]] <- TRUE
				plot[["obj"]] <- content[["obj"]]
				plot[["data"]] <- content[["png"]]
				# plot[["data"]]  <- content
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
