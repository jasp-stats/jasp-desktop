#
# Copyright (C) 2013-2018 University of Amsterdam
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

RegressionLogLinearBayesian <- function(dataset, options, perform="run", callback, ...) {
    # Init end result container
    endResults <- list()
    
    # subsresult containter 
    results <- list()
    meta <- list()
    .meta <-  list(list(name="title", type="title"),
                   list(name="table", type="table"),
                   list(name="posteriorTable", type="table"),
                   list(name="Bayesianlogregression", type="table"),
                   list(name="BayesianSublogregression", type="table")
    )
    
    
    results[[".meta"]] <- .meta
    results[["title"]] <- "Bayesian Log-Linear Regression"
    
    logLinearBayesianCitations <- 	list(
        "Overstall, A., & King, R. (2014). conting: an R package for Bayesian analysis of complete and incomplete contingency tables. Journal of Statistical Software, 58(7), 1-27."
    )
    
    # Init table
    posteriorTable <- list()
    
    posteriorTable[["title"]] <- "Model Comparison"
    posteriorTable[["citation"]] <- logLinearBayesianCitations
    
    if (options$bayesFactorType == "BF10") {
        bfTitle <- "BF<sub>10</sub>"
    } else if (options$bayesFactorType == "BF01") {
        bfTitle <- "BF<sub>01</sub>"
    } else {
        bfTitle <- "Log(BF<sub>10</sub>)"
    }
    
    # Declare table elements
    fields <- list(
        list(name = "number", type = "integer",title = " "),
        list(name = "model", type = "string", title = "Models"),
        list(name = "pMdata", type = "number", format = "dp:3", title = "P(M|data)"),
        list(name = "bf", type = "number", format="sf:4;dp:3", title = bfTitle)
    )
    
    emptyRow <- list( #for empty elements in tables when given output
        "number" = "",
        "model" = "",
        "pMdata" = "",
        "bf" = ""
    )
    
    dotted.line <- list( #for empty tables
        "number" = ".",
        "model" = ".",
        "pMdata" = ".",
        "bf" = "."
    )
    
    posteriorTable[["schema"]] <- list(fields = fields)
    
    footnotes <- .newFootnotes()
    
    if (is.null(dataset)) {
	    if (options$counts == "") {
	        countsVar <- NULL
	    } else {
	        countsVar <- options$counts
	    }
	    
	    if (perform == "run") {
	        dataset <- .readDataSetToEnd(columns.as.factor=options$factors, columns.as.numeric=countsVar)
	    } else {
	        dataset <- .readDataSetHeader(columns.as.factor=options$factors, columns.as.numeric=countsVar)
		}
	}	 
	 
	listOfErrors <- list()
	errorMessage <- NULL
	
	# Default error result
	# 
	# 
	initialObject <- list("anthonyObj" = NULL, 
	                      "variables" = c("...", "... "),
	                      "nModelsVisited" = NULL,
	                      "nBurnIn" = NULL,
	                      "bf10s" = rep(".", length=2), 
	                      "postModelProbs" = rep(".", length=2), 
	                      "modelNames" = NULL, 
	                      "hasErrors" = FALSE, 
	                      "errorMessages" = list()
	)
	
	errorBfObj <- list(nModelsVisited=NA, postModelProbs=rep(NA, length=options$maxModels), 
	                    bf10s=rep(NA, length=options$maxModels)
	)
	
	numberOfModels <- length(options$modelTerms)
	
	variablesInModel <- NULL
	
	if (perform == "init") {
	    if (numberOfModels == 0) {
	        variablesInModel <- c("...", "... ")
	    } else {
	        for (i in 1:length(options$modelTerms)){
	            components <- options$modelTerms[[i]]$components
	            
	            if (length(components) == 1) {
	                variablesInModel <- c(variablesInModel, components[[1]])
	            } else {
	                componentsUnlisted <- unlist(components)
	                variablesInModel <- c(variablesInModel, paste0(componentsUnlisted, collapse=":"))
	            }
	        }
	        
	        
	        # Remove all empty variables
	        variablesInModel <- variablesInModel[ variablesInModel != ""]
	        
	        
	        if (length(variablesInModel)==0){
	            variablesInModel <- c("...", "... ")
	        } else if ( length(variablesInModel) == 1 ) {
	            variablesInModel <- c(variablesInModel, "... ")
	        }
	    } 
	    
	    bfObject <- initialObject
	    bfObject$variables <- variablesInModel
	}
	
	# 
	if (perform == "run"){
	    # Data screening
	    #
	    # TODO: STATE RETRIEVAL HERE State retrieval here. If not retrieval, then create bfObject
	    #   Try saving them by names of the variables.
	    # 
	    # order(variablesInModel) 
	    # TODO: Ask Anthony about what to do when we subset models 
	    # 
	    bfObject <- initialObject
	    anthonyObj <- NULL
	    
	    
	    # TODO: If bfObject has an error, then skip this 
	    # if (isTRUE(bfObject$error)) 
	    
	    # Counts defined
	    if (options$counts != "") {
	        # Start tallying
	        badVariableNames <- NULL

	        for (counts in options$counts) {
	            if ( any(is.na(dataset [[.v (options$counts)]])) ) {
	                badVariableNames <- c (badVariableNames, options$counts)
	            }
	        }

	        if (!is.null(badVariableNames)) {
	            errorMessage <- paste0("Bayes factor is undefined -- incomplete contingency table, the count variable ", badVariableNames, " contain(s) empty cell and/or NaN.")
	            listOfErrors[[ length(listOfErrors) + 1 ]] <- errorMessage
	            
	            # TODO: Tim's error handling
	            bfObject$hasErrors <- TRUE
	            bfObject$errorMessages[[length(bfObject$errorMessages)+1]] <- list(errorMessage=errorMessage, errorType="badData")
	        }
	        
	        if (length(listOfErrors)==0) {
	            badVariableNames <- NULL
	            
	            for (counts in options$counts) {
	                if (any (!is.finite (dataset [[.v (options$counts)]])) || any(dataset [[.v (options$counts)]] < 0 )) {
	                    badVariableNames <- c (badVariableNames, options$counts)
	                }
	            }

	            if (!is.null(badVariableNames)) {
	                errorMessage <- paste0("Bayes factor is undefined -- the count variable ", badVariableNames, " contain(s) infinity and/or negative numbers.")
	                listOfErrors[[ length(listOfErrors) + 1 ]] <- errorMessage
	                
	                # TODO: Tim's error handling
	                bfObject$hasErrors <- TRUE
	                bfObject$errorMessages[[length(bfObject$errorMessages)+1]] <- list(errorMessage=errorMessage, errorType="badData")
	            }
	        }
	    }
	    
	    # Counts not given
	    #
	    if (options$counts == "") {
	        dataset <- plyr::count(dataset)
	    } else {
	        dataset <- dataset
	    }

	    # Data check here
	    if (length(listOfErrors)==0) {
	        if ( isTRUE(anyNA(dataset[.v(options$factors)]))) {
	            errorMessage <- "Bayes factor is undefined -- the factors contain(s) empty cell and/or NaN or incomplete contingency table."
	            listOfErrors[[ length(listOfErrors) + 1 ]] <- errorMessage
	            
	            # TODO: Tim's error handling
	            bfObject$hasErrors <- TRUE
	            bfObject$errorMessages[[length(bfObject$errorMessages)+1]] <- list(errorMessage=errorMessage, errorType="badData")
	        }
	    }

	    # Extract models needed to be compared
	    # 
	    if (options$counts == ""){
	        dependentVariable <- "freq"
	    } else {
	        dependentVariable <- unlist(options$counts)
	    }

	    dependentBase64 <- .v(dependentVariable)

	    # Retrieve model terms
	    #
	    # Start tallying
	    # 
	    variablesInModel <- NULL
	    variablesInModelBase64 <- NULL
	    
	    for (i in seq_along(options$modelTerms)) {
	        components <- options$modelTerms[[i]]$components

	        if (length(components) == 1) {
	            term <- components[[1]]
	            termBase64 <- .v(components[[1]])
	        } else {
	            componentsUnlisted <- unlist(components)
	            term <- paste0(componentsUnlisted, collapse=":")
	            termBase64 <- paste0(.v(componentsUnlisted), collapse=":")
	        }

	        # Add to tally
	        variablesInModel <- c(variablesInModel, term)
	        variablesInModelBase64 <- c(variablesInModelBase64, termBase64)

	        # Remove empty stuff
	        variablesInModel <- variablesInModel[variablesInModel != ""]
	    }

	    # Prune the variables
	    #
	    if (length(variablesInModel)==0) {
	        variablesInModel <- c("...", "... ")
	        modelDefinition <- NULL #this model has no parameters
	    } else if (length(variablesInModel)==1 && options$counts =="") {
	        variablesInModel <- c(variablesInModel, "... ")
	        modelDefinition <- NULL #this model has only one parameters
	    } else if (length(variablesInModel) > 1 || options$counts != "") {
	        modelDefinition <- paste(dependentBase64, "~", paste(variablesInModelBase64, collapse = "+"))
	    } else {
	        # Nothing worked out:
	        modelDefinition <- NULL #this model has no parameters
	        
	        # TODO: Tim's centralised error message 
	        bfObject$hasErrors <- TRUE
	        bfObject$errorMessages[[length(bfObject$errorMessages)+1]] <- list(errorMessage="variables cannot be read", 
	                                                                           errorType="input")
	    }
	    
	    # Save in object
	    bfObject$variables <- variablesInModel
	    
	    # START analysis Bayesian Log Linear regression -----------------
	    # if no error and modelDefinition is okay
	    #
	    if (!is.null(modelDefinition) && !isTRUE(bfObject$hasErrors)) {
		    modelFormula <- as.formula(modelDefinition)

		    if (options$counts == ""){
	 			names(dataset)[names(dataset)== "freq"] <- dependentBase64
		    }

		    # Calculate here
		    anthonyObj <- try(conting::bcct(formula=modelFormula, data = dataset, prior = "SBH", n.sample=2000, a=options$priorShape, b=options$priorScale), silent = TRUE)
		    # anthonyObj <- conting::bcct(formula=modelFormula, data = dataset, prior = "SBH", n.sample=2000, a=options$priorShape, b=options$priorScale)
		    bfObject$nBurnIn <- 2000 * 0.2
		    
		    # TODO: check the maximal number of models visited, if it's one, then sample more
		    # dim(anthonyObj$maximal.mod$x)[2] <- gives the max number of model visited, or
		    #
		    # Alternative, constraint the maximum number of additional iterations
		    # In between do call.backs() <- need to figure out

		    # Always do auto and then manual adds additional samples
		    if (options$sampleMode == "manual"){
		        anthonyObj <- try(conting::bcctu(object = anthonyObj, n.sample = options$fixedSamplesNumber), silent = TRUE)
		        bfObject$nBurnIn <- (2000 + options$fixedSamplesNumber)* 0.2
		    }
		    
		    # Anthony object checking
		    if (class(anthonyObj) == "bcct") {
		        bfObject$anthonyObj <- anthonyObj
		    } else if (isTryError(anthonyObj)) {
		        error <- paste0("R Package error: ", .extractErrorMessage(anthonyObj))
		        listOfErrors[[ length(listOfErrors) + 1 ]]  <- error
		        
		        bfObject <- list(anthonyObj = NA, variables = variablesInModel)
		        # NAs: nModelsVisited, bf10s, postModelProbs
		        bfObject <- modifyList(bfObject, errorBfObj)
		        
		        bfObject$hasErrors <- TRUE
		        bfObject$errorMessages[[length(bfObject$errorMessages)+1]] <- list(errorMessage=error, 
		                                                                           errorType="package")
		    } 
		    # No clue error changed in to exception
		    # else {
		    #     # NAs: nModelsVisited, bf10s, postModelProbs
		    #     bfObject <- modifyList(bfObject, errorBfObj)
		    #     bfObject$hasErrors <- TRUE
		    #     bfObject$errorMessages[[length(bfObject$errorMessages)+1]] <- list(errorMessage="No clue", 
		    #                                                                        errorType="code")
		    # }
		} else if (isTRUE(bfObject$hasErrors)) {
		    # NAs: nModelsVisited, bf10s, postModelProbs
		    bfObject <- modifyList(bfObject, errorBfObj)
		} else if (is.null(modelDefinition)) {
		    # NAs: nModelsVisited, bf10s, postModelProbs
		    bfObject <- modifyList(bfObject, errorBfObj)
		    
		    bfObject$hasErrors <- TRUE
		    bfObject$errorMessages[[length(bfObject$errorMessages)+1]] <- list(errorMessage="Cannot define model", 
		                                                                       errorType="input")
		}
	    # No clue error changed in to exception
	    # else { 
		#     # NAs: nModelsVisited, bf10s, postModelProbs
		#     bfObject <- modifyList(bfObject, errorBfObj)
		#     bfObject <- list(anthonyObj = NA, variables = variablesInModel)
		#     
		#     # Add error message to bfObject
		#     bfObject$hasErrors <- TRUE
		#     bfObject$errorMessages[[length(bfObject$errorMessages)+1]] <- list(errorMessage="No clue", 
		#                                                                        errorType="code")
		# }
	    
	    # Post processing
	    #########################################################################
	    #						 Posterior model probabilities  				#
	    #########################################################################
	    #
	    #
	    if (!isTRUE(bfObject$hasErrors)) {
	        if (class(bfObject$anthonyObj) == "bcct") {
	            # Good case
	            # TODO: Here check anthonySummary$totmodsvisit if this is one, then nothing going on, resample
	            #
	            anthonySummary <- try(conting::mod_probs(bfObject$anthonyObj, scale=0, best = options$maxModels), silent=TRUE)
	            
	            if (inherits(anthonySummary, "modprobs")) {
	                # Good case
	                bfObject$nModelsVisited <- anthonySummary$totmodsvisit
	                bfObject$modelNames <- substring(as.character(anthonySummary$table$model_formula), first=2)
	                
	                if (bfObject$nModelsVisited == 1) {
	                    bfObject$postModelProbs <- 1
	                    bfObject$bf10s <- 1
	                } else if (bfObject$nModelsVisited > 1) {
	                    # Note the following BFs are based on a uniform prior on the models
	                    
	                    if (!is.null(anthonySummary$table$prob.Freq)) {
	                        bfObject$postModelProbs <- anthonySummary$table$prob.Freq
	                        bfObject$bf10s <- anthonySummary$table$prob.Freq / max(anthonySummary$table$prob.Freq)
	                    } else {
	                        # NAs: nModelsVisited, bf10s, postModelProbs
	                        bfObject <- modifyList(bfObject, errorBfObj)
	                        
	                        bfObject$hasErrors <- TRUE
	                        bfObject$errorMessages <- list(errorMessages="R Package error: Cannot retrieve table probabilities", errorType="package")
	                    }
	                }
	                # No clue error changed in to exception
	                # else {
	                #     # This means that bfObject$nModelsVisited not >= 1
	                #     # TODO return error totally
	                #     # NAs: nModelsVisited, bf10s, postModelProbs
	                #     bfObject <- modifyList(bfObject, errorBfObj)
	                #     
	                #     bfObject$hasErrors <- TRUE
	                #     bfObject$errorMessages <- list(errorMessages="No clue", errorType="code")
	                # }
	            } else if (isTryError(anthonySummary)) {
	                # NAs: nModelsVisited, bf10s, postModelProbs
	                bfObject <- modifyList(bfObject, errorBfObj)
	                
	                # TODO: Tim centralised 
	                #
	                bfObject$hasErrors <- TRUE
	                bfObject$errorMessages[[length(bfObject$errorMessages)+1]] <- list(errorMessage=paste0("R Package error: ",.extractErrorMessage(anthonySummary)), 
	                                                                                   errorType="package")
	            } 
	            # No clue error changed in to exception
	            # else {
	            #     # NAs: nModelsVisited, bf10s, postModelProbs
	            #     bfObject <- modifyList(bfObject, errorBfObj)
	            #     
	            #     # TODO: Tim centralised 
	            #     #
	            #     bfObject$hasErrors <- TRUE
	            #     bfObject$errorMessages[[length(bfObject$errorMessages)+1]] <- list(errorMessage="No clue", 
	            #                                                                        errorType="code")
	            # }
	        } else {
	            # Here bfObject$anthonyObj is not of the right class "bcct"
	            # NAs: nModelsVisited, bf10s, postModelProbs
	            bfObject <- modifyList(bfObject, errorBfObj)
	            
	            # TODO: Tim centralised 
	            #
	            bfObject$hasErrors <- TRUE
	            bfObject$errorMessages[[length(bfObject$errorMessages)+1]] <- list(errorMessage="R Package error: Object of wrong class", 
	                                                                               errorType="package")
	        }
	    } else {
	        # Meaning error detected: listOfErrors > 0
	        
	        # NAs: nModelsVisited, bf10s, postModelProbs
	        bfObject <- modifyList(bfObject, errorBfObj)
	    }
	} # ends performs=="run" to calculate a bfObject
	
	
	# TODO: I'm not sure what this is about, perhaps I should ask Tahira
	#
	# if (length(listOfErrors) > 1){
	#     anthonyObj <- try(conting::bcct(modelFormula, data = dataset,  prior = "SBH", n.sample=1000), silent = TRUE)
	#     
	#     # try(conting::bcct(formula=modelFormula, data = dataset, prior = "SBH", n.sample=2000, a=options$priorShape, b=options$priorScale), silent = TRUE)
	# 
	# 	if (isTryError(anthonyObj)) {
	# 		error <- .extractErrorMessage(anthonyObj)
	# 	}
	# 	posteriorTable[["error"]] <- list(errorType="badData", errorMessage = error)
	# } else if (length(listOfErrors) == 1){
	# 	posteriorTable[["error"]] <- list(errorType = "badData", errorMessage = listOfErrors[[ 1 ]])
	# }
	
	
	
	
	
	# Result processing --------------------
	# always result processing
	#
	posteriorTableRows <- list()
	# posteriorTableRows <- list()
	
	nModelsReport <- try(min(bfObject$nModelsVisited, options$maxModels))
	
	if (isTryError(nModelsReport) || isTRUE(bfObject$hasErrors) || perform=="init") {
	    # Report error Bfs
	    nModelsReport <- 2
	} 
	
	if (!is.null(bfObject$modelNames)) {
	    reportNames <- .unvf(bfObject$modelNames)
	} else if (!is.null(bfObject$variables)) {
	    reportNames <- bfObject$variables
	} else {
	    reportNames <- c("...", "... ")
	}
	
	if (!is.null(bfObject$bf10s)) {
	    reportBfs <- bfObject$bf10s
	} else {
	    # TODO: Too much with the error list already??
	    reportBfs <- rep(NA, length=nModelsReport)
	}
	
	if (is.numeric(reportBfs)) {
	    if (options$bayesFactorType == "BF01") {
	        reportBfs <- 1/reportBfs
	    } else if (options$bayesFactorType == "LogBF10") {
	        reportBfs <- log(reportBfs)
	    }
	}
	
	if (!is.null(bfObject$postModelProbs)) {
	    reportPostModelProbs <- bfObject$postModelProbs
	} else {
	    reportPostModelProbs <- rep(NA, length=nModelsReport)
	}
	
	for (i in 1:nModelsReport){
	    posteriorTableRows[[i]] <- emptyRow
	    posteriorTableRows[[i]]$"number" <- as.integer(i)
	    # posteriorTableRows[[i]]$"Model" <- .clean(reportNames[i])
	    posteriorTableRows[[i]]$"model" <- .clean(reportNames[i])
	    posteriorTableRows[[i]]$"pMdata" <- .clean(reportPostModelProbs[i])
	    posteriorTableRows[[i]]$"bf" <- .clean(reportBfs[i])
	    # posteriorTableRows[[i]]$"footnotes" <- as.list(".")
	}
	
	message <- paste ("Total number of models visited =", bfObject$nModelsVisited, sep=" ")
	.addFootnote (footnotes, symbol = "<em>Note.</em>", text = message)
	
	if (isTRUE(bfObject$hasErrors)) {
	    for (i in 1:length(bfObject$errorMessage)) {
	        # Note: Most likely i == 1
	        # TODO: Figure out how error messages are stacked
	        posteriorTable[["error"]] <- bfObject$errorMessage[[i]]
	    }
	} 
		
    posteriorTable[["footnotes"]] <- as.list(footnotes)
	posteriorTable[["data"]] <- posteriorTableRows
	results[["posteriorTable"]] <- posteriorTable
	
	################################################################################
	#						   MODEL COEFFICIENTS TABLE   						#
	################################################################################		
	if (options$regressionCoefficientsEstimates  == TRUE){
		Bayesianlogregression <- list()
		Bayesianlogregression[["title"]] <- "Posterior Summary Statistics"
		Bayesianlogregression[["citation"]] <- logLinearBayesianCitations
		#ci.label <- paste(100*options$regressionCoefficientsCredibleIntervalsInterval, "% Highest posterior density intervals", sep="")
		ci.label <- paste0(100*options$regressionCoefficientsCredibleIntervalsInterval, "% Credible intervals")
		# Declare table elements
		fields <- list(
			list(name = "Name", title = " ", type = "string"),
			list(name = "post_prob", title="P(incl|data)", type = "number", format = "dp:3"),
			list(name = "post_mean", title = "Mean",type="number", format = "dp:3"),
			list(name = "post_var", title = "Variance",type="number", format = "dp:3"))
		if (options$regressionCoefficientsCredibleIntervals == TRUE){
			fields <- c(fields,list(
				list(name = "lower_lim", title = "Lower", overTitle=ci.label, type="number", format = "sf:4;dp:3"),
				list(name = "upper_lim", title = "Upper", overTitle=ci.label, type = "number", format = "sf:4;dp:3")))
		}

		emptyRow <- list(                     #for empty elements in tables when given output
			"Name" = "",
			"post_prob" = "",
			"post_mean" = "",
			"post_var" = "",
			"lower_lim" = "",
			"upper_lim" = "")
	
		dotted.line <- list(                     #for empty tables
			"Name" = ".",
			"post_prob" = ".",
			"post_mean" = ".",
			"post_var" = ".",
			"lower_lim" = ".",
			"upper_lim" = ".")

		Bayesianlogregression[["schema"]] <- list(fields = fields)
		
		Bayesianlogregression.result <- list()
		
		lookup.table <- .regressionLogLinearBayesianBuildLookup(dataset, options$factors)
		lookup.table[["(Intercept)"]] <- "(Intercept)"
			
		if (perform == "run" && length(listOfErrors) == 0 ) {
		    if (inherits(bfObject$anthonyObj, "try-error")) {
				error <- .extractErrorMessage (bfObject$anthonyObj)
				Bayesianlogregression[["error"]] <- list(errorType= "badData",errorMessage = error)
			} else if ( class(bfObject$anthonyObj) == "bcct") {
			    logBlm.summary = summary(bfObject$anthonyObj, n.burnin=bfObject$nBurnIn, cutoff = options$posteriorProbabilityCutOff, prob.level = options$regressionCoefficientsCredibleIntervalsInterval)
				logBlm.estimates<- logBlm.summary$int_stats
		
				len.Blogreg <- length(Bayesianlogregression.result) + 1		
				term.names <- logBlm.estimates$term			
				
				if (length(bfObject$variables) > 0) {
				
					variablesInModel <- bfObject$variables
					terms<- as.character(logBlm.estimates$term)
					coef<-base::strsplit (terms, split = ":", fixed = TRUE)				
					
					for (var in seq_along(coef)) {
					
						Bayesianlogregression.result[[ len.Blogreg ]] <- emptyRow
						terms <- coef[[var]]
						actualName<-list()
					
						for (j in seq_along(terms)){
							actualName[[j]] <- paste(lookup.table[[ terms[j] ]], collapse=" = ")
						}			
						varName<-paste0(actualName, collapse="*")
							
						Bayesianlogregression.result[[ len.Blogreg ]]$"Name" <- varName
						Bayesianlogregression.result[[ len.Blogreg ]]$"post_prob" <- as.numeric(logBlm.estimates$prob[var])
						Bayesianlogregression.result[[ len.Blogreg ]]$"post_mean" <- as.numeric(logBlm.estimates$post_mean[var])
						Bayesianlogregression.result[[ len.Blogreg ]]$"post_var" <- as.numeric(logBlm.estimates$post_var[var])
						
						if (options$regressionCoefficientsCredibleIntervals == TRUE){			
							Bayesianlogregression.result[[ len.Blogreg ]]$"lower_lim" <- as.numeric(logBlm.estimates$lower[var])
							Bayesianlogregression.result[[ len.Blogreg ]]$"upper_lim" <- as.numeric(logBlm.estimates$upper[var])
						}
					
						len.Blogreg <- len.Blogreg + 1
					}		
				}			

			} else {
		
				len.Blogreg <- length(Bayesianlogregression.result) + 1
				Bayesianlogregression.result[[ len.Blogreg ]] <- dotted.line
		
				if (length(bfObject$variables) > 0) {
			
					variablesInModel <- bfObject$variables
			
					len.Blogreg <- len.Blogreg + 1
			
					for (var in 1:length(variablesInModel)) {
				
						Bayesianlogregression.result[[ len.Blogreg ]] <- dotted.line
				
						if (base::grepl(":", variablesInModel[var])) {
					
							# if interaction term					
							vars <- unlist(strsplit(variablesInModel[var], split = ":"))
							name <- paste0(vars, collapse="\u2009\u273b\u2009")
					
						} else {
					
							name <- as.character(variablesInModel[ var])
						}
				
						Bayesianlogregression.result[[ len.Blogreg ]]$"Name" <- name
						len.Blogreg <- len.Blogreg + 1
					}
				}
			}
		
		} else {
						
			len.Blogreg <- length(Bayesianlogregression.result) + 1

			if (length(bfObject$variables) > 0) {

				variablesInModel <- bfObject$variables
				
			}

			Bayesianlogregression.result[[ len.Blogreg ]] <- dotted.line
			#len.Blogreg <- length(Bayesianlogregression.result) + 1
			#Bayesianlogregression.result[[ len.Blogreg ]] <- dotted.line
			Bayesianlogregression.result[[ len.Blogreg ]]$"Model" <- 1
			if (length(listOfErrors) == 1){
			    Bayesianlogregression[["error"]] <- list(errorType = "badData", errorMessage = listOfErrors[[ 1 ]])
			}
		}
	
		
		Bayesianlogregression[["data"]] <- Bayesianlogregression.result
		results[["Bayesianlogregression"]] <- Bayesianlogregression
	}
	
################################################################################
	#						  SUB-MODEL COEFFICIENTS TABLE   						#
################################################################################		
	if (options$regressionCoefficientsSubmodel  == TRUE){
		BayesianSublogregression <- list()
		BayesianSublogregression[["title"]] <-paste( "Posterior Summary Statistics For Submodel", options$regressionCoefficientsSubmodelNo, sep=" ")
		BayesianSublogregression[["citation"]] <- logLinearBayesianCitations
		ci.label <- paste(100*options$regressionCoefficientsSubmodelCredibleIntervalsInterval, "% Credible intervals", sep="")
		# Declare table elements
		fields <- list(
			list(name = "Name", title = " ", type = "string"),
			list(name = "post_mean", title="Mean", type = "number", format = "dp:3"),
			list(name = "post_var", title = "Variance",type="number", format = "dp:3"))
		if (options$regressionCoefficientsSubmodelCredibleIntervals == TRUE){
			fields <- c(fields,list(
				list(name = "lower_lim", title = "Lower", overTitle=ci.label, type="number", format = "sf:4;dp:3"),
				list(name = "upper_lim", title = "Upper", overTitle=ci.label, type = "number", format = "sf:4;dp:3")))
		}

		emptyRow <- list(                     #for empty elements in tables when given output
			"Name" = "",
			"post_mean" = "",
			"post_var" = "",
			"lower_lim" = "",
			"upper_lim" = "")
	
		dotted.line <- list(                     #for empty tables
			"Name" = ".",
			"post_mean" = ".",
			"post_var" = ".",
			"lower_lim" = ".",
			"upper_lim" = ".")

		BayesianSublogregression[["schema"]] <- list(fields = fields)
		
		BayesianSublogregression.result <- list()
		
		lookup.table <- .regressionLogLinearBayesianBuildLookup(dataset, options$factors)
		lookup.table[["(Intercept)"]] <- "(Intercept)"
		footnotes <- .newFootnotes()
		
			
		if (perform == "run" && length(listOfErrors) == 0 && !is.null(bfObject$anthonyObj)  ) {
		    logBlm.subestimates = try(conting::sub_model(bfObject$anthonyObj, n.burnin=bfObject$nBurnIn, order=options$regressionCoefficientsSubmodelNo, 
				             prob.level = options$regressionCoefficientsSubmodelCredibleIntervalsInterval), silent = TRUE)
			
			if (inherits(logBlm.subestimates, "try-error")) {
				error <- .extractErrorMessage (logBlm.subestimates)
				BayesianSublogregression[["error"]] <- list(errorType= "badData",errorMessage = error)
		
			} else if ( class(logBlm.subestimates) == "submod"){
	
				len.Blogreg <- length(BayesianSublogregression.result) + 1		
				term.names <- logBlm.subestimates$term	
				
				extractedModelFormula <- logBlm.subestimates$formula
				
				extractedModelFormula <- as.character(extractedModelFormula)
				extractedModelFormula <- substring(extractedModelFormula, first=2)  # trim leading ~
				extractedModelFormula <- .unvf(extractedModelFormula)	
				message1 <- extractedModelFormula
				.addFootnote (footnotes, symbol = "<em>Model formula:</em>", text = message1)
													
				Post.pob <- round(logBlm.subestimates$post_prob, 3)
				.addFootnote (footnotes, symbol = "<em>Posterior model probability =</em>", text = Post.pob )	
	
				if (length(bfObject$variables) > 0) {
				
					variablesInModel <- bfObject$variables
					terms<- as.character(logBlm.subestimates$term)
					coef<-base::strsplit (terms, split = ":", fixed = TRUE)				
					
					for (var in seq_along(coef)) {
					
						BayesianSublogregression.result[[ len.Blogreg ]] <- emptyRow
						terms <- coef[[var]]
						actualName<-list()
					
						for (j in seq_along(terms)){
							actualName[[j]] <- paste(lookup.table[[ terms[j] ]], collapse=" = ")
						}			
						varName<-paste0(actualName, collapse="*")
							
						BayesianSublogregression.result[[ len.Blogreg ]]$"Name" <- varName
						BayesianSublogregression.result[[ len.Blogreg ]]$"post_mean" <- as.numeric(logBlm.subestimates$post_mean[var])
						BayesianSublogregression.result[[ len.Blogreg ]]$"post_var" <- as.numeric(logBlm.subestimates$post_var[var])
						
						if (options$regressionCoefficientsSubmodelCredibleIntervals == TRUE){			
							BayesianSublogregression.result[[ len.Blogreg ]]$"lower_lim" <- as.numeric(logBlm.subestimates$lower[var])
							BayesianSublogregression.result[[ len.Blogreg ]]$"upper_lim" <- as.numeric(logBlm.subestimates$upper[var])
						}
					
						len.Blogreg <- len.Blogreg + 1
					}		
				}			

			} else {
		
				len.Blogreg <- length(BayesianSublogregression.result) + 1
				BayesianSublogregression.result[[ len.Blogreg ]] <- dotted.line
		
				if (length(bfObject$variables) > 0) {
			
					variablesInModel <- bfObject$variables
			
					len.Blogreg <- len.Blogreg + 1
			
					for (var in 1:length(variablesInModel)) {
				
						BayesianSublogregression.result[[ len.Blogreg ]] <- dotted.line
				
						if (base::grepl(":", variablesInModel[var])) {
					
							# if interaction term					
							vars <- unlist(strsplit(variablesInModel[var], split = ":"))
							name <- paste0(vars, collapse="\u2009\u273b\u2009")
					
						} else {
					
							name <- as.character(variablesInModel[ var])
						}
				
						BayesianSublogregression.result[[ len.Blogreg ]]$"Name" <- name
						len.Blogreg <- len.Blogreg + 1
					}
				}
			}
		
		} else {		
			
			len.Blogreg <- length(BayesianSublogregression.result) + 1

			if (length(bfObject$variables) > 0) {
			    variablesInModel <- bfObject$variables
			}

			BayesianSublogregression.result[[ len.Blogreg ]] <- dotted.line
			BayesianSublogregression.result[[ len.Blogreg ]]$"Model" <- 1
			
			if (length(listOfErrors) == 1){
				BayesianSublogregression[["error"]] <- list(errorType = "badData", errorMessage = listOfErrors[[ 1 ]])
			}
		}
		BayesianSublogregression[["footnotes"]] <- as.list (footnotes)	
		BayesianSublogregression[["data"]] <- BayesianSublogregression.result
		results[["BayesianSublogregression"]] <- BayesianSublogregression
		
	}
	
########################################################################	
	if (perform == "init") {
	    if (options$counts == "" && numberOfModels < 2) {
	        endResults <- list(results=results, status="complete") #, keep=keep)
	    } else if (options$counts != "" && numberOfModels < 1) {
	        endResults <- list(results=results, status="complete") #, keep=keep)
	    } else {
	        endResults <- list(results=results, status="inited") #, state=state, keep=keep)
	    }
	} else {
	    endResults <- list(results=results, status="complete")
	}
	
	return(endResults)
}

.regressionLogLinearBayesianBuildLookup <- function(dataset, factors) {
    table <- list()

	for (factorName in factors) {
	    levels <- base::levels(dataset[[ .v(factorName) ]])
	    
	    for (i in seq_along(levels)) {
	        levelName <- levels[i]
	        base64Name <- paste(.v(factorName), i, sep="")
	        actualName <- c(factorName, levelName)
			table[[base64Name]] <- actualName
		}
	}
	return(table)
}


# levels(checkData[[.v("facGender")]])
# 
# for myOptions$factors
# .regressionLogLinearBayesianBuildLookup(checkData, myOptions$factors)
# 
