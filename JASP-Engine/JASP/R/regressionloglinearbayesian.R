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

RegressionLogLinearBayesian <- function(dataset, options, perform="run", callback, ...) {
    counts.var <- options$counts
	if (counts.var == "") {
	    counts.var <- NULL
	}
		
	if (is.null(dataset)) {
	    if (perform == "run") {
	        dataset <- .readDataSetToEnd(columns.as.factor=options$factors, columns.as.numeric=counts.var)
	    } else {
	        dataset <- .readDataSetHeader(columns.as.factor=options$factors, columns.as.numeric=counts.var)
		}
	}	 
	 
	listOfErrors <- list()
	error.message <- NULL
		
	if (options$counts != "" && perform == "run") {
	    # Start tallying
		variableNames <- NULL
		
		for (counts in options$counts) {
			if (any(is.na(dataset [[.v (options$counts)]]))){
			    variableNames <- c (variableNames, options$counts)
			}
		}
	
		if (!is.null(variableNames)) {
		    error.message <- paste0("Bayes factor is undefined -- incomplete contingency table, the count variable ", variableNames, " contain(s) empty cell and/or NaN. ")
		    listOfErrors[[ length(listOfErrors) + 1 ]] <- error.message
		}
			
	
		if (length(listOfErrors)==0) {
			variableNames <- NULL
			for (counts in options$counts) {
				if (any (!is.finite (dataset [[.v (options$counts)]])) || any  (dataset [[.v (options$counts)]] < 0 )) {
				    variableNames <- c (variableNames, options$counts)
				}
			}
	
			if ( !is.null (variableNames)) {
			    error.message <- paste ("Bayes factor is undefined -- the count variable ", variableNames, " contain(s) infinity and/or negative numbers.", sep = "")
			    listOfErrors[[ length(listOfErrors) + 1 ]] <- error.message
			}
		}
	}
	
	if (options$counts == "") {
	    dataset <- plyr::count(dataset)
	} else {
	 	dataset <- dataset
	}
	
	# Data check here
	if ( perform == "run" && length(listOfErrors)==0  ) { 
	    if (isTRUE(anyNA(dataset[.v(options$factors)]))) {
	        error.message <- "Bayes factor is undefined -- the factors contain(s) empty cell and/or NaN or incomplete contingency table."
	        listOfErrors[[ length(listOfErrors) + 1 ]] <- error.message
	    }
	    # 
	#     
	#     # Tally up variables names
	# 	naVariables <- NULL #variableNames <- NULL
	# 	
	# 	
	# 	for (factor in options$factors) {
	# 		if ( any(is.na(dataset[.v(factor)])) ){
	# 		    naVariables <- c (naVariables, factor)
	# 		    error.message <- paste0("The factor ", factor, " contains an empty entry")
	# 		    listOfErrors[[ length(listOfErrors) + 1 ]] <- error.message
	# 		}
	# 	}
	# 	
	# 	
	# 	if ( !is.null(variableNames) ) {
	# 	    error.message <- "Bayes factor is undefined -- the factors contain(s) empty cell and/or NaN or incomplete contingency table."
	# 	    listOfErrors[[ length(listOfErrors) + 1 ]] <- error.message
	# 	}
	}
	
	results <- list()
	meta <- list()
	.meta <-  list(list(name = "title", type = "title"),
	               list(name = "table", type = "table"),
	               list(name = "Bayesianposterior", type = "table"),
	               list(name = "Bayesianlogregression", type = "table"),
	               list(name = "BayesianSublogregression", type = "table")
	)
	
		
	results[[".meta"]] <- .meta
	results[["title"]] <- "Bayesian Log-Linear Regression"
	
	logLinearBayesianCitations <- 	list(
		"Overstall, A., & King, R. (2014). conting: an R package for Bayesian analysis of complete and incomplete contingency tables. Journal of Statistical Software, 58(7), 1-27."
	)


    #######################################
	###	 	 BAYESIAN LOGLINEAR REGRESSION		###
	#######################################
	# Fit Loglinear Model
	#footnotes <- .newFootnotes()
	
	logBlm.model <- list()
	logBlm.fit <- NULL
	emptyModel <- list(logBlm.fit, variables = NULL)
	
	 if (options$counts == ""){ 
	 	dependent.variable <- "freq"
	 } else {
	 	dependent.variable <- unlist(options$counts)
	 }
	
	if (length(options$modelTerms) > 0) {
	    variables.in.model <- NULL
		variables.in.model.base64 <- NULL
		
		for (i in seq_along(options$modelTerms)) {
		    components <- options$modelTerms[[i]]$components
		    
			if (length(components) == 1) {
			    variables.in.model <- c(variables.in.model, components[[1]])
				variables.in.model.base64 <- c(variables.in.model.base64, .v(components[[1]]))
			} else {
			    components.unlisted <- unlist(components)
				term.base64 <- paste0(.v(components.unlisted), collapse=":")
				term <- paste0(components.unlisted, collapse=":")
				variables.in.model <- c(variables.in.model, term)
				variables.in.model.base64 <- c(variables.in.model.base64, term.base64)
			}
		}
		
		independent.base64 <- variables.in.model.base64
		# Remove empty stuff
		variables.in.model <- variables.in.model[ variables.in.model != ""]
		variables.in.model.copy <- variables.in.model
	}
		
	dependent.base64 <- .v(dependent.variable)
		
	if (length(options$modelTerms) > 0) {
	    if (length(variables.in.model) > 0 ) {
	        model.definition <- paste(dependent.base64, "~", paste(independent.base64, collapse = "+"))
		} else {
		    model.definition <- NULL #this model has no parameters				
		}
	    
			
		if (perform == "run"  && !is.null(model.definition) && length(listOfErrors) == 0 ) {
		    model.formula <- as.formula(model.definition)
		    
		    if (options$counts == ""){ 
	 			names(dataset)[names(dataset)== "freq"] <- dependent.base64
	 		}	 		
		    
		    logBlm.fit <- try( conting::bcct(formula=model.formula, data = dataset, prior = "SBH", n.sample=2000, a=options$priorShape, b=options$priorScale), silent = TRUE)
		    no.burnin = 2000 * 0.2
		    
		    # TODO: check the maximal number of models visited, if it's one, then sample more
		    # dim(logBlm.fit$maximal.mod$x)[2] <- gives the max number of model visited
		    
		    # Always do auto and then manual adds additional samples
		    if (options$sampleMode == "manual"){
			    logBlm.fit <- try(conting::bcctu(object = logBlm.fit, n.sample = options$fixedSamplesNumber), silent = TRUE)
			    no.burnin = (2000 + options$fixedSamplesNumber)* 0.2
			} 
			
		    if (isTryError(logBlm.fit)) {
		        error <- .extractErrorMessage (logBlm.fit)
		        listOfErrors[[ length(listOfErrors) + 1 ]]  <- error
		        logBlm.model <- list(logBlm.fit = NULL, variables = variables.in.model)
		    } else if (class(logBlm.fit) == "bcct") {
			    logBlm.model <- list(logBlm.fit = logBlm.fit, variables = variables.in.model)
			} 
		} else {
		    logBlm.model <- list(logBlm.fit = NULL, variables = variables.in.model)
		}
	} else {
		logBlm.model <- emptyModel
	}
			
	#########################################################################
	#						 Posterior model probabilities  				#
	#########################################################################
		
	Bayesianposterior <- list()
	
	Bayesianposterior[["title"]] <- "Model Comparison"
	Bayesianposterior[["citation"]] <- logLinearBayesianCitations
		
	if (options$bayesFactorType == "BF10") {
		bfTitle <- "BF<sub>10</sub>"
	} else if (options$bayesFactorType == "BF01") {
		bfTitle <- "BF<sub>01</sub>"
	} else {
		bfTitle <- "Log(BF<sub>10</sub>)"
	}
	
	# Declare table elements
	fields <- list(
		list(name = "Number", type = "integer",title=" "),
		list(name="model", type="string", title="Models"),
		list(name="PMdata", type="number", format="dp:3", title="P(M|data)"),
		list(name="BF", type="number", format="sf:4;dp:3", title=bfTitle)
	)
		
	emptyRow <- list( #for empty elements in tables when given output
		"Number" = "",
		"Model" = "",
		"PMdata" = "",
		"BF" = ""
	)
		
	dotted.line <- list( #for empty tables
		"Number" = ".",
		"Model" = ".",
		"PMdata" = ".",
		"BF" = "."
	)

	Bayesianposterior[["schema"]] <- list(fields = fields)
	
	Bayesianposterior.result <- list()
	footnotes <- .newFootnotes()
	
	if (perform == "run" && length(listOfErrors) == 0 ) {		
	    if ( class(logBlm.model$logBlm.fit) == "bcct") {
	        # TODO: Here check logBlm.posterior$totmodsvisit if this is one, then nothing going on, resample
	        logBlm.posterior <- conting::mod_probs(logBlm.fit, scale=0, best = options$maxModels)
				
			len.Blogreg <- length(Bayesianposterior.result) + 1
			v <- 0
				
			if (length(logBlm.model$variables) > 0) {
			    variables.in.model <- logBlm.model$variables
			    
			    max.prob <- base::max(logBlm.posterior$table$prob.Freq)
			    
			    # TODO: if numeric(0), then... 
			    # identical(0, numeric(0)), then resample using bcctu or something
			    # logBlm.fit <- try(conting::bcctu(object = logBlm.fit, n.sample = 1000), silent = TRUE)
			    # n.sample = 1000
			    # no.burnin = (2000 + 1000)* 0.2
				BFactor <- logBlm.posterior$table$prob.Freq / max.prob
				
				
				if (options$bayesFactorType == "BF10") {
				    BFactor <- .clean(BFactor)
				} else if (options$bayesFactorType == "BF01") {
				    BFactor <- .clean(1/BFactor)
				} else if (options$bayesFactorType == "LogBF10") {
				    BFactor <- .clean(log(BFactor))
				}
	 
				model.names <- logBlm.posterior$table$model_formula
				totalmodels <- options$maxModels
				t.mods.visit <- logBlm.posterior$totmodsvisit
				
				message <- paste ("Total number of models visited =", t.mods.visit, sep=" ")
				.addFootnote (footnotes, symbol = "<em>Note.</em>", text = message)									
				
				
				if(totalmodels > t.mods.visit){
					totalmodels <- t.mods.visit
				} else {
					totalmodels <- totalmodels
				}
				
				for (i in 1:totalmodels) {
				    Bayesianposterior.result[[ len.Blogreg ]] <- emptyRow
					
					model.name <- as.character(model.names[[i]])
					model.name <- substring(model.name, 2)  # trim leading ~
					model.name <- .unvf(model.name)						
					
					Bayesianposterior.result[[ len.Blogreg ]]$"Number" <-as.integer(i)
					Bayesianposterior.result[[ len.Blogreg ]]$"model" <- model.name
					Bayesianposterior.result[[ len.Blogreg ]]$"PMdata" <- as.numeric(logBlm.posterior$table$prob.Freq[i])			
					Bayesianposterior.result[[ len.Blogreg ]]$"BF" <- as.numeric(BFactor[i])
					Bayesianposterior.result[[ len.Blogreg ]]$ "footnotes" <- as.list (footnotes)					
					
					len.Blogreg <- len.Blogreg + 1
				}
			}				
			
		} else {
			
			len.Blogreg <- length(Bayesianposterior.result) + 1
			Bayesianposterior.result[[ len.Blogreg ]] <- dotted.line
			
			if (length(logBlm.model$variables) > 0) {
				
				variables.in.model <- logBlm.model$variables
			
				len.Blogreg <- len.Blogreg + 1
				
				for (var in 1:length(variables.in.model)) {
				
					#Bayesianposterior.result[[ len.Blogreg ]] <- dotted.line
				
					if (base::grepl(":", variables.in.model[var])) {
					
						# if interaction term					
						vars <- unlist(strsplit(variables.in.model[var], split = ":"))
						name <- paste0(vars, collapse="\u2009\u273b\u2009")
					
					} else {
					
						name <- as.character(variables.in.model[ var])
					}
				
					Bayesianposterior.result[[ len.Blogreg ]]$"Name" <- name
					len.Blogreg <- len.Blogreg + 1
				}
			}
		}
			
	} else {
				
		len.Blogreg <- length(Bayesianposterior.result) + 1

		if (length(logBlm.model$variables) > 0) {
	
			variables.in.model <- logBlm.model$variables
			
		}

		len.Blogreg <- length(Bayesianposterior.result) + 1
		Bayesianposterior.result[[ len.Blogreg ]] <- dotted.line
		Bayesianposterior.result[[ len.Blogreg ]]$"Model" <- 1
	}
	
	if (length(listOfErrors) > 1){
	    logBlm.fit <- try( conting::bcct( model.formula, data = dataset,  prior = "SBH", n.sample=1000), silent = TRUE)

		if (inherits(logBlm.fit, "try-error")) {
			error <- .extractErrorMessage (logBlm.fit)
		}
		Bayesianposterior[["error"]] <- list(errorType="badData",errorMessage = error)
	} else if (length(listOfErrors) == 1){
		Bayesianposterior[["error"]] <- list(errorType = "badData", errorMessage = listOfErrors[[ 1 ]])
	}
		
    Bayesianposterior[["footnotes"]] <- as.list (footnotes)
	Bayesianposterior[["data"]] <- Bayesianposterior.result
	results[["Bayesianposterior"]] <- Bayesianposterior


	################################################################################
	#						   MODEL COEFFICIENTS TABLE   						#
	################################################################################		
	if (options$regressionCoefficientsEstimates  == TRUE){
		Bayesianlogregression <- list()
		Bayesianlogregression[["title"]] <- "Posterior Summary Statistics"
		Bayesianlogregression[["citation"]] <- logLinearBayesianCitations
		#ci.label <- paste(100*options$regressionCoefficientsCredibleIntervalsInterval, "% Highest posterior density intervals", sep="")
		ci.label <- paste(100*options$regressionCoefficientsCredibleIntervalsInterval, "% Credible Intervals", sep="")
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
		    if (inherits(logBlm.fit, "try-error")) {
				error <- .extractErrorMessage (logBlm.fit)
				Bayesianlogregression[["error"]] <- list(errorType= "badData",errorMessage = error)
			} else if ( class(logBlm.model$logBlm.fit) == "bcct") {
			    logBlm.summary = summary(logBlm.fit, n.burnin=no.burnin, cutoff = options$posteriorProbabilityCutOff, prob.level = options$regressionCoefficientsCredibleIntervalsInterval)
				logBlm.estimates<- logBlm.summary$int_stats
		
				len.Blogreg <- length(Bayesianlogregression.result) + 1		
				term.names <- logBlm.estimates$term			
				
				if (length(logBlm.model$variables) > 0) {
				
					variables.in.model <- logBlm.model$variables
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
		
				if (length(logBlm.model$variables) > 0) {
			
					variables.in.model <- logBlm.model$variables
			
					len.Blogreg <- len.Blogreg + 1
			
					for (var in 1:length(variables.in.model)) {
				
						Bayesianlogregression.result[[ len.Blogreg ]] <- dotted.line
				
						if (base::grepl(":", variables.in.model[var])) {
					
							# if interaction term					
							vars <- unlist(strsplit(variables.in.model[var], split = ":"))
							name <- paste0(vars, collapse="\u2009\u273b\u2009")
					
						} else {
					
							name <- as.character(variables.in.model[ var])
						}
				
						Bayesianlogregression.result[[ len.Blogreg ]]$"Name" <- name
						len.Blogreg <- len.Blogreg + 1
					}
				}
			}
		
		} else {
						
			len.Blogreg <- length(Bayesianlogregression.result) + 1

			if (length(logBlm.model$variables) > 0) {

				variables.in.model <- logBlm.model$variables
				
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
		ci.label <- paste(100*options$regressionCoefficientsSubmodelCredibleIntervalsInterval, "% Credible Intervals", sep="")
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
		
			
		if (perform == "run" && length(listOfErrors) == 0 && !is.null(logBlm.fit)  ) {
		    logBlm.subestimates = try(conting::sub_model(logBlm.fit, n.burnin=no.burnin, order=options$regressionCoefficientsSubmodelNo, 
				             prob.level = options$regressionCoefficientsSubmodelCredibleIntervalsInterval), silent = TRUE)
			
			if (inherits(logBlm.subestimates, "try-error")) {
				error <- .extractErrorMessage (logBlm.subestimates)
				BayesianSublogregression[["error"]] <- list(errorType= "badData",errorMessage = error)
		
			} else if ( class(logBlm.subestimates) == "submod"){
	
				len.Blogreg <- length(BayesianSublogregression.result) + 1		
				term.names <- logBlm.subestimates$term	
				
				Model.formula <- logBlm.subestimates$formula
				
				Model.formula <- as.character(Model.formula)
				Model.formula <- substring(Model.formula, 2)  # trim leading ~
				Model.formula <- .unvf(Model.formula)	
				message1 <- Model.formula
				.addFootnote (footnotes, symbol = "<em>Model formula:</em>", text = message1)
													
				Post.pob <- round(logBlm.subestimates$post_prob, 3)
				.addFootnote (footnotes, symbol = "<em>Posterior model probability =</em>", text = Post.pob )	
	
				if (length(logBlm.model$variables) > 0) {
				
					variables.in.model <- logBlm.model$variables
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
					
						BayesianSublogregression.result[[ len.Blogreg ]]$ "footnotes" <- as.list (footnotes)
					
						len.Blogreg <- len.Blogreg + 1
					}		
				}			

			} else {
		
				len.Blogreg <- length(BayesianSublogregression.result) + 1
				BayesianSublogregression.result[[ len.Blogreg ]] <- dotted.line
		
				if (length(logBlm.model$variables) > 0) {
			
					variables.in.model <- logBlm.model$variables
			
					len.Blogreg <- len.Blogreg + 1
			
					for (var in 1:length(variables.in.model)) {
				
						BayesianSublogregression.result[[ len.Blogreg ]] <- dotted.line
				
						if (base::grepl(":", variables.in.model[var])) {
					
							# if interaction term					
							vars <- unlist(strsplit(variables.in.model[var], split = ":"))
							name <- paste0(vars, collapse="\u2009\u273b\u2009")
					
						} else {
					
							name <- as.character(variables.in.model[ var])
						}
				
						BayesianSublogregression.result[[ len.Blogreg ]]$"Name" <- name
						len.Blogreg <- len.Blogreg + 1
					}
				}
			}
		
		} else {		
			
			len.Blogreg <- length(BayesianSublogregression.result) + 1

			if (length(logBlm.model$variables) > 0) {
			    variables.in.model <- logBlm.model$variables
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

		list(results=results, status="inited")
		
	} else {
	
		list(results=results, status="complete")
	}
}

.regressionLogLinearBayesianBuildLookup <- function(dataset, factors) {

	table <- list()

	for (v in factors) {
	
		levels <- base::levels(dataset[[ .v(v) ]])

		for (i in seq_along(levels)) {
		
			l <- levels[i]
			mangled.name <- paste(.v(v), i, sep="")
			actual <- c(v, l)
			table[[mangled.name]] <- actual
		}
	}
	
	table
}

