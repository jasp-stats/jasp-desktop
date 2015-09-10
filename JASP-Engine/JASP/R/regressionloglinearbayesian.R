RegressionLogLinearBayesian <- function(dataset, options, perform="run", callback, ...) {
	
	#all.variables <- options$factors
	counts.var <- options$counts
	if (counts.var == "")
		counts.var <- NULL
	#	all.variables <- c(all.variables, options$counts)

	if (is.null(dataset)) {
	
		if (perform == "run") {
		
			dataset <- .readDataSetToEnd(columns.as.factor=options$factors, columns.as.numeric=counts.var)
		
		} else {
		
			dataset <- .readDataSetHeader(columns.as.factor=options$factors, columns.as.numeric=counts.var)
		}
	}

	if (options$counts == ""){ 
	 	dataset <- plyr::count(dataset)
	 }else{
	 	dataset <- dataset
	 	}
	#print(dataset) 	
	results <- list()
	
	meta <- list()
	
	.meta <-  list(
		list(name = "title", type = "title"),
		list(name = "table", type = "table"),
		list(name = "Bayesianlogregression", type = "table"),
		list(name = "Bayesianposterior", type = "table")
		
		)
	
	results[[".meta"]] <- .meta
	
	results[["title"]] <- "Bayesian Log Linear Regression"
	

    #######################################
	###	 	 BAYESIAN LOGLINEAR REGRESSION		###
	#######################################
	# Fit Loglinear Model
	logBlm.model <- list()
	empty.model <- list(logBlm.fit = NULL, variables = NULL)
	
	 if (options$counts == ""){ 
	 	dependent.variable <- "freq"
	 }else{
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

			
		if (perform == "run" && !is.null(model.definition) ) {
				
			model.formula <- as.formula(model.definition)

			if (options$counts == ""){ 
	 			names(dataset)[names(dataset)== "freq"]<- dependent.base64
	 		}
			
			logBlm.fit <- try( conting::bcct( model.formula, data = dataset, prior = options$priorType, n.sample=1000), silent = TRUE)
				
			if ( class(logBlm.fit) == "bcct") {
					
				logBlm.model <- list(logBlm.fit = logBlm.fit, variables = variables.in.model)
					
			} else {
					
				#list.of.errors[[ length(list.of.errors) + 1 ]]  <- "An unknown error occurred, please contact the author."
				logBlm.model <- list(logBlm.fit = NULL, variables = variables.in.model)
			}
				
		} else {
				
			logBlm.model <- list(logBlm.fit = NULL, variables = variables.in.model)
		}
		
	} else {
			
		logBlm.model <- empty.model
	}
			
	################################################################################
	#						   MODEL COEFFICIENTS TABLE   						#
	################################################################################		
	Bayesianlogregression <- list()
	Bayesianlogregression[["title"]] <- "Posterior Summary Statistics"
	ci.label <- paste(95, "% Highest posterior density Intervals", sep="")
	# Declare table elements
	fields <- list(
		#list(name = "Model", type = "integer"),
		list(name = "Name", title = "Model terms", type = "string"),
		list(name = "post_prob", title="P(incl|data)", type = "number", format = "dp:3"),
		list(name = "post_mean", title = "Mean",type="number", format = "dp:3"),
		list(name = "lower_lim", title = "Lower", overTitle=ci.label, type="number", format = "sf:4;dp:3"),
		list(name = "upper_lim", title = "Upper", overTitle=ci.label, type = "number", format = "sf:4;dp:3"))

	empty.line <- list( #for empty elements in tables when given output
		#"Model" = "",
		"Name" = "",
		"post_prob" = "",
		"post_mean" = "",
		"lower_lim" = "",
		"upper_lim" = "")
	
	dotted.line <- list( #for empty tables
		#"Model" = ".",
		"Name" = ".",
		"post_prob" = ".",
		"post_mean" = ".",
		"lower_lim" = ".",
		"upper_lim" = ".")

	Bayesianlogregression[["schema"]] <- list(fields = fields)
		
	Bayesianlogregression.result <- list()
		
	lookup.table <- .regressionLogLinearBayesianBuildLookup(dataset, options$factors)
	lookup.table[["(Intercept)"]] <- "(Intercept)"
		
	if (perform == "run" ) {		
		
		if ( class(logBlm.model$logBlm.fit) == "bcct") {
				
			#na.estimate.names <- NULL
				
			logBlm.summary = summary(logBlm.model$logBlm.fit, cutoff = options$posteriorProbabilityCutOff)
			logBlm.estimates<- logBlm.summary$int_stats

#print(logBlm.estimates)		
			
			len.Blogreg <- length(Bayesianlogregression.result) + 1
			v <- 0
			
			term.names <- logBlm.estimates$term
			
				
			if (length(logBlm.model$variables) > 0) {
				
				variables.in.model <- logBlm.model$variables
					
				terms<- as.character(logBlm.estimates$term)
				
				coef<-base::strsplit (terms, split = ":", fixed = TRUE)				
					
				for (var in seq_along(coef)) {
					Bayesianlogregression.result[[ len.Blogreg ]] <- empty.line
					terms <- coef[[var]]
					actualName<-list()
					for (j in seq_along(terms)){
						actualName[[j]] <- paste(lookup.table[[ terms[j] ]], collapse=" = ")
					}			
					varName<-paste0(actualName, collapse="*")
							
					Bayesianlogregression.result[[ len.Blogreg ]]$"Name" <- varName
					Bayesianlogregression.result[[ len.Blogreg ]]$"post_prob" <- as.numeric(logBlm.estimates$prob[var])
					Bayesianlogregression.result[[ len.Blogreg ]]$"post_mean" <- as.numeric(logBlm.estimates$post_mean[var])			
					Bayesianlogregression.result[[ len.Blogreg ]]$"lower_lim" <- as.numeric(logBlm.estimates$lower[var])
					Bayesianlogregression.result[[ len.Blogreg ]]$"upper_lim" <- as.numeric(logBlm.estimates$upper[var])
					
					len.Blogreg <- len.Blogreg + 1
				}
						
			}
				
###############					
		
		} else {
		
			len.Blogreg <- length(Bayesianlogregression.result) + 1
			Bayesianlogregression.result[[ len.Blogreg ]] <- dotted.line
			#logregression.result[[ len.logreg ]]$"Model" <- as.integer(m)
		
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

			for (var in 1:length(variables.in.model)) {
	
				Bayesianlogregression.result[[ len.Blogreg ]] <- dotted.line
				Bayesianlogregression.result[[ len.Blogreg ]]$"Model" <- ""
	
				if (var == 1) {
					#logregression.result[[ len.logreg ]]$"Model" <- as.integer(m)
					Bayesianlogregression.result[[ len.Blogreg ]][[".isNewGroup"]] <- TRUE
				}
	
				if (base::grepl(":", variables.in.model[var])) {
		
					# if interaction term
		
					vars <- unlist(strsplit(variables.in.model[var], split = ":"))
					name <- paste0(vars, collapse="\u2009\u273b\u2009")
		
				} else {
		
					name <- as.character(variables.in.model[ var])
				}
	
				Bayesianlogregression.result[[ len.Blogreg ]]$"Name" <- name
				Blen.logreg <- len.Blogreg + 1
			}
		}

	len.Blogreg <- length(Bayesianlogregression.result) + 1
	Bayesianlogregression.result[[ len.Blogreg ]] <- dotted.line
	Bayesianlogregression.result[[ len.Blogreg ]]$"Model" <- 1
	
	}

	Bayesianlogregression[["data"]] <- Bayesianlogregression.result
	results[["Bayesianlogregression"]] <- Bayesianlogregression
		

################################################################################
	#						 Posterior model probabilities  				#
################################################################################
		
	Bayesianposterior <- list()
	
	Bayesianposterior[["title"]] <- "Model Comparison"
		
		
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
		list(name="PM", type="number", format="dp:3", title="P(M)"),
		list(name="PMdata", type="number", format="dp:3", title="P(M|data)"),
		list(name="BF", type="number", format="sf:4;dp:3", title=bfTitle))
		
	empty.line <- list( #for empty elements in tables when given output
		"Number" = "",
		"Model" = "",
		"PM" = "",
		"PMdata" = "",
		"BF" = "")
		
	dotted.line <- list( #for empty tables
		"Number" = ".",
		"Model" = ".",
		"PM" = ".",
		"PMdata" = ".",
		"BF" = ".")

	Bayesianposterior[["schema"]] <- list(fields = fields)
	
	Bayesianposterior.result <- list()
	footnotes <- .newFootnotes()
		
	if (perform == "run" ) {		
			
		if ( class(logBlm.model$logBlm.fit) == "bcct") {
		
			logBlm.posterior <- conting::mod_probs(logBlm.fit,scale=0.001,best = options$maxModels)
				
			len.Blogreg <- length(Bayesianposterior.result) + 1
			v <- 0
				
			if (length(logBlm.model$variables) > 0) {
					
				variables.in.model <- logBlm.model$variables
						
				max.prob <- base::max(logBlm.posterior$table$prob)
				BFactor <- logBlm.posterior$table$prob / max.prob
 
				if (options$bayesFactorType == "BF10") {
	
					BFactor <- .clean(BFactor)
	
				} else if (options$bayesFactorType == "BF01") {
	
					BFactor <- .clean(1/BFactor)
		
				} else {
	
					BFactor <- .clean(log(BFactor))
				}
				
				#nfactors.in.model <- length(variables.in.model)
				#n.factors<-length(options$factors)
				#term.in.model <- nfactors.in.model - n.factors +1
				#prior.model.prob <- 1 / term.in.model
	 
				model.names <- logBlm.posterior$table$model_formula
				totalmodels <- options$maxModels
				t.mods.visit <- logBlm.posterior$totmodsvisit
				
				footnote <- paste ("Total number of models visited =", t.mods.visit, sep=" ")
				.addFootnote (footnotes, symbol = "<em>Note.</em>", text = footnote)									
				
				
				if(totalmodels > t.mods.visit){
					totalmodels <- t.mods.visit
				} else {
					totalmodels <- totalmodels
				}
				
				for (i in 1:totalmodels) {
								  
					Bayesianposterior.result[[ len.Blogreg ]] <- empty.line
					
					model.name <- as.character(model.names[[i]])
					model.name <- substring(model.name, 2)  # trim leading ~
					model.name <- .unvf(model.name)						
					
					Bayesianposterior.result[[ len.Blogreg ]]$"Number" <-as.integer(i)
					Bayesianposterior.result[[ len.Blogreg ]]$"model" <- model.name
					Bayesianposterior.result[[ len.Blogreg ]]$"PM" <- as.numeric(1)
					Bayesianposterior.result[[ len.Blogreg ]]$"PMdata" <- as.numeric(logBlm.posterior$table$prob[i])			
					Bayesianposterior.result[[ len.Blogreg ]]$"BF" <- as.numeric(BFactor[i])
					Bayesianposterior.result[[ len.Blogreg ]]$ "footnotes" <- as.list (footnotes)					
					len.Blogreg <- len.Blogreg + 1
				}

			#Bayesianposterior.result[[ len.Blogreg ]]$ "footnotes" <- as.list (footnotes)
			}
					
	###############					
			
		} else {
			
			len.Blogreg <- length(Bayesianposterior.result) + 1
			Bayesianposterior.result[[ len.Blogreg ]] <- dotted.line
			#posterior.result[[ len.logreg ]]$"Model" <- as.integer(m)
			
			if (length(logBlm.model$variables) > 0) {
				
				variables.in.model <- logBlm.model$variables
			
				len.Blogreg <- len.Blogreg + 1
				
				for (var in 1:length(variables.in.model)) {
				
					Bayesianposterior.result[[ len.Blogreg ]] <- dotted.line
				
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
	
			for (var in 1:length(variables.in.model)) {
		
				Bayesianposterior.result[[ len.Blogreg ]] <- dotted.line
				Bayesianposterior.result[[ len.Blogreg ]]$"Model" <- ""
		
				if (var == 1) {
					#posterior.result[[ len.logreg ]]$"Model" <- as.integer(m)
					Bayesianposterior.result[[ len.Blogreg ]][[".isNewGroup"]] <- TRUE
				}
		
				if (base::grepl(":", variables.in.model[var])) {
			
					vars <- unlist(strsplit(variables.in.model[var], split = ":"))
					name <- paste0(vars, collapse="\u2009\u273b\u2009")
			
				} else {
			
					name <- as.character(variables.in.model[ var])
				}
		
				Bayesianposterior.result[[ len.Blogreg ]]$"Name" <- name
				Blen.logreg <- len.Blogreg + 1
			}
		}

		len.Blogreg <- length(Bayesianposterior.result) + 1
		Bayesianposterior.result[[ len.Blogreg ]] <- dotted.line
		Bayesianposterior.result[[ len.Blogreg ]]$"Model" <- 1

	}
	
    Bayesianposterior[["footnotes"]] <- as.list (footnotes)
	Bayesianposterior[["data"]] <- Bayesianposterior.result
	results[["Bayesianposterior"]] <- Bayesianposterior
		
################################################################

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
			actual       <- c(v, l)
			table[[mangled.name]] <- actual
		}
	}
	
	table
}

