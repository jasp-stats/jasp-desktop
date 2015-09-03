

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
	print(dataset) 	
	results <- list()
	
	meta <- list()
	
	.meta <-  list(
		list(name = "title", type = "title"),
		list(name = "table", type = "table"),
		list(name = "Bayesianlogregression", type = "table")
		#list(name = "logregressionanova", type = "table")
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
	
	#if (dependent.variable == options$counts) {
	
		
		
		dependent.base64 <- .v(dependent.variable)
	
		print (dependent.base64)
		
	 if (length(options$modelTerms) > 0) {
			
		if (length(variables.in.model) > 0 ) {
		
			#dependent.base64 <- .unv(dependent.base64)
			#independent.base64 <- .unv(independent.base64)
			
			
			
			model.definition <- paste(dependent.base64, "~", paste(independent.base64, collapse = "+"))
			

					
		}  else {
				
			model.definition <- NULL #this model has no parameters
				
		}
			
print(model.definition)
			
			
		if (perform == "run" && !is.null(model.definition) ) {
				
			model.formula <- as.formula(model.definition)
print(model.formula )
#print(head(dataset))
			
			if (options$counts == ""){ 
	 	names(dataset)[names(dataset)== "freq"]<- dependent.base64
	 }
			
			logBlm.fit <- try( conting::bcct( model.formula, data = dataset, n.sample=1000), silent = TRUE)
#print(logBlm.fit)
				
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
			
		
	
	#print(length(loglm.model))
	#print(loglm.model)
	################################################################################
	#						   MODEL COEFFICIENTS TABLE   						#
	################################################################################
	
	#if (options$regressionCoefficientsEstimates == TRUE) {
		
		Bayesianlogregression <- list()
		Bayesianlogregression[["title"]] <- "Coefficients"
		
		# Declare table elements
		fields <- list(
			list(name = "Model", type = "integer"),
			list(name = "Name", title = "  ", type = "string"),
			list(name = "post_prob", title = "Posterior probability", type = "number", format = "dp:3"),
			list(name = "post_mean", title = "Posterior mean", type="number", format = "dp:3"),
			list(name = "lower_lim", type="number", format = "sf:4;dp:3"),
			list(name = "upper_lim", type = "number", format = "dp:3;p:.001"))
		
		empty.line <- list( #for empty elements in tables when given output
			"Model" = "",
			"Name" = "",
			"post_prob" = "",
			"post_mean" = "",
			"lower_lim" = "",
			"upper_lim" = "")
			
		dotted.line <- list( #for empty tables
			"Model" = ".",
			"Name" = ".",
			"post_prob" = ".",
			"post_mean" = ".",
			"lower_lim" = ".",
			"upper_lim" = ".")
	
print("we are so far")	
		Bayesianlogregression[["schema"]] <- list(fields = fields)
		
		Bayesianlogregression.result <- list()
		
		if (perform == "run" ) {		
			
			if ( class(logBlm.model$logBlm.fit) == "bcct") {
					
				#na.estimate.names <- NULL
					
				logBlm.summary = summary(logBlm.model$logBlm.fit)
				logBlm.estimates <- logBlm.summary$int_stats

print(logBlm.estimates)		
				
				len.Blogreg <- length(Bayesianlogregression.result) + 1
				v <- 0
					
				#sd.dep <- sd( dataset[[ dependent.base64 ]] )
				
					
					
				if (length(logBlm.model$variables) > 0) {
					
					variables.in.model <- logBlm.model$variables
						
					l <- length(logBlm.estimates$term)
 print(l)
 #print("we are so far")
					 
					for (var in 1:l) {
					
					  
						 Bayesianlogregression.result[[ len.Blogreg ]] <- empty.line
						
							name<-logBlm.estimates$term	
							#name1<-unlist(nam)
							print(str(name))
							#name <- nam[var]
								
								
							Bayesianlogregression.result[[ len.Blogreg ]]$"Name" <- name[var]
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
		
	#}
	
################################################################



	llTable <- list()
	
	llTable[["title"]] <- "Bayesian Log Linear Regression"

	fields <- list()
	
	fields[[length(fields)+1]] <- list(name="model", type="string", title="Models")
	fields[[length(fields)+1]] <- list(name="PM", type="number", format="dp:3", title="P(M)")
	fields[[length(fields)+1]] <- list(name="PMdata", type="number", format="dp:3", title="P(M|data)")
	
	if (options$bayesFactorType == "BF10") {
		bfTitle <- "BF<sub>10</sub>"
	} else if (options$bayesFactorType == "BF01") {
		bfTitle <- "BF<sub>01</sub>"
	} else {
		bfTitle <- "Log(BF<sub>10</sub>)"
	}
	
	fields[[length(fields)+1]] <- list(name="BF", type="number", format="sf:4;dp:3;log10", title=bfTitle)
	
	llTable[["schema"]] <- list(fields=fields)
	
	llTableData <- list()
	
	llTableData[[1]] <- list(model="Gregory") 
	llTableData[[2]] <- list(model="Bronson")
	llTableData[[3]] <- list(model="Lachlan")

	if (perform == "run") {
	
		llTableData[[1]] <- list(model="Gregory", PM=1, PMdata=1, BF=1) 
		llTableData[[2]] <- list(model="Bronson", PM=1, PMdata=1, BF=2)
		llTableData[[3]] <- list(model="Lachlan", PM=1, PMdata=1, BF=3)
	}

	llTable[["data"]] <- llTableData

	

	results[["table"]] <- llTable
	
	if (perform == "init") {

		list(results=results, status="inited")
		
	} else {
	
		list(results=results, status="complete")
	}
}

