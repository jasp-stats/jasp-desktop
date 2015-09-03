
RegressionLogLinear <- function(dataset, options, perform="run", callback, ...) {
	
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
	
	#print(options$counts)
	
	#print(unlist( options$factors))
	
	#colname <-c(options$counts, unlist( options$factors))
	
	#colnames(dataset)<- colname

     print(dataset)
	results <- list()
	
	meta <- list()
	
	#meta <- list()
	
	#meta[[1]] <- list(name="title", type="title")
	#meta[[2]] <- list(name="table", type="table")
	#meta[[3]] <- list(name="regression", type="table")
	
	.meta <-  list(
		list(name = "title", type = "title"),
		list(name = "table", type = "table"),
		list(name = "logregression", type = "table"),
		list(name = "logregressionanova", type = "table")
		)
	
	results[[".meta"]] <- .meta
	
	results[["title"]] <- "Log Linear Regression"
	
	
    #######################################
	###	 	   LOGLINEAR REGRESSION		###
	#######################################
	# Fit Loglinear Model
	loglm.model <- list()
	empty.model <- list(loglm.fit = NULL, variables = NULL)
	
	 if (options$counts == ""){ 
	 	dependent.variable <- "freq"
	 }else{
	 	dependent.variable <- unlist(options$counts)
	 	}
	#dependent.variable <- unlist(options$counts)

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
			
			loglm.fit <- try( stats::glm( model.formula, family = poisson(), data = dataset), silent = TRUE)
print(loglm.fit)
				
				if ( class(loglm.fit) == "glm") {
					
					loglm.model <- list(loglm.fit = loglm.fit, variables = variables.in.model)
					
				} else {
					
					#list.of.errors[[ length(list.of.errors) + 1 ]]  <- "An unknown error occurred, please contact the author."
					loglm.model <- list(loglm.fit = NULL, variables = variables.in.model)
				}
				
			} else {
				
				loglm.model <- list(loglm.fit = NULL, variables = variables.in.model)
			}
		
		} else {		
			loglm.model <- empty.model
			}
			
		
		
	
		
		
	
	#print(length(loglm.model))
	#print(loglm.model)
	################################################################################
	#						   MODEL COEFFICIENTS TABLE   						#
	################################################################################
	
	if (options$regressionCoefficientsEstimates == TRUE) {
		
		logregression <- list()
		logregression[["title"]] <- "Coefficients"
		
		# Declare table elements
		fields <- list(
			list(name = "Model", type = "integer"),
			list(name = "Name", title = "  ", type = "string"),
			list(name = "Coefficient", title = "Estimate", type = "number", format = "dp:3"),
			list(name = "Standard Error", type="number", format = "dp:3"),
			list(name = "z-value", type="number", format = "sf:4;dp:3"),
			list(name = "z", type = "number", format = "dp:3;p:.001"))
		
		empty.line <- list( #for empty elements in tables when given output
			"Model" = "",
			"Name" = "",
			"Coefficient" = "",
			"Standard Error" = "",
			"z-value" = "",
			"z" = "")
			
		dotted.line <- list( #for empty tables
			"Model" = ".",
			"Name" = ".",
			"Coefficient" = ".",
			"Standard Error" = ".",
			"z-value" = ".",
			"z" = ".")
	
	
		logregression[["schema"]] <- list(fields = fields)
		
		logregression.result <- list()
		
		if (perform == "run" ) {		
			
			if ( class(loglm.model$loglm.fit) == "glm") {
					
				#na.estimate.names <- NULL
					
				loglm.summary = summary(loglm.model$loglm.fit)
				loglm.estimates <- loglm.summary$coefficients
print(loglm.estimates)		
				
				len.logreg <- length(logregression.result) + 1
				v <- 0
					
				#sd.dep <- sd( dataset[[ dependent.base64 ]] )
				
					
					
				if (length(loglm.model$variables) > 0) {
					
					variables.in.model <- loglm.model$variables
						
					l <- dim(loglm.estimates)[1]
 print(l)
 #print("we are so far")
					 
					for (var in 1:l) {
					
					  
						 logregression.result[[ len.logreg ]] <- empty.line
						
							name<-dimnames(loglm.estimates)[[1]]	
							#name1<-unlist(nam)
							print(str(name))
							#name <- nam[var]
								
								
							logregression.result[[ len.logreg ]]$"Name" <- name[var]
							logregression.result[[ len.logreg ]]$"Coefficient" <- as.numeric(unname(loglm.estimates[var,1]))
							logregression.result[[ len.logreg ]]$"Standard Error" <- as.numeric(loglm.estimates[var,2])			
							logregression.result[[ len.logreg ]]$"z-value" <- as.numeric(loglm.estimates[var,3])
							logregression.result[[ len.logreg ]]$"z" <- as.numeric(loglm.estimates[var,4])
							
							len.logreg <- len.logreg + 1
					}
							
				}
					
	###############					
			
			} else {
			
				len.logreg <- length(logregression.result) + 1
				logregression.result[[ len.logreg ]] <- dotted.line
				#logregression.result[[ len.logreg ]]$"Model" <- as.integer(m)
			
				if (length(loglm.model$variables) > 0) {
				
					variables.in.model <- loglm.model$variables
				
					len.logreg <- len.logreg + 1
				
					for (var in 1:length(variables.in.model)) {
					
						logregression.result[[ len.logreg ]] <- dotted.line
					
						if (base::grepl(":", variables.in.model[var])) {
						
							# if interaction term
						
							vars <- unlist(strsplit(variables.in.model[var], split = ":"))
							name <- paste0(vars, collapse="\u2009\u273b\u2009")
						
						} else {
						
							name <- as.character(variables.in.model[ var])
						}
					
						logregression.result[[ len.logreg ]]$"Name" <- name
						len.logreg <- len.logreg + 1
					}
				}
			}
			
		} else {
				
			len.logreg <- length(logregression.result) + 1

			if (length(loglm.model$variables) > 0) {
	
				variables.in.model <- loglm.model$variables
	
				for (var in 1:length(variables.in.model)) {
		
					logregression.result[[ len.logreg ]] <- dotted.line
					logregression.result[[ len.logreg ]]$"Model" <- ""
		
					if (var == 1) {
						#logregression.result[[ len.logreg ]]$"Model" <- as.integer(m)
						logregression.result[[ len.logreg ]][[".isNewGroup"]] <- TRUE
					}
		
					if (base::grepl(":", variables.in.model[var])) {
			
						# if interaction term
			
						vars <- unlist(strsplit(variables.in.model[var], split = ":"))
						name <- paste0(vars, collapse="\u2009\u273b\u2009")
			
					} else {
			
						name <- as.character(variables.in.model[ var])
					}
		
					logregression.result[[ len.logreg ]]$"Name" <- name
					len.logreg <- len.logreg + 1
				}
			}

		len.logreg <- length(logregression.result) + 1
		logregression.result[[ len.logreg ]] <- dotted.line
		logregression.result[[ len.logreg ]]$"Model" <- 1

		}

	logregression[["data"]] <- logregression.result
	results[["logregression"]] <- logregression
		
	}
	
################################################################

	if (options$method == "forward") {	
		logregressionanova <- list()
		logregressionanova[["title"]] <- "Anova"
		
		# Declare table elements
		fields <- list(
			list(name = "Model", type = "integer"),
			list(name = "Name", title = "  ", type = "string"),
			list(name = "Df", title = "Df", type = "number", format = "dp:3"),
			list(name = "Deviance", type="number", format = "dp:3"),
			list(name = "Resid. Df", type="number", format = "sf:4;dp:3"),
			list(name = "Resid. Dev", type="number", format = "sf:4;dp:3"),
			list(name = "prob Chi", type = "number", format = "dp:3;p:.001"))
		
		empty.line <- list( #for empty elements in tables when given output
			"Model" = "",
			"Name" = "",
			"Df" = "",
			"Deviance" = "",
			"Resid. Df" = "",
			"Resid. Dev" = "",
			"prob Chi"="")
			
		dotted.line <- list( #for empty tables
			"Model" = ".",
			"Name" = ".",
			"Df" = ".",
			"Deviance" = ".",
			"Resid. Df" = ".",
			"Resid. Dev" = ".",
			"prob Chi"=".")
			
	
		logregressionanova[["schema"]] <- list(fields = fields)
		
		logregressionanova.result <- list()
		
		if (perform == "run" ) {		
			
			if ( class(loglm.model$loglm.fit) == "glm") {
					
				#na.estimate.names <- NULL
					
				loglm.anova = anova(loglm.model$loglm.fit, test="Chisq")
				loglm.estimates <- loglm.anova
print(loglm.estimates)		
				
				len.logreg <- length(logregressionanova.result) + 1
				v <- 0
					
				#sd.dep <- sd( dataset[[ dependent.base64 ]] )
				
					
					
				if (length(loglm.model$variables) > 0) {
					
					variables.in.model <- loglm.model$variables
						
					l <- dim(loglm.estimates)[1]
 print(l)
 #print("we are so far")
					 
					for (var in 1:l) {
					
					  
						 logregressionanova.result[[ len.logreg ]] <- empty.line
						
							name<-dimnames(loglm.estimates)[[1]]	
							#name1<-unlist(nam)
							print(str(name))
							#name <- nam[var]
							
								
							logregressionanova.result[[ len.logreg ]]$"Model" <- as.integer(var)	
							logregressionanova.result[[ len.logreg ]]$"Name" <- as.character(name[var])
							
							logregressionanova.result[[ len.logreg ]]$"Df" <- as.integer(loglm.estimates$Df[var])
							logregressionanova.result[[ len.logreg ]]$"Deviance" <- as.numeric(loglm.estimates$Deviance[var])			
							logregressionanova.result[[ len.logreg ]]$"Resid. Df" <- as.integer(loglm.estimates$"Resid. Df"[var])
							logregressionanova.result[[ len.logreg ]]$"Resid. Dev" <- as.numeric(loglm.estimates$"Resid. Dev"[var])
							logregressionanova.result[[ len.logreg ]]$"prob Chi" <- as.numeric(loglm.estimates$"Pr(>Chi)"[var])
							
							len.logreg <- len.logreg + 1
					}
							
				}
					
	###############					
			
			} else {
			
				len.logreg <- length(logregressionanova.result) + 1
				logregressionanova.result[[ len.logreg ]] <- dotted.line
				#logregressionanova.result[[ len.logreg ]]$"Model" <- as.integer(m)
			
				if (length(loglm.model$variables) > 0) {
				
					variables.in.model <- loglm.model$variables
				
					len.logreg <- len.logreg + 1
				
					for (var in 1:length(variables.in.model)) {
					
						logregressionanova.result[[ len.logreg ]] <- dotted.line
					
						if (base::grepl(":", variables.in.model[var])) {
						
							# if interaction term
						
							vars <- unlist(strsplit(variables.in.model[var], split = ":"))
							name <- paste0(vars, collapse="\u2009\u273b\u2009")
						
						} else {
						
							name <- as.character(variables.in.model[ var])
						}
					
						logregressionanova.result[[ len.logreg ]]$"Name" <- name
						len.logreg <- len.logreg + 1
					}
				}
			}
			
		} else {
				
			len.logreg <- length(logregressionanova.result) + 1

			if (length(loglm.model$variables) > 0) {
	
				variables.in.model <- loglm.model$variables
	
				for (var in 1:length(variables.in.model)) {
		
					logregressionanova.result[[ len.logreg ]] <- dotted.line
					logregressionanova.result[[ len.logreg ]]$"Model" <- ""
		
					if (var == 1) {
						#logregressionanova.result[[ len.logreg ]]$"Model" <- as.integer(m)
						logregressionanova.result[[ len.logreg ]][[".isNewGroup"]] <- TRUE
					}
		
					if (base::grepl(":", variables.in.model[var])) {
			
						# if interaction term
			
						vars <- unlist(strsplit(variables.in.model[var], split = ":"))
						name <- paste0(vars, collapse="\u2009\u273b\u2009")
			
					} else {
			
						name <- as.character(variables.in.model[ var])
					}
		
					logregressionanova.result[[ len.logreg ]]$"Name" <- name
					len.logreg <- len.logreg + 1
				}
			}

		len.logreg <- length(logregressionanova.result) + 1
		logregressionanova.result[[ len.logreg ]] <- dotted.line
		logregressionanova.result[[ len.logreg ]]$"Model" <- 1

		}

	logregressionanova[["data"]] <- logregressionanova.result
	results[["logregressionanova"]] <- logregressionanova
	
}		
#######################################################################	
	llTable <- list()
	
	llTable[["title"]] <- "Log Linear Regression"

	fields <- list(
		list(name="x", type="string", title=""),
		list(name="y", type="number", format="sf:4;dp:3"),
		list(name="z", type="number", format="sf:4;dp:3"),
		list(name="p", type="number", format="dp:3;p:.001"))
	
	llTable[["schema"]] <- list(fields=fields)
	

	llTableData <- list()
	
	llTableData[[1]] <- list(x="Jonathon")

	if (perform == "run") {
	
		llTableData[[1]] <- list(x="Jonathon", y=123.456, z=456.789, p=0)
		
	}
	
	llTable[["data"]] <- llTableData
	
	
	results[["title"]] <- "Log Linear Regression"
	results[["table"]] <- llTable
	
	if (perform == "init") {

		list(results=results, status="inited")
		
	} else {
	
		list(results=results, status="complete")
	}
}

