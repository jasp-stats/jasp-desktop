RegressionLinear <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {
	#######################################
	###	   VARIABLE DECLARATION	   ##
	#######################################
	dependent.variable <- unlist(options$dependent)
	wls.weight <- unlist(options$wlsWeight)
	independent.variables <- NULL
	number.of.blocks <- length(options$blocks)
	if (number.of.blocks > 0){
		for (b in 1:number.of.blocks) {
			independent.variables <- c(independent.variables, unlist(options$block[[ b ]][["variables"]]))
		}
	}
	
	to.be.read.variables <- c(dependent.variable, independent.variables, wls.weight)
	to.be.read.variables <- to.be.read.variables[ to.be.read.variables != ""]
	
	#######################################
	###			FETCH DATA			##
	#######################################
	
	if (is.null(dataset)) {
		if (perform == "run") {
			dataset <- .readDataSetToEnd(columns.as.numeric = to.be.read.variables, exclude.na.listwise=to.be.read.variables)
		} else {
			dataset <- .readDataSetHeader(columns.as.numeric = to.be.read.variables)
		}
	}
	
	#######################################
	###		    DATA QUALITY CHECK  	 ##
	#######################################
	list.of.errors <- list()
	
	
	if (perform == "run" && dependent.variable != ""){
		#check weights
		if (options$wlsWeight != "") {
			weight.base64 <- .v(wls.weight)
			min.weight <- min(dataset[[ weight.base64 ]] )
			if (is.finite(min.weight)) {
				if (min.weight <= 0) {
					list.of.errors[[ length(list.of.errors) + 1 ]] <- "Least squares regression model undefined -- there are nonpositive weights encountered"
				}
			} else {
				list.of.errors[[ length(list.of.errors) + 1 ]] <- "Least squares regression model undefined -- the weights contain infinities"
			}
		}
		
		#check for data
		number.of.rows <- length( dataset[[ .v(dependent.variable) ]])
		number.of.columns <- 1 + length(independent.variables)
		x <- matrix(0, number.of.rows, number.of.columns)
		x[,1] <- as.numeric( dataset[[ .v(dependent.variable) ]])
		if (number.of.columns > 1){
			for (i in 1:(number.of.columns - 1)) {
				x[,i+1] <- as.numeric( dataset[[ .v(independent.variables[ i ]) ]])
			}
		}
		
		#check on number of valid observations.
		n.valid.cases <- nrow(x)
		if (n.valid.cases == 0) {
			list.of.errors[[ length(list.of.errors) + 1 ]] <- "Least squares regression model is undefined -- there are no observations for the dependent variable (possibly only after rows with missing values are excluded)"
		}
		
		#check for variance in variables.
		number.of.values.vars <- sapply(1:ncol(x),function(i){length(unique(x[,i]))})
		indicator <- which(number.of.values.vars <= 1)
		if (length(indicator) != 0 ){
			if (number.of.values.vars[1] <= 1){
				list.of.errors[[ length(list.of.errors) + 1 ]] <- "Least squares regression model is undefined -- the dependent variable contains all the same value (the variance is zero)"
			} else {
				if (length(indicator) == 1){
					list.of.errors[[ length(list.of.errors) + 1 ]] <- paste("Least squares regression model is undefined -- the independent variable(s)", independent.variables[indicator-1] ," contain(s) all the same value (the variance is zero)",sep="")
				} else {
					var.names <- paste(independent.variables[indicator-1], collapse = ", ",sep="")
					list.of.errors[[ length(list.of.errors) + 1 ]] <- paste("Least squares regression model is undefined -- the independent variable(s)", var.names, " contain(s) all the same value (their variance is zero)", sep="")
				}
			}
		}
		
		#check for Inf's in dataset.
		column.sums <- colSums(x,na.rm=TRUE)
		indicator <- which(is.infinite(column.sums))
		if (length(indicator) > 0 ){
			if ( 1%in%indicator){
				list.of.errors[[ length(list.of.errors) + 1 ]]  <- "Least squares regression model is undefined -- the dependent variable contains infinity"
			} else {
				var.names <- paste(independent.variables[indicator-1], collapse = ", ",sep="")
				list.of.errors[[ length(list.of.errors) + 1 ]]  <- paste("Least squares regression model undefined -- the independent variable(s) ",var.names, " contain(s) infinity",sep="")
			}
		}
	}
	
	#######################################
	###			  META			   ###
	#######################################
	
	results <- list()
	
	.meta <-  list(
		list(name = "title", type = "title"),
		list(name = "descriptives", type = "table"),
		list(name = "correlations", type = "table"),
		list(name = "model summary", type = "table"),
		list(name = "anova", type = "table"),
		list(name = "regression", type = "table"),
		list(name = "coefficient correlations", type = "table"))
	
	results[[".meta"]] <- .meta
	
	results[["title"]] <- "Linear Regression"
	
	#######################################
	###	 	   LINEAR REGRESSION		###
	#######################################
	
	# Fit Linear Model
	lm.model <- list()
	empty.model <- list(lm.fit = NULL, variables = NULL)
	
	if (dependent.variable != "") {
		dependent.base64 <- .v(dependent.variable)
		
		if (wls.weight != "" ) {
			weight.base64 <- .v(wls.weight)
			weights <- dataset[[ weight.base64 ]]
		} else {
			weights <- rep(1,length(dataset[[ dependent.base64 ]] ))
		}
		
		if (number.of.blocks > 0)
		{
			variables.in.model <- NULL
			
			for (b in 1:number.of.blocks)
			{
				block.options <- options$blocks[[ b ]]
				
				if (block.options$method == "Enter") {
					if (b == 1) {
						variables.in.model <- unlist(block.options[["variables"]] )
						variables.in.model <- variables.in.model[ variables.in.model != ""]
					} else {
						if (length(lm.model[[ b - 1 ]]$variables) > 0) {
							variables.in.model <- lm.model[[ b - 1 ]]$variables
							variables.in.model <- c(variables.in.model, unlist(block.options[["variables"]] ))
							variables.in.model <- variables.in.model[ variables.in.model != ""]
						} else {
							variables.in.model <- unlist(block.options[["variables"]] )
							variables.in.model <- variables.in.model[ variables.in.model != ""]
						}
					}
					
					if (length(variables.in.model) > 0 ) {
						independent.base64 <- .v(variables.in.model)
						if (options$includeConstant == TRUE) {
							model.definition <- paste(dependent.base64, "~", paste(independent.base64, collapse = "+"))
						} else {
							model.definition <- paste(dependent.base64, "~", paste(independent.base64, collapse = "+"), "-1")
						}
						
					} else {
						
						if (options$includeConstant == TRUE)
						{
							model.definition <- paste(dependent.base64, "~ 1")
						} else {
							model.definition <- NULL #this model has no parameters
						}
					}
					
					if (perform == "run" && !is.null(model.definition) && length(list.of.errors) == 0) {
						model.formula <- as.formula(model.definition)
						lm.fit <- try( stats::lm( model.formula, data = dataset, weights = weights ), silent = TRUE)
						if ( class(lm.fit) == "lm") {
							lm.model[[ b ]] <- list(lm.fit = lm.fit, variables = variables.in.model)
						} else {							
							list.of.errors[[ length(list.of.errors) + 1 ]]  <- "An unknown error occurred, please contact the author."
							lm.model[[ b ]] <- list(lm.fit = NULL, variables = variables.in.model)
						}
					} else {
						lm.model[[ b ]] <- list(lm.fit = NULL, variables = variables.in.model)
					}
				}
				
				if (block.options$method == "Stepwise") {
					lm.model[[ b ]] <- empty.model
				}
				
				if (block.options$method == "Remove") {
					lm.model[[ b ]] <- empty.model
				}
				
				if (block.options$method == "Backward") {
					lm.model[[ b ]] <- empty.model
				}
				
				if (block.options$method == "Forward") {
					lm.model[[ b ]] <- empty.model
				}
			}
		} else {
			
			if (options$includeConstant == TRUE)
			{
				model.definition <- paste(dependent.base64, "~ 1")
				model.formula <- as.formula(model.definition)
				
				if (perform == "run" && !is.null(model.definition) && length(list.of.errors) == 0) {
					lm.fit <- try( stats::lm(model.formula, data = dataset, weight = weights))
					if ( class(lm.fit) == "lm") {
						lm.model[[ 1 ]] <- list(lm.fit = lm.fit, variables = NULL)
					} else {
						list.of.errors[[ length(list.of.errors) + 1 ]]  <- "An unknown error occurred, please contact the author."
						lm.model[[ 1 ]] <- list(lm.fit = NULL, variables = variables.in.model)
					}
				} else {
					lm.model[[ 1 ]] <- empty.model
				}
				
			} else {
				lm.model[[ 1 ]] <- empty.model
			}
		}
	} else {
		if (number.of.blocks > 0) {
			variables.in.model <- NULL
			
			for (b in 1:number.of.blocks) {
				block.options <- options$blocks[[ b ]]
				
				if (block.options$method == "Enter") {
					variables.in.model <- c(variables.in.model, unlist(block.options[["variables"]] ) )
					variables.in.model <- variables.in.model[ variables.in.model != ""]
					
					lm.model[[ b ]] <- list(lm.fit = NULL, variables = variables.in.model)
				}
				if (block.options$method == "Stepwise") {
					lm.model[[ b ]] <- empty.model
				}
				
				if (block.options$method == "Remove") {
					lm.model[[ b ]] <- empty.model
				}
				
				if (block.options$method == "Backward") {
					lm.model[[ b ]] <- empty.model
				}
				
				if (block.options$method == "Forward") {
					lm.model[[ b ]] <- empty.model
				}
			}
		} else {
			lm.model [[ 1 ]] <- empty.model
		}
		
	}
	
	
	################################################################################
	#							 MODEL SUMMARY TABLE							  #
	################################################################################
	model.table <- list()
	model.table[["title"]] <- "Model Summary"
	
	fields <- list(
		list(name = "Model", type = "integer"),
		list(name = "R", type = "number", format = "dp:3"),
		list(name = "R2", title = "R\u00B2", type = "number", format = "dp:3"),
		list(name = "aR2", title = "Adjusted R\u00B2", type = "number", format = "dp:3"),
		list(name = "se", title = "RMSE", type = "number", format = "dp:3"))
	
	empty.line <- list("Model" = ".", "R" = ".", "R2" = ".", "aR2" = ".", "se" = ".")
	
	if (options$rSquaredChange == TRUE) {
		fields[[ length(fields) + 1 ]] <- list(name = "R2c", title = "R\u00B2 Change", type = "number", format = "dp:3")
		fields[[ length(fields) + 1 ]] <- list(name = "Fc", title = "F Change", type = "number", format = "sf:4;dp:3")
		fields[[ length(fields) + 1 ]] <- list(name = "df1", type = "integer")
		fields[[ length(fields) + 1 ]] <- list(name = "df2", type = "integer")
		fields[[ length(fields) + 1 ]] <- list(name = "p", type = "number",format = "dp:3;p:.001")
		empty.line <- list("Model" = ".", "R" = ".", "R2" = ".", "aR2" = ".", "se" = ".", "R2c" = ".", "Fc" = ".", "df1" = ".", "df2" = ".", "p" = ".")
		r.squared.old = 0.0 #This is going to accumulate through models.
	}
	model.table[["schema"]] <- list(fields = fields)
	table.rows <- list()
	
	if (perform == "run" && length(list.of.errors) == 0 ) {
		
		for (m in 1:length(lm.model)) {
			if ( !is.null(lm.model[[ m ]]$lm.fit) ) {
				
				lm.summary <- summary(lm.model[[ m ]]$lm.fit)
				
				table.rows[[ m ]] <- empty.line
				table.rows[[ m ]]$"Model" <- as.integer(m)
				table.rows[[ m ]]$"R" <- as.numeric(sqrt(lm.summary$r.squared))
				table.rows[[ m ]]$"R2" <- as.numeric(lm.summary$r.squared)
				table.rows[[ m ]]$"aR2" <- as.numeric(lm.summary$adj.r.squared)
				table.rows[[ m ]]$"se" <- as.numeric(lm.summary$sigma)
				
				if (options$rSquaredChange == TRUE) {
					#R^2_change in Field (2013), Eqn. 8.15:
					#F.change = (n-p_new - 1)R^2_change / p_change ( 1- R^2_new)
					#df1 = p_change = abs( p_new - p_old )
					#df2 = n-p_new - 1
					
					r.squared <- lm.summary$r.squared
					r.squared.change <- r.squared - r.squared.old
					
					df1 <- 1
					if (m > 1){
						df1 <- abs(length(lm.model[[ m ]]$variables) - length(lm.model[[ m-1 ]]$variables) )
					}
					
					df2 <- length( dataset[[ dependent.base64 ]]) - length(lm.model[[ m ]]$variables) - 1
					F.change <- (df2 * r.squared.change) / (df1 * (1 - r.squared))
					
					p <- pf(q = F.change, df1 = df1, df2 = df2, lower.tail = FALSE )
					
					table.rows[[ m ]]$"R2c" <- .clean(r.squared.change)
					table.rows[[ m ]]$"Fc" <- .clean(F.change)
					table.rows[[ m ]]$"df1" <- .clean(df1)
					table.rows[[ m ]]$"df2" <- .clean(df2)
					table.rows[[ m ]]$"p" <- .clean(p)
					
					r.squared.old <- r.squared
				}
				
			} else {
				table.rows[[ m ]] <- empty.line
				table.rows[[ m ]]$"Model" <- as.integer(m)
			}
		}
	} else {
		number.of.init.models <- max(1, length(lm.model))
		
		for (m in 1:number.of.init.models) {
			table.rows[[ m ]] <- empty.line
			table.rows[[ m ]]$"Model" <- as.integer(m)
		}
		
		if (length(list.of.errors) == 1){
			model.table[["error"]] <- list(errorType = "badData", errorMessage = list.of.errors[[ 1 ]])
		}
		
		if (length(list.of.errors) > 1){
				model.table[["error"]] <- list(errorType = "badData", errorMessage = paste("The following errors were encountered: <br> <br>", paste(unlist(list.of.errors),collapse="<br>"), sep=""))
		}
	}
	model.table[["data"]] <- table.rows
	results[["model summary"]] <- model.table
	
	
	################################################################################
	#							  MODEL ANOVA TABLE							   #
	################################################################################
	if (options$modelFit == TRUE) {
		
		anova <- list()
		anova[["title"]] <- "ANOVA"
		
		fields <- list(
			list(name = "Model", type = "integer"),
			list(name = "Cases", title = " ", type = "string"),
			list(name = "Sum of Squares", type = "number", format = "sf:4;dp:3"),
			list(name = "df", type = "integer"),
			list(name = "Mean Square", type = "number", format = "sf:4;dp:3"),
			list(name = "F", type = "number", format = "sf:4;dp:3"),
			list(name = "p", type = "number", format = "dp:3;p:.001"))
		
		anova[["schema"]] <- list(fields = fields)
		anova.result <- list()
		
		empty.line <- list(
			"Model" = "",
			"Cases" = "",
			"Sum of Squares" = "",
			"df" = "",
			"Mean Square" = "",
			"F" = "",
			"p" = "")
		
		.addEmptyModel <- function(anova.result,m) {
			dotted.line <- list(
				"Model" = "",
				"Cases" = ".",
				"Sum of Squares" = ".",
				"df" = ".",
				"Mean Square" = ".",
				"F" = ".",
				"p" = ".")
			
			len.an <- length(anova.result) + 1
			anova.result[[ len.an ]] <- dotted.line
			anova.result[[ len.an ]]$"Model" <- as.integer(m)
			anova.result[[ len.an ]]$"Cases" <- as.character("Regression")
			
			len.an <- len.an + 1
			anova.result[[ len.an ]] <- dotted.line
			anova.result[[ len.an ]]$"Cases" <- as.character("Residual")
			
			len.an <- len.an + 1
			anova.result[[ len.an ]] <- dotted.line
			anova.result[[ len.an ]]$"Cases" <- as.character("Total")
			
			return(anova.result)
		}
		
		
		if (perform == "run" && length(list.of.errors) == 0) {
			
			for (m in 1:length(lm.model)) {
				
				if ( class(lm.model[[ m ]]$lm.fit) == "lm" && length( lm.model[[m]]$variables) > 0) {
					
					lm.summary <- summary(lm.model[[ m ]]$lm.fit)
					
					F			   <- lm.summary$fstatistic[1]
					mss.residual	<- (lm.summary$sigma) ^2
					mss.model	   <- F * mss.residual
					df.residual	 <- lm.summary$fstatistic[3]
					df.model		<- lm.summary$fstatistic[2]
					df.total		<- df.residual + df.model
					ss.residual	 <- mss.residual * df.residual
					ss.model		<- mss.model * df.model
					ss.total		<- ss.residual + ss.model
					
					p <- pf(q = F, df1 = df.model, df2 = df.residual, lower.tail = FALSE )
					
					len.an <- length(anova.result) + 1
					
					anova.result[[ len.an ]] <- empty.line
					anova.result[[ len.an ]]$"Model" <- as.integer(m)
					anova.result[[ len.an ]]$"Cases" <- "Regression"
					anova.result[[ len.an ]]$"Sum of Squares" <- as.numeric(ss.model)
					anova.result[[ len.an ]]$"df" <- as.integer(df.model)
					anova.result[[ len.an ]]$"Mean Square" <- as.numeric(mss.model)
					anova.result[[ len.an ]]$"F" <- as.numeric(F)
					anova.result[[ len.an ]]$"p" <- as.numeric(p)
					
					len.an <- len.an + 1
					
					anova.result[[ len.an ]] <- empty.line
					anova.result[[ len.an ]]$"Cases" <- "Residual"
					anova.result[[ len.an ]]$"Sum of Squares" <- as.numeric(ss.residual)
					anova.result[[ len.an ]]$"df" <- as.integer(df.residual)
					anova.result[[ len.an ]]$"Mean Square" <- as.numeric(mss.residual)
					
					len.an <- len.an + 1
					
					anova.result[[ len.an ]] <- empty.line
					anova.result[[ len.an ]]$"Cases" <- "Total"
					anova.result[[ len.an ]]$"Sum of Squares" <- as.numeric(ss.total)
					anova.result[[ len.an ]]$"df" <- as.integer(df.total)
					
				} else {
					anova.result <- .addEmptyModel(anova.result,m)
				}
			}
		} else {
			number.of.init.models <- max(1, length(lm.model))
			
			for (m in 1:number.of.init.models) {
				anova.result <- .addEmptyModel(anova.result,m)
			}
			
			if (length(list.of.errors) > 0){
				anova[["error"]] <- list(errorType = "badData")
			}
		}
		anova[["data"]] <- anova.result
		
		results[["anova"]] <- anova
	}
	
	
	################################################################################
	#						   MODEL COEFFICIENTS TABLE						   #
	################################################################################
	
	
	if (options$regressionCoefficients[["estimates"]] == TRUE) {
		
		regression <- list()
		regression[["title"]] <- "Coefficients"
		
		# Declare table elements
		fields <- list(
			list(name = "Model", type = "integer"),
			list(name = "Name", title = "  ", type = "string"),
			list(name = "Coefficient", title = "Unstandardized", type = "number", format = "dp:3"),
			list(name = "Standard Error", type="number", format = "dp:3"),
			list(name = "Standardized Coefficient", title = "Standardized", type = "number", format = "dp:3"),
			list(name = "t-value", type="number", format = "sf:4;dp:3"),
			list(name = "p", type = "number", format = "dp:3;p:.001"))
		
		empty.line <- list( #for empty elements in tables when given output
			"Model" = "",
			"Name" = "",
			"Coefficient" = "",
			"Standard Error" = "",
			"Standardized Coefficient" = "",
			"t-value" = "",
			"p" = "")
		dotted.line <- list( #for empty tables
			"Model" = ".",
			"Name" = ".",
			"Coefficient" = ".",
			"Standard Error" = ".",
			"Standardized Coefficient" = ".",
			"t-value" = ".",
			"p" = ".")			
		
		if (options$regressionCoefficients[["confidenceIntervals"]] == TRUE) {
			alpha <- options$regressionCoefficients[["confidenceIntervalsInterval"]]
			alpha <- alpha / 100
			fields[[ length(fields) + 1 ]] <- list(name = "Lower Bound", title = paste(round(100*(1-alpha)/2,1),"%",sep=""), type = "number", format = "dp:3")
			fields[[ length(fields) + 1 ]] <- list(name = "Upper Bound", title = paste(round(100*(1+alpha)/2,1),"%",sep=""), type = "number", format = "dp:3")
			empty.line$"Lower Bound" = ""
			empty.line$"Upper Bound" = ""
			dotted.line$"Lower Bound" = "."
			dotted.line$"Upper Bound" = "."
		}
		
		regression[["schema"]] <- list(fields = fields)
		
		regression.result <- list()
		
		if (perform == "run" && length(list.of.errors) == 0) {
			
			for (m in 1:length(lm.model)) {
				
				if ( class(lm.model[[ m ]]$lm.fit) == "lm") {
					tmp <- 
					na.estimate.names <- NULL
					if(any(is.na(lm.model[[m]]$lm.fit$coefficients))){ 
						#these estimates give back NA
						na.estimate.names <- names(lm.model[[m]]$lm.fit$coefficients)[which(is.na(lm.model[[m]]$lm.fit$coefficients))]
# !!!!! if(all(is.na(tmp))) 
					}
					lm.summary = summary(lm.model[[ m ]]$lm.fit)
					lm.estimates <- lm.summary$coefficients
					if (options$regressionCoefficients[["confidenceIntervals"]] == TRUE) {
						lm.confidence.interval <- confint(lm.model[[ m ]]$lm.fit, level = alpha)
					}
					
					len.reg <- length(regression.result) + 1
					v <- 0
					
					if (options$includeConstant == TRUE) {
						if(is.null(na.estimate.names) || na.estimate.names[1] != "(Intercept)"){							
							v <- v + 1
							regression.result[[ len.reg ]] <- empty.line
							regression.result[[ len.reg ]]$"Model" <- as.integer(m)
							regression.result[[ len.reg ]]$"Name" <- as.character("intercept")
							regression.result[[ len.reg ]]$"Coefficient" <- as.numeric(lm.estimates[v,1])
							regression.result[[ len.reg ]]$"Standard Error" <- as.numeric(lm.estimates[v,2])
							regression.result[[ len.reg ]]$"t-value" <- as.numeric(lm.estimates[v,3])
							regression.result[[ len.reg ]]$"p" <- as.numeric(lm.estimates[v,4])
						
							if (options$regressionCoefficients[["confidenceIntervals"]] == TRUE) {
								regression.result[[ len.reg ]]$"Lower Bound" <- as.numeric( lm.confidence.interval[v,1] )
								regression.result[[ len.reg ]]$"Upper Bound" <- as.numeric( lm.confidence.interval[v,2] )
							}
						} else {
							regression.result[[ len.reg ]] <- empty.line
							regression.result[[ len.reg ]]$"Model" <- as.integer(m)
							regression.result[[ len.reg ]]$"Name" <- as.character("intercept")
							regression.result[[ len.reg ]]$"Coefficient" <- "NA"
						}
						len.reg <- len.reg + 1
					}
					sd.dep <- sd( dataset[[ dependent.base64 ]])
					if (length(lm.model[[ m ]]$variables) > 0) {
						variables.in.model <- lm.model[[ m ]]$variables

						for (var in 1:length(variables.in.model)) {
							if(!is.null(na.estimate.names) && .v(variables.in.model[var])%in%na.estimate.names){
								v <- v - 1
								regression.result[[ len.reg ]] <- empty.line
								if (var == 1 && options$includeConstant == FALSE) {
									regression.result[[ len.reg ]]$"Model" <- as.integer(m)
								}
								regression.result[[ len.reg ]]$"Name" <- as.character(variables.in.model[ var])
								regression.result[[ len.reg ]]$"Coefficient" <- "NA"								
								len.reg <- len.reg + 1																
							} else {
								sd.ind <- sd( dataset[[ .v(variables.in.model[var]) ]])
		
								regression.result[[ len.reg ]] <- empty.line
								if (var == 1 && options$includeConstant == FALSE) {
									regression.result[[ len.reg ]]$"Model" <- as.integer(m)
								}
								regression.result[[ len.reg ]]$"Name" <- as.character(variables.in.model[ var])
								regression.result[[ len.reg ]]$"Coefficient" <- as.numeric(lm.estimates[v+var,1])
								regression.result[[ len.reg ]]$"Standard Error" <- as.numeric(lm.estimates[v+var,2])
								regression.result[[ len.reg ]]$"Standardized Coefficient" <- as.numeric(lm.estimates[v+var,1] * sd.ind / sd.dep)
							
								regression.result[[ len.reg ]]$"t-value" <- as.numeric(lm.estimates[v+var,3])
								regression.result[[ len.reg ]]$"p" <- as.numeric(lm.estimates[v+var,4])
							
								if (options$regressionCoefficients[["confidenceIntervals"]] == TRUE) {
									regression.result[[ len.reg ]]$"Lower Bound" <- as.numeric( lm.confidence.interval[v+var,1] )
									regression.result[[ len.reg ]]$"Upper Bound" <- as.numeric( lm.confidence.interval[v+var,2] )
								}
								len.reg <- len.reg + 1
							}
						}
					}
				} else {
					
					len.reg <- length(regression.result) + 1
					regression.result[[ len.reg ]] <- empty.line
					regression.result[[ len.reg ]]$"Model" <- as.numeric(m)
					
				}
			}
		} else {
			
			if (length(lm.model) > 0 ) {
				for (m in 1:length(lm.model)) {
					len.reg <- length(regression.result) + 1
					
					if (options$includeConstant == TRUE) {
						regression.result[[ len.reg ]] <- dotted.line
						regression.result[[ len.reg ]]$"Model" <- as.integer(m)
						regression.result[[ len.reg ]]$"Name" <- as.character("intercept")
						len.reg <- len.reg + 1
					}
					
					if (length(lm.model[[ m ]]$variables) > 0) {
						variables.in.model <- lm.model[[ m ]]$variables
						
						for (var in 1:length(variables.in.model)) {
							regression.result[[ len.reg ]] <- dotted.line
							regression.result[[ len.reg ]]$"Model" <- ""
							
							if (var == 1 && options$includeConstant == FALSE) {
								regression.result[[ len.reg ]]$"Model" <- as.integer(m)
							}
							regression.result[[ len.reg ]]$"Name" <- as.character(variables.in.model[ var])
							len.reg <- len.reg + 1
						}
					}
				}
			} else {
				len.reg <- length(regression.result) + 1
				regression.result[[ len.reg ]] <- dotted.line
				regression.result[[ len.reg ]]$"Model" <- as.numeric(m)
				
				if (options$includeConstant == TRUE) {
					regression.result[[ len.reg ]]$"Name" <- as.character("intercept")
				}
			}
			if(length(list.of.errors) > 0){
				regression[["error"]] <- list(errorType="badData")
			}
		}
		
		regression[["data"]] <- regression.result
		results[["regression"]] <- regression
		
	}
	
	results
}

