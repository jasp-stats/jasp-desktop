RegressionLinear <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {
	
	#######################################
	###	   VARIABLE DECLARATION			##
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
	
	list.variables <- c(dependent.variable, independent.variables)
	list.variables <- list.variables[list.variables != ""]
	to.be.read.variables <- c(dependent.variable, independent.variables, wls.weight)
	to.be.read.variables <- to.be.read.variables[ to.be.read.variables != ""]
	
	
	#######################################
	###			FETCH DATA				 ##
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
		list(name = "coefficient covariances", type = "table"),
		list(name = "collinearity diagnostics", type = "table"),
		list(name = "casewise diagnostics", type = "table"),
		list(name = "residuals statistics", type = "table")
		)
	
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
		
		
		if (perform == "run" && number.of.blocks == 1 && options$blocks[[ 1 ]]$method == "Backward") {
			
			variables.in.model <- unlist( options$blocks[[ 1 ]][[ "variables" ]] )
			independent.base64 <- .v(variables.in.model)
			lm.model <- .backwardRegression(dependent.base64, independent.base64, dataset, options, weights)
		
		} else if (number.of.blocks > 0) {
		
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
	#							 DESCRIPTIVES TABLE								   #
	################################################################################
	
	if (options$descriptives) {
	
	    descriptives <- list()
	
		descriptives[["title"]] <- "Descriptives"
		
		fields <- list(
			list(name="v",    title="",   type="string"),
			list(name="N",    title="N",  type="integer"),
			list(name="mean", title="Mean", type="number", format="sf:4;dp:3"),
			list(name="sd",   title="SD", type="number",   format="sf:4;dp:3"),
			list(name="se",   title="SE", type="number",   format="sf:4;dp:3"))

		descriptives[["schema"]] <- list(fields=fields)
		descriptives.results <- list()
		
		if (length(list.variables) == 0) {
		
			descriptives.results[[length(descriptives.results)+1]] <- list(v=".", N=".", mean=".", sd= ".", se=".")
		
		} else {

			for (variable in list.variables) {
				
				if (perform == "run") {
	
					data <- na.omit(dataset[[ .v(variable) ]])
	
					if (class(data) != "factor") {
	
						n    <- .clean(length(data))
						mean <- .clean(mean(data))
						stdDeviation <- .clean(sd(data))
						stdErrorMean <- .clean(sd(data)/sqrt(length(data)))
	
						result <- list(v=variable, N=n, mean=mean, sd=stdDeviation, se=stdErrorMean)
					} else {
				
						n <- .clean(length(data))
						result <- list(v=variable, N=n, mean="", sd="", se="")
					}
				
				} else {
				
					result <- list(v=variable, N=".", mean=".", sd= ".", se=".")
				
				}
				
				descriptives.results[[length(descriptives.results)+1]] <- result
			}
		}
		
		descriptives[["data"]] <- descriptives.results
		
		results[["descriptives"]] <- descriptives
	}
	
	
	################################################################################
	#						   PART & PARTIAL CORRELATIONS   					   #
	################################################################################
	
	if (options$partAndPartialCorrelations) {
		
		correlations <- list()
		correlations[["title"]] <- "Part And Partial Correlations"
		
		# Declare table elements
		fields <- list(
			list(name = "Model", type = "integer"),
			list(name = "Name", title = "  ", type = "string"),
			list(name = "Partial", title = "Partial", type = "number", format = "dp:3"),
			list(name = "Part", title = "Part", type="number", format = "dp:3"))
	
		correlations[["schema"]] <- list(fields = fields)
		
		correlations.rows <- list()
		
		
		if (perform == "run" && length(list.of.errors) == 0 && dependent.variable != "") {
		
			if (number.of.blocks == 0) {
			
				correlations.rows[[length(correlations.rows)+1]] <- list(Model=".", Name=".", Partial=".", Part=".")
				
			} else {
			
				for (m in 1:length(lm.model)) {
				
					variables.model <- lm.model[[m]]$variables
					
					if (length(variables.model) > 0) {
					
						for (variable in variables.model) {
						
							if ( which(variables.model == variable) == 1) {
							
								partAndPartial <- .partAndPartialCorrelation(dependent.variable, variable, variables.model, dataset)
								partial <- .clean(partAndPartial$partialCor)
								part <- .clean(partAndPartial$partCor)
								correlations.rows[[length(correlations.rows)+1]] <- list(Model=m, Name=variable, Partial=partial, Part=part, .isNewGroup=TRUE)
								
							} else {
							
								partAndPartial <- .partAndPartialCorrelation(dependent.variable, variable, variables.model, dataset)
								partial <- .clean(partAndPartial$partialCor)
								part <- .clean(partAndPartial$partCor)
								correlations.rows[[length(correlations.rows)+1]] <- list(Model="", Name=variable, Partial=partial, Part=part)
							}
						}
					}
				}
			}
			
		} else {
		
			if (number.of.blocks == 0) {
			
				correlations.rows[[length(correlations.rows)+1]] <- list(Model=".", Name=".", Partial=".", Part=".")
				
			} else {
			
				for (m in 1:length(lm.model)) {
				
					variables.model <- lm.model[[m]]$variables
					
					if (length( variables.model) > 0) {
					
						for (variable in variables.model) {
						
							if ( which(variables.model == variable) == 1) {
							
								correlations.rows[[length(correlations.rows)+1]] <- list(Model=m, Name=variable, Partial=".", Part=".", .isNewGroup=TRUE)
								
							} else {
							
								correlations.rows[[length(correlations.rows)+1]] <- list(Model="", Name=variable, Partial=".", Part=".")
							}
						}
					}
				}
			}
			
			if (length(list.of.errors) > 0)
				correlations[["error"]] <- list(errorType="badData")
		
		}
		
		correlations[["data"]] <- correlations.rows
		results[["correlations"]] <- correlations
	}
	
	
	################################################################################
	#							 MODEL SUMMARY TABLE							   #
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
	
	if (options$residualsDurbinWatson) {
	
		fields[[length(fields)+1]] <- list(name = "Durbin-Watson", title = "Durbin-Watson", type = "number", format = "dp:3")
		empty.line$"Durbin-Watson" <- "."
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
				
				if (options$residualsDurbinWatson) {
					
					if (m == length(lm.model)) {
					
						table.rows[[ m ]]$"Durbin-Watson" <- .clean(car::durbinWatsonTest(lm.model[[ m ]]$lm.fit)$dw)
						
					} else {
					
						table.rows[[ m ]]$"Durbin-Watson" <- ""
					}
				}
				
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
	#							  MODEL ANOVA TABLE								   #
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
			anova.result[[ len.an ]]$".isNewGroup" <- TRUE
			
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
					anova.result[[ len.an ]]$".isNewGroup" <- TRUE
					
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
	#						   MODEL COEFFICIENTS TABLE   						#
	################################################################################
	
	collinearity.diagnostics <- list()
	
	if (options$regressionCoefficientsEstimates == TRUE) {
		
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
		
		if (options$regressionCoefficientsConfidenceIntervals == TRUE) {
			alpha <- options$regressionCoefficientsConfidenceIntervalsInterval
			alpha <- alpha / 100
			fields[[ length(fields) + 1 ]] <- list(name = "Lower Bound", title = paste(round(100*(1-alpha)/2,1),"%",sep=""), type = "number", format = "dp:3")
			fields[[ length(fields) + 1 ]] <- list(name = "Upper Bound", title = paste(round(100*(1+alpha)/2,1),"%",sep=""), type = "number", format = "dp:3")
			empty.line$"Lower Bound" = ""
			empty.line$"Upper Bound" = ""
			dotted.line$"Lower Bound" = "."
			dotted.line$"Upper Bound" = "."
		}
		
		if (options$collinearityDiagnostics) {
		
			fields[[ length(fields) + 1 ]] <- list(name = "Tolerance", title = "Tolerance", type = "number", format = "dp:3", overTitle="Collinearity Statistics")
			fields[[ length(fields) + 1 ]] <- list(name = "VIF", title = "VIF", type = "number", format = "dp:3", overTitle="Collinearity Statistics")
			empty.line$"Tolerance" = ""
			empty.line$"VIF" = ""
			dotted.line$"Tolerance" = "."
			dotted.line$"VIF" = "."
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
					if (options$regressionCoefficientsConfidenceIntervals == TRUE) {
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
							regression.result[[ len.reg ]][[".isNewGroup"]] <- TRUE
						
							if (options$regressionCoefficientsConfidenceIntervals == TRUE) {
								regression.result[[ len.reg ]]$"Lower Bound" <- as.numeric( lm.confidence.interval[v,1] )
								regression.result[[ len.reg ]]$"Upper Bound" <- as.numeric( lm.confidence.interval[v,2] )
							}
							
							if (options$collinearityDiagnostics) {
								
								regression.result[[ len.reg ]]$"Tolerance" <- ""
								regression.result[[ len.reg ]]$"VIF" <- ""
							}
							
						} else {
							regression.result[[ len.reg ]] <- empty.line
							regression.result[[ len.reg ]]$"Model" <- as.integer(m)
							regression.result[[ len.reg ]]$"Name" <- as.character("intercept")
							regression.result[[ len.reg ]]$"Coefficient" <- "NA"
							regression.result[[ len.reg ]][[".isNewGroup"]] <- TRUE
						}
						len.reg <- len.reg + 1
					}
					sd.dep <- sd( dataset[[ dependent.base64 ]])
					if (length(lm.model[[ m ]]$variables) > 0) {
						variables.in.model <- lm.model[[ m ]]$variables
						
						if (options$collinearityDiagnostics) 
							collinearity.diagnostics[[length(collinearity.diagnostics)+1]] <- .collinearityDiagnostics(lm.model[[ m ]]$lm.fit, dataset, includeConstant=options$includeConstant)
						
						for (var in 1:length(variables.in.model)) {
							if(!is.null(na.estimate.names) && .v(variables.in.model[var])%in%na.estimate.names){
								v <- v - 1
								regression.result[[ len.reg ]] <- empty.line
								if (var == 1 && options$includeConstant == FALSE) {
									regression.result[[ len.reg ]]$"Model" <- as.integer(m)
									regression.result[[ len.reg ]][[".isNewGroup"]] <- TRUE
								}
								regression.result[[ len.reg ]]$"Name" <- as.character(variables.in.model[ var])
								regression.result[[ len.reg ]]$"Coefficient" <- "NA"
								
								if (options$collinearityDiagnostics) {
								
									regression.result[[ len.reg ]]$"Tolerance" <- .clean(collinearity.diagnostics[[length(collinearity.diagnostics)]]$tolerance[[var]])
									regression.result[[ len.reg ]]$"VIF" <- .clean(collinearity.diagnostics[[length(collinearity.diagnostics)]]$VIF[[var]])
								}
								
								len.reg <- len.reg + 1
							} else {
								sd.ind <- sd( dataset[[ .v(variables.in.model[var]) ]])
		
								regression.result[[ len.reg ]] <- empty.line
								if (var == 1 && options$includeConstant == FALSE) {
									regression.result[[ len.reg ]]$"Model" <- as.integer(m)
									regression.result[[ len.reg ]][[".isNewGroup"]] <- TRUE
								}
								regression.result[[ len.reg ]]$"Name" <- as.character(variables.in.model[ var])
								regression.result[[ len.reg ]]$"Coefficient" <- as.numeric(lm.estimates[v+var,1])
								regression.result[[ len.reg ]]$"Standard Error" <- as.numeric(lm.estimates[v+var,2])
								regression.result[[ len.reg ]]$"Standardized Coefficient" <- as.numeric(lm.estimates[v+var,1] * sd.ind / sd.dep)
							
								regression.result[[ len.reg ]]$"t-value" <- as.numeric(lm.estimates[v+var,3])
								regression.result[[ len.reg ]]$"p" <- as.numeric(lm.estimates[v+var,4])
							
								if (options$regressionCoefficientsConfidenceIntervals == TRUE) {
									regression.result[[ len.reg ]]$"Lower Bound" <- as.numeric( lm.confidence.interval[v+var,1] )
									regression.result[[ len.reg ]]$"Upper Bound" <- as.numeric( lm.confidence.interval[v+var,2] )
								}
								
								if (options$collinearityDiagnostics) {
								
									regression.result[[ len.reg ]]$"Tolerance" <- .clean(collinearity.diagnostics[[length(collinearity.diagnostics)]]$tolerance[[var]])
									regression.result[[ len.reg ]]$"VIF" <- .clean(collinearity.diagnostics[[length(collinearity.diagnostics)]]$VIF[[var]])
								}
								
								len.reg <- len.reg + 1
							}
						}
					}
				} else {
					
					len.reg <- length(regression.result) + 1
					regression.result[[ len.reg ]] <- dotted.line
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
						regression.result[[ len.reg ]][[".isNewGroup"]] <- TRUE
						len.reg <- len.reg + 1
					}
					
					if (length(lm.model[[ m ]]$variables) > 0) {
						variables.in.model <- lm.model[[ m ]]$variables
						
						for (var in 1:length(variables.in.model)) {
							regression.result[[ len.reg ]] <- dotted.line
							regression.result[[ len.reg ]]$"Model" <- ""
							
							if (var == 1 && options$includeConstant == FALSE) {
								regression.result[[ len.reg ]]$"Model" <- as.integer(m)
								regression.result[[ len.reg ]][[".isNewGroup"]] <- TRUE
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
					regression.result[[ len.reg ]][[".isNewGroup"]] <- TRUE
				}
			}
			if(length(list.of.errors) > 0){
				regression[["error"]] <- list(errorType="badData")
			}
		}
		
		regression[["data"]] <- regression.result
		results[["regression"]] <- regression
		
	}
	
	
	################################################################################
	#					 MODEL COEFFICIENTS COVARIANCE TABLE   					   #
	################################################################################
	
	
	if (options$regressionCoefficientsCovarianceMatrix) {
		
		covmatrix <- list()
		covmatrix[["title"]] <- "Coefficients Covariance Matrix"
		
		fields <- list(
			list(name = "Model", type = "integer"),
			list(name = "Name", title = "  ", type = "string"))
		
		
		for (variable in independent.variables) {
		
			fields[[length(fields)+1]] <- list(name = variable, title = variable, type = "number", format = "dp:3")
		}
		
		covmatrix[["schema"]] <- list(fields = fields)
		
		covmatrix.rows <- list()
		
		if (number.of.blocks == 0) {
			
			covmatrix.rows[[length(covmatrix.rows)+1]] <- list(Model=".", Name=".")
			
			
		} else if (perform == "run" && length(list.of.errors) == 0 && dependent.variable != "") {
			
			for (m in 1:length(lm.model)) {
				
				variables.model <- lm.model[[m]]$variables
				
				
				if (length(variables.model) > 0) {
				
					model.fit <- lm.model[[m]]$"lm.fit"
					model.covmatrix <- vcov(model.fit)[-1, -1] # remove intercept row and column
					
					if (length(variables.model) == 1) {
					
						# if only one variable in model, model.covmatrix is just a vector -> convert back to matrix and label row and column
						model.covmatrix <- as.matrix(model.covmatrix)
						colnames(model.covmatrix) <- .v(variables.model[1])
						rownames(model.covmatrix) <- .v(variables.model[1])
					}
					
					
					rownames.covmatrix <- .unv(rownames(model.covmatrix))
					rownames(model.covmatrix) <- rownames.covmatrix
					colnames.covmatrix <- .unv(colnames(model.covmatrix))
					colnames(model.covmatrix) <- colnames.covmatrix
					
					
					for (row.variable in rownames.covmatrix) {
						
						row.index <- which(variables.model == row.variable)
					
						if (row.index == 1) {
						
							covmatrix.row <- list(Model=m, Name=row.variable, .isNewGroup=TRUE)
							
						} else {
						
							covmatrix.row <- list(Model="", Name=row.variable)
						}
					
						for (col.variable in colnames.covmatrix) {
						
							col.index <- which(variables.model == col.variable)
						
							if (row.index > col.index) {
							
								covmatrix.row[[col.variable]] <- ""
							
							} else {
							
								covmatrix.row[[col.variable]] <- .clean(model.covmatrix[row.variable, col.variable])
							}
						
						}
						
						variables.not.in.model.index <- which(independent.variables != variables.model)
						
						if (length(variables.not.in.model.index) > 0) {
						
							variables.not.in.model <- independent.variables[variables.not.in.model.index]
						
							for (variable in variables.not.in.model) {
							
								covmatrix.row[[variable]] <- ""
							}
							
						}
						
						covmatrix.rows[[length(covmatrix.rows)+1]] <- covmatrix.row
					}
					
				} else {
				
					covmatrix.rows[[length(covmatrix.rows)+1]] <- list(Model=".", Name=".")
				}
			}
			
		} else {
		
			# init phase 
			
			for (m in 1:length(lm.model)) {
				
				variables.model <- lm.model[[m]]$variables
				
				if (length(variables.model) > 0) {
				
					for (row.variable in variables.model) {
							
						row.index <- which(variables.model == row.variable)
						
						if (row.index == 1) {
						
							covmatrix.row <- list(Model=m, Name=row.variable, .isNewGroup=TRUE)
							
						} else {
						
							covmatrix.row <- list(Model="", Name=row.variable)
						}
						
						for (col.variable in independent.variables) {
						
							covmatrix.row[[col.variable]] <- ""
						}
						
						covmatrix.rows[[length(covmatrix.rows)+1]] <- covmatrix.row
					}
				}
			}
		}
		
		if (length(list.of.errors) > 0)
			covmatrix[["error"]] <- list(errorType="badData")
		
		covmatrix[["data"]] <- covmatrix.rows
		
		results[["coefficient covariances"]] <- covmatrix
	}
	
	
	################################################################################
	#					 COLLINEARITY DIAGNOSTICS TABLE   					       #
	################################################################################
	
	
	if (options$collinearityDiagnostics) {
		
		diagnostics.table <- list()
		diagnostics.table[["title"]] <- "Collinearity Diagnostics"
		
		fields <- list(
			list(name = "Model", type = "integer"),
			list(name = "Dimension", type = "integer"),
			list(name = "Eigenvalue", type = "number", format = "dp:3"),
			list(name = "Condition Index", type = "number", format = "dp:3")
			)
		
		if (options$includeConstant && dependent.variable != "")
			independent.variables <- c("intercept", independent.variables)
		
		for (variable in independent.variables) {
		
			fields[[length(fields)+1]] <- list(name = variable, title = variable, type = "number", format = "dp:3", overTitle="Variance Proportions")
		}
		
		diagnostics.table[["schema"]] <- list(fields = fields)
		
		diagnostics.rows <- list()
		
		if (number.of.blocks == 0) {
			
			diagnostics.rows[[length(diagnostics.rows)+1]] <- list(Model=".", Dimension=".", Eigenvalue=".", "Condition Index"=".")
			
			
		} else if (perform == "run" && length(list.of.errors) == 0 && dependent.variable != "") {
		
			for (m in 1:length(lm.model)) {
			
				if ( ! options$regressionCoefficientsEstimates)
					collinearity.diagnostics[[length(collinearity.diagnostics)+1]] <- .collinearityDiagnostics(lm.model[[ m ]]$lm.fit, dataset, includeConstant=options$includeConstant)
				
				variables.model <- lm.model[[m]]$variables
				
				if (length(variables.model) > 0) {
				
					if (options$includeConstant) {
					
						predictors <- c("intercept", variables.model)
					
					} else {
					
						predictors <- variables.model
					}
					
					for (predictor in predictors) {
					
						predictor.index <- which(predictors == predictor)
						
						if (predictor.index == 1) {
						
							diagnostics.rows[[length(diagnostics.rows)+1]] <- list(Model=m, Dimension=predictor.index, .isNewGroup=TRUE)
						
						} else {
						
							diagnostics.rows[[length(diagnostics.rows)+1]] <- list(Model="", Dimension=predictor.index)
						}
						
						diagnostics.rows[[length(diagnostics.rows)]]$"Eigenvalue" <- .clean(collinearity.diagnostics[[m]]$eigenvalues[predictor.index])
						diagnostics.rows[[length(diagnostics.rows)]]$"Condition Index" <- .clean(collinearity.diagnostics[[m]]$conditionIndices[predictor.index])
						
						for (colPredictor in predictors) {
						
							colPredictor.index <- which(predictors == colPredictor)
							diagnostics.rows[[length(diagnostics.rows)]][[colPredictor]] <- .clean(collinearity.diagnostics[[m]]$varianceProportions[predictor.index, colPredictor.index])
						
						}
						
						variables.not.in.model.index <- which(! independent.variables %in% predictors)
						
						if (length(variables.not.in.model.index) > 0) {
						
							variables.not.in.model <- independent.variables[variables.not.in.model.index]
						
							for (variable in variables.not.in.model) {
							
								diagnostics.rows[[length(diagnostics.rows)]][[variable]] <- ""
							}
							
						}
						
					}
				
				} else {
				
					diagnostics.rows[[length(diagnostics.rows)+1]] <- list(Model=".", Dimension=".", Eigenvalue=".", "Condition Index"=".")
					
					if (options$includeIntercept) 
						diagnostics.rows[[length(diagnostics.rows)]]$"intercept" <- "."
				}
			}
		
		} else {
		
			# init phase
		
			for (m in 1:length(lm.model)) {
				
				variables.model <- lm.model[[m]]$variables
				
				if (length(variables.model) > 0) {
				
					if (options$includeConstant) {
					
						predictors <- c("intercept", variables.model)
					
					} else {
					
						predictors <- variables.model
					}
					
					for (predictor in predictors) {
					
						predictor.index <- which(predictors == predictor)
						
						if (predictor.index == 1) {
						
							diagnostics.rows[[length(diagnostics.rows)+1]] <- list(Model=m, Dimension=predictor.index, .isNewGroup=TRUE)
						
						} else {
						
							diagnostics.rows[[length(diagnostics.rows)+1]] <- list(Model="", Dimension=predictor.index)
						}
						
						diagnostics.rows[[length(diagnostics.rows)]]$"Eigenvalue" <- ""
						diagnostics.rows[[length(diagnostics.rows)]]$"Condition Index" <- ""
						
						for (colPredictor in predictors) {
							
							colPredictor.index <- which(predictors == colPredictor)
							diagnostics.rows[[length(diagnostics.rows)]][[colPredictor]] <- ""
						
						}
					}
				}
			}
		}
		
		if (length(list.of.errors) > 0)
			diagnostics.table[["error"]] <- list(errorType="badData")
		
		diagnostics.table[["data"]] <- diagnostics.rows
		results[["collinearity diagnostics"]] <- diagnostics.table
		
	}
	
	
	################################################################################
	#						   Casewise Diagnostics Table   					   #
	################################################################################
	
	if (options$residualsCasewiseDiagnostics) {
		
		casewiseDiagnostics <- list()
		casewiseDiagnostics[["title"]] <- "Casewise Diagnostics"
		
		# Declare table elements
		fields <- list(
			list(name = "caseNumber", title = "Case Number", type="number", format = "dp:0"),
			list(name = "stdResidual", title = "Std. Residual", type = "number", format = "dp:3"),
			list(name = "dependentVariable", title = dependent.variable, type="number", format = "dp:3"),
			list(name = "predictedValue", title = "Predicted Value", type="number", format = "dp:3"),
			list(name = "residual", title = "Residual", type="number", format = "dp:3")
			)
		
		casewiseDiagnostics[["schema"]] <- list(fields = fields)
		
		casewiseDiagnostics.rows <- list()
		
		
		if (perform == "run" && length(list.of.errors) == 0 && dependent.variable != "") {
		
			lm.fit <- lm.model[[length(lm.model)]]$lm.fit
			
			if (is.null(lm.fit) || number.of.blocks == 0) {
				
				casewiseDiagnostics.rows[[length(casewiseDiagnostics.rows)+1]] <- list(caseNumber=".", stdResidual=".", dependentVariable=".", predictedValue=".", residual=".")
				
			} else if (perform == "run" && length(list.of.errors) == 0 && dependent.variable != "") {
			
				casewiseAll <- FALSE
				
				if (options$residualsCasewiseDiagnosticsType == "allCases")
					casewiseAll <- TRUE
				
				casewiseDiag <- .casewiseDiagnostics(lm.fit, casewiseAll=casewiseAll, outliersOutside=options$residualsCasewiseDiagnosticsOutliersOutside)
				caseNumbers <- casewiseDiag$index
				
				if (is.na(caseNumbers)) {
				
					casewiseDiagnostics.rows[[length(casewiseDiagnostics.rows)+1]] <- list(caseNumber=".", stdResidual=".", dependentVariable=".", predictedValue=".", residual=".")
				
				} else {
				
					for (case in seq_along(caseNumbers)) 
						casewiseDiagnostics.rows[[length(casewiseDiagnostics.rows)+1]] <- list(caseNumber=caseNumbers[case], stdResidual=casewiseDiag$stdResiduals[case], dependentVariable=casewiseDiag$dependent[case],
																								predictedValue=casewiseDiag$predictedValues[case], residual=casewiseDiag$residuals[case])
				}
			}
			
		} else {
		
			# init phase
			
			casewiseDiagnostics.rows[[length(casewiseDiagnostics.rows)+1]] <- list(caseNumber=".", stdResidual=".", dependentVariable=".", predictedValue=".", residual=".")
		}
		
		if (length(list.of.errors) > 0)
			casewiseDiagnostics[["error"]] <- list(errorType="badData")
		
		casewiseDiagnostics[["data"]] <- casewiseDiagnostics.rows
		results[["casewise diagnostics"]] <- casewiseDiagnostics
	}
	
	
	################################################################################
	#						   Residuals Statistics Table   					   #
	################################################################################
	
	if (options$residualsDurbinWatson || options$residualsCasewiseDiagnostics) {
		
		residualsStatistics <- list()
		residualsStatistics[["title"]] <- "Residuals Statistics"
		
		# Declare table elements
		fields <- list(
			list(name = "Type", title = "  ", type = "string"),
			list(name = "Minimum", title = "Minimum", type = "number", format = "dp:3"),
			list(name = "Maximum", title = "Maximum", type="number", format = "dp:3"),
			list(name = "Mean", title = "Mean", type="number", format = "dp:3"),
			list(name = "SD", title = "SD", type="number", format = "dp:3"),
			list(name = "N", title = "N", type="number", format = "dp:0")
			)
	
		residualsStatistics[["schema"]] <- list(fields = fields)
		
		types <- c("Predicted Value", "Residual", "Std. Predicted Value", "Std. Residual")
		
		residualsStatistics.rows <- list()
		
		if (perform == "run" && length(list.of.errors) == 0 && dependent.variable != "") {
		
			lm.fit <- lm.model[[length(lm.model)]]$lm.fit
		
			if (is.null(lm.fit) || number.of.blocks == 0) {
			
				for (type in types)
					residualsStatistics.rows[[length(residualsStatistics.rows)+1]] <- list(Type=type, Minimum=".", Maximum=".", Mean=".", SD=".", N=".")
				
			} else if (perform == "run" && length(list.of.errors) == 0 && dependent.variable != "") {
				
				resStats <- .residualsStatistics(lm.fit)
				
				for (type in types)
					residualsStatistics.rows[[length(residualsStatistics.rows)+1]] <- list(Type=type, Minimum=resStats[[type]]$Minimum, Maximum=resStats[[type]]$Maximum,
																							Mean=resStats[[type]]$Mean, SD=resStats[[type]]$SD, N=resStats[[type]]$N)
			
			}
			
		} else {
		
			# init phase
			
			for (type in types)
					residualsStatistics.rows[[length(residualsStatistics.rows)+1]] <- list(Type=type, Minimum=".", Maximum=".", Mean=".", SD=".", N=".")
			
		}
		
		if (length(list.of.errors) > 0)
			residualsStatistics[["error"]] <- list(errorType="badData")
		
		residualsStatistics[["data"]] <- residualsStatistics.rows
		results[["residuals statistics"]] <- residualsStatistics
	}
	
	results
}

.partAndPartialCorrelation <- function(dependent.variable, variable.of.interest, model.variables, dataset) {
	
	dataset <- na.omit(dataset)
	dependent <- dataset[[ .v(dependent.variable) ]]
	
	# remove variable.of.interest from model.variables
	index <- which(model.variables == variable.of.interest)
	variables.to.control.for <- model.variables[-index]
	
	# if there are no variables to control for, return regular correlation
	if (length(variables.to.control.for) == 0) {
	
		correlation <- cor(dependent, dataset[[ .v(variable.of.interest) ]])
		
		return(list(partCor=correlation, partialCor=correlation))
	}
	
	# v variables
	variables.to.control.for <- .v(variables.to.control.for)
	dependent.variable <- .v(dependent.variable)
	variable.of.interest <- .v(variable.of.interest)
	
	# create formulas
	definition1 <- paste(variable.of.interest, "~", paste(variables.to.control.for, collapse="+"))
	formula1 <- as.formula(definition1)
	definition2 <- paste(dependent.variable, "~", paste(variables.to.control.for, collapse="+"))
	formula2 <- as.formula(definition2)
	
	# remove variables.to.control.for from variable.of.interest
	cleaned.variable.of.interest <- residuals( lm(formula1, data=dataset) )
	
	# remove variables.to.control.for from dependent.variable
	cleaned.dependent.variable <- residuals( lm(formula2, data=dataset) )
	
	# part (semi-partial) correlation
	partCor <- cor(cleaned.variable.of.interest, dependent)
	
	# partial correlation
	partialCor <- cor(cleaned.variable.of.interest, cleaned.dependent.variable)
		
	return(list(partCor=partCor, partialCor=partialCor))
	
}

.collinearityDiagnostics <- function(lm.fit, dataset, includeConstant=TRUE) {
	
	### create predictor variable matrix
	X <- lm.fit$model[ ,-1, drop=FALSE] # remove dependent variable
	X <- X[ ,-ncol(X), drop=FALSE] # remove weights column
	
	if (includeConstant)
		X <- cbind(1, X) # add intercept
	
	### scale predictor matrix
	for (i in seq_len(ncol(X))) {
		
		X[ ,i] <- X[ ,i] / sqrt(sum(X[ ,i]^2)) # scale each column using Euclidean norm
		
	}
	
	### eigenvalues
	eigenvalues <- svd(X)$d^2 # see Liao & Valliant (2012)
	
	### condition indices
	conditionIndices <- sqrt(max(eigenvalues) / eigenvalues)
	
	### variance proportions ( see e.g., Liao & Valliant, 2012 )
	svdX <- svd(X) # singular value decomposition
	M <- svdX$v %*% solve(diag(svdX$d))
	Q <- M*M # Hadamard (elementwise) product
	tQ <- t(Q)
	
	for (i in seq_len(ncol(tQ))) {
		
		tQ[, i] <- tQ[ ,i] / sum(tQ[ ,i])
		
	}
	
	varianceProportions <- tQ
	
	### VIF (variance inflation factor)
	predictors <- colnames(lm.fit$model[,-c(1, ncol(lm.fit$model)), drop=FALSE]) # predictors in model (remove dependent variable and weights)
	
	VIF <- list()
	tolerance <- list()
	
	if (length(predictors) == 1) {
	
		VIF[[predictors]] <- 1
		tolerance[[predictors]] <- 1
	
	} else if (length(predictors) > 1) {
	
		for (predictor in predictors) {
			
			# remove predictor from other predictors
			index <- which(predictors == predictor)
			cleanedPredictors <- predictors[-index]
			
			# create formula
			definition <- paste(predictor, "~", paste(cleanedPredictors, collapse="+"))
			formula <- as.formula(definition)
			
			# fit lm
			fitVIF <- try(lm(formula, data=dataset), silent=TRUE)
			
			# VIF (variance inflation factor)
			VIF[[predictor]] <- 1 / (1 - summary(fitVIF)$"r.squared")
			
			# tolerance
			tolerance[[predictor]] <- 1 / VIF[[predictor]]
		}
	}
	
	output <- list(	eigenvalues=eigenvalues,
					conditionIndices=conditionIndices,
					varianceProportions=varianceProportions,
					VIF=VIF,
					tolerance=tolerance)
	
	return(output)
	
}

.casewiseDiagnostics <- function(lm.fit, casewiseAll=TRUE, outliersOutside=3) {
	
	# predicted values
	predictedValuesAll <- predict(lm.fit)
	
	# residuals
	residualsAll <- residuals(lm.fit)
	
	# standardized predicted values
	stdPredictedValuesAll <- (predictedValuesAll - mean(predictedValuesAll)) / sd(predictedValuesAll)
	
	# standardized residuals
	stdResidualsAll <- rstandard(lm.fit)
	
	stdResiduals <- NA
	dependent <- NA
	predictedValues <- NA
	residuals <- NA
	
	if (casewiseAll) {
		
		index <- seq_along(predictedValuesAll)
		
	} else {
		
		index <- which(abs(stdResidualsAll) > outliersOutside)
	}
	
	if (length(index) == 0) {
		
		index <- NA
		
	} else {
		
		stdResiduals <- stdResidualsAll[index]
		dependent <- lm.fit$model[index, 1]
		predictedValues <- predictedValuesAll[index]
		residuals <- residualsAll[index]
		
	}
	
	return(list(index=unname(index),
				stdResiduals=unname(stdResiduals),
				dependent=dependent,
				predictedValues=unname(predictedValues),
				residuals=unname(residuals))
			)
}

.residualsStatistics <- function(lm.fit) {
	
	# predicted values
	predictedValues <- predict(lm.fit)
	minPredictedValues <- min(predictedValues)
	maxPredictedValues <- max(predictedValues)
	meanPredictedValues <- mean(predictedValues)
	sdPredictedValues <- sd(predictedValues)
	
	# residuals
	residuals <- residuals(lm.fit)
	minResiduals <- min(residuals)
	maxResiduals <- max(residuals)
	meanResiduals <- mean(residuals)
	sdResiduals <- sd(residuals)
	
	# N
	N <- length(predictedValues)
	
	# standardized predicted values
	stdPredictedValues <- (predictedValues - meanPredictedValues) / sdPredictedValues
	minStdPredictedValues <- min(stdPredictedValues)
	maxStdPredictedValues <- max(stdPredictedValues)
	meanStdPredictedValues <- mean(stdPredictedValues)
	sdStdPredictedValues <- sd(stdPredictedValues)
	
	# standardized residuals
	stdResiduals <- rstandard(lm.fit)
	minStdResiduals <- min(stdResiduals)
	maxStdResiduals <- max(stdResiduals)
	meanStdResiduals <- mean(stdResiduals)
	sdStdResiduals <- sd(stdResiduals)
		
	# residuals statistics
	return(list("Predicted Value" = list(
					Minimum=minPredictedValues,
					Maximum=maxPredictedValues,
					Mean=meanPredictedValues,
					SD=sdPredictedValues,
					N=N),
				"Residual" = list(
					Minimum=minResiduals,
					Maximum=maxResiduals,
					Mean=meanResiduals,
					SD=sdResiduals,
					N=N),
				"Std. Predicted Value" = list(
					Minimum=minStdPredictedValues,
					Maximum=maxStdPredictedValues,
					Mean=meanStdPredictedValues,
					SD=sdStdPredictedValues,
					N=N),
				"Std. Residual" = list(
					Minimum=minStdResiduals,
					Maximum=maxStdResiduals,
					Mean=meanStdResiduals,
					SD=sdStdResiduals,
					N=N)
				)
			)
}

################################
##    stepwise procedures     ##
################################

.removeVariable <- function(dependent.variable, independent.variables, data, options, weights) {
	
	if (options$includeConstant) {
		
		formula <- as.formula(paste(dependent.variable, "~", paste(independent.variables, collapse = "+")))
		
	} else {
		
		formula <- as.formula(paste(dependent.variable, "~", paste(independent.variables, collapse = "+")), "-1")
	}
	
	fit <- try(lm(formula, data=data, weights = weights), silent=TRUE)
	tValues <- summary(fit)$coefficients[ ,"t value"]
	pValues <- summary(fit)$coefficients[ ,"Pr(>|t|)"]
	
	if (options$includeConstant) {
		
		tValues <- tValues[-1]
		pValues <- pValues[-1]
		
	}
	
	fValues <- tValues^2
	
	if (options$steppingMethodCriteriaType == "useFValue") {
		
		minimumFvalue <- min(fValues)
		
		if (minimumFvalue < options$steppingMethodCriteriaFRemoval) {
			
			minimumFvalueVariable <- names(which.min(fValues))	
			new.independent.variables <- independent.variables[independent.variables != minimumFvalueVariable]
			
		} else {
			
			new.independent.variables <- independent.variables
			
		}
		
	} else if (options$steppingMethodCriteriaType == "usePValue") {
		
		maximumPvalue <- max(pValues)
		
		if (maximumPvalue > options$steppingMethodCriteriaPRemoval) {
			
			maximumPvalueVariable <- names(which.max(pValues))	
			new.independent.variables <- independent.variables[independent.variables != maximumPvalueVariable]

		} else {
			
			new.independent.variables <- independent.variables
			
		}
	}
	
	if (length(new.independent.variables) > 0) {
		
		formula.new <- as.formula(paste(dependent.variable, "~", paste(new.independent.variables, collapse = "+")))	
		
	} else if (options$includeConstant) {
		
		formula.new <- as.formula(paste(dependent.variable, "~", "1"))
	
	} else {
	
		formula.new <- NULL
	}
	
	lm.fit <- try(lm(formula.new, data=data, weights = weights), silent=TRUE)
	
	return(list(lm.fit=lm.fit, variables=.unv(new.independent.variables)))
}

.backwardRegression <- function(dependent.variable, independent.variables, data, options, weights) {
	
	formula1 <- as.formula(paste(dependent.variable, "~", paste(independent.variables, collapse = "+")))
	lm.fit1 <- try(lm(formula1, data=data, weights = weights), silent=TRUE)
	lm.model <- list(list(lm.fit=lm.fit1, variables=.unv(independent.variables)))
	
	new.independent.variables <- independent.variables
	old.independent.variables <- ""
	
	while ( ! identical(old.independent.variables, new.independent.variables) && length(new.independent.variables != 0)) {
		
		old.independent.variables <- .v(lm.model[[ length(lm.model) ]]$variables)
		lm.model[[ length(lm.model) + 1 ]] <- .removeVariable(dependent.variable, old.independent.variables, data, options, weights)
		new.independent.variables <- .v(lm.model[[ length(lm.model) ]]$variables)
		
	}
	
	if (length(new.independent.variables != 0))
		lm.model <- lm.model[-length(lm.model)] # remove last fit that did not change independent variables
	
	return(lm.model)
}
