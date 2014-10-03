
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
			# casewise removal is the dafault in lm( ). MM
			# if( options$missingValues == "excludeCasesListwise"){
			#	dataset <- .readDataSetToEnd(columns.as.numeric = to.be.read.variables, exclude.na.listwise=to.be.read.variables)
			#} else {
				dataset <- .readDataSetToEnd(columns.as.numeric = to.be.read.variables)
			#}
		} else {
			dataset <- .readDataSetHeader(columns.as.numeric = to.be.read.variables)
		}
	}

	#######################################
	###			  META			   ###
	#######################################
	
	results <- list()
	
	.meta <-  list(
		list(name = "descriptives", type = "table"),
		list(name = "correlations", type = "table"),
		list(name = "model summary", type = "table"),
		list(name = "anova", type = "table"),
		list(name = "regression", type = "table"),
		list(name = "coefficient correlations", type = "table"))
	
	results[[".meta"]] <- .meta

	#######################################
	###		LINEAR REGRESSION		###
	#######################################

	# Fit Linear Model
	lm.model <- list()
	empty.model <- list(lm.fit = NULL, variables = NULL)
	weights.error <- FALSE

	if (dependent.variable != "") {
		dependent.base64 <- .v(dependent.variable)

# Check if weights are all positive.
# If not, an error should be placed somewhere. TODO(MM)
# In case of error, unweighted estimation is used.
		weights.error <- FALSE
		weights. <- rep(1,length(dataset[[ dependent.base64 ]] ))
		if (length(wls.weight) == 1 ) {
			weight.base64 <- .v(wls.weight)
			min.weight <- min(dataset[[ weight.base64 ]] )
			if (min.weight > 0) {
				weights. <- dataset[[ weight.base64 ]]
			} else {
				weights.error <- TRUE
			}
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
# This is a bug, and should be fixed in GUI. TODO(JL)
					variables.in.model <- unique(variables.in.model)
					
					if (length(variables.in.model) > 0 ) {
						independent.base64 <- .v(variables.in.model)
						if (options$includeConstant == TRUE)
						{
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
					
					if (perform == "run" && !is.null(model.definition)) {
						model.formula <- as.formula(model.definition)
						lm.fit <- try(lm(model.formula, data = dataset, weights = weights.), silent = TRUE)
						if ( class(lm.fit) == "lm") {
							lm.model[[ b ]] <- list(lm.fit = lm.fit, variables = variables.in.model)
						} else {
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

				if (perform == "run") {
					lm.fit <- lm(model.formula, data = dataset, weight = weights.)
					lm.model[[ 1 ]] <- list(lm.fit = lm.fit, variables = NULL)

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
		}

	}
	
# Model' variable descriptives
# TODO(MM / JL)


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
		fields[[ length(fields) + 1 ]] <- list(name = "Fc", title = "F Change", type = "number", format = "dp:3")
		fields[[ length(fields) + 1 ]] <- list(name = "df1", type = "integer")
		fields[[ length(fields) + 1 ]] <- list(name = "df2", type = "integer")
		fields[[ length(fields) + 1 ]] <- list(name = "p", type = "number",format = "dp:3;p:0.001")
		empty.line <- list("Model" = ".", "R" = ".", "R2" = ".", "aR2" = ".", "se" = ".", "R2c" = ".", "Fc" = ".", "df1" = ".", "df2" = ".", "p" = ".")
		r.squared.old = 0.0 #This is going to accumulate through models.
	}
	model.table[["schema"]] <- list(fields = fields)
	table.rows <- list()
	
	if (perform == "run" && length(lm.model) > 0 ) {		
		
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
						
						table.rows[[ m ]]$"R2c" <- as.numeric(r.squared.change)
						table.rows[[ m ]]$"Fc" <- as.numeric(F.change)
						table.rows[[ m ]]$"df1" <- as.integer(df1)
						table.rows[[ m ]]$"df2" <- as.integer(df2)
						table.rows[[ m ]]$"p" <- as.numeric(p)
						
						r.squared.old <- r.squared
					}
					
				} else {
					table.rows[[ m ]] <- empty.line
					table.rows[[ m ]]$"Model" <- as.integer(m)
				}
			}
	} else {
		#To ensure that there is at least one empty model shown in the anova table if perform == init
		number.of.init.models <- max(1, length(lm.model))

		for (m in 1:number.of.init.models) {
			table.rows[[ m ]] <- empty.line
			table.rows[[ m ]]$"Model" <- as.integer(m)
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
			list(name = "Sum of Squares", type = "number", format = "dp:3"),
			list(name = "df", type = "integer"),
			list(name = "Mean Square", type = "number", format = "dp:3"),
			list(name = "F", type = "number", format = "dp:3"),
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


		if (perform == "run" && length(lm.model) > 0 ) {
			
			for (m in 1:length(lm.model)) {
				
				if (!is.null(lm.model[[ m ]]$lm.fit) && !is.null(lm.model[[ m ]]$variables) ) {
					
					lm.summary <- summary(lm.model[[ m ]]$lm.fit)
# Should we round to ensure that ss.model + ss.residual = ss.total?

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
#To ensure that there is at least one empty model shown in the anova table if perform == init
			number.of.init.models <- max(1, length(lm.model))
			
			for (m in 1:number.of.init.models) {
				anova.result <- .addEmptyModel(anova.result,m)
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
			list(name = "t-value", type="number", format = "dp:3"),
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
			empty.line <- list( #for empty elements in tables when given output
				"Model" = "",
				"Name" = "",
				"Coefficient" = "",
				"Standard Error" = "",
				"Standardized Coefficient" = "",
				"t-value" = "",
				"p" = "",
				"Lower Bound" = "",
				"Upper Bound" = "")
			dotted.line <- list( #for empty elements in tables when given output
				"Model" = ".",
				"Name" = ".",
				"Coefficient" = ".",
				"Standard Error" = ".",
				"Standardized Coefficient" = ".",
				"t-value" = ".",
				"p" = ".",
				"Lower Bound" = ".",
				"Upper Bound" = ".")
				#			regression[["footnotes"]] <- list(paste( round(100*alpha,1),"% Confidence Intervals",sep=""))
		}

		regression[["schema"]] <- list(fields = fields)
		
		regression.result <- list()

		if (perform == "run" && length(lm.model) > 0) {
			
			for (m in 1:length(lm.model)) {

				if ( ! is.null(lm.model[[ m ]]$lm.fit)) {
					lm.summary = summary(lm.model[[ m ]]$lm.fit)
					lm.estimates <- lm.summary$coefficients
					if (options$regressionCoefficients[["confidenceIntervals"]] == TRUE) {
						lm.confidence.interval <- confint(lm.model[[ m ]]$lm.fit, level = alpha)
					}

					len.reg <- length(regression.result) + 1
					v <- 0
					
					if (options$includeConstant == TRUE) {
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
						len.reg <- len.reg + 1
					}
					sd.dep <- sd( dataset[[ dependent.base64 ]] )
					if (length(lm.model[[ m ]]$variables) > 0) {
						variables.in.model <- lm.model[[ m ]]$variables

						for (var in 1:length(variables.in.model)) {
							sd.ind <- sd( dataset[[ .v(variables.in.model[var]) ]] )
							
							regression.result[[ len.reg ]] <- empty.line
							regression.result[[ len.reg ]]$"Model" <- ""
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
# if length(lm.model$vars) ==0  && includeConstant == FALSE, length(lm.model) should be zero!
				} else {
# something went wrong, there should be a model here, but it has not performed
# have a look at the encapsulated anova version of JL.
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
		}
		
		regression[["data"]] <- regression.result
		results[["regression"]] <- regression

	}

	results
}

