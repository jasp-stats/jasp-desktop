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

RegressionLinear <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {

	#######################################
	###	   VARIABLE DECLARATION			##
	#######################################

	dependent.variable <- unlist(options$dependent)
	wls.weight <- unlist(options$wlsWeight)
	independent.variables <- NULL
	to.be.read.variables.factors <- NULL
	if (length(options$covariates) > 0){
		independent.variables <- unlist(options$covariates)
	}

	list.variables <- c(dependent.variable, independent.variables)
	list.variables <- list.variables[list.variables != ""]
	to.be.read.variables.numeric <- c(dependent.variable, independent.variables, wls.weight)
	to.be.read.variables.numeric <- to.be.read.variables.numeric [ to.be.read.variables.numeric  != ""]

	if (length(options$factors) > 0)
		to.be.read.variables.factors <- unlist(options$factors)


	#######################################
	###			FETCH DATA				 ##
	#######################################

	if (is.null(dataset)) {
		if (perform == "run") {
			dataset <- .readDataSetToEnd(	columns.as.factor = to.be.read.variables.factors, columns.as.numeric = to.be.read.variables.numeric,
											exclude.na.listwise=c(to.be.read.variables.numeric, to.be.read.variables.factors)	)
		} else {
			dataset <- .readDataSetHeader(columns.as.factor = to.be.read.variables.factors, columns.as.numeric = to.be.read.variables.numeric)
		}
	}

	#######################################
	###		    DATA QUALITY CHECK  	 ##
	#######################################

	list.of.errors <- list()


	if (perform == "run" && dependent.variable != "") {

		#check weights
		if (options$wlsWeight != "") {

			weight.base64 <- .v(wls.weight)
			min.weight <- min(dataset[[ weight.base64 ]])

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

			list.of.errors[[ length(list.of.errors) + 1 ]] <- "Least squares regression model is undefined -- there are no observations for the dependent variable (possibly only after rows with missing values are
			excluded)"
		}

		#check for variance in variables.
		number.of.values.vars <- sapply(1:ncol(x),function(i){length(unique(x[,i]))})
		indicator <- which(number.of.values.vars <= 1)

		if (length(indicator) != 0 ){

			if (number.of.values.vars[1] <= 1){

				list.of.errors[[ length(list.of.errors) + 1 ]] <- "Least squares regression model is undefined -- the dependent variable contains all the same value (the variance is zero)"

			} else {

				if (length(indicator) == 1){

					list.of.errors[[ length(list.of.errors) + 1 ]] <- paste("Least squares regression model is undefined -- the independent variable(s)", independent.variables[indicator-1] ," contain(s) all the
						same value (the variance is zero)",sep="")

				} else {

					var.names <- paste(independent.variables[indicator-1], collapse = ", ",sep="")
					list.of.errors[[ length(list.of.errors) + 1 ]] <- paste("Least squares regression model is undefined -- the independent variable(s)", var.names, " contain(s) all the same value (their variance
						is zero)", sep="")
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
	###			  META			   		###
	#######################################

	results <- list()

	.meta <-  list(
		list(name = "title", type = "title"),
		list(name = "model summary", type = "table"),
		list(name = "anova", type = "table"),
		list(name = "regression", type = "table"),
		list(name = "descriptives", type = "table"),
		list(name = "correlations", type = "table"),
		list(name = "coefficient covariances", type = "table"),
		list(name = "collinearity diagnostics", type = "table"),
		list(name = "casewise diagnostics", type = "table"),
		list(name = "residuals statistics", type = "table"),
		list(name="plotResVsDep", type="image"),
		list(name="plotsResVsCov", type="collection", meta="image"),
		list(name="plotResVsPred", type="image"),
		list(name="plotResHist", type="image"),
		list(name="plotResQQ", type="image")
		)

	results[[".meta"]] <- .meta

	results[["title"]] <- "Linear Regression"

	#######################################
	###	 	   LINEAR REGRESSION		###
	#######################################

	state <- .retrieveState()

	diff <- NULL

	if (!is.null(state)) {

		diff <- .diff(options, state$options)

	}

	# Fit Linear Model
	lm.model <- list()
	empty.model <- list(lm.fit = NULL, variables = NULL)

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


	if (length(options$modelTerms) > 0) {

		max.no.components <- max(sapply (options$modelTerms, function(term){length (term$components)}))

		if (max.no.components > 1) {

			# In case of interactions, check whether all main effects and lower-order interaction terms are in the model

			for (term in options$modelTerms) {

				components <- term$components

				if (length (components) > 1) {

					no.children <- 2^length (components) - 1
					inclusion <- sapply (options$modelTerms, function (terms) {

							term.components <- terms$components

							if (sum (term.components %in% components) == length (term.components)) {
								return (TRUE)
							}
							return (FALSE)
						})

					if (sum (inclusion) != no.children) {

						error.message <- "Main effects and lower-order interactions must be included whenever the corresponding higher-order interaction is included"
						list.of.errors[[ length(list.of.errors) + 1 ]] <- error.message
					}
				}
			}
		}
	}

	if (dependent.variable != "") {

		dependent.base64 <- .v(dependent.variable)

		if (wls.weight != "" ) {
			weight.base64 <- .v(wls.weight)
			weights <- dataset[[ weight.base64 ]]
		} else {
			weights <- rep(1,length(dataset[[ dependent.base64 ]] ))
		}


		if (perform == "run" && (options$method == "backward" || options$method == "forward" || options$method == "stepwise")) {

			if (length(options$modelTerm) > 0) {

				interactionIndicator <- logical(length(options$modelTerms))

				for (i in seq_along(options$modelTerms))
					interactionIndicator[i] <- length(options$modelTerms[[i]]$components) > 1

				if (any(interactionIndicator))
					list.of.errors[[ length(list.of.errors) + 1 ]] <- "Stepwise procedures are not supported for models containing interaction terms"

			}

			if (options$steppingMethodCriteriaType == "usePValue" && options$steppingMethodCriteriaPEntry > options$steppingMethodCriteriaPRemoval) {

				list.of.errors[[ length(list.of.errors) + 1 ]] <- "Error in Stepping Method Criteria: Entry p-value needs to be smaller than removal p-value"

			} else if (options$steppingMethodCriteriaType == "useFValue" && options$steppingMethodCriteriaFEntry < options$steppingMethodCriteriaFRemoval) {

				list.of.errors[[ length(list.of.errors) + 1 ]] <- "Error in Stepping Method Criteria: Entry F-value needs to be larger than removal F-value"

			} else if (length(options$modelTerms) > 0) {

				if (options$method == "backward") {

					lm.model <- .backwardRegression(dependent.base64, independent.base64, dataset, options, weights)

				} else if (options$method == "forward") {

					lm.model <- .forwardRegression(dependent.base64, independent.base64, dataset, options, weights)

				} else if (options$method == "stepwise") {

					lm.model <- .stepwiseRegression(dependent.base64, independent.base64, dataset, options, weights)
				}

			} else {

				lm.model [[ 1 ]] <- empty.model
			}

		} else if (length(options$modelTerms) > 0) {

			if (length(variables.in.model) > 0 ) {

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
				lm.fit <- try( stats::lm( model.formula, data = dataset, weights = weights, x=TRUE ), silent = TRUE)

				if ( class(lm.fit) == "lm") {

					lm.model[[1]] <- list(lm.fit = lm.fit, variables = variables.in.model)

				} else {

					list.of.errors[[ length(list.of.errors) + 1 ]]  <- "An unknown error occurred, please contact the author."
					lm.model[[1]] <- list(lm.fit = NULL, variables = variables.in.model)
				}

			} else {

				lm.model[[1]] <- list(lm.fit = NULL, variables = variables.in.model)
			}

		} else {

			if (options$includeConstant == TRUE)
			{
				model.definition <- paste(dependent.base64, "~ 1")
				model.formula <- as.formula(model.definition)

				if (perform == "run" && !is.null(model.definition) && length(list.of.errors) == 0) {

					lm.fit <- try( stats::lm(model.formula, data = dataset, weight = weights, x=TRUE))

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

		if (length(options$modelTerms) > 0) {

			lm.model[[1]] <- list(lm.fit = NULL, variables = variables.in.model)

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

			if (length(options$modelTerms) == 0) {

				correlations.rows[[length(correlations.rows)+1]] <- list(Model=".", Name=".", Partial=".", Part=".")

			} else {

				for (m in 1:length(lm.model)) {

					variables.model <- lm.model[[m]]$variables

					if (length(variables.model) > 0) {

						for (variable in variables.model) {

							if (grepl(":", variable)) {

								# if interaction term

								vars <- unlist(strsplit(variable, split = ":"))
								name <- paste0(vars, collapse="\u2009\u273b\u2009")

							} else {

								name <- variable
							}

							if ( which(variables.model == variable) == 1) {

								partAndPartial <- .partAndPartialCorrelation(dependent.variable, variable, variables.model, dataset)
								partial <- .clean(partAndPartial$partialCor)
								part <- .clean(partAndPartial$partCor)
								correlations.rows[[length(correlations.rows)+1]] <- list(Model=m, Name=name, Partial=partial, Part=part, .isNewGroup=TRUE)

							} else {

								partAndPartial <- .partAndPartialCorrelation(dependent.variable, variable, variables.model, dataset)
								partial <- .clean(partAndPartial$partialCor)
								part <- .clean(partAndPartial$partCor)
								correlations.rows[[length(correlations.rows)+1]] <- list(Model="", Name=name, Partial=partial, Part=part)
							}
						}
					}
				}
			}

		} else {

			if (length(options$modelTerms) == 0) {

				correlations.rows[[length(correlations.rows)+1]] <- list(Model=".", Name=".", Partial=".", Part=".")

			} else {

				for (m in 1:length(lm.model)) {

					variables.model <- lm.model[[m]]$variables

					if (length( variables.model) > 0) {

						for (variable in variables.model) {

							if (grepl(":", variable)) {

								# if interaction term

								vars <- unlist(strsplit(variable, split = ":"))
								name <- paste0(vars, collapse="\u2009\u273b\u2009")

							} else {

								name <- variable
							}

							if ( which(variables.model == variable) == 1) {

								correlations.rows[[length(correlations.rows)+1]] <- list(Model=m, Name=name, Partial=".", Part=".", .isNewGroup=TRUE)

							} else {

								correlations.rows[[length(correlations.rows)+1]] <- list(Model="", Name=name, Partial=".", Part=".")
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

					if (m == 1) {

						df1 <- abs(length(lm.model[[ m ]]$variables))

					} else if (m > 1){

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
		footnotes <- .newFootnotes()

		fields <- list(
			list(name = "Model", type = "integer"),
			list(name = "Cases", title = " ", type = "string"),
			list(name = "Sum of Squares", type = "number", format = "sf:4;dp:3"),
			list(name = "df", type = "integer"),
			list(name = "Mean Square", type = "number", format = "sf:4;dp:3"),
			list(name = "F", type = "number", format = "sf:4;dp:3"),
			list(name = "p", type = "number", format = "dp:3;p:.001"))

		if (options$VovkSellkeMPR) {
			.addFootnote(footnotes, symbol = "\u002A", text = "Vovk-Sellke Maximum
	    <em>p</em>-Ratio: Based the <em>p</em>-value, the maximum
	    possible odds in favor of H\u2081 over H\u2080 equals
	    1/(-e <em>p</em> log(<em>p</em>)) for <em>p</em> \u2264 .37
	    (Sellke, Bayarri, & Berger, 2001).")
	    fields[[length(fields) + 1]] <- list(name = "VovkSellkeMPR",
	                                        title = "VS-MPR\u002A",
	                                        type = "number",
	                                        format = "sf:4;dp:3")
	 	}

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

		if (options$VovkSellkeMPR) {
			empty.line[["VovkSellkeMPR"]] <- ""
		}
		.addEmptyModel <- function(anova.result,m) {
			dotted.line <- list(
				"Model" = "",
				"Cases" = ".",
				"Sum of Squares" = ".",
				"df" = ".",
				"Mean Square" = ".",
				"F" = ".",
				"p" = ".")

			if (options$VovkSellkeMPR) {
				dotted.line[["VovkSellkeMPR"]] <- "."
			}

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
					ss.residual <- mss.residual * df.residual
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
					if (options$VovkSellkeMPR) {
						anova.result[[ len.an ]]$"VovkSellkeMPR" <- .VovkSellkeMPR(anova.result[[ len.an ]]$"p")
					}
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
		anova[["footnotes"]] <- as.list(footnotes)

		results[["anova"]] <- anova
	}


	################################################################################
	#						   MODEL COEFFICIENTS TABLE   						#
	################################################################################

	collinearity.diagnostics <- list()

	if (options$regressionCoefficientsEstimates == TRUE) {

		regression <- list()
		regression[["title"]] <- "Coefficients"

		footnotes <- .newFootnotes()

		# Declare table elements
		fields <- list(
			list(name = "Model", type = "integer"),
			list(name = "Name", title = "  ", type = "string"),
			list(name = "Coefficient", title = "Unstandardized", type = "number", format = "dp:3"),
			list(name = "Standard Error", type="number", format = "dp:3"),
			list(name = "Standardized Coefficient", title = "Standardized", type = "number", format = "dp:3"),
			list(name = "t", type="number", format = "sf:4;dp:3"),
			list(name = "p", type = "number", format = "dp:3;p:.001"))

		if (options$VovkSellkeMPR) {
			.addFootnote(footnotes, symbol = "\u002A", text = "Vovk-Sellke Maximum
			<em>p</em>-Ratio: Based the <em>p</em>-value, the maximum
			possible odds in favor of H\u2081 over H\u2080 equals
			1/(-e <em>p</em> log(<em>p</em>)) for <em>p</em> \u2264 .37
			(Sellke, Bayarri, & Berger, 2001).")
			fields[[length(fields) + 1]] <- list(name = "VovkSellkeMPR",
																					title = "VS-MPR\u002A",
																					type = "number",
																					format = "sf:4;dp:3")
		}

		empty.line <- list( #for empty elements in tables when given output
			"Model" = "",
			"Name" = "",
			"Coefficient" = "",
			"Standard Error" = "",
			"Standardized Coefficient" = "",
			"t" = "",
			"p" = "")

		if (options$VovkSellkeMPR) {
			empty.line[["VovkSellkeMPR"]] <- ""
		}

		dotted.line <- list( #for empty tables
			"Model" = ".",
			"Name" = ".",
			"Coefficient" = ".",
			"Standard Error" = ".",
			"Standardized Coefficient" = ".",
			"t" = ".",
			"p" = ".")

		if (options$VovkSellkeMPR) {
			dotted.line[["VovkSellkeMPR"]] <- "."
		}

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
							regression.result[[ len.reg ]]$"t" <- as.numeric(lm.estimates[v,3])
							regression.result[[ len.reg ]]$"p" <- as.numeric(lm.estimates[v,4])
							if (options$VovkSellkeMPR) {
									regression.result[[ len.reg ]]$"VovkSellkeMPR" <- .VovkSellkeMPR(regression.result[[ len.reg ]]$"p")
							}
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

					sd.dep <- sd( dataset[[ dependent.base64 ]] )

					if (length(lm.model[[ m ]]$variables) > 0) {

						variables.in.model <- lm.model[[ m ]]$variables

						if (options$collinearityDiagnostics)
							collinearity.diagnostics[[length(collinearity.diagnostics)+1]] <- .collinearityDiagnostics(lm.model[[ m ]]$lm.fit, dataset, includeConstant=options$includeConstant)

						for (var in 1:length(variables.in.model)) {

							if(!is.null(na.estimate.names) && .v(variables.in.model[var])%in%na.estimate.names) {

								v <- v - 1
								regression.result[[ len.reg ]] <- empty.line

								if (var == 1 && options$includeConstant == FALSE) {

									regression.result[[ len.reg ]]$"Model" <- as.integer(m)
									regression.result[[ len.reg ]][[".isNewGroup"]] <- TRUE
								}

								if (grepl(":", variables.in.model[var])) {

									# if interaction term

									vars <- unlist(strsplit(variables.in.model[var], split = ":"))
									name <- paste0(vars, collapse="\u2009\u273b\u2009")

								} else {

									name <- as.character(variables.in.model[ var])
								}

								regression.result[[ len.reg ]]$"Name" <- name
								regression.result[[ len.reg ]]$"Coefficient" <- "NA"

								if (options$collinearityDiagnostics) {

									regression.result[[ len.reg ]]$"Tolerance" <- .clean(collinearity.diagnostics[[length(collinearity.diagnostics)]]$tolerance[[var]])
									regression.result[[ len.reg ]]$"VIF" <- .clean(collinearity.diagnostics[[length(collinearity.diagnostics)]]$VIF[[var]])
								}

								len.reg <- len.reg + 1

							} else {

								if (grepl(":", variables.in.model[var])) {

									# if interaction term

									vars <- unlist(strsplit(variables.in.model[var], split = ":"))

									int.var <- rep(1, nrow(dataset))

									for (i in seq_along(vars))
										int.var <- int.var * dataset[[ .v(vars[i]) ]]

									sd.ind <- sd(int.var)

								} else {

									sd.ind <- sd( dataset[[ .v(variables.in.model[var]) ]])
								}

								regression.result[[ len.reg ]] <- empty.line

								if (var == 1 && options$includeConstant == FALSE) {

									regression.result[[ len.reg ]]$"Model" <- as.integer(m)
									regression.result[[ len.reg ]][[".isNewGroup"]] <- TRUE
								}

								if (grepl(":", variables.in.model[var])) {

									# if interaction term

									vars <- unlist(strsplit(variables.in.model[var], split = ":"))
									name <- paste0(vars, collapse="\u2009\u273b\u2009")

								} else {

									name <- as.character(variables.in.model[ var])
								}

								regression.result[[ len.reg ]]$"Name" <- name
								regression.result[[ len.reg ]]$"Coefficient" <- as.numeric(lm.estimates[v+var,1])
								regression.result[[ len.reg ]]$"Standard Error" <- as.numeric(lm.estimates[v+var,2])
								regression.result[[ len.reg ]]$"Standardized Coefficient" <- as.numeric(lm.estimates[v+var,1] * sd.ind / sd.dep)

								regression.result[[ len.reg ]]$"t" <- as.numeric(lm.estimates[v+var,3])
								regression.result[[ len.reg ]]$"p" <- as.numeric(lm.estimates[v+var,4])

								if (options$VovkSellkeMPR) {
										regression.result[[ len.reg ]]$"VovkSellkeMPR" <- .VovkSellkeMPR(regression.result[[ len.reg ]]$"p")
								}

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
					regression.result[[ len.reg ]]$"Model" <- as.integer(m)

					if (length(lm.model[[ m ]]$variables) > 0) {

						variables.in.model <- lm.model[[ m ]]$variables


						if (options$includeConstant == TRUE) {

							regression.result[[ len.reg ]]$"Name" <- as.character("intercept")
						}

						len.reg <- len.reg + 1

						for (var in 1:length(variables.in.model)) {

							regression.result[[ len.reg ]] <- dotted.line

							if (grepl(":", variables.in.model[var])) {

								# if interaction term

								vars <- unlist(strsplit(variables.in.model[var], split = ":"))
								name <- paste0(vars, collapse="\u2009\u273b\u2009")

							} else {

								name <- as.character(variables.in.model[ var])
							}

							regression.result[[ len.reg ]]$"Name" <- name
							len.reg <- len.reg + 1
						}
					}
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

							if (grepl(":", variables.in.model[var])) {

								# if interaction term

								vars <- unlist(strsplit(variables.in.model[var], split = ":"))
								name <- paste0(vars, collapse="\u2009\u273b\u2009")

							} else {

								name <- as.character(variables.in.model[ var])
							}

							regression.result[[ len.reg ]]$"Name" <- name
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
		regression[["footnotes"]] <- as.list(footnotes)
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

		if (length(options$modelTerms) > 0) {

			for (variable in variables.in.model.copy) {

				if (grepl(":", variable)) {

					# if interaction term

					vars <- unlist(strsplit(variable, split = ":"))
					variable.name <- paste0(vars, collapse="\u2009\u273b\u2009")

				} else {

					variable.name <- variable
				}

				fields[[length(fields)+1]] <- list(name = variable.name, title = variable.name, type = "number", format = "dp:3")
			}
		}

		covmatrix[["schema"]] <- list(fields = fields)

		covmatrix.rows <- list()

		if (length(options$modelTerms) == 0) {

			covmatrix.rows[[length(covmatrix.rows)+1]] <- list(Model=".", Name=".")


		} else if (perform == "run" && length(list.of.errors) == 0 && dependent.variable != "") {

			for (m in 1:length(lm.model)) {


				variables.model <- lm.model[[m]]$variables

				if (length(variables.model) > 0) {

					model.fit <- lm.model[[m]]$"lm.fit"

					model.covmatrix <- vcov(model.fit)

					if (options$includeConstant)
						model.covmatrix <- model.covmatrix[-1, -1] # remove intercept row and column

					if (length(variables.model) == 1) {

						# if only one variable in model, model.covmatrix is just a vector -> convert back to matrix and label row and column
						model.covmatrix <- as.matrix(model.covmatrix)
						colnames(model.covmatrix) <- variables.model
						rownames(model.covmatrix) <- variables.model
					}

					rownames.covmatrix <- variables.model
					rownames(model.covmatrix) <- rownames.covmatrix
					colnames.covmatrix <- variables.model
					colnames(model.covmatrix) <- colnames.covmatrix


					for (row.variable in rownames.covmatrix) {

						if (grepl(":", row.variable)) {

							# if interaction term

							vars <- unlist(strsplit(row.variable, split = ":"))
							row.variable.name <- paste0(vars, collapse="\u2009\u273b\u2009")

						} else {

							row.variable.name <- row.variable
						}

						row.index <- which(rownames.covmatrix == row.variable)

						if (row.index == 1) {

							covmatrix.row <- list(Model=m, Name=row.variable.name, .isNewGroup=TRUE)

						} else {

							covmatrix.row <- list(Model="", Name=row.variable.name)
						}

						for (col.variable in colnames.covmatrix) {

							if (grepl(":", col.variable)) {

								# if interaction term

								vars <- unlist(strsplit(col.variable, split = ":"))
								col.variable.name <- paste0(vars, collapse="\u2009\u273b\u2009")

							} else {

								col.variable.name <- col.variable
							}

							col.index <- which(colnames.covmatrix == col.variable)

							if (row.index > col.index) {

								covmatrix.row[[col.variable.name]] <- ""

							} else {
								covmatrix.row[[col.variable.name]] <- .clean(model.covmatrix[row.index, col.index])
							}

						}

						inModel <- logical(length(variables.in.model.copy))

						for (i in seq_along(variables.in.model.copy)) {

							inModel[i] <- variables.in.model.copy[i] %in% variables.model
						}

						if (! all(inModel)) {

							variables.not.in.model <- variables.in.model.copy[! inModel]

							for (variable in variables.not.in.model) {

								if (grepl(":", variable)) {

									# if interaction term

									vars <- unlist(strsplit(variable, split = ":"))
									variable.name <- paste0(vars, collapse="\u2009\u273b\u2009")

								} else {

									variable.name <- variable
								}

								covmatrix.row[[variable.name]] <- ""
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

						if (grepl(":", row.variable)) {

							# if interaction term

							vars <- unlist(strsplit(row.variable, split = ":"))
							row.variable.name <- paste0(vars, collapse="\u2009\u273b\u2009")

						} else {

							row.variable.name <- row.variable
						}

						row.index <- which(variables.model == row.variable)

						if (row.index == 1) {

							covmatrix.row <- list(Model=m, Name=row.variable.name, .isNewGroup=TRUE)

						} else {

							covmatrix.row <- list(Model="", Name=row.variable.name)
						}

						for (col.variable in variables.in.model.copy) {

							if (grepl(":", col.variable)) {

								# if interaction term

								vars <- unlist(strsplit(col.variable, split = ":"))
								col.variable.name <- paste0(vars, collapse="\u2009\u273b\u2009")

							} else {

								col.variable.name <- col.variable
							}

							covmatrix.row[[col.variable.name]] <- ""
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


		if (length(options$modelTerms) > 0) {

		if (options$includeConstant && dependent.variable != "")
			variables.in.model <- c("intercept", variables.in.model)

			for (variable in variables.in.model) {

				if (grepl(":", variable)) {

					# if interaction term

					vars <- unlist(strsplit(variable, split = ":"))
					name <- paste0(vars, collapse="\u2009\u273b\u2009")

				} else {

					name <- variable
				}

				fields[[length(fields)+1]] <- list(name = name, title = name, type = "number", format = "dp:3", overTitle="Variance Proportions")
			}
		}

		diagnostics.table[["schema"]] <- list(fields = fields)

		diagnostics.rows <- list()

		if (length(options$modelTerms) == 0) {

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

							if (grepl(":", colPredictor)) {

								# if interaction term

								vars <- unlist(strsplit(colPredictor, split = ":"))
								name <- paste0(vars, collapse="\u2009\u273b\u2009")

							} else {

								name <- colPredictor
							}

							diagnostics.rows[[length(diagnostics.rows)]][[name]] <- .clean(collinearity.diagnostics[[m]]$varianceProportions[predictor.index, colPredictor.index])

						}

						variables.not.in.model.index <- which(! variables.in.model %in% predictors)

						if (length(variables.not.in.model.index) > 0) {

							variables.not.in.model <- variables.in.model[variables.not.in.model.index]

							for (variable in variables.not.in.model) {

								if (grepl(":", variable)) {

									# if interaction term

									vars <- unlist(strsplit(variable, split = ":"))
									name <- paste0(vars, collapse="\u2009\u273b\u2009")

								} else {

									name <- variable
								}

								diagnostics.rows[[length(diagnostics.rows)]][[name]] <- ""
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

							if (grepl(":", colPredictor)) {

								# if interaction term

								vars <- unlist(strsplit(colPredictor, split = ":"))
								name <- paste0(vars, collapse="\u2009\u273b\u2009")

							} else {

								name <- colPredictor
							}

							diagnostics.rows[[length(diagnostics.rows)]][[name]] <- ""

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

			if (is.null(lm.fit) || length(options$modelTerms) == 0) {

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
						casewiseDiagnostics.rows[[length(casewiseDiagnostics.rows)+1]] <- list(caseNumber=caseNumbers[case], stdResidual=casewiseDiag$stdResiduals[case],
							dependentVariable=casewiseDiag$dependent[case], predictedValue=casewiseDiag$predictedValues[case], residual=casewiseDiag$residuals[case])
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

			if (is.null(lm.fit) || length(options$modelTerms) == 0) {

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


	#######################################
	###	 	   RESIDUAL PLOTS   		###
	#######################################

	plots.regression <- list()
	plotTypes <- list()
	plotsResVsCov <- list()

	lm.model <- lm.model[[length(lm.model)]]

	if (options$plotResidualsDependent) {

		if (!is.null(state) && paste0("plotResidualsDependent", dependent.variable) %in% state$plotTypes && !is.null(diff) && (is.list(diff) && (diff$modelTerms == FALSE && diff$dependent == FALSE &&
			diff$includeConstant == FALSE && diff$method == FALSE && diff$steppingMethodCriteriaFEntry == FALSE && diff$steppingMethodCriteriaFRemoval == FALSE && diff$steppingMethodCriteriaPEntry == FALSE
			&& diff$steppingMethodCriteriaPRemoval == FALSE && diff$steppingMethodCriteriaType == FALSE && diff$wlsWeights == FALSE && diff$missingValues == FALSE))) {

			# if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
			# then, if the requested plot already exists, use it

			stateIndex <- which(state$plotTypes == paste0("plotResidualsDependent", dependent.variable))[1]

			plots.regression[[length(plots.regression)+1]] <- state$plotsRegression[[stateIndex]]
			results[["plotResVsDep"]] <- state$plotsRegression[[stateIndex]]

		} else {

			plot <- list()

			plot[["title"]] <- paste0("Residuals vs. Dependent")
			plot[["width"]]  <- 530
			plot[["height"]] <- 400
			plot[["status"]] <- "waiting"

			# image <- .beginSaveImage(530, 400)

			.plotFunc <- function() {
			if (length(options$modelTerms) > 0 && dependent.variable != "") {
				.plotResiduals(xlab=dependent.variable, ylab="Residuals", dontPlotData=TRUE)
			} else {
				.plotResiduals(xlab="", ylab="Residuals", dontPlotData=TRUE)
			}
			}

			content <- .writeImage(width = 530, height = 400, plot = .plotFunc, obj = TRUE)
			plot[["convertible"]] <- TRUE
			plot[["obj"]] <- content[["obj"]]
			plot[["data"]] <- content[["png"]]
			
			# plot[["data"]] <- .endSaveImage(image)

			plots.regression[[length(plots.regression)+1]] <- plot
			results[["plotResVsDep"]] <- plots.regression[[length(plots.regression)]]

			if ( ! .shouldContinue(callback(results)))
			return()

		}

		plotTypes[[length(plotTypes)+1]] <- paste0("plotResidualsDependent", dependent.variable)

	}



	if (options$plotResidualsCovariates && length(options$modelTerms) > 0 && dependent.variable != "") {

		results[["plotsResVsCov"]] <- list(title="Residuals vs. Covariates")

		variables.in.model <- variables.in.model.copy

		for (var in seq_along(variables.in.model)) {

			if (grepl(":", variables.in.model[var])) {

				vars <- unlist(strsplit(variables.in.model[var], split = ":"))
				name <- paste0(vars, collapse=" * ")
				nameTitle <- paste0(vars, collapse="\u2009\u273b\u2009")

			} else {

				name <- variables.in.model[var]
				nameTitle <- name
			}

			if (!is.null(state) && paste0("plotResidualsCovariates", variables.in.model[var]) %in% state$plotTypes && !is.null(diff) && (is.list(diff) && (diff$modelTerms == FALSE && diff$dependent == FALSE &&
				diff$includeConstant == FALSE && diff$method == FALSE && diff$steppingMethodCriteriaFEntry == FALSE && diff$steppingMethodCriteriaFRemoval == FALSE && diff$steppingMethodCriteriaPEntry == FALSE
				&& diff$steppingMethodCriteriaPRemoval == FALSE && diff$steppingMethodCriteriaType == FALSE && diff$wlsWeights == FALSE && diff$missingValues == FALSE))) {

				# if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
				# then, if the requested plot already exists, use it

				stateIndex <- which(state$plotTypes == paste0("plotResidualsCovariates", variables.in.model[var]))[1]

				plots.regression[[length(plots.regression)+1]] <- state$plotsRegression[[stateIndex]]
				plotsResVsCov[[length(plotsResVsCov)+1]] <- state$plotsRegression[[stateIndex]]
				results[["plotsResVsCov"]]$collection <- plotsResVsCov

			} else {


				plot <- list()

				plot[["title"]] <- paste0("Residuals vs. ", nameTitle)
				plot[["width"]]  <- 530
				plot[["height"]] <- 400
				plot[["status"]] <- "waiting"

				# image <- .beginSaveImage(530, 400)
				# .plotResiduals(xlab=name, ylab="Residuals", dontPlotData=TRUE)
				# plot[["data"]] <- .endSaveImage(image)
				
				.plotFunc <- function() {
					.plotResiduals(xlab=name, ylab="Residuals", dontPlotData=TRUE)
				}
				content <- .writeImage(width = 530, height = 400, plot = .plotFunc, obj = TRUE)
				plot[["convertible"]] <- TRUE
				plot[["obj"]] <- content[["obj"]]
				plot[["data"]] <- content[["png"]]

				plots.regression[[length(plots.regression)+1]] <- plot
				plotsResVsCov[[length(plotsResVsCov)+1]] <- plot
				results[["plotsResVsCov"]]$collection <- plotsResVsCov

				if ( ! .shouldContinue(callback(results)))
				return()

			}

			plotTypes[[length(plotTypes)+1]] <- paste0("plotResidualsCovariates", variables.in.model[var])

		}
	}


	if (options$plotResidualsPredicted) {

		if (!is.null(state) && paste0("plotResidualsPredicted", dependent.variable) %in% state$plotTypes && !is.null(diff) && (is.list(diff) && (diff$modelTerms == FALSE && diff$dependent == FALSE &&
			diff$includeConstant == FALSE && diff$method == FALSE && diff$steppingMethodCriteriaFEntry == FALSE && diff$steppingMethodCriteriaFRemoval == FALSE && diff$steppingMethodCriteriaPEntry == FALSE
			&& diff$steppingMethodCriteriaPRemoval == FALSE && diff$steppingMethodCriteriaType == FALSE && diff$wlsWeights == FALSE && diff$missingValues == FALSE))) {

			# if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
			# then, if the requested plot already exists, use it

			stateIndex <- which(state$plotTypes == paste0("plotResidualsPredicted", dependent.variable))[1]

			plots.regression[[length(plots.regression)+1]] <- state$plotsRegression[[stateIndex]]
			results[["plotResVsPred"]] <- state$plotsRegression[[stateIndex]]

		} else {

			plot <- list()

			plot[["title"]] <- paste0("Residuals vs. Predicted")
			plot[["width"]]  <- 530
			plot[["height"]] <- 400
			plot[["status"]] <- "waiting"

			# image <- .beginSaveImage(530, 400)
			# .plotResiduals(xlab="Predicted Values", ylab="Residuals", dontPlotData=TRUE)
			# plot[["data"]] <- .endSaveImage(image)
			
			.plotFunc <- function() {
					.plotResiduals(xlab="Predicted Values", ylab="Residuals", dontPlotData=TRUE)
			}
			content <- .writeImage(width = 530, height = 400, plot = .plotFunc, obj = TRUE)
			plot[["convertible"]] <- TRUE
			plot[["obj"]] <- content[["obj"]]
			plot[["data"]] <- content[["png"]]

			plots.regression[[length(plots.regression)+1]] <- plot

			results[["plotResVsPred"]] <- plots.regression[[length(plots.regression)]]

			if ( ! .shouldContinue(callback(results)))
				return()

		}

		plotTypes[[length(plotTypes)+1]] <- paste0("plotResidualsPredicted", dependent.variable)
	}

	if (options$plotResidualsHistogram) {

		if (!is.null(state) && paste0("plotResidualsHistogram", dependent.variable) %in% state$plotTypes && !is.null(diff) && (is.list(diff) && (diff$plotResidualsHistogramStandardized == FALSE
			&& diff$modelTerms == FALSE && diff$dependent == FALSE && diff$includeConstant == FALSE && diff$method == FALSE && diff$steppingMethodCriteriaFEntry == FALSE &&
			diff$steppingMethodCriteriaFRemoval == FALSE && diff$steppingMethodCriteriaPEntry == FALSE && diff$steppingMethodCriteriaPRemoval == FALSE && diff$steppingMethodCriteriaType == FALSE &&
			diff$wlsWeights == FALSE && diff$missingValues == FALSE))) {

			# if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
			# then, if the requested plot already exists, use it

			stateIndex <- which(state$plotTypes == paste0("plotResidualsHistogram", dependent.variable))[1]

			plots.regression[[length(plots.regression)+1]] <- state$plotsRegression[[stateIndex]]
			results[["plotResHist"]] <- state$plotsRegression[[stateIndex]]

		} else {

			plot <- list()

			if (options$plotResidualsHistogramStandardized) {

				plot[["title"]] <- "Standardized Residuals Histogram"
				resName <- "Standardized Residuals"

			} else {

				plot[["title"]] <- "Residuals Histogram"
				resName <- "Residuals"
			}

			plot[["width"]]  <- 530
			plot[["height"]] <- 400
			plot[["status"]] <- "waiting"

			# image <- .beginSaveImage(530, 400)
			# .plotResidualsHistogram(resName=resName, dontPlotData=TRUE)
			# plot[["data"]] <- .endSaveImage(image)
			
			.plotFunc <- function() {
				.plotResidualsHistogram(resName=resName, dontPlotData=TRUE)
			}
			content <- .writeImage(width = 530, height = 400, plot = .plotFunc, obj = TRUE)
			plot[["convertible"]] <- TRUE
			plot[["obj"]] <- content[["obj"]]
			plot[["data"]] <- content[["png"]]

			plots.regression[[length(plots.regression)+1]] <- plot

			results[["plotResHist"]] <- plots.regression[[length(plots.regression)]]

			if ( ! .shouldContinue(callback(results)))
				return()

		}

		plotTypes[[length(plotTypes)+1]] <- paste0("plotResidualsHistogram", dependent.variable)
	}

	if (options$plotResidualsQQ) {

		if (!is.null(state) && paste0("plotResidualsQQ", dependent.variable) %in% state$plotTypes && !is.null(diff) && (is.list(diff) && (diff$modelTerms == FALSE && diff$dependent == FALSE &&
			diff$includeConstant == FALSE && diff$method == FALSE && diff$steppingMethodCriteriaFEntry == FALSE && diff$steppingMethodCriteriaFRemoval == FALSE && diff$steppingMethodCriteriaPEntry == FALSE
			&& diff$steppingMethodCriteriaPRemoval == FALSE && diff$steppingMethodCriteriaType == FALSE && diff$wlsWeights == FALSE && diff$missingValues == FALSE))) {

			# if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
			# then, if the requested plot already exists, use it

			stateIndex <- which(state$plotTypes == paste0("plotResidualsQQ", dependent.variable))[1]

			plots.regression[[length(plots.regression)+1]] <- state$plotsRegression[[stateIndex]]
			results[["plotResQQ"]] <- state$plotsRegression[[stateIndex]]

		} else {

			plot <- list()

			plot[["title"]] <- "Q-Q Plot Standardized Residuals"
			plot[["width"]]  <- 530
			plot[["height"]] <- 400
			plot[["status"]] <- "waiting"

			# image <- .beginSaveImage(530, 400)
			# .plotQQresidualsRegression(dontPlotData=TRUE)
			# plot[["data"]] <- .endSaveImage(image)
			
			.plotFunc <- function() {
				.plotQQresidualsRegression(dontPlotData=TRUE)
			}
			content <- .writeImage(width = 530, height = 400, plot = .plotFunc, obj = TRUE)
			plot[["convertible"]] <- TRUE
			plot[["obj"]] <- content[["obj"]]
			plot[["data"]] <- content[["png"]]

			plots.regression[[length(plots.regression)+1]] <- plot

			results[["plotResQQ"]] <- plots.regression[[length(plots.regression)]]

			if ( ! .shouldContinue(callback(results)))
				return()

		}

		plotTypes[[length(plotTypes)+1]] <- paste0("plotResidualsQQ", dependent.variable)

	}


	if (perform == "run") {

		j <- 1

		if (options$plotResidualsDependent) {

			if (!is.null(state) && paste0("plotResidualsDependent", dependent.variable) %in% state$plotTypes && !is.null(diff) && (is.list(diff) && (diff$modelTerms == FALSE && diff$dependent == FALSE &&
				diff$includeConstant == FALSE && diff$method == FALSE && diff$steppingMethodCriteriaFEntry == FALSE && diff$steppingMethodCriteriaFRemoval == FALSE && diff$steppingMethodCriteriaPEntry == FALSE
				&& diff$steppingMethodCriteriaPRemoval == FALSE && diff$steppingMethodCriteriaType == FALSE && diff$wlsWeights == FALSE && diff$missingValues == FALSE))) {

				# if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
				# then, if the requested plot already exists, use it

				stateIndex <- which(state$plotTypes == paste0("plotResidualsDependent", dependent.variable))[1]

				plots.regression[[j]] <- state$plotsRegression[[stateIndex]]
				results[["plotResVsDep"]] <- state$plotsRegression[[stateIndex]]

			} else {

				plots.regression[[j]]$status <- "running"

				results[["plots"]] <- plots.regression

				if ( ! .shouldContinue(callback(results)))
					return()

				plot <- plots.regression[[j]]

				if (length(list.of.errors) > 0) {

					plot[["error"]] <- list(errorType="badData", errorMessage=list.of.errors[[1]])

				} else {

					if (length(options$modelTerms) > 0 && dependent.variable != "") {

						dependent <- lm.model$lm.fit$model[ , 1]
						res <- residuals(lm.model$lm.fit)

						p <- try(silent=FALSE, expr= {

							# image <- .beginSaveImage(530, 400)
							# .plotResiduals(xVar=dependent, res=res, xlab=dependent.variable, ylab="Residuals")
							# plot[["data"]] <- .endSaveImage(image)
							
							.plotFunc <- function() {
								.plotResiduals(xVar=dependent, res=res, xlab=dependent.variable, ylab="Residuals")
							}
							content <- .writeImage(width = 530, height = 400, plot = .plotFunc, obj = TRUE)
							plot[["convertible"]] <- TRUE
							plot[["obj"]] <- content[["obj"]]
							plot[["data"]] <- content[["png"]]
							
						})

						if (class(p) == "try-error") {

							errorMessage <- .extractErrorMessage(p)
							plot[["error"]] <- list(error="badData", errorMessage= paste("Plotting is not possible:", errorMessage))
						}
					} else {

						# image <- .beginSaveImage(530, 400)
						# .plotResiduals(dontPlotData=TRUE, xlab="", ylab="Residuals")
						# plot[["data"]] <- .endSaveImage(image)
						
						.plotFunc <- function() {
							.plotResiduals(dontPlotData=TRUE, xlab="", ylab="Residuals")
						}
						content <- .writeImage(width = 530, height = 400, plot = .plotFunc, obj = TRUE)
						plot[["convertible"]] <- TRUE
						plot[["obj"]] <- content[["obj"]]
						plot[["data"]] <- content[["png"]]
						
					}
				}

				plot[["status"]] <- "complete"
				plots.regression[[j]] <- plot
				results[["plotResVsDep"]] <- plots.regression[[j]]

				if ( ! .shouldContinue(callback(results)))
					return()
			}

			j <- j + 1

		}

		if (options$plotResidualsCovariates && length(options$modelTerms) > 0 && dependent.variable != "") {

			for (var in seq_along(variables.in.model)) {


				if (!is.null(state) && paste0("plotResidualsCovariates", variables.in.model[var]) %in% state$plotTypes && !is.null(diff) && (is.list(diff) && (diff$modelTerms == FALSE && diff$dependent == FALSE
					&& diff$includeConstant == FALSE && diff$method == FALSE && diff$steppingMethodCriteriaFEntry == FALSE && diff$steppingMethodCriteriaFRemoval == FALSE &&
					diff$steppingMethodCriteriaPEntry == FALSE && diff$steppingMethodCriteriaPRemoval == FALSE && diff$steppingMethodCriteriaType == FALSE && diff$wlsWeights == FALSE &&
					diff$missingValues == FALSE))) {

					# if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
					# then, if the requested plot already exists, use it

					stateIndex <- which(state$plotTypes == paste0("plotResidualsCovariates", variables.in.model[var]))[1]

					plots.regression[[j]] <- state$plotsRegression[[stateIndex]]
					plotsResVsCov[[j]] <- state$plotsRegression[[stateIndex]]
					results[["plotsResVsCov"]]$collection <- plotsResVsCov

				} else {

					if (grepl(":", variables.in.model[var])) {

						# if interaction term

						vars <- unlist(strsplit(variables.in.model[var], split = ":"))
						name <- paste0(vars, collapse=" * ")

						int.var <- rep(1, nrow(dataset))

						for (i in seq_along(vars))
							int.var <- int.var * dataset[[ .v(vars[i]) ]]

						xVar <- int.var

					} else {

						xVar <- dataset[[ .v(variables.in.model[var]) ]]
						name <- variables.in.model[var]
					}

					plots.regression[[j]]$status <- "running"
					plotsResVsCov[[j]] <- plots.regression[[j]]

					results[["plotsResVsCov"]]$collection <- plotsResVsCov

					if ( ! .shouldContinue(callback(results)))
						return()

					plot <- plots.regression[[j]]

					if (length(list.of.errors) > 0) {

						plot[["error"]] <- list(errorType="badData", errorMessage=list.of.errors[[1]])

					} else {

						res <- residuals(lm.model$lm.fit)

						p <- try(silent=FALSE, expr= {

							# image <- .beginSaveImage(530, 400)
							# .plotResiduals(xVar=xVar, res=res, xlab=name, ylab="Residuals")
							# plot[["data"]] <- .endSaveImage(image)
							
							.plotFunc <- function() {
								.plotResiduals(xVar=xVar, res=res, xlab=name, ylab="Residuals")
							}
							content <- .writeImage(width = 530, height = 400, plot = .plotFunc, obj = TRUE)
							plot[["convertible"]] <- TRUE
							plot[["obj"]] <- content[["obj"]]
							plot[["data"]] <- content[["png"]]
							
						})

						if (class(p) == "try-error") {

							errorMessage <- .extractErrorMessage(p)
							plot[["error"]] <- list(error="badData", errorMessage= paste("Plotting is not possible:", errorMessage))
						}
					}

					plot[["status"]] <- "complete"
					plots.regression[[j]] <- plot
					plotsResVsCov[[j]] <- plot
					results[["plotsResVsCov"]]$collection <- plotsResVsCov

					if ( ! .shouldContinue(callback(results)))
						return()

				}

				j <- j + 1

			}
		}


		if (options$plotResidualsPredicted) {

			if (!is.null(state) && paste0("plotResidualsPredicted", dependent.variable) %in% state$plotTypes && !is.null(diff) && (is.list(diff) && (diff$modelTerms == FALSE && diff$dependent == FALSE &&
				diff$includeConstant == FALSE && diff$method == FALSE && diff$steppingMethodCriteriaFEntry == FALSE && diff$steppingMethodCriteriaFRemoval == FALSE && diff$steppingMethodCriteriaPEntry == FALSE
				&& diff$steppingMethodCriteriaPRemoval == FALSE && diff$steppingMethodCriteriaType == FALSE && diff$wlsWeights == FALSE && diff$missingValues == FALSE))) {

				# if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
				# then, if the requested plot already exists, use it

				stateIndex <- which(state$plotTypes == paste0("plotResidualsPredicted", dependent.variable))[1]

				plots.regression[[j]] <- state$plotsRegression[[stateIndex]]
				results[["plotResVsPred"]] <- state$plotsRegression[[stateIndex]]

			} else {

				plots.regression[[j]]$status <- "running"

				results[["plotResVsPred"]] <- plots.regression[[j]]

				if ( ! .shouldContinue(callback(results)))
					return()

				plot <- plots.regression[[j]]

				if (length(list.of.errors) > 0) {

					plot[["error"]] <- list(errorType="badData", errorMessage=list.of.errors[[1]])

				} else {

					if (length(options$modelTerms) > 0 && dependent.variable != "") {

						pred <- predict(lm.model$lm.fit)
						res <- residuals(lm.model$lm.fit)

						p <- try(silent=FALSE, expr= {

							# image <- .beginSaveImage(530, 400)
							# .plotResiduals(xVar=pred, res=res, xlab="Predicted Values", ylab="Residuals")
							# plot[["data"]] <- .endSaveImage(image)
							
							.plotFunc <- function() {
								.plotResiduals(xVar=pred, res=res, xlab="Predicted Values", ylab="Residuals")
							}
							content <- .writeImage(width = 530, height = 400, plot = .plotFunc, obj = TRUE)
							plot[["convertible"]] <- TRUE
							plot[["obj"]] <- content[["obj"]]
							plot[["data"]] <- content[["png"]]
							
						})

						if (class(p) == "try-error") {

							errorMessage <- .extractErrorMessage(p)
							plot[["error"]] <- list(error="badData", errorMessage= paste("Plotting is not possible:", errorMessage))
						}

					} else {

						# image <- .beginSaveImage(530, 400)
						# .plotResiduals(dontPlotData=TRUE, xlab="Predicted Values", ylab="Residuals")
						# plot[["data"]] <- .endSaveImage(image)
						
						.plotFunc <- function() {
							.plotResiduals(dontPlotData=TRUE, xlab="Predicted Values", ylab="Residuals")
						}
						content <- .writeImage(width = 530, height = 400, plot = .plotFunc, obj = TRUE)
						plot[["convertible"]] <- TRUE
						plot[["obj"]] <- content[["obj"]]
						plot[["data"]] <- content[["png"]]
						
					}

				}

				plot[["status"]] <- "complete"
				plots.regression[[j]] <- plot
				results[["plotResVsPred"]] <- plots.regression[[j]]

				if ( ! .shouldContinue(callback(results)))
					return()

			}

			j <- j + 1

		}

		if (options$plotResidualsHistogram) {

			if (!is.null(state) && paste0("plotResidualsHistogram", dependent.variable) %in% state$plotTypes && !is.null(diff) && (is.list(diff) && (diff$plotResidualsHistogramStandardized == FALSE &&
				diff$modelTerms == FALSE && diff$dependent == FALSE && diff$includeConstant == FALSE && diff$method == FALSE && diff$steppingMethodCriteriaFEntry == FALSE &&
				diff$steppingMethodCriteriaFRemoval == FALSE && diff$steppingMethodCriteriaPEntry == FALSE && diff$steppingMethodCriteriaPRemoval == FALSE && diff$steppingMethodCriteriaType == FALSE &&
				diff$wlsWeights == FALSE && diff$missingValues == FALSE))) {

				# if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
				# then, if the requested plot already exists, use it

				stateIndex <- which(state$plotTypes == paste0("plotResidualsHistogram", dependent.variable))[1]

				plots.regression[[j]] <- state$plotsRegression[[stateIndex]]
				results[["plotResHist"]] <- state$plotsRegression[[stateIndex]]

			} else {

				plots.regression[[j]]$status <- "running"

				results[["plotResHist"]] <- plots.regression[[j]]

				if ( ! .shouldContinue(callback(results)))
					return()

				plot <- plots.regression[[j]]

				if (length(list.of.errors) > 0) {

					plot[["error"]] <- list(errorType="badData", errorMessage=list.of.errors[[1]])

				} else {

					if (length(options$modelTerms) > 0 && dependent.variable != "") {

						res <- residuals(lm.model$lm.fit)
						resName <- "Residuals"

						if (options$plotResidualsHistogramStandardized) {

							res <- res / sd(res)
							resName <- "Standardized Residuals"
						}

						p <- try(silent=FALSE, expr= {

							# image <- .beginSaveImage(530, 400)
							# .plotResidualsHistogram(res=res, resName=resName)
							# plot[["data"]] <- .endSaveImage(image)
							
							.plotFunc <- function() {
								.plotResidualsHistogram(res=res, resName=resName)
							}
							content <- .writeImage(width = 530, height = 400, plot = .plotFunc, obj = TRUE)
							plot[["convertible"]] <- TRUE
							plot[["obj"]] <- content[["obj"]]
							plot[["data"]] <- content[["png"]]
							
						})

						if (class(p) == "try-error") {

							errorMessage <- .extractErrorMessage(p)
							plot[["error"]] <- list(error="badData", errorMessage= paste("Plotting is not possible:", errorMessage))
						}

					} else {

						# image <- .beginSaveImage(530, 400)
						# .plotResidualsHistogram(dontPlotData=TRUE, resName=resName)
						# plot[["data"]] <- .endSaveImage(image)
						
						.plotFunc <- function() {
							.plotResidualsHistogram(dontPlotData=TRUE, resName=resName)
						}
						content <- .writeImage(width = 530, height = 400, plot = .plotFunc, obj = TRUE)
						plot[["convertible"]] <- TRUE
						plot[["obj"]] <- content[["obj"]]
						plot[["data"]] <- content[["png"]]
						
					}
				}

				plot[["status"]] <- "complete"
				plots.regression[[j]] <- plot
				results[["plotResHist"]] <- plots.regression[[j]]

				if ( ! .shouldContinue(callback(results)))
					return()

			}

			j <- j + 1

		}

		if (options$plotResidualsQQ) {

			if (!is.null(state) && paste0("plotResidualsQQ", dependent.variable) %in% state$plotTypes && !is.null(diff) && (is.list(diff) && (diff$modelTerms == FALSE && diff$dependent == FALSE &&
				diff$includeConstant == FALSE && diff$method == FALSE && diff$steppingMethodCriteriaFEntry == FALSE && diff$steppingMethodCriteriaFRemoval == FALSE && diff$steppingMethodCriteriaPEntry == FALSE
				&& diff$steppingMethodCriteriaPRemoval == FALSE && diff$steppingMethodCriteriaType == FALSE && diff$wlsWeights == FALSE && diff$missingValues == FALSE))) {

				# if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
				# then, if the requested plot already exists, use it

				stateIndex <- which(state$plotTypes == paste0("plotResidualsQQ", dependent.variable))[1]

				plots.regression[[j]] <- state$plotsRegression[[stateIndex]]
				results[["plotResQQ"]] <- state$plotsRegression[[stateIndex]]

			} else {

				plots.regression[[j]]$status <- "running"

				results[["plotResQQ"]] <- plots.regression[[j]]

				if ( ! .shouldContinue(callback(results)))
					return()

				plot <- plots.regression[[j]]

				if (length(list.of.errors) > 0) {

					plot[["error"]] <- list(errorType="badData", errorMessage=list.of.errors[[1]])

				} else {

					if (length(options$modelTerms) > 0 && dependent.variable != "") {

						res <- residuals(lm.model$lm.fit)
						resSt <- res / sd(res)

						p <- try(silent=FALSE, expr= {

							# image <- .beginSaveImage(530, 400)
							# .plotQQresidualsRegression(res=resSt)
							# plot[["data"]] <- .endSaveImage(image)
							
							.plotFunc <- function() {
								.plotQQresidualsRegression(res=resSt)
							}
							content <- .writeImage(width = 530, height = 400, plot = .plotFunc, obj = TRUE)
							plot[["convertible"]] <- TRUE
							plot[["obj"]] <- content[["obj"]]
							plot[["data"]] <- content[["png"]]
							
						})

						if (class(p) == "try-error") {

							errorMessage <- .extractErrorMessage(p)
							plot[["error"]] <- list(error="badData", errorMessage= paste("Plotting is not possible:", errorMessage))
						}

					} else {

						# image <- .beginSaveImage(530, 400)
						# .plotQQresidualsRegression(dontPlotData=TRUE)
						# plot[["data"]] <- .endSaveImage(image)
						
						.plotFunc <- function() {
							.plotQQresidualsRegression(dontPlotData=TRUE)
						}
						content <- .writeImage(width = 530, height = 400, plot = .plotFunc, obj = TRUE)
						plot[["convertible"]] <- TRUE
						plot[["obj"]] <- content[["obj"]]
						plot[["data"]] <- content[["png"]]

					}

				}

				plot[["status"]] <- "complete"
				plots.regression[[j]] <- plot
				results[["plotResQQ"]] <- plots.regression[[j]]

				if ( ! .shouldContinue(callback(results)))
					return()

			}

			j <- j + 1

		}

	}

	keep <- NULL

	for (plot in plots.regression)
		keep <- c(keep, plot$data)

	if (perform == "init") {

		return(list(results=results, status="inited", state=state, keep=keep))

	} else {

		return(list(results=results, status="complete", state=list(options=options, results=results, plotsRegression=plots.regression, plotTypes=plotTypes), keep=keep))
	}

}

.partAndPartialCorrelation <- function(dependent.variable, variable.of.interest, model.variables, dataset) {

	dataset <- na.omit(dataset)
	dependent <- dataset[[ .v(dependent.variable) ]]

	# remove variable.of.interest from model.variables
	index <- which(model.variables == variable.of.interest)
	variables.to.control.for <- model.variables[-index]

	# if there are no variables to control for, return regular correlation
	if (length(variables.to.control.for) == 0) {

		if (grepl(":", variable.of.interest)) {

			# if interaction term

			vars <- unlist(strsplit(variable.of.interest, split = ":"))
			vVars <- .v(vars)
			int.var <- rep(1, nrow(dataset))

			for (i in seq_along(vVars ))
				int.var <- int.var * dataset[[ vVars[i] ]]

			correlation <- cor(dependent, int.var)

		} else {

			correlation <- cor(dependent, dataset[[ .v(variable.of.interest) ]])
		}

		return(list(partCor=correlation, partialCor=correlation))
	}

	# v variables
	variables.to.control.for <- .vWithInteraction(variables.to.control.for)
	dependent.variable <- .v(dependent.variable)
	variable.of.interest <- .vWithInteraction(variable.of.interest)

	if (grepl(":", variable.of.interest)) {

			# if interaction term

			vars <- unlist(strsplit(variable.of.interest, split = ":"))
			int.var <- rep(1, nrow(dataset))

			for (i in seq_along(vars))
				int.var <- int.var * dataset[[ vars[i] ]]

			variable.of.interest <- paste0(vars, collapse=".")
			dataset[[variable.of.interest]] <- int.var

	}

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
	X <- lm.fit$x

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

	if ( ! includeConstant) {

		predictors <- colnames(lm.fit$x) # predictors in model (remove dependent variable and weights)

	} else {

		predictors <- colnames(lm.fit$x)[-1]
	}

	VIF <- list()
	tolerance <- list()

	if (length(predictors) == 1) {

		VIF[[predictors]] <- 1
		tolerance[[predictors]] <- 1

	} else if (length(predictors) > 1) {

		for (predictor in predictors) {

			if (grepl(":", predictor)) {

				# if interaction term

				vars <- unlist(strsplit(predictor, split = ":"))
				int.var <- rep(1, nrow(dataset))

				for (i in seq_along(vars))
					int.var <- int.var * dataset[[ vars[i] ]]

				predictor.d <- paste0(vars, collapse=".")
				dataset[[predictor.d]] <- int.var

			} else {

				predictor.d <- predictor
			}

			# remove predictor from other predictors
			index <- which(predictors == predictor)
			cleanedPredictors <- predictors[-index]

			# create formula
			definition <- paste(predictor.d, "~", paste(cleanedPredictors, collapse="+"))
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

.includeVariable <- function(dependent.variable, candidate.variables, data, options, weights, variables.in.model=NULL) {

	fValues <- numeric(length(candidate.variables))
	pValues <- numeric(length(candidate.variables))
	fits <- list()

	for (i in seq_along(candidate.variables)) {

		if (options$includeConstant) {

			formula <- as.formula(paste(dependent.variable, "~", paste(c(variables.in.model, candidate.variables[i]), collapse = "+")))
			fits[[candidate.variables[i]]] <- try(lm(formula, data=data, weights = weights, x=TRUE), silent=TRUE)
			fValues[i] <- summary(fits[[i]])$coefficients[ ,"t value"][length(variables.in.model) + 2]^2
			pValues[i] <- summary(fits[[i]])$coefficients[ ,"Pr(>|t|)"][length(variables.in.model) + 2]

		} else {

			formula <- as.formula(paste(dependent.variable, "~", paste(c(variables.in.model, candidate.variables[i]), collapse = "+"), "-1"))
			fits[[i]] <- try(lm(formula, data=data, weights = weights, x=TRUE), silent=TRUE)
			fValues[i] <- summary(fits[[i]])$coefficients[ ,"t value"][length(variables.in.model) + 1]^2
			pValues[i] <- summary(fits[[i]])$coefficients[ ,"Pr(>|t|)"][length(variables.in.model) + 1]
		}
	}

	if (options$steppingMethodCriteriaType == "useFValue") {

		maximumFvalue <- max(fValues)

		if (maximumFvalue > options$steppingMethodCriteriaFEntry) {

			maximumFvalueVariable <- candidate.variables[which.max(fValues)]
			variables.in.model <- c(variables.in.model, maximumFvalueVariable)
			candidate.variables <- candidate.variables[candidate.variables != maximumFvalueVariable]
		}

	} else if (options$steppingMethodCriteriaType == "usePValue") {

		minimumPvalue <- min(pValues)

		if (minimumPvalue < options$steppingMethodCriteriaPEntry) {

			minimumPvalueVariable <- candidate.variables[which.min(pValues)]
			variables.in.model <- c(variables.in.model, minimumPvalueVariable)
			candidate.variables <- candidate.variables[candidate.variables != minimumPvalueVariable]
		}
	}

	if (options$includeConstant) {

		if (is.null(variables.in.model)) {

			formula1 <- as.formula(paste(dependent.variable, "~", "1"))

		} else {

			formula1 <- as.formula(paste(dependent.variable, "~", paste(variables.in.model, collapse = "+")))
		}

	} else {

		if (is.null(variables.in.model)) {

			formula1 <- as.formula(paste(dependent.variable, "~", "1", "-1"))

		} else {

			formula1 <- as.formula(paste(dependent.variable, "~", paste(variables.in.model, collapse = "+"), "-1"))
		}
	}

	lm.fit <- try(lm(formula1, data=data, weights = weights, x=TRUE), silent=TRUE)

	return(list(lm.fit=lm.fit, variables=variables.in.model, candidate.variables=candidate.variables))

}

.removeVariable <- function(dependent.variable, independent.variables, data, options, weights) {


	if (options$includeConstant) {

		formula <- as.formula(paste(dependent.variable, "~", paste(independent.variables, collapse = "+")))

	} else {

		formula <- as.formula(paste(dependent.variable, "~", paste(independent.variables, collapse = "+"), "-1"))
	}

	fit <- try(lm(formula, data=data, weights = weights, x=TRUE), silent=TRUE)
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

		if (options$includeConstant) {

			formula.new <- as.formula(paste(dependent.variable, "~", paste(new.independent.variables, collapse = "+")))

		} else {

			formula.new <- as.formula(paste(dependent.variable, "~", paste(new.independent.variables, collapse = "+"), "-1"))

		}

	} else if (options$includeConstant) {

		formula.new <- as.formula(paste(dependent.variable, "~", "1"))

	} else {

		formula.new <- NULL
	}

	if (!is.null(formula.new)) {

		lm.fit <- try(lm(formula.new, data=data, weights = weights, x=TRUE), silent=TRUE)

	} else {

		lm.fit <- NULL
	}

	return(list(lm.fit=lm.fit, variables=.unvWithInteraction(new.independent.variables)))
}

.backwardRegression <- function(dependent.variable, independent.variables, data, options, weights) {

	if (options$includeConstant) {

		formula1 <- as.formula(paste(dependent.variable, "~", paste(independent.variables, collapse = "+")))

	} else {

		formula1 <- as.formula(paste(dependent.variable, "~", paste(independent.variables, collapse = "+"), "-1"))
	}

	lm.fit1 <- try(lm(formula1, data=data, weights = weights, x=TRUE), silent=TRUE)

	lm.model <- list(list(lm.fit=lm.fit1, variables=.unvWithInteraction(independent.variables)))

	new.independent.variables <- independent.variables
	old.independent.variables <- ""

	while ( ! identical(old.independent.variables, new.independent.variables) && length(new.independent.variables) > 0) {

		old.independent.variables <- .vWithInteraction(lm.model[[ length(lm.model) ]]$variables)
		lm.model[[ length(lm.model) + 1 ]] <- .removeVariable(dependent.variable, old.independent.variables, data, options, weights)
		new.independent.variables <- .vWithInteraction(lm.model[[ length(lm.model) ]]$variables)

	}

	if (length(new.independent.variables) > 0)
		lm.model <- lm.model[-length(lm.model)] # remove last fit that did not change independent variables

	return(lm.model)
}

.forwardRegression <- function(dependent.variable, independent.variables, data, options, weights) {

	old.candidate.variables <- ""
	candidate.variables <- independent.variables
	variables.in.model <- NULL
	lm.model <- list()

	counter <- 0

	while ( ! identical(old.candidate.variables, candidate.variables) && length(candidate.variables) > 0) {

		old.candidate.variables <- candidate.variables
		out <- .includeVariable(dependent.variable, old.candidate.variables, data, options, weights, variables.in.model)
		candidate.variables <- out$candidate.variables
		variables.in.model <- out$variables
		lm.model.tmp <- list(lm.fit=out$lm.fit, variables=.unvWithInteraction(out$variables))
		lm.model[[ length(lm.model) + 1 ]] <- lm.model.tmp

		counter <- counter + 1
	}


	if (length(candidate.variables) > 0 && counter > 1)
		lm.model <- lm.model[-length(lm.model)] # remove last fit that did not change independent variables

	return(lm.model)

}

.stepwiseRegression <- function(dependent.variable, independent.variables, data, options, weights) {

	old.candidate.variables <- ""
	candidate.variables <- independent.variables
	new.variables.in.model <- NULL
	old.variables.in.model <- "dummy"
	lm.model <- list()

	counter <- 0

	while ( ! (identical(old.candidate.variables, candidate.variables) && identical(old.variables.in.model, new.variables.in.model)) && length(candidate.variables) > 0) {

		old.candidate.variables <- candidate.variables
		out <- .includeVariable(dependent.variable, old.candidate.variables, data, options, weights, new.variables.in.model)
		candidate.variables <- out$candidate.variables
		old.variables.in.model <- out$variables
		lm.model.tmp <- list(lm.fit=out$lm.fit, variables=.unvWithInteraction(out$variables))
		lm.model[[ length(lm.model) + 1 ]] <- lm.model.tmp

		if (is.null(old.variables.in.model))
			break

		removeStep <- .removeVariable(dependent.variable, old.variables.in.model, data, options, weights)
		new.variables.in.model <- .vWithInteraction(removeStep$variables)

		if ( ! identical(new.variables.in.model, old.variables.in.model)) {

			lm.model[[ length(lm.model) + 1 ]] <- list(lm.fit=removeStep$lm.fit, variables=(removeStep$variables))

		}

		counter <- counter + 1
	}

	if (length(candidate.variables) > 0 && counter > 1)
		lm.model <- lm.model[-length(lm.model)] # remove last fit that did not change independent variables

	return(lm.model)

}

.unvWithInteraction <- function(variables) {

	unvVars <- character(length(variables))

	for (i in seq_along(variables)) {

		if (grepl(":", variables[i])) {

			# if interaction term

			vars <- unlist(strsplit(variables[i], split = ":"))
			unvVarsTmp <- .unv(vars)
			unvVars[i] <- paste0(unvVarsTmp, collapse=":")

		} else {

			unvVars[i] <- .unv(variables[i])
		}
	}

	return(unvVars)
}

.vWithInteraction <- function(variables) {

	vVars <- character(length(variables))

	for (i in seq_along(variables)) {

		if (grepl(":", variables[i])) {

			# if interaction term

			vars <- unlist(strsplit(variables[i], split = ":"))
			vVarsTmp <- .v(vars)
			vVars[i] <- paste0(vVarsTmp, collapse=":")

		} else {

			vVars[i] <- .v(variables[i])
		}
	}

	return(vVars)
}


.plotResiduals <- function(xVar=NULL, res=NULL, xlab, ylab="Residuals", dontPlotData=FALSE, cexPoints= 1.3, cexXAxis= 1.3, cexYAxis= 1.3, lwd= 2, lwdAxis=1.2) {

	op <- par(mar= c(5.6, 7, 4, 7) + 0.1, las=1, xpd=TRUE)

	if (dontPlotData) {

		plot(1, type='n', xlim=0:1, ylim=0:1, bty='n', axes=FALSE, xlab="", ylab="")

		axis(1, at=0:1, labels=FALSE, cex.axis=cexXAxis, lwd=lwdAxis)
		axis(2, at=0:1, labels=FALSE, cex.axis=cexYAxis, lwd=lwdAxis)
		axis(4, at=0:1, labels=FALSE, cex.axis=cexYAxis, lwd=lwdAxis)
		mtext(text = xlab, side = 1, cex=1.5, line = 2.9)
		mtext(text = ylab, side = 2, cex=1.5, line = 3.25, las=0)
		xx <- grconvertX(0.92, "ndc", "user")
		text(xx, .5, "Standardized Residuals", srt= -90, cex= 1.6)

		return()

	}

	d <- data.frame(xx= xVar, yy= res)
	d <- na.omit(d)
	xVar <- d$xx
	res <- d$yy

	xlow <- min((min(xVar) - 0.1* min(xVar)), min(pretty(xVar)))
	xhigh <- max((max(xVar) + 0.1* max(xVar)), max(pretty(xVar)))
	xticks <- pretty(c(xlow, xhigh))
	ylow <- min((min(res) - 0.1* min(res)), min(pretty(res)))
	yhigh <- max((max(res) + 0.1* max(res)), max(pretty(res)))

	yticks <- pretty(c(ylow, yhigh, 0))

	yLabs <- vector("character", length(yticks))

	for (i in seq_along(yticks)) {

		if (yticks[i] < 10^6) {

			yLabs[i] <- format(yticks[i], digits= 3, scientific = FALSE)

		} else {

			yLabs[i] <- format(yticks[i], digits= 3, scientific = TRUE)
		}
	}

	plot(1, type="n", ylab="", xlab="", axes=FALSE, ylim= range(yticks), xlim= range(xticks), cex= cexPoints)
	lines(range(xticks), rep(0, 2), lwd=lwd, col="darkred")

	points(xVar, res, col="black", pch=21, bg = "grey", cex= cexPoints)

	axis(1, line= 0.4, labels= xticks, at= xticks, cex.axis= cexXAxis, lwd=lwdAxis)
	axis(2, line= 0.2, labels= yLabs, at= yticks, cex.axis= cexYAxis, lwd=lwdAxis)

	maxYlab <- max(nchar(yLabs))
	distLab <- maxYlab / 1.8
	mtext(text = xlab, side = 1, cex=1.5, line = 2.9)
	mtext(text = ylab, side = 2, cex=1.5, line = distLab + 2.1, las=0)

	stAxisTmp <- pretty( yticks / sd(res) )
	stAxisOriginalScaleTmp <- stAxisTmp * sd(res)
	stAxisOriginalScale <- stAxisOriginalScaleTmp[stAxisOriginalScaleTmp < max(yticks) & stAxisOriginalScaleTmp > min(yticks)]
	stAxis <- stAxisOriginalScale / sd(res)

	axis(side = 4, at= stAxisOriginalScale, labels=stAxis, cex.axis= cexYAxis, lwd=lwdAxis)
	xx <- grconvertX(0.92, "ndc", "user")
	text(xx, mean(range(yticks)), "Standardized Residuals", srt= -90, cex= 1.6)

	par(op)

}

.plotResidualsHistogram <- function(res=NULL, resName="Residuals", dontPlotData=FALSE, cexYlab= 1.3, lwd= 2, rugs= FALSE) {

	op <- par(mar= c(5.6, 7, 4, 4) + 0.1)

	if (dontPlotData) {

		plot(1, type='n', xlim=0:1, ylim=0:1, bty='n', axes=FALSE, xlab="", ylab="")
		ax1 <- axis(1, line = 0.3, at=0:1, labels=FALSE, cex.axis = 1.2)
		axis(2, at = c(0, .5, 1) , labels = c("", "Density", ""), lwd.ticks=0, pos= range(ax1)- 0.05*diff(range(ax1)), cex.axis= 1.5, mgp= c(3, 0.7, 0), las=0)
		mtext(text = resName, side = 1, cex=1.5, line = 3)

		return()
	}

	density <- density(res)

	h <- hist(res, plot = FALSE)
	jitVar <- jitter(res)
	yhigh <- max(max(h$density), max(density$y))
	ylow <- 0
	xticks <- pretty(c(res, h$breaks), min.n= 3)

	plot(1, xlim= range(xticks), ylim= c(ylow, yhigh), type="n", axes=FALSE, ylab="", xlab="")
	h <- hist(res, freq=F, main = "", ylim= c(ylow, yhigh), xlab = "", ylab = " ", axes = F, col = "grey", add= TRUE, nbreaks= round(length(res)/5))
	ax1 <- axis(1, line = 0.3, at= xticks, lab= xticks, cex.axis = 1.2)
	mtext(text = resName, side = 1, cex=1.5, line = 3)
	ax2 <- axis(2, at = c(0, max(max(h$density), max(density$y))/2, max(max(h$density), max(density$y))) , labels = c("", "Density", ""), lwd.ticks=0, pos= range(ax1)- 0.05*diff(range(ax1)), cex.axis= 1.5, mgp= c(3, 0.7, 0), las=0)

	if(rugs)
		rug(jitVar)

	lines(density$x[density$x>= min(ax1) & density$x <= max(ax1)], density$y[density$x>= min(ax1) & density$x <= max(ax1)], lwd= lwd)

	par(op)

}


.plotQQresidualsRegression <- function(res=NULL, xlab="Theoretical Quantiles", ylab= "Standardized Residuals", dontPlotData=FALSE, cexPoints= 1.3, cexXAxis= 1.3, cexYAxis= 1.3, lwd= 2, lwdAxis=1.2) {

	op <- par(mar= c(5.6, 7, 4, 4) + 0.1, las=1, xpd=FALSE)

	if (dontPlotData) {

		plot(1, type='n', xlim=0:1, ylim=0:1, bty='n', axes=FALSE, xlab="", ylab="")

		axis(1, at=0:1, labels=FALSE, cex.axis=cexXAxis, lwd=lwdAxis, xlab="")
		axis(2, at=0:1, labels=FALSE, cex.axis=cexYAxis, lwd=lwdAxis, ylab="")
		mtext(text = xlab, side = 1, cex=1.5, line = 2.9)
		mtext(text = ylab, side = 2, cex=1.5, line = 3.25, las=0)

		return()
	}

	d <- data.frame(qqnorm(res, plot.it=FALSE))
	d <- na.omit(d)
	xVar <- d$x
	yVar <- d$y

	xlow <- min((min(xVar) - 0.1* min(xVar)), min(pretty(xVar)))
	xhigh <- max((max(xVar) + 0.1* max(xVar)), max(pretty(xVar)))
	xticks <- pretty(c(xlow, xhigh))
	ylow <- min((min(yVar) - 0.1* min(yVar)), min(pretty(yVar)))
	yhigh <- max((max(yVar) + 0.1* max(yVar)), max(pretty(yVar)))

	yticks <- pretty(c(ylow, yhigh))

	yLabs <- vector("character", length(yticks))

	for (i in seq_along(yticks)) {

		if (yticks[i] < 10^6) {

			yLabs[i] <- format(yticks[i], digits= 3, scientific = FALSE)

		} else {

			yLabs[i] <- format(yticks[i], digits= 3, scientific = TRUE)
		}
	}

	plot(1, type="n", ylab="", xlab="", axes=FALSE, ylim= range(yticks), xlim= range(xticks))
	plotrix::ablineclip(a=0, b=1, x1=min(xticks), x2=max(xticks), y1=min(yticks), y2=max(yticks), lwd=lwd, col="darkred")
	points(xVar, yVar, pch=21, bg = "grey", cex= cexPoints)

	axis(1, line= 0.4, labels= xticks, at= xticks, cex.axis= cexXAxis, lwd=lwdAxis)
	axis(2, line= 0.2, labels= yLabs, at= yticks, cex.axis= cexYAxis, lwd=lwdAxis, las=1)

	maxYlab <- max(nchar(yLabs))
	distLab <- maxYlab / 1.8
	mtext(text = xlab, side = 1, cex=1.5, line = 2.9)
	mtext(text = ylab, side = 2, cex=1.5, line = distLab + 2.1, las=0)

	par(op)

}
