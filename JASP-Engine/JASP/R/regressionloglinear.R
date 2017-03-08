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

RegressionLogLinear <- function(dataset, options, perform="run", callback, ...) {

	counts.var <- options$counts
	if (counts.var == "")
		counts.var <- NULL

	if (is.null(dataset)) {

		if (perform == "run") {

			dataset <- .readDataSetToEnd(columns.as.factor=options$factors, columns.as.numeric=counts.var)

		} else {

			dataset <- .readDataSetHeader(columns.as.factor=options$factors, columns.as.numeric=counts.var)
		}
	}

	list.of.errors <- list()
	error.message <- NULL

	if (options$counts != "" && perform == "run") {
		variable.names <- NULL
		for (counts in options$counts) {
			if(any(is.na(dataset [[.v (options$counts)]])))
				variable.names <- c (variable.names, options$counts)
			}

		if ( !is.null (variable.names))
			error.message <- paste ("Poisson glm is undefined -- the count variable ", variable.names, " contain(s) empty cell or NaN. ", sep = "")
		list.of.errors[[ length(list.of.errors) + 1 ]] <- error.message

		if (length(list.of.errors)==0 ){
			variable.names <- NULL
			for (counts in options$counts) {
				if (any (!is.finite (dataset [[.v (options$counts)]])) || any  (dataset [[.v (options$counts)]] < 0 ))
					variable.names <- c (variable.names, options$counts)
			}

			if ( !is.null (variable.names))
				error.message <- paste ("Poisson glm is undefined -- the count variable ", variable.names, " contain(s) infinity and/or negative numbers. ", sep = "")
				list.of.errors[[ length(list.of.errors) + 1 ]] <- error.message
			}
		}

	if (options$counts == ""){
	 	dataset <- plyr::count(dataset)
	 } else {
	 	dataset <- dataset
	 }


	if ( perform == "run" && length(list.of.errors)==0  ) {
		variable.names <- NULL
		for (factor in options$factors) {
			if(any(is.na(dataset[.v(factor)])))
			variable.names <- c (variable.names, factor)
			}

		if ( !is.null (variable.names))
		error.message <- "Poisson glm is undefined -- the factors contain(s) empty cell or NaN or incomplete contingency table."
		list.of.errors[[ length(list.of.errors) + 1 ]] <- error.message
	}

	results <- list()

	meta <- list()
	.meta <-  list(
		list(name = "title", type = "title"),
		list(name = "table", type = "table"),
		list(name = "logregressionanova", type = "table"),
		list(name = "logregression", type = "table")
		)

	results[[".meta"]] <- .meta

	results[["title"]] <- "Log-Linear Regression"

	.LogLinearCitations <- 	list(
		"R Core Team (2015). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/."
		)


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

		print (dependent.base64)

	 if (length(options$modelTerms) > 0) {

		if (length(variables.in.model) > 0 ) {

			model.definition <- paste(dependent.base64, "~", paste(independent.base64, collapse = "+"))

		}  else {

			model.definition <- NULL #this model has no parameters
		}

		if (perform == "run" && !is.null(model.definition)&& length(list.of.errors) == 0 ) {

			model.formula <- as.formula(model.definition)

			if (options$counts == ""){
	 			names(dataset)[names(dataset)== "freq"]<- dependent.base64
			}

			loglm.fit <- try( stats::glm( model.formula, family = poisson(), data = dataset), silent = TRUE)

			if ( class(loglm.fit) == "glm") {

				loglm.model <- list(loglm.fit = loglm.fit, variables = variables.in.model)

			} else {
				if (inherits(loglm.fit, "try-error")) {
				error <- .extractErrorMessage (loglm.fit)}
				list.of.errors[[ length(list.of.errors) + 1 ]]  <- error
				loglm.model <- list(loglm.fit = NULL, variables = variables.in.model)
			}

		} else {

			loglm.model <- list(loglm.fit = NULL, variables = variables.in.model)
		}

	} else {
		loglm.model <- empty.model
	}

	################################################################

		logregressionanova <- list()
		logregressionanova[["title"]] <- "ANOVA"
		logregressionanova[["citation"]] <- .LogLinearCitations

		footnotes <- .newFootnotes()
		# Declare table elements
		fields <- list(
			list(name = "Name", title = "  ", type = "string"),
			list(name = "df", title = "df", type="integer"),
			list(name = "Deviance",title = "Deviance", type="number", format = "sf:4;dp:3"),
			list(name = "Residual df", type="integer"),
			list(name = "Residual Deviance", type="number", format = "sf:4;dp:3"),
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
			"Name" = "",
			"Df" = "",
			"Deviance" = "",
			"Residual df" = "",
			"Residual Deviance" = "",
			"p"="")
		if (options$VovkSellkeMPR) {
			empty.line[["VovkSellkeMPR"]] <- ""
		}

		dotted.line <- list( #for empty tables
			"Name" = ".",
			"Df" = ".",
			"Deviance" = ".",
			"Residual df" = ".",
			"Residual Deviance" = ".",
			"p"=".")
		if (options$VovkSellkeMPR) {
			dotted.line[["VovkSellkeMPR"]] <- "."
		}

		logregressionanova[["schema"]] <- list(fields = fields)

		logregressionanova.result <- list()

		if (perform == "run" && length(list.of.errors) == 0 ) {

			if ( class(loglm.model$loglm.fit) == "glm") {

				loglm.anova = anova(loglm.model$loglm.fit, test="Chisq")
				loglm.estimates <- loglm.anova
				len.logreg <- length(logregressionanova.result) + 1

				v <- 0
				null.model <- "Null model"
				if (length(loglm.model$variables) > 0) {

					variables.in.model <- loglm.model$variables
					l <- dim(loglm.estimates)[1]
					name <- dimnames(loglm.estimates)[[1]]

					for (var in 1:l) {

						logregressionanova.result[[ len.logreg ]] <- empty.line
						model.name <- .unvf(name)

						if(var==1){
							logregressionanova.result[[ len.logreg ]]$"Name" <- "NULL"
							logregressionanova.result[[ len.logreg ]]$"df" <- " "
							logregressionanova.result[[ len.logreg ]]$"Deviance" <- " "
							logregressionanova.result[[ len.logreg ]]$"p" <- " "
							if (options$VovkSellkeMPR){
							  logregressionanova.result[[ len.logreg ]]$"VovkSellkeMPR" <- " "
							}

						}else{
							logregressionanova.result[[ len.logreg ]]$"Name" <- model.name[var]
							logregressionanova.result[[ len.logreg ]]$"df" <- as.integer(loglm.estimates$Df[var])
							logregressionanova.result[[ len.logreg ]]$"Deviance" <- as.numeric(loglm.estimates$Deviance[var])
							logregressionanova.result[[ len.logreg ]]$"p" <- as.numeric(loglm.estimates$"Pr(>Chi)"[var])
							if (options$VovkSellkeMPR){
							  logregressionanova.result[[ len.logreg ]]$"VovkSellkeMPR" <- .VovkSellkeMPR(logregressionanova.result[[ len.logreg ]]$"p")
							}
						}
						logregressionanova.result[[ len.logreg ]]$"Residual df" <- as.integer(loglm.estimates$"Resid. Df"[var])
						res <- as.numeric(loglm.estimates$"Resid. Dev"[var])
						if (abs(res) < 10^(-4))
							res <- 0

						logregressionanova.result[[ len.logreg ]]$"Residual Deviance" <- res

						len.logreg <- len.logreg + 1
					}

				}

			} else {

				len.logreg <- length(logregressionanova.result) + 1
				logregressionanova.result[[ len.logreg ]] <- dotted.line

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

			}

			len.logreg <- length(logregressionanova.result) + 1
			logregressionanova.result[[ len.logreg ]] <- dotted.line
			logregressionanova.result[[ len.logreg ]]$"Model" <- 1
		}

	if (length(list.of.errors) > 1){

		loglm.fit <- try( stats::glm( model.formula, family = poisson(), data = dataset), silent = TRUE)

		if (inherits(loglm.fit, "try-error")) {
			error <- .extractErrorMessage (loglm.fit)
		}
		logregressionanova[["error"]] <- list(errorType="badData",errorMessage = error)

	} else if (length(list.of.errors) == 1){

		logregressionanova[["error"]] <- list(errorType = "badData", errorMessage = list.of.errors[[ 1 ]])
	}

  logregressionanova[["footnotes"]] <- as.list(footnotes)
	logregressionanova[["data"]] <- logregressionanova.result
	results[["Bayesianposterior"]] <- logregressionanova

	################################################################################
	#						   MODEL COEFFICIENTS TABLE   						#
	################################################################################

	if (options$regressionCoefficientsEstimates == TRUE) {

		logregression <- list()
		logregression[["title"]] <- "Coefficients"
		logregression[["citation"]] <- .LogLinearCitations
		footnotes <- .newFootnotes()

		if (options$regressionCoefficientsConfidenceIntervals == TRUE){
			ci.label <- paste(100*options$regressionCoefficientsConfidenceIntervalsInterval, "% Confidence Intervals", sep="")
		}
		#ci.label <- paste(95, "% Confidence intervals", sep="")

		# Declare table elements
		fields <- list(
			list(name = "Name", title = "  ", type = "string"),
			list(name = "Coefficient", title = "Estimate", type = "number", format = "dp:3"),
			list(name = "Standard Error", type="number", format = "dp:3"))

			if (options$regressionCoefficientsConfidenceIntervals == TRUE){
				fields <- c(fields,list(
					list(name = "Lower",overTitle=ci.label, type="number", format = "dp:3"),
					list(name = "Upper",overTitle=ci.label,type="number", format = "dp:3")))
			}

			fields <- c(fields,list(

			list(name = "Z", type="number", format = "sf:4;dp:3"),
			list(name = "p", type = "number", format = "dp:3;p:.001")))

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

		empty.line <- list(                      #for empty elements in tables when given output
			"Name" = "",
			"Coefficient" = "",
			"Standard Error" = "",
			"Lower" = "",
			"Upper" = "",
			"Z" = "",
			"p" = "")
		if (options$VovkSellkeMPR) {
			empty.line[["VovkSellkeMPR"]] <- ""
		}

		dotted.line <- list(                     #for empty tables
			"Name" = ".",
			"Coefficient" = ".",
			"Standard Error" = ".",
			"Lower" = ".",
			"Upper" = ".",
			"Z" = ".",
			"p" = ".")
		if (options$VovkSellkeMPR) {
			dotted.line[["VovkSellkeMPR"]] <- "."
		}

		lookup.table <- .regressionLogLinearBuildLookup(dataset, options$factors)
		lookup.table[["(Intercept)"]] <- "(Intercept)"

		logregression[["schema"]] <- list(fields = fields)

		logregression.result <- list()

		if (perform == "run" && length(list.of.errors) == 0) {

			if ( class(loglm.model$loglm.fit) == "glm") {

				loglm.summary   <- summary(loglm.model$loglm.fit)
				loglm.estimates <- loglm.summary$coefficients
				print(str(loglm.estimates))
				loglm.coeff <- loglm.estimates[,"Estimate"]
				loglm.estimates.SE <- loglm.estimates[,"Std. Error"]
				sig    <- options$regressionCoefficientsConfidenceIntervalsInterval
				alpha  <- (1 - sig) / 2
				lower  <- loglm.coeff+ stats::qnorm(alpha)*loglm.estimates.SE
				upper  <- loglm.coeff+ stats::qnorm(1-alpha)*loglm.estimates.SE

				print(cbind(lower,upper))

				len.logreg <- length(logregression.result) + 1

				if (length(loglm.model$variables) > 0) {

					variables.in.model <- loglm.model$variables
					coefficients <- dimnames(loglm.estimates)[[1]]
					coef <-base::strsplit (coefficients, split = ":", fixed = TRUE)

					for (i in seq_along(coef)) {

						logregression.result[[ len.logreg ]] <- empty.line

						coefficient <- coef[[i]]

						actualName<-list()
						for (j in seq_along(coefficient)){
						  actualName[[j]] <- paste(lookup.table[[ coefficient[j] ]], collapse=" = ")
						}
						var<-paste0(actualName, collapse="*")
						#print(var)

						logregression.result[[ len.logreg ]]$"Name" <- var
						logregression.result[[ len.logreg ]]$"Coefficient" <- as.numeric(unname(loglm.estimates[i,1]))
						logregression.result[[ len.logreg ]]$"Standard Error" <- as.numeric(loglm.estimates[i,2])

						if (options$regressionCoefficientsConfidenceIntervals == TRUE){
							logregression.result[[ len.logreg ]]$"Lower" <- as.numeric(lower[i])
							logregression.result[[ len.logreg ]]$"Upper" <- as.numeric(upper[i])
						}

						logregression.result[[ len.logreg ]]$"Z" <- as.numeric(loglm.estimates[i,3])
						logregression.result[[ len.logreg ]]$"p" <- as.numeric(loglm.estimates[i,4])
						if (options$VovkSellkeMPR){
						  logregression.result[[ len.logreg ]]$"VovkSellkeMPR" <- .VovkSellkeMPR(logregression.result[[ len.logreg ]]$"p")
						}
						len.logreg <- len.logreg + 1
					}
				}

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

			}

			len.logreg <- length(logregression.result) + 1
			logregression.result[[ len.logreg ]] <- dotted.line
			logregression.result[[ len.logreg ]]$"Model" <- 1

		}

		if (length(list.of.errors) > 1){
			loglm.fit <- try( stats::glm( model.formula, family = poisson(), data = dataset), silent = TRUE)

			if (inherits(loglm.fit, "try-error")) {
				error <- .extractErrorMessage (loglm.fit)}
				logregression[["error"]] <- list(errorType="badData",errorMessage = error)

		} else if (length(list.of.errors) == 1){

		logregression[["error"]] <- list(errorType = "badData", errorMessage = list.of.errors[[ 1 ]])
	}

	logregression[["data"]] <- logregression.result

	results[["logregression"]] <- logregression
	}

	#######################################################################


	logregressionanova[["data"]] <- logregressionanova.result
	results[["logregressionanova"]] <- logregressionanova


	if (perform == "init") {

		list(results=results, status="inited")

	} else {

		list(results=results, status="complete")
	}
}


.regressionLogLinearBuildLookup <- function(dataset, factors) {

	table <- list()

	for (v in factors) {

		levels <- base::levels(dataset[[ .v(v) ]])

		for (l in levels) {

			mangled.name <- paste(.v(v), l, sep="")
			actual       <- c(v, l)
			table[[mangled.name]] <- actual
		}
	}

	table
}
