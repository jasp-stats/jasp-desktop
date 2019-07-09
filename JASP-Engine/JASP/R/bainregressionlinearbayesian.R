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

BainRegressionLinearBayesian <- function(jaspResults, dataset, options, ...) {

	### READY ###
	ready <- (options[["dependent"]] != "" && unlist(options[["covariates"]]) != "" && !is.null(unlist(options[["covariates"]])))
	
	### READ DATA ###
	readList <- .readDataBainLinearRegression(options, dataset)
	dataset <- readList[["dataset"]]
	missingValuesIndicator <- readList[["missingValuesIndicator"]]
	
	bainContainer <- .bainGetContainer(jaspResults, deps=c("dependent", "covariates", "model", "standardized"))

	### LEGEND ###
	.bainLegendRegression(dataset, options, jaspResults)
	
	### RESULTS ###
	.bainLinearRegressionResultsTable(dataset, options, bainContainer, missingValuesIndicator, ready)
	
	### COEFFICIENTS ###
	.bainLinearRegressionCoefficientsTable(dataset, options, bainContainer, ready)
	
	### BAYES FACTOR MATRIX ###
	.bainBayesFactorMatrix(dataset, options, bainContainer, ready, type = "regression")
	
	### BAYES FACTOR PLOT ###
	.bainLinearRegressionBayesFactorPlots(dataset, options, bainContainer, ready)

}

.bainLinearRegressionResultsTable <- function(dataset, options, bainContainer, missingValuesIndicator, ready) {

	if (!is.null(bainContainer[["bainTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

	variables <- c(options[["dependent"]], unlist(options[["covariates"]]))

	bainTable <- createJaspTable("Bain Linear Regression")
	bainContainer[["bainTable"]] <- bainTable
	bainTable$position <- 1

	bainTable$addColumnInfo(name="hypotheses", type="string", title="")
	bainTable$addColumnInfo(name="BF", type="number", title= "BF.c")
	bainTable$addColumnInfo(name="PMP1", type="number", title="PMP a")
	bainTable$addColumnInfo(name="PMP2", type="number", title="PMP b")

	message <- "BF.c denotes the Bayes factor of the hypothesis in the row versus its complement.
				Posterior model probabilities (a: excluding the unconstrained hypothesis, b: including the unconstrained hypothesis) are based on equal prior model probabilities."
	bainTable$addFootnote(message=message)

	bainTable$addCitation(.bainGetCitations())

	if (!ready)
		return()

	if (any(variables %in% missingValuesIndicator)) {
		i <- which(variables %in% missingValuesIndicator)
		if (length(i) > 1) {
			bainTable$addFootnote(message= paste0("The variables ", paste(variables[i], collapse = ", "), " contain missing values, the rows containing these values are removed in the analysis."), symbol="<b>Warning.</b>")
		} else if (length(i) == 1) {
			bainTable$addFootnote(message= paste0("The variable ", variables[i], " contains missing values, the rows containing these values are removed in the analysis."), symbol="<b>Warning.</b>")
		}
	}

	if (options$model == "") {
		formula <- paste(options[["dependent"]], "~", paste(options[["covariates"]], collapse=' + '))
		rest.string <- paste0(paste0(options[["covariates"]], " = 0"), collapse = " & ")
		rest.string <- .bainCleanModelInput(rest.string)

		inpt <- list()
		inpt[[1]] <- formula
		names(dataset) <- .unv(names(dataset))
		inpt[[2]] <- dataset
		inpt[[3]] <- rest.string
		inpt[[4]] <- options$standardized

		p <- try({
			bainResult <- Bain::Bain_regression_cm(formula = inpt[[1]], data = inpt[[2]], hyp = inpt[[3]], standardize = inpt[[4]])
			bainContainer[["bainResult"]] <- createJaspState(bainResult)
		})

	} else {

		formula <- paste(options[["dependent"]], "~", paste(options[["covariates"]], collapse=' + '))
		rest.string <- .bainCleanModelInput(options$model)

		inpt <- list()
		inpt[[1]] <- formula
		names(dataset) <- .unv(names(dataset))
		inpt[[2]] <- dataset
		inpt[[3]] <- rest.string
		inpt[[4]] <- options$standardized

		p <- try({
			bainResult <- Bain::Bain_regression_cm(formula = inpt[[1]], data = inpt[[2]], hyp = inpt[[3]], standardize = inpt[[4]])
			bainContainer[["bainResult"]] <- createJaspState(bainResult)
		})
	}

	if (isTryError(p)) {
		bainContainer$setError(paste0("An error occurred in the analysis:<br>", .extractErrorMessage(p), "<br><br>Please double check your variables and model constraints."))
		return()
	}

	BF <- bainResult$BF
	for (i in 1:length(BF)) {
		row <- data.frame(hypotheses = paste0("H",i), BF = BF[i], PMP1 = bainResult$PMPa[i], PMP2 = bainResult$PMPb[i])
		bainTable$addRows(row)
	}
	row <- data.frame(hypotheses = "Hu", BF = "", PMP1 = "", PMP2 = 1-sum(bainResult$PMPb))
	bainTable$addRows(row)
}

.bainLinearRegressionBayesFactorPlots <- function(dataset, options, bainContainer, ready) {
	if (!is.null(bainContainer[["bayesFactorPlot"]]) || !options[["bayesFactorPlot"]]) return()

	bayesFactorPlot <- createJaspPlot(plot = NULL, title = "Bayes Factor Comparison", height = 400, width = 600)
	bayesFactorPlot$dependOn(options = "bayesFactorPlot")
	bayesFactorPlot$position <- 4

	bainContainer[["bayesFactorPlot"]] <- bayesFactorPlot

	if (!ready || bainContainer$getError())
		return()

	bainResult <- bainContainer[["bainResult"]]$object
	bayesFactorPlot$plotObject <- .suppressGrDevice(.plot.BainR(bainResult))
}

.bainLinearRegressionCoefficientsTable <- function(dataset, options, bainContainer, ready) {
	if (!is.null(bainContainer[["coefficientsTable"]]) || !options[["coefficients"]]) return()

	coefficientsTable <- createJaspTable("Coefficients")
	coefficientsTable$dependOn(options = "coefficients")
	coefficientsTable$position <- 2

	overTitle <- "95% Credible Interval"

	coefficientsTable$addColumnInfo(name="v",       title="Covariate",   type="string")
	coefficientsTable$addColumnInfo(name="mean",    title="Coefficient", type="number")
	coefficientsTable$addColumnInfo(name="SE",      title="SE",          type="number")
	coefficientsTable$addColumnInfo(name="CiLower", title="Lower",     type="number", overtitle=overTitle)
	coefficientsTable$addColumnInfo(name="CiUpper", title="Upper",     type="number", overtitle=overTitle)

	bainContainer[["coefficientsTable"]] <- coefficientsTable

	if (!ready || bainContainer$getError())
		return()

	bainResult <- bainContainer[["bainResult"]]$object
	sum_model <- bainResult[["estimate_res"]]

	if (!options[["standardized"]]) {
		covcoef <- data.frame(sum_model[["coefficients"]])
		groups <- rownames(covcoef)
		estim <- summary(sum_model)$coefficients[, 1]
		SE <- summary(sum_model)$coefficients[, 2]
		CiLower <- estim - (1.96 * SE)
		CiUpper <- estim + (1.96 * SE)
	} else {
		covcoef <- data.frame(sum_model$CIs)
		groups <- .v(options$covariates)
		estim <- covcoef[, 2]
		SE <- sum_model$SEs
		CiLower <- covcoef[, 1]
		CiUpper <- covcoef[, 3]
	}
	for (i in 1:length(estim)) {
		if (i == 1 && !options$standardized) {
			row <- data.frame(v = groups[i], mean = estim[i], SE = SE[i], CiLower = CiLower[i], CiUpper = CiUpper[i])
			coefficientsTable$addRows(row)
		} else {
			row <- data.frame(v = .unv(groups[i]), mean = estim[i], SE = SE[i], CiLower = CiLower[i], CiUpper = CiUpper[i])
			coefficientsTable$addRows(row)
		}
	}
}

.readDataBainLinearRegression <- function(options, dataset) {
	all.variables <- c(options$dependent, unlist(options$covariates))
	all.variables <- all.variables[all.variables != ""]
	if (is.null(dataset)) {
		trydata <- .readDataSetToEnd(columns.as.numeric=all.variables)
		missingValuesIndicator <- .unv(names(which(apply(trydata, 2, function(x) { any(is.na(x))} ))))
		dataset <- .readDataSetToEnd(columns.as.numeric=all.variables, exclude.na.listwise=all.variables)
	} else {
		dataset <- .vdf(dataset, columns.as.numeric=all.variables)
	}
	.hasErrors(dataset, perform, type=c("infinity", "variance", "observations"),
				all.target=all.variables, observations.amount="< 3",
				exitAnalysisIfErrors = TRUE)
	readList <- list()
  readList[["dataset"]] <- dataset
  readList[["missingValuesIndicator"]] <- missingValuesIndicator
  return(readList)
}

.bainLegendRegression <- function(dataset, options, jaspResults) {
	if (!is.null(jaspResults[["legendTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

	legendTable <- createJaspTable("Hypothesis Legend")
	legendTable$dependOn(options =c("model", "covariates"))
	legendTable$position <- 0
	legendTable$addColumnInfo(name="number", type="string", title="Abbreviation")
	legendTable$addColumnInfo(name="hypothesis", type="string", title="Hypothesis")

	jaspResults[["legendTable"]] <- legendTable

	if (options$model != "") {
		rest.string <- .bainCleanModelInput(options$model)
		hyp.vector <- unlist(strsplit(rest.string, "[;]"))

			for (i in 1:length(hyp.vector)) {
				row <- list(number = paste0("H",i), hypothesis = hyp.vector[i])
				legendTable$addRows(row)
			}
	} else {
		variables <- options$covariates
		if (length(variables) == 0) {
			string <- ""
			row <- list(number = "H1", hypothesis = string)
			legendTable$addRows(row)
		} else if (length(variables) == 1) {
			string <- paste(variables, "= 0")
			row <- list(number = "H1", hypothesis = string)
			legendTable$addRows(row)
		} else {
			string <- paste0(paste0(variables, " = 0"), collapse = " & ")
			row <- list(number = "H1", hypothesis = string)
			legendTable$addRows(row)
		}
	}
}

.plot.BainR <- function(x, y, ...)
{
    PMPa <- x$PMPa
    PMPb <- c(x$PMPb, 1 - sum(x$PMPb))
    numH <- length(x$BF)
    P_lables <- paste("H", 1:numH, sep = "")
    ggdata1 <- data.frame(lab = P_lables, PMP = PMPa)
    ggdata2 <- data.frame(lab = c(P_lables, "Hu"), PMP = PMPb)
    if (numH == 1) {
        p <- ggplot2::ggplot(data = ggdata2, mapping = ggplot2::aes(x = "", y = PMP,
                            fill = lab)) +
						ggplot2::geom_bar(stat = "identity", width = 1e10, color = "black", size = 1) +
            ggplot2::geom_col()
        pp <- p + ggplot2::coord_polar(theta = "y", direction = -1) +
            ggplot2::labs(x = "", y = "", title = "PMP") +
						ggplot2::theme(panel.grid = ggplot2::element_blank(),
                          legend.position = "none") +
						ggplot2::scale_y_continuous(breaks = cumsum(rev(PMPb)) - rev(PMPb)/2,
																				labels = rev(c(P_lables, "Hu")))
				pp <- pp + ggplot2::theme(panel.background = ggplot2::element_blank(),
																	axis.text=ggplot2::element_text(size=17, color = "black"),
																	plot.title = ggplot2::element_text(size=18, hjust = .5),
																	axis.ticks.y = ggplot2::element_blank())
				pp <- pp + ggplot2::scale_fill_brewer(palette="Set1")
        return(pp)
    }
    if (numH > 1) {
        p <- ggplot2::ggplot(data = ggdata1, mapping = ggplot2::aes(x = "", y = PMP,
                            	fill = lab)) +
						ggplot2::geom_bar(stat = "identity", width = 1e10, color = "black", size = 1) +
            ggplot2::geom_col()
        p1 <- p + ggplot2::coord_polar(theta = "y", direction = -1) +
            ggplot2::labs(x = "", y = "", title = "PMP excluding Hu") +
            ggplot2::theme(panel.grid = ggplot2::element_blank(),
													legend.position = "none") +
            ggplot2::scale_y_continuous(breaks = cumsum(rev(PMPa)) - rev(PMPa)/2,
                               					labels = rev(P_lables))
				p1 <- p1 + ggplot2::theme(panel.background = ggplot2::element_blank(),
	                                axis.text=ggplot2::element_text(size=17, color = "black"),
	                                plot.title = ggplot2::element_text(size=18, hjust = .5),
																	axis.ticks.y = ggplot2::element_blank())
				p1 <- p1 + ggplot2::scale_fill_brewer(palette="Set1")
        p <- ggplot2::ggplot(data = ggdata2, mapping = ggplot2::aes(x = "", y = PMP,
                            	fill = lab)) +
						ggplot2::geom_bar(stat = "identity", width = 1e10, color = "black", size = 1) +
            ggplot2::geom_col()
        p2 <- p + ggplot2::coord_polar(theta = "y", direction = -1) +
            ggplot2::labs(x = "", y = "", title = "PMP including Hu") +
            ggplot2::theme(panel.grid = ggplot2::element_blank(), legend.position = "none") +
            ggplot2::scale_y_continuous(breaks = cumsum(rev(PMPb)) - rev(PMPb)/2,
                               					labels = rev(c(P_lables, "Hu")))
				p2 <- p2 + ggplot2::theme(panel.background = ggplot2::element_blank(),
	                                axis.text=ggplot2::element_text(size=17, color = "black"),
	                                plot.title = ggplot2::element_text(size=18, hjust = .5),
																	axis.ticks.y = ggplot2::element_blank())
				p2 <- p2 + ggplot2::scale_fill_brewer(palette="Set1")
        pp <- gridExtra::grid.arrange(gridExtra::arrangeGrob(p1, p2, ncol = 2))
        return(pp)
    }
}
