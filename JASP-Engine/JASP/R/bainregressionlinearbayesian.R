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

	.bainCommonErrorCheck(dataset, options)
	
	bainContainer <- .bainGetContainer(jaspResults, deps=c("dependent", "covariates", "model", "standardized", "seed"))

	### LEGEND ###
	.bainLegendRegression(dataset, options, jaspResults, position = 0)
	
	### RESULTS ###
	.bainLinearRegressionResultsTable(dataset, options, bainContainer, missingValuesIndicator, ready, position = 1)

	### BAYES FACTOR MATRIX ###
	.bainBayesFactorMatrix(dataset, options, bainContainer, ready, type = "regression", position = 2)
	
	### COEFFICIENTS ###
	.bainLinearRegressionCoefficientsTable(dataset, options, bainContainer, ready, position = 3)
	
	### BAYES FACTOR PLOT ###
	.bainLinearRegressionBayesFactorPlots(dataset, options, bainContainer, ready, position = 4)
}

.bainLinearRegressionResultsTable <- function(dataset, options, bainContainer, missingValuesIndicator, ready, position) {

	if (!is.null(bainContainer[["bainTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

	variables <- c(options[["dependent"]], unlist(options[["covariates"]]))

  bainTable <- createJaspTable(gettext("Bain Linear Regression"))
	bainContainer[["bainTable"]] <- bainTable
	bainTable$position <- position

	bainTable$addColumnInfo(name="hypotheses", type="string", title="")
  bainTable$addColumnInfo(name="BF",         type="number", title=gettext("BF.c"))
  bainTable$addColumnInfo(name="PMP1",       type="number", title=gettext("PMP a"))
  bainTable$addColumnInfo(name="PMP2",       type="number", title=gettext("PMP b"))

  message <- gettext("BF.c denotes the Bayes factor of the hypothesis in the row versus its complement.\
Posterior model probabilities (a: excluding the unconstrained hypothesis, b: including the unconstrained hypothesis) are based on equal prior model probabilities.")
	bainTable$addFootnote(message=message)

	bainTable$addCitation(.bainGetCitations())

	if (!ready)
		return()

	if (any(variables %in% missingValuesIndicator)) {
		i <- which(variables %in% missingValuesIndicator)
		if (length(i) > 1) {
      bainTable$addFootnote(message= gettextf("The variables %s contain missing values, the rows containing these values are removed in the analysis.", paste(variables[i], collapse = ", ")), symbol=gettext("<b>Warning.</b>"))
		} else if (length(i) == 1) {
      bainTable$addFootnote(message= gettextf("The variable %s contains missing values, the rows containing these values are removed in the analysis.", variables[i]), symbol=gettext("<b>Warning.</b>"))
		}
	}

	if (options$model == "") {
		rest.string <- NULL
	} else {
		rest.string <- .v(.bainCleanModelInput(options[["model"]]))
	}

	p <- try({
		bainResult <- bain:::bain_regression_cran(X = dataset, dep = .v(options[["dependent"]]), pred = paste(.v(options[["covariates"]]), collapse = " "), hyp = rest.string, std = options[["standardized"]], seed = options[["seed"]])
		bainContainer[["bainResult"]] <- createJaspState(bainResult)
	})

	if (isTryError(p)) {
    bainContainer$setError(gettextf("An error occurred in the analysis:<br>%s<br><br>Please double check your variables and model constraints.", .unv(.extractErrorMessage(p))))
		return()
	}

	for (i in 1:(length(bainResult$fit$BF)-1)) {
    row <- list(hypotheses = gettextf("H%i",i), BF = bainResult$fit$BF[i], PMP1 = bainResult$fit$PMPa[i], PMP2 = bainResult$fit$PMPb[i])
		bainTable$addRows(row)
	}
  row <- list(hypotheses = gettext("Hu"), BF = "", PMP1 = "", PMP2 = bainResult$fit$PMPb[length(bainResult$fit$BF)])
	bainTable$addRows(row) 
}

.bainLinearRegressionBayesFactorPlots <- function(dataset, options, bainContainer, ready, position) {
	
	if (!is.null(bainContainer[["bayesFactorPlot"]]) || !options[["bayesFactorPlot"]]) return()

	if(options[["model"]] == ""){
		height <- 300
		width <- 400
	} else {
		height <- 400
		width <- 600
	}

  bayesFactorPlot <- createJaspPlot(plot = NULL, title = gettext("Posterior Probabilities"), height = height, width = width)
	bayesFactorPlot$dependOn(options = c("bayesFactorPlot"))
	bayesFactorPlot$position <- position

	bainContainer[["bayesFactorPlot"]] <- bayesFactorPlot

	if (!ready || bainContainer$getError())
		return()

	bainResult <- bainContainer[["bainResult"]]$object
	bayesFactorPlot$plotObject <- .suppressGrDevice(.plot_bain_regression_cran(bainResult))
}

.bainLinearRegressionCoefficientsTable <- function(dataset, options, bainContainer, ready, position) {
	
	if (!is.null(bainContainer[["coefficientsTable"]]) || !options[["coefficients"]]) return()

  coefficientsTable <- createJaspTable(gettext("Coefficients"))
	coefficientsTable$dependOn(options = c("coefficients", "CredibleInterval"))
	coefficientsTable$position <- position

  overTitle <- gettextf("%i%% Credible Interval", round(options[["CredibleInterval"]] * 100))

  coefficientsTable$addColumnInfo(name="v",       title=gettext("Covariate"),   type="string")
  coefficientsTable$addColumnInfo(name="N",     	title=gettext("N"), 	 	 	    type="integer")
  coefficientsTable$addColumnInfo(name="mean",    title=gettext("Coefficient"), type="number")
  coefficientsTable$addColumnInfo(name="SE",      title=gettext("SE"),          type="number")
  coefficientsTable$addColumnInfo(name="CiLower", title=gettext("Lower"),     	type="number", overtitle = overTitle)
  coefficientsTable$addColumnInfo(name="CiUpper", title=gettext("Upper"),     	type="number", overtitle = overTitle)

	if(options[["standardized"]])
    coefficientsTable$addFootnote(message = gettext("The displayed coefficients are standardized."))
	
	bainContainer[["coefficientsTable"]] <- coefficientsTable

	if (!ready || bainContainer$getError())
		return()

	bainResult <- bainContainer[["bainResult"]]$object
	bainSummary <- summary(bainResult, ci = options[["CredibleInterval"]])
	
	# Extract names, mean and n from bain result
	groups <- .unv(bainSummary[["Parameter"]])
	N <- bainSummary[["n"]]
	mu <- bainSummary[["Estimate"]]
	CiLower <- bainSummary[["lb"]]
	CiUpper <- bainSummary[["ub"]]

	# Standard error according to bain package
	se <- sqrt(diag(bainResult$posterior))

	row <- data.frame(v = groups, N = N, mean = mu, SE = se, CiLower = CiLower, CiUpper = CiUpper)
	coefficientsTable$addRows(row)
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
	readList <- list()
  readList[["dataset"]] <- dataset
  readList[["missingValuesIndicator"]] <- missingValuesIndicator
  return(readList)
}

.bainLegendRegression <- function(dataset, options, jaspResults, position) {
	
	if (!is.null(jaspResults[["legendTable"]])) return()

	legendTable <- createJaspTable("Hypothesis Legend")
	legendTable$dependOn(options =c("model", "covariates"))
	legendTable$position <- position
  legendTable$addColumnInfo(name="number",     type="string", title="")
  legendTable$addColumnInfo(name="hypothesis", type="string", title=gettext("Hypothesis"))

	jaspResults[["legendTable"]] <- legendTable

	if (options$model != "") {
		rest.string <- .bainCleanModelInput(options$model)
		hyp.vector <- unlist(strsplit(rest.string, "[;]"))

			for (i in 1:length(hyp.vector)) {
        row <- list(number = gettextf("H%i",i), hypothesis = hyp.vector[i])
				legendTable$addRows(row)
			}
	} else {
		variables <- options$covariates
		if (length(variables) == 0) {
			string <- ""
      row <- list(number = gettext("H1"), hypothesis = string)
			legendTable$addRows(row)
		} else if (length(variables) == 1) {
			string <- paste(variables, "= 0")
      row <- list(number = gettext("H1"), hypothesis = string)
			legendTable$addRows(row)
		} else {
			string <- paste0(paste0(variables, " = 0"), collapse = " & ")
      row <- list(number = gettext("H1"), hypothesis = string)
			legendTable$addRows(row)
		}
	}
}

.plot_bain_regression_cran <- function(x)
{
  PMPa <- na.omit(x$fit$PMPa)
  PMPb <- x$fit$PMPb
  numH <- length(PMPa)
  P_lables <- paste(gettext("H"), 1:numH, sep = "")
  ggdata1 <- data.frame(lab = P_lables, PMP = PMPa)
  ggdata2 <- data.frame(lab = c(P_lables, gettext("Hu")), PMP = PMPb)
  
  if (numH == 1) {
    
    p <- ggplot2::ggplot(data = ggdata2, mapping = ggplot2::aes(x = "", y = PMP, fill = lab)) +
          ggplot2::geom_bar(stat = "identity", width = 1e10, color = "black", size = 1) +
          ggplot2::geom_col() + 
          ggplot2::coord_polar(theta = "y", direction = -1) +
          ggplot2::labs(x = "", y = "", title = "") +
          ggplot2::theme(panel.grid = ggplot2::element_blank(), legend.position = "none") +
          ggplot2::scale_y_continuous(breaks = cumsum(rev(PMPb)) - rev(PMPb)/2, labels = rev(c(P_lables, "Hu"))) +
          ggplot2::theme(panel.background = ggplot2::element_blank(),
                          axis.text=ggplot2::element_text(size=17, color = "black"),
                          plot.title = ggplot2::element_text(size=18, hjust = .5),
                          axis.ticks.y = ggplot2::element_blank()) +
          ggplot2::scale_fill_brewer(palette="Set1")
    
    return(p)
    
  } else if (numH > 1) {
    
    p1 <- ggplot2::ggplot(data = ggdata1, mapping = ggplot2::aes(x = "", y = PMP, fill = lab)) +
          ggplot2::geom_bar(stat = "identity", width = 1e10, color = "black", size = 1) +
          ggplot2::geom_col() +
          ggplot2::coord_polar(theta = "y", direction = -1) +
          ggplot2::labs(x = "", y = "", title = gettext("Excluding Hu")) +
          ggplot2::theme(panel.grid = ggplot2::element_blank(),legend.position = "none") +
          ggplot2::scale_y_continuous(breaks = cumsum(rev(PMPa)) - rev(PMPa)/2, labels = rev(P_lables)) +            
          ggplot2::theme(panel.background = ggplot2::element_blank(),
                              axis.text=ggplot2::element_text(size=17, color = "black"),
                              plot.title = ggplot2::element_text(size=18, hjust = .5),
                              axis.ticks.y = ggplot2::element_blank()) +
          ggplot2::scale_fill_brewer(palette="Set1")
    
    p2 <- ggplot2::ggplot(data = ggdata2, mapping = ggplot2::aes(x = "", y = PMP, fill = lab)) +
          ggplot2::geom_bar(stat = "identity", width = 1e10, color = "black", size = 1) +
          ggplot2::geom_col() + 
          ggplot2::coord_polar(theta = "y", direction = -1) +
          ggplot2::labs(x = "", y = "", title = gettext("Including Hu")) +
          ggplot2::theme(panel.grid = ggplot2::element_blank(), legend.position = "none") +
          ggplot2::scale_y_continuous(breaks = cumsum(rev(PMPb)) - rev(PMPb)/2, labels = rev(c(P_lables, "Hu"))) +
          ggplot2::theme(panel.background = ggplot2::element_blank(),
                          axis.text=ggplot2::element_text(size=17, color = "black"),
                          plot.title = ggplot2::element_text(size=18, hjust = .5),
                          axis.ticks.y = ggplot2::element_blank()) +
          ggplot2::scale_fill_brewer(palette="Set1")
    
    plotMat <- list(p1 = p1, p2 = p2)
	pp <- JASPgraphs::ggMatrixPlot(plotList = plotMat, layout = matrix(c(1, 2), ncol = 2))
	
    return(pp)
  }
}
