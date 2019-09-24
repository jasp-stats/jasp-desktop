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

BainAncovaBayesian	 <- function(jaspResults, dataset, options, ...) {

	### READY ###
	ready <- options[["dependent"]] != "" && options[["fixedFactors"]] != ""  && !is.null(unlist(options[["covariates"]]))
	
	### READ DATA ###
	readList <- .readDataBainAncova(options, dataset)
	dataset <- readList[["dataset"]]
	missingValuesIndicator <- readList[["missingValuesIndicator"]]
	
	bainContainer <- .bainGetContainer(jaspResults, deps=c("dependent", "fixedFactors", "covariates", "model"))
	
	### LEGEND ###
	.bainLegendAncova(dataset, options, jaspResults)
	
	### RESULTS ###
	.bainAncovaResultsTable(dataset, options, bainContainer, missingValuesIndicator, ready)
	
	### BAYES FACTOR MATRIX ###
	.bainBayesFactorMatrix(dataset, options, bainContainer, ready, type = "ancova")
	
	### COEFFICIENTS ###
	.bainAncovaCoefficientsTable(dataset, options, bainContainer, ready)
	
	### BAYES FACTOR PLOT ###
	.bainAnovaBayesFactorPlots(dataset, options, bainContainer, ready)
	
	### DESCRIPTIVES PLOT ###
	.bainAnovaDescriptivesPlot(dataset, options, bainContainer, ready, type = "ancova")
}

.bainAncovaResultsTable <- function(dataset, options, bainContainer, missingValuesIndicator, ready) {

	if (!is.null(bainContainer[["bainTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

	variables <- c(options$dependent, options$fixedFactors, unlist(options$covariates))
	bainTable <- createJaspTable("Bain ANCOVA")

	bainTable$addColumnInfo(name="hypotheses", 		type="string", title="")
	bainTable$addColumnInfo(name="BF", 						type="number", title= "BF.c")
	bainTable$addColumnInfo(name="PMP1", 					type="number", title= "PMP a")
	bainTable$addColumnInfo(name="PMP2", 					type="number", title= "PMP b")
	bainTable$position <- 1

	message <-  "BF.c denotes the Bayes factor of the hypothesis in the row versus its complement.
				Posterior model probabilities (a: excluding the unconstrained hypothesis, b: including the unconstrained hypothesis) are based on equal prior model probabilities."
	bainTable$addFootnote(message=message)

	bainTable$addCitation(.bainGetCitations())
	
	bainContainer[["bainTable"]] <- bainTable

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

	groupCol <- dataset[ , .v(options[["fixedFactors"]])]
	varLevels <- levels(groupCol)

	if (length(varLevels) > 15) {
		bainTable$setError("The fixed factor has too many levels for a Bain analysis.")
		return()
	}

	if (options$model == "") {
		# We have to make a default matrix depending on the levels of the grouping variable...meh
		# The default hypothesis is that all groups are equal (e.g., 3 groups, "p1=p2=p3")
		len <- length(varLevels)
		null.mat <- matrix(0, nrow = (len-1), ncol = (len+1))
		indexes <- row(null.mat) - col(null.mat)
		null.mat[indexes == 0] <- 1
		null.mat[indexes == -1] <- -1
		ERr <- null.mat
	  IRr <- NULL

		p <- try({
			bainResult <- Bain::Bain_ancova(X = dataset, dep_var = .v(options[["dependent"]]), covariates = .v(options[["covariates"]]), group = .v(options[["fixedFactors"]]), ERr, IRr)
			bainContainer[["bainResult"]] <- createJaspState(bainResult)
		})

	} else {

		rest.string <- .bainCleanModelInput(options$model)

		inpt <- list()
		names(dataset) <- .unv(names(dataset))
		inpt[[1]] <- dataset
		inpt[[2]] <- options[["dependent"]]
		inpt[[3]] <- options[["covariates"]]
		inpt[[4]] <- options[["fixedFactors"]]
		inpt[[5]] <- rest.string

		p <- try({
			bainResult <- Bain::Bain_ancova_cm(X = inpt[[1]], dep_var = inpt[[2]], covariates = inpt[[3]], group = inpt[[4]], hyp = inpt[[5]])
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

.bainBayesFactorMatrix <- function(dataset, options, bainContainer, ready, type) {

	if (!is.null(bainContainer[["bayesFactorMatrix"]]) || !options[["bayesFactorMatrix"]]) return() #The options for this table didn't change so we don't need to rebuild it

	bayesFactorMatrix <- createJaspTable("Bayes Factor Matrix")
	bayesFactorMatrix$position <- 3

	if (type == "regression")
		bayesFactorMatrix$dependOn(options = c("bayesFactorMatrix", "covariates", "standardized"))
	if (type == "ancova")
		bayesFactorMatrix$dependOn(options = c("bayesFactorMatrix", "fixedFactors", "covariates"))
	if (type == "anova")
		bayesFactorMatrix$dependOn(options = c("bayesFactorMatrix", "fixedFactors"))
		
	bayesFactorMatrix$addColumnInfo(name = "hypothesis", title = "", type = "string")
	bayesFactorMatrix$addColumnInfo(name = "H1", type = "number")
	
	bainContainer[["bayesFactorMatrix"]] <- bayesFactorMatrix

	if (!ready || bainContainer$getError()) {
		row <- data.frame(hypothesis = "H1", H1 = ".")
		bayesFactorMatrix$addRows(row)
		return()
	}

	bainResult <- bainContainer[["bainResult"]]$object

	BFmatrix <- diag(1, length(bainResult$BF))
	for (h1 in 1:length(bainResult$BF)) {
		for (h2 in 1:length(bainResult$BF)) {
			BFmatrix[h1, h2] <- bainResult$fit[h1]/bainResult$fit[h2]/(bainResult$complexity[h1]/bainResult$complexity[h2])
		}
	}

	if (nrow(BFmatrix) > 1) {
		for (i in 2:nrow(BFmatrix))
			bayesFactorMatrix$addColumnInfo(name = paste0("H", i), type = "number")
	}

	for (i in 1:nrow(BFmatrix)) {
		tmp <- list(hypothesis = paste0("H", i))
		for (j in 1:ncol(BFmatrix)) {
			tmp[[paste0("H", j)]] <- BFmatrix[i,j]
		}
		row <- tmp
		bayesFactorMatrix$addRows(row)
	}
}

.bainAncovaCoefficientsTable <- function(dataset, options, bainContainer, ready) {

	if (!is.null(bainContainer[["coefficientsTable"]]) || !options[["coefficients"]]) return()
	
	coefficientsTable <- createJaspTable("Coefficients for Groups plus Covariates")
	coefficientsTable$dependOn(options="coefficients")
	coefficientsTable$position <- 2

	coefficientsTable$addColumnInfo(name="v",				title="Covariate",		type="string")
	coefficientsTable$addColumnInfo(name="N",				title="N",						type="integer")
	coefficientsTable$addColumnInfo(name="mean",		title="Coefficient",	type="number")
	coefficientsTable$addColumnInfo(name="SE",			title="SE",						type="number")
	coefficientsTable$addColumnInfo(name="CiLower",	title="lower",			type="number", overtitle="95% Credible Interval")
	coefficientsTable$addColumnInfo(name="CiUpper",	title="upper",			type="number", overtitle="95% Credible Interval")

	bainContainer[["coefficientsTable"]] <- coefficientsTable
	
	if (!ready || bainContainer$getError())
		return()

	bainResult <- bainContainer[["bainResult"]]$object

	sum_model <- bainResult$estimate_res
	covcoef <- data.frame(sum_model$coefficients)
	SEs <- summary(sum_model)$coefficients[, 2]

	rownames(covcoef) <- gsub("groupf", "", rownames(covcoef))
	x <- rownames(covcoef)
	x <- sapply(regmatches(x, gregexpr("covars", x)), length)
	x <- sum(x)
	if (x > 1) {
			rownames(covcoef)[(length(rownames(covcoef)) - (x-1)):length(rownames(covcoef))] <- options$covariates
	} else {
			rownames(covcoef) <- gsub("covars", options$covariates, rownames(covcoef))
	}
	# mucho fixo

	groups <- rownames(covcoef)
	estim <- covcoef[, 1]
	CiLower <- estim - 1.96 * SEs
	CiUpper <- estim + 1.96 * SEs

	groupCol <- dataset[ , .v(options[["fixedFactors"]])]
	varLevels <- levels(groupCol)
	varLevels <- levels(groupCol)

	N <- NULL

	for (variable in varLevels) {
		column <- dataset[ , .v(options$dependent)]
		column <- column[which(groupCol == variable)]
		N <- c(N,length(column))
	}

	covVars <- options$covariates
	covVars <- unlist(covVars)

	for (var in covVars) {
		col <- dataset[ , .v(var)]
		col <- na.omit(col)
		N <- c(N, length(col))
	}

	for (i in 1:length(groups)) {
		row <- data.frame(v = groups[i], mean = estim[i], N = N[i], SE = SEs[i], CiLower = CiLower[i], CiUpper = CiUpper[i])
		coefficientsTable$addRows(row)
	}
}

.readDataBainAncova <- function(options, dataset) {
	numeric.variables	<- c(unlist(options$dependent),unlist(options$covariates))
	numeric.variables	<- numeric.variables[numeric.variables != ""]
	factor.variables	<- unlist(options$fixedFactors)
	factor.variables	<- factor.variables[factor.variables != ""]
	all.variables			<- c(numeric.variables, factor.variables)
	if (is.null(dataset)) {
		trydata									<- .readDataSetToEnd(columns.as.numeric=all.variables)
		missingValuesIndicator	<- .unv(names(which(apply(trydata, 2, function(x) { any(is.na(x))} ))))
		dataset									<- .readDataSetToEnd(columns.as.numeric=numeric.variables, columns.as.factor=factor.variables, exclude.na.listwise=all.variables)
	} else {
		dataset									<- .vdf(dataset, columns.as.numeric=numeric.variables, columns.as.factor=factor.variables)
	}
	.hasErrors(dataset, perform, type=c("infinity", "variance", "observations"),
				all.target=all.variables, observations.amount="< 3",
				exitAnalysisIfErrors = TRUE)
	readList <- list()
  readList[["dataset"]] <- dataset
  readList[["missingValuesIndicator"]] <- missingValuesIndicator
  return(readList)
}

.bainLegendAncova <- function(dataset, options, jaspResults) {

	if (!is.null(jaspResults[["legendTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

		legendTable <- createJaspTable("Hypothesis Legend")
		legendTable$dependOn(options =c("model", "fixedFactors"))
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
		} else if (options$fixedFactors != "") {
			factor <- options$fixedFactors
			fact <- dataset[, .v(factor)]
			levels <- levels(fact)
			string <- paste(paste(factor, levels, sep = "."), collapse = " = ")
			row <- list(number = "H1", hypothesis = string)
			legendTable$addRows(row)
		}
}

.plot.BainA <- function(x, y, ...)
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
            			ggplot2::labs(x = "", y = "", title = "PMP") + ggplot2::theme(panel.grid = ggplot2::element_blank(),
                          			legend.position = "none") + ggplot2::scale_y_continuous(
																	breaks = cumsum(rev(PMPb)) - rev(PMPb)/2,
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
            			ggplot2::labs(x = "", y = "", title = "PMP excluding Hu", size = 30) +
            			ggplot2::theme(panel.grid = ggplot2::element_blank(), legend.position = "none") +
            			ggplot2::scale_y_continuous(
										breaks = cumsum(rev(PMPa)) - rev(PMPa)/2,
                  	labels = rev(P_lables))
        p1 <- p1 + ggplot2::theme(panel.background = ggplot2::element_blank(),
                         axis.text=ggplot2::element_text(size=17, color = "black"),
												 plot.title = ggplot2::element_text(size=18, hjust = .5),
											 	 axis.ticks.y = ggplot2::element_blank())
				p1 <- p1 + ggplot2::scale_fill_brewer(palette="Set1")

        p <- ggplot2::ggplot(data = ggdata2, mapping = ggplot2::aes(x = "",
																																		y = PMP,
                          																					fill = lab)) +
						ggplot2::geom_bar(stat = "identity", width = 1e10, color = "black", size = 1) +
						ggplot2::geom_col()
        p2 <- p + ggplot2::coord_polar(theta = "y", direction = -1) +
            			ggplot2::labs(x = "", y = "",
																title = "PMP including Hu", size = 30) +
            			ggplot2::theme(panel.grid = ggplot2::element_blank(),
																legend.position = "none") +
            			ggplot2::scale_y_continuous(
											breaks = cumsum(rev(PMPb)) - rev(PMPb)/2,
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
