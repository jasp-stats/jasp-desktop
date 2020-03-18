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

	.bainCommonErrorCheck(dataset, options)
	
	bainContainer <- .bainGetContainer(jaspResults, deps=c("dependent", "fixedFactors", "covariates", "model"))
	
	### LEGEND ###
	.bainLegendAncova(dataset, options, jaspResults, position = 0)
	
	### RESULTS ###
	.bainAncovaResultsTable(dataset, options, bainContainer, missingValuesIndicator, ready, position = 1)
	
	### BAYES FACTOR MATRIX ###
	.bainBayesFactorMatrix(dataset, options, bainContainer, ready, type = "ancova", position = 2)
	
	### COEFFICIENTS ###
	.bainAnovaDescriptivesTable(dataset, options, bainContainer, ready, type = "ancova", position = 3)

	### BAYES FACTOR PLOT ###
	.bainAnovaBayesFactorPlots(dataset, options, bainContainer, ready, position = 4)

	### DESCRIPTIVES PLOT ###
	.bainAnovaDescriptivesPlot(dataset, options, bainContainer, ready, type = "ancova", position = 5)
}

.bainAncovaResultsTable <- function(dataset, options, bainContainer, missingValuesIndicator, ready, position) {

	if (!is.null(bainContainer[["bainTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

	variables <- c(options[["dependent"]], options[["fixedFactors"]], unlist(options[["covariates"]]))
	bainTable <- createJaspTable("Bain ANCOVA")

  bainTable$addColumnInfo(name="hypotheses", type="string", title="")
  bainTable$addColumnInfo(name="BF", 				 type="number", title= gettext("BF.c"))
  bainTable$addColumnInfo(name="PMP1", 			 type="number", title= gettext("PMP a"))
  bainTable$addColumnInfo(name="PMP2", 			 type="number", title= gettext("PMP b"))
	bainTable$position <- position

  message <-  gettext("BF.c denotes the Bayes factor of the hypothesis in the row versus its complement.\
Posterior model probabilities (a: excluding the unconstrained hypothesis, b: including the unconstrained hypothesis) are based on equal prior model probabilities.")
	bainTable$addFootnote(message=message)

	bainTable$addCitation(.bainGetCitations())
	
	bainContainer[["bainTable"]] <- bainTable

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

	groupCol <- dataset[ , .v(options[["fixedFactors"]])]
	varLevels <- levels(groupCol)

	if (length(varLevels) > 15) {
    bainTable$setError(gettext("The fixed factor has too many levels for a Bain analysis."))
		return()
	}

	if (options[["model"]] == "") {
		rest.string <- NULL
	} else {
		rest.string <- .v(.bainCleanModelInput(options[["model"]]))
	}

	p <- try({
		bainResult <- bain:::bain_ancova_cran(X = dataset, dep = .v(options[["dependent"]]), cov = paste(.v(options[["covariates"]]), collapse = " "), group = .v(options[["fixedFactors"]]), hyp = rest.string, seed = options[["seed"]])
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

.bainBayesFactorMatrix <- function(dataset, options, bainContainer, ready, type, position) {

	if (!is.null(bainContainer[["bayesFactorMatrix"]]) || !options[["bayesFactorMatrix"]]) return()

  bayesFactorMatrix <- createJaspTable(gettext("Bayes Factor Matrix"))
	bayesFactorMatrix$position <- position

	if (type == "regression")
		bayesFactorMatrix$dependOn(options = c("bayesFactorMatrix", "standardized", "seed"))

	if (type == "ancova" || type == "anova")
		bayesFactorMatrix$dependOn(options = c("bayesFactorMatrix", "seed"))
		
  bayesFactorMatrix$addColumnInfo(name = "hypothesis",  title = "",             type = "string")
  bayesFactorMatrix$addColumnInfo(name = "H1",          title = gettext("H1"),  type = "number")
	
	bainContainer[["bayesFactorMatrix"]] <- bayesFactorMatrix

	if (!ready || bainContainer$getError()) {
    row <- data.frame(hypothesis = gettext("H1"), H1 = ".")
		bayesFactorMatrix$addRows(row)
		return()
	}

	bainResult <- bainContainer[["bainResult"]]$object
	BFmatrix <- bainResult[["BFmatrix"]]

	if (nrow(BFmatrix) > 1) {
		for (i in 2:nrow(BFmatrix))
      bayesFactorMatrix$addColumnInfo(name = paste0("H", i), title = gettextf("H%i", i), type = "number")
	}

	for (i in 1:nrow(BFmatrix)) {
    tmp <- list(hypothesis = gettextf("H%i", i))
		for (j in 1:ncol(BFmatrix)) {
			tmp[[paste0("H", j)]] <- BFmatrix[i,j]
		}
		row <- tmp
		bayesFactorMatrix$addRows(row)
	}
}

.readDataBainAncova <- function(options, dataset) {
	numeric.variables	<- c(options[["dependent"]],unlist(options[["covariates"]]))
	numeric.variables	<- numeric.variables[numeric.variables != ""]
	factor.variables	<- options[["fixedFactors"]]
	factor.variables	<- factor.variables[factor.variables != ""]
	all.variables			<- c(numeric.variables, factor.variables)
	if (is.null(dataset)) {
		trydata									<- .readDataSetToEnd(columns.as.numeric=all.variables)
		missingValuesIndicator	<- .unv(names(which(apply(trydata, 2, function(x) { any(is.na(x))} ))))
		dataset									<- .readDataSetToEnd(columns.as.numeric=numeric.variables, columns.as.factor=factor.variables, exclude.na.listwise=all.variables)

		if(options[["fixedFactors"]] != ""){
			if(any(grepl(pattern = " ", x = levels(dataset[, .v(options[["fixedFactors"]])])))){
				JASP:::.quitAnalysis(gettext("Bain does not accept factor levels that contain spaces. Please remove the spaces from your factor levels to continue."))
			}
		}
	} else {
		dataset									<- .vdf(dataset, columns.as.numeric=numeric.variables, columns.as.factor=factor.variables)
	}
	readList <- list()
  readList[["dataset"]] <- dataset
  readList[["missingValuesIndicator"]] <- missingValuesIndicator
  return(readList)
}

.bainLegendAncova <- function(dataset, options, jaspResults, position) {

	if (!is.null(jaspResults[["legendTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

    legendTable <- createJaspTable(gettext("Hypothesis Legend"))
		legendTable$dependOn(options =c("model", "fixedFactors"))
		legendTable$position <- position

    legendTable$addColumnInfo(name="number"    , type="string", title="")
    legendTable$addColumnInfo(name="hypothesis", type="string", title=gettext("Hypothesis"))
		
		jaspResults[["legendTable"]] <- legendTable

		if (options[["model"]] != "") {
			rest.string <- .bainCleanModelInput(options[["model"]])
			hyp.vector <- unlist(strsplit(rest.string, "[;]"))
				for (i in 1:length(hyp.vector)) {
          row <- list(number = gettextf("H%i",i), hypothesis = hyp.vector[i])
					legendTable$addRows(row)
				}
		} else if (options[["fixedFactors"]] != "") {
			factor <- options[["fixedFactors"]]
			fact <- dataset[, .v(factor)]
			levels <- levels(fact)
			string <- paste(paste(factor, levels, sep = ""), collapse = " = ")
      row <- list(number = gettext("H1"), hypothesis = string)
			legendTable$addRows(row)
		}
}

.plot_bain_ancova_cran <- function(x)
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
          ggplot2::scale_y_continuous(breaks = cumsum(rev(PMPb)) - rev(PMPb)/2, labels = rev(c(P_lables, gettext("Hu")))) +
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
          ggplot2::labs(x = "", y = "", title = gettext("Excluding Hu"), size = 30) +
          ggplot2::theme(panel.grid = ggplot2::element_blank(), legend.position = "none") +
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
          ggplot2::labs(x = "", y = "", title = gettext("Including Hu"), size = 30) +
          ggplot2::theme(panel.grid = ggplot2::element_blank(), legend.position = "none") +
          ggplot2::scale_y_continuous(breaks = cumsum(rev(PMPb)) - rev(PMPb)/2, labels = rev(c(P_lables, gettext("Hu")))) +
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
