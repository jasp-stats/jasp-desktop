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

BainAnovaBayesian <- function(jaspResults, dataset, options, ...) {

	### READY ###
	ready <- options[["fixedFactors"]] != "" && options[["dependent"]] != ""
	
	### READ DATA ###
	readList <- .readDataBainAnova(options, dataset)
	dataset <- readList[["dataset"]]
	missingValuesIndicator <- readList[["missingValuesIndicator"]]
	
	bainContainer <- .bainGetContainer(jaspResults, deps=c("dependent", "fixedFactors", "model"))
	
	### LEGEND ###
	.bainLegendAncova(dataset, options, jaspResults, position = 0)
	
	### RESULTS ###
	.bainAnovaResultsTable(dataset, options, bainContainer, missingValuesIndicator, ready, position = 1)
	
	### BAYES FACTOR MATRIX ###
	.bainBayesFactorMatrix(dataset, options, bainContainer, ready, type = "anova", position = 2)
	
	### DESCRIPTIVES ###
	.bainAnovaDescriptivesTable(dataset, options, bainContainer, ready, type = "anova", position = 3)
	
	### BAYES FACTOR PLOT ###
	.bainAnovaBayesFactorPlots(dataset, options, bainContainer, ready, position = 4)
	
	### DESCRIPTIVES PLOT ###
	.bainAnovaDescriptivesPlot(dataset, options, bainContainer, ready, type = "anova", position = 5)
}

.bainAnovaResultsTable <- function(dataset, options, bainContainer, missingValuesIndicator, ready, position) {

	if (!is.null(bainContainer[["bainTable"]])) return()

	variables <- c(options[["dependent"]], options[["fixedFactors"]])
	bainTable <- createJaspTable("Bain ANOVA")
	bainTable$position <- position

	bainTable$addColumnInfo(name="hypotheses", 				type="string", title="")
	bainTable$addColumnInfo(name="BF", 						type="number", title="BF.c")
	bainTable$addColumnInfo(name="PMP1", 					type="number", title="PMP a")
	bainTable$addColumnInfo(name="PMP2", 					type="number", title="PMP b")

	message <- "BF.c denotes the Bayes factor of the hypothesis in the row versus its complement.
				Posterior model probabilities (a: excluding the unconstrained hypothesis, b: including the unconstrained hypothesis) are based on equal prior model probabilities."
	bainTable$addFootnote(message=message)

	bainTable$addCitation(.bainGetCitations())

	bainTable$dependOn(options = c("seed"))

	bainContainer[["bainTable"]] <- bainTable

	if (!ready)
		return()

	if (any(variables %in% missingValuesIndicator)) {
		i <- which(variables %in% missingValuesIndicator)
		if (length(i) > 1) {
			bainTable$addFootnote(message= paste0("The variables ", variables[1], " and ", variables[2], " contain missing values, the rows containing these values are removed in the analysis."), symbol="<b>Warning.</b>")
		} else if (length(i) == 1) {
			bainTable$addFootnote(message= paste0("The variable ", variables[i], " contains missing values, the rows containing these values are removed in the analysis."), symbol="<b>Warning.</b>")
		}
	}

	groupCol <- dataset[ , .v(options[["fixedFactors"]])]
	varLevels <- levels(groupCol)

	if (length(varLevels) > 15) {
		bainContainer$setError("The fixed factor has too many levels for a Bain analysis.")
		return()
	}

	if (options[["model"]] == "") {
		rest.string <- NULL
	} else {
		rest.string <- .bainCleanModelInput(options[["model"]])
	}
			
	names(dataset) <- .unv(names(dataset))

	p <- try(silent= FALSE, expr= {
		bainResult <- .bain_anova_cran(X = dataset, dep = options[["dependent"]], group = options[["fixedFactors"]], hyp = rest.string, seed = options[["seed"]])
		bainContainer[["bainResult"]] <- createJaspState(bainResult)
	})

	if (isTryError(p)) {
		bainContainer$setError(paste0("An error occurred in the analysis:<br>", .extractErrorMessage(p), "<br><br>Please double check your variables and model constraints."))
		return()
	}

	for (i in 1:(length(bainResult$fit$BF)-1)) {
		row <- list(hypotheses = paste0("H",i), BF = bainResult$fit$BF[i], PMP1 = bainResult$fit$PMPa[i], PMP2 = bainResult$fit$PMPb[i])
		bainTable$addRows(row)
	}
	row <- list(hypotheses = "Hu", BF = "", PMP1 = "", PMP2 = bainResult$fit$PMPb[length(bainResult$fit$BF)])
	bainTable$addRows(row) 
}

.bainAnovaDescriptivesTable <- function(dataset, options, bainContainer, ready, type = "anova", position) {

	if (!is.null(bainContainer[["descriptivesTable"]]) || !options[["descriptives"]]) return()

	title <- ifelse(type == "anova", yes = "Descriptive Statistics", no = "Coefficients for Groups plus Covariates")
	meanTitle <- ifelse(type == "anova", yes = "Mean", no = "Coefficient")

	descriptivesTable <- createJaspTable(title)
	descriptivesTable$dependOn(options =c("descriptives", "CredibleInterval", "coefficients"))
	descriptivesTable$position <- position

	descriptivesTable$addColumnInfo(name="v",    		title="",	type="string")
	descriptivesTable$addColumnInfo(name="N",    		title="N",			type="integer")
	descriptivesTable$addColumnInfo(name="mean", 		title=meanTitle,		type="number")
	if(type == "anova")
		descriptivesTable$addColumnInfo(name="sd", 		title="SD",		type="number")
	descriptivesTable$addColumnInfo(name="se",   		title="SE", 		type="number")

	interval <- options[["CredibleInterval"]] * 100
	overTitle <- paste0(interval, "% Credible Interval")
	descriptivesTable$addColumnInfo(name="lowerCI",      title = "Lower", type="number", overtitle = overTitle)
	descriptivesTable$addColumnInfo(name="upperCI",      title = "Upper", type="number", overtitle = overTitle)
	
	bainContainer[["descriptivesTable"]] <- descriptivesTable

	if (!ready || bainContainer$getError())
		return()

	groupCol <- dataset[ , .v(options[["fixedFactors"]])]
	varLevels <- levels(groupCol)

	bainResult <- bainContainer[["bainResult"]]$object
	bainSummary <- summary(bainResult, ci = options[["CredibleInterval"]])
	sigma <- diag(bainResult$posterior)
	
	# Extract all but sd and se from bain result
	variable <- bainSummary[["Parameter"]]
	N <- bainSummary[["n"]]
	mu <- bainSummary[["Estimate"]]
	CiLower <- bainSummary[["lb"]]
	CiUpper <- bainSummary[["ub"]]

	if(type == "anova"){
		# The standard errors are taken from the data
		sd <- aggregate(dataset[, .v(options[["dependent"]])], list(dataset[, .v(options[["fixedFactors"]])]), sd)[, 2]
		se <- sd / sqrt(N)	
	} else {
		# The standard errors are taken from bain, since we have adjusted means
		se <- sqrt(sigma)
	}

	row <- data.frame(v = variable, N = N, mean = mu, se = se, lowerCI = CiLower, upperCI = CiUpper)
	if(type == "anova")
		row <- cbind(row, sd = sd)
	descriptivesTable$addRows(row)
}

.bainAnovaBayesFactorPlots <- function(dataset, options, bainContainer, ready, position) {
	if (!is.null(bainContainer[["bayesFactorPlot"]]) || !options[["bayesFactorPlot"]]) return()

	bayesFactorPlot <- createJaspPlot(plot = NULL, title = "Posterior Probabilities", height = 400, width = 600)
	bayesFactorPlot$dependOn(options=c("bayesFactorPlot", "seed"))
	bayesFactorPlot$position <- position
	
	bainContainer[["bayesFactorPlot"]] <- bayesFactorPlot

	if (!ready || bainContainer$getError())
		return()
		
	bainResult <- bainContainer[["bainResult"]]$object
	bayesFactorPlot$plotObject <- .suppressGrDevice(.plot_bain_ancova_cran(bainResult))
}

.bainAnovaDescriptivesPlot <- function(dataset, options, bainContainer, ready, type = "anova", position) {
	if (!is.null(bainContainer[["descriptivesPlot"]]) || !options[["descriptivesPlot"]]) return()
	
	plotTitle <- ifelse(type == "anova", yes = "Descriptives Plot", no = "Adjusted Means")

	descriptivesPlot <- createJaspPlot(plot = NULL, title = plotTitle)
	descriptivesPlot$dependOn(options=c("descriptivesPlot", "CredibleInterval"))
	descriptivesPlot$position <- position

	bainContainer[["descriptivesPlot"]] <- descriptivesPlot

	if (!ready || bainContainer$getError())
		return()
	
	bainBreaks <- function(x, plotErrorBars = TRUE) {
			ci.pos <- c(x[,"mean"], x[,"lowerCI"], x[,"upperCI"])
			b <- pretty(ci.pos)
			d <- data.frame(x=-Inf, xend=-Inf, y=min(b), yend=max(b))
			list(ggplot2::geom_segment(data=d, ggplot2::aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE, size = 1),
					ggplot2::scale_y_continuous(breaks=c(min(b),max(b))))
	}

	groupCol <- dataset[ , .v(options[["fixedFactors"]])]
	varLevels <- levels(groupCol)

	bainResult <- bainContainer[["bainResult"]]$object
	bainSummary <- summary(bainResult, ci = options[["CredibleInterval"]])

	# Remove covariates in ANCOVA
	if(type == "ancova")
		bainSummary <- bainSummary[1:length(varLevels), ]
	
	# Extract all but sd and se from bain result
	variable <- bainSummary[["Parameter"]]
	N <- bainSummary[["n"]]
	mu <- bainSummary[["Estimate"]]
	CiLower <- bainSummary[["lb"]]
	CiUpper <- bainSummary[["ub"]]

	d <- data.frame(v = variable, N = N, mean = mu, lowerCI = CiLower, upperCI = CiUpper, index = 1:length(variable))

	p <- ggplot2::ggplot(d, ggplot2::aes(x=index, y=mean)) +
			ggplot2::geom_errorbar(ggplot2::aes(ymin=lowerCI, ymax=upperCI), colour="black", width=.2, position = ggplot2::position_dodge(.2)) +
			ggplot2::geom_line(position=ggplot2::position_dodge(.2), size = .7) +
			ggplot2::geom_point(position=ggplot2::position_dodge(.2), size=4) +
			ggplot2::scale_fill_manual(values = c(rep(c("white","black"),5),rep("grey",100)), guide=ggplot2::guide_legend(nrow=10)) +
			ggplot2::scale_shape_manual(values = c(rep(c(21:25),each=2),21:25,7:14,33:112), guide=ggplot2::guide_legend(nrow=10)) +
			ggplot2::scale_color_manual(values = rep("black",200), guide=ggplot2::guide_legend(nrow=10)) +
			ggplot2::ylab(options[["dependent"]]) +
			ggplot2::xlab(options[["fixedFactors"]]) +
			bainBreaks(d, TRUE) +
			ggplot2::scale_x_continuous(breaks = 1:length(varLevels), labels = as.character(varLevels))
	p <- JASPgraphs::themeJasp(p)

	descriptivesPlot$plotObject <- p
}

.readDataBainAnova <- function(options, dataset) {
	numeric.variables	<- c(unlist(options[["dependent"]]))
	numeric.variables	<- numeric.variables[numeric.variables != ""]
	factor.variables	<- unlist(options[["fixedFactors"]])
	factor.variables	<- factor.variables[factor.variables != ""]
	all.variables			<- c(numeric.variables, factor.variables)

	if (is.null(dataset)) {
		trydata									<- .readDataSetToEnd(columns.as.numeric=all.variables)
		missingValuesIndicator	<- .unv(names(which(apply(trydata, 2, function(x) { any(is.na(x))} ))))
		dataset 								<- .readDataSetToEnd(columns.as.numeric=numeric.variables, columns.as.factor=factor.variables, exclude.na.listwise=all.variables)
	} else {
		dataset 								<- .vdf(dataset, columns.as.numeric=numeric.variables, columns.as.factor=factor.variables)
	}

	.hasErrors(dataset, perform, type=c("infinity", "variance", "observations"),
				all.target=all.variables, observations.amount="< 3",
				exitAnalysisIfErrors = TRUE)
	readList <- list()
  readList[["dataset"]] <- dataset
  readList[["missingValuesIndicator"]] <- missingValuesIndicator
  return(readList)
}

.bainCleanModelInput <- function(input) {
  return(gsub("\n+", ";", input))
}

.bainGetCitations <- function() {
	citations <- c(
		"Gu, X., Mulder, J., and Hoijtink, H. (2017). Approximate adjusted fractional Bayes factors: A general method for testing informative hypotheses. British Journal of Mathematical and Statistical Psychology, 71, 229-261. DOI: 10.1111/bmsp.12110",
        "Hoijtink, H., Mulder, J., van Lissa, C., and Gu, X. (2018). A Tutorial on testing hypotheses using the Bayes factor. Psychological Methods, 24, 539-556. DOI: 10.1037/met0000201.",
        "Hoijtink, H., Gu, X., and Mulder, J. (2018). Bayesian evaluation of informative hypotheses for multiple populations. British Journal of Mathematical and Statistical Psychology, 72, 219-243. DOI: 10.1111/bmsp.12145"
    )
  return(citations)
}

.bainGetContainer <- function(jaspResults, deps) {
	if (is.null(jaspResults[["bainContainer"]])) {
		jaspResults[["bainContainer"]] <- createJaspContainer()
		jaspResults[["bainContainer"]]$dependOn(options=deps)
		jaspResults[["bainContainer"]]$position = 1
	}
	invisible(jaspResults[["bainContainer"]])
}

# This function is not from JASP and will be migrated to the bain CRAN package in time
.bain_anova_cran<-function(X, dep, group, hyp, seed){

	set.seed(seed)

	# Make group a factor
	c1 <- paste0("X$",group,"<- as.factor(X$",group,")")
	eval(parse(text = c1))
	# roep lm aan
	c2 <- paste0("lmres <- lm(",dep,"~",group,"-1, data = X)")
	eval(parse(text = c2))  

	# Make hyp if argument is NULL
	if (is.null(hyp)){
		hyp <- names(coef(lmres))
		hyp <- paste0(hyp, collapse = "=")
	}

	# Call bain with lmres and hyp as input
	c3 <- paste0("bain::bain(lmres,","\"",hyp,"\"",")")
	result <- eval(parse(text = c3))

	return(invisible(result))
}