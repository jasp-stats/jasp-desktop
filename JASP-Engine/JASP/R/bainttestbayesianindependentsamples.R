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

BainTTestBayesianIndependentSamples <- function(jaspResults, dataset, options, ...) {

	### READY ###
	ready <- length(options[["variables"]][options[["variables"]] != ""] > 0) && options[["groupingVariable"]] != ""
	
	### READ DATA ###
	readList <- .readDataBainTwoSample(options, dataset)
	dataset	<- readList[["dataset"]]
	missingValuesIndicator	<- readList[["missingValuesIndicator"]]

	.bainTwoSampleErrorCheck(dataset, options)

	bainContainer <- .bainGetContainer(jaspResults, deps=c("groupingVariable", "seed"))
    
	### RESULTS ###
  .bainIndependentSamplesResultsTable(dataset, options, bainContainer, missingValuesIndicator, ready, position = 1)
    
	### DESCRIPTIVES ###
	.bainIndependentSamplesDescriptivesTable(dataset, options, bainContainer, ready, position = 2)

	### BAYES FACTOR PLOTS ###
	.bainTTestFactorPlots(dataset, options, bainContainer, ready, type = "independentSamples", position = 3)

	### DESCRIPTIVES PLOTS ###
	.bainIndependentSamplesDescriptivesPlots(dataset, options, bainContainer, ready, position = 4)
}

.readDataBainTwoSample <- function(options, dataset) {

	all.variables 									<- unlist(options$variables)
	grouping   										<- options$groupingVariable
	read.variables 									<- c(all.variables, grouping)
	if (options[["groupingVariable"]] == "")
		grouping <- NULL

	if(is.null(dataset)) {
		trydata <- .readDataSetToEnd(columns.as.numeric=all.variables)
		missingValuesIndicator <- .unv(names(which(apply(trydata, 2, function(x) { any(is.na(x))} ))))
		dataset <- .readDataSetToEnd(columns.as.numeric=all.variables, columns.as.factor=grouping, exclude.na.listwise=read.variables)
  }
	
	readList <- list()
  	readList[["dataset"]] <- dataset
  	readList[["missingValuesIndicator"]] <- missingValuesIndicator
	return(readList)
}

.bainTwoSampleErrorCheck <- function(dataset, options){
	if(options[["groupingVariable"]] != ""){
		.hasErrors(dataset, type="factorLevels",
			factorLevels.target=options[["groupingVariable"]], factorLevels.amount = "!= 2",
			exitAnalysisIfErrors = TRUE)
	}
	
	.bainCommonErrorCheck(dataset, options)
}

.bainTwoSampleState <- function(variable, options, dataset, bainContainer){

  if(!is.null(bainContainer[[variable]]))
    return(bainContainer[[variable]]$object)

  type <- base::switch(options[["hypothesis"]],
                    "equalNotEqual"       = 1,
                    "equalBigger"         = 2,
                    "equalSmaller"        = 3,
                    "biggerSmaller"       = 4,
                    "equalBiggerSmaller"  = 5)

	levels <- base::levels(dataset[[ .v(options[["groupingVariable"]]) ]])
	if (length(levels) != 2) {
		g1 <- "1"
		g2 <- "2"
	} else {
		g1 <- levels[1]
		g2 <- levels[2]
	}

	subDataSet <- dataset[, c(.v(variable), .v(options[["groupingVariable"]]))]

	group1 <- subDataSet[subDataSet[[.v(options[["groupingVariable"]])]]== g1,.v(variable)]
	group2 <- subDataSet[subDataSet[[.v(options[["groupingVariable"]])]]== g2,.v(variable)]

	p <- try({
		bain:::bain_ttest_cran(x = group1, y = group2, type = type, seed = options[["seed"]])
	})
    
  bainContainer[[variable]] <- createJaspState(p, dependencies = c("hypothesis", "seed"))
  bainContainer[[variable]]$dependOn(optionContainsValue=list("variables" = variable))
  
  return(bainContainer[[variable]]$object)
}

.bainIndependentSamplesResultsTable <- function(dataset, options, bainContainer, missingValuesIndicator, ready, position) {

	if (!is.null(bainContainer[["bainTable"]])) return()

  bainTable <- createJaspTable(gettext("Bain Independent Samples Welch's T-Test"))
	bainTable$dependOn(options = c("variables", "bayesFactorType", "hypothesis"))
	bainTable$position <- position

	bf.type <- options[["bayesFactorType"]]
	BFH1H0 <- FALSE
  bf.title <- gettext("BF")

	if (options[["hypothesis"]] == "equalBiggerSmaller"){
      bainTable$addColumnInfo(name="Variable",      type="string",                    title="")
      bainTable$addColumnInfo(name="type[equal]",   type = "string",                  title=gettext("Hypothesis"))
      bainTable$addColumnInfo(name="BF[equal]",     type="number",                    title=bf.title)
      bainTable$addColumnInfo(name="pmp[equal]",    type="number",    format="dp:3",  title=gettext("Posterior probability"))
      bainTable$addColumnInfo(name="type[greater]", type = "string",                  title=gettext("Hypothesis"))
      bainTable$addColumnInfo(name="BF[greater]",   type="number",                    title=bf.title)
      bainTable$addColumnInfo(name="pmp[greater]",  type="number",    format="dp:3",  title=gettext("Posterior probability"))
      bainTable$addColumnInfo(name="type[less]",    type = "string",                  title=gettext("Hypothesis"))
      bainTable$addColumnInfo(name="BF[less]",      type = "number",                  title=bf.title)
      bainTable$addColumnInfo(name="pmp[less]",     type="number",    format="dp:3",  title=gettext("Posterior probability"))
	} else {
      bainTable$addColumnInfo(name="Variable",          type="string",                    title="")
      bainTable$addColumnInfo(name="hypothesis[type1]", type="string",                    title=gettext("Hypothesis"))
      bainTable$addColumnInfo(name="BF[type1]",         type="number",                    title=bf.title)
      bainTable$addColumnInfo(name="pmp[type1]",        type="number",    format="dp:3",  title=gettext("Posterior probability"))
      bainTable$addColumnInfo(name="hypothesis[type2]", type="string",                    title=gettext("Hypothesis"))
      bainTable$addColumnInfo(name="BF[type2]",         type="number",                    title=bf.title)
      bainTable$addColumnInfo(name="pmp[type2]",        type="number",    format="dp:3",  title=gettext("Posterior probability"))
	}
	
	type <- base::switch(options[["hypothesis"]],
													"equalNotEqual"		= 1,
													"equalBigger"		= 2,
													"equalSmaller"		= 3,
													"biggerSmaller"		= 4,
													"equalBiggerSmaller"= 5)
	message <- base::switch(options[["hypothesis"]],
                          "equalNotEqual"     = gettext("The alternative hypothesis H1 specifies that the mean of group 1 is unequal to the mean of group 2. The posterior probabilities are based on equal prior probabilities."),
                          "equalSmaller"      = gettext("The alternative hypothesis H1 specifies that the mean of group 1 is smaller than the mean of group 2. The posterior probabilities are based on equal prior probabilities."),
                          "equalBigger"       = gettext("The alternative hypothesis H1 specifies that mean of group 1 is bigger than the mean of group 2. The posterior probabilities are based on equal prior probabilities."),
                          "biggerSmaller"     = gettext("The hypothesis H1 specifies that the mean of group 1 is bigger than the mean of group 2. The hypothesis H2 specifies that the mean in group 1 is smaller than the mean in group 2. The posterior probabilities are based on equal prior probabilities."),
                          "equalBiggerSmaller"= gettext("The null hypothesis H0 (equal group means) is tested against H1 (first mean larger than second mean) and H2 (first mean smaller than second mean). The posterior probabilities are based on equal prior probabilities."))
  
	bainTable$addFootnote(message=message)
	bainTable$addCitation(.bainGetCitations())
	
	bainContainer[["bainTable"]] <- bainTable

	if (!ready)
		return()

	bainTable$setExpectedSize(length(options[["variables"]]))

	startProgressbar(length(options[["variables"]]))

  for (variable in options[["variables"]]) {

		bainAnalysis <- .bainTwoSampleState(variable, options, dataset, bainContainer)

		if (isTryError(bainAnalysis)) {
			bainTable$addRows(list(Variable=variable), rowNames=variable)
      bainTable$addFootnote(message=gettextf("Results not computed: %s", .extractErrorMessage(bainAnalysis)), colNames="Variable", rowNames=variable)
			progressbarTick()
			next
		}
	
		if (variable %in% missingValuesIndicator) {
      bainTable$addFootnote(message= gettext("Variable contains missing values, the rows containing these values are removed in the analysis."), colNames="Variable", rowNames=variable)
		}

		if (type == 1) {
			BF_0u <- bainAnalysis$fit$BF[1]
			PMP_u <- bainAnalysis$fit$PMPb[2]
			PMP_0 <- bainAnalysis$fit$PMPb[1]
			if (options$bayesFactorType == "BF10")
				BF_0u <- 1/BF_0u
		}
		if (type == 2) {
			BF_01 <- bainAnalysis$BFmatrix[1,2]
			PMP_1 <- bainAnalysis$fit$PMPa[2]
			PMP_0 <- bainAnalysis$fit$PMPa[1]
			if (options$bayesFactorType == "BF10")
				BF_01 <- 1/BF_01
		}
		if (type == 3) {
			BF_01 <- bainAnalysis$BFmatrix[1,2]
			PMP_0 <- bainAnalysis$fit$PMPa[1]
			PMP_1 <- bainAnalysis$fit$PMPa[2]
			if (options$bayesFactorType == "BF10")
				BF_01 <- 1/BF_01
		}
		if (type == 4) {
			BF_01 <- bainAnalysis$BFmatrix[1,2]
			PMP_0 <- bainAnalysis$fit$PMPa[1]
			PMP_1 <- bainAnalysis$fit$PMPa[2]
			if (options$bayesFactorType == "BF10")
				BF_01 <- 1/BF_01
		}
		if (type == 5) {
			BF_01 <- bainAnalysis$BFmatrix[1,2]
			BF_02 <- bainAnalysis$BFmatrix[1,3]
			BF_12 <- bainAnalysis$BFmatrix[2,3]
			PMP_0 <- bainAnalysis$fit$PMPa[1]
			PMP_1 <- bainAnalysis$fit$PMPa[2]
			PMP_2 <- bainAnalysis$fit$PMPa[3]
			if (options$bayesFactorType == "BF10")
			{
				BF_01 <- 1/BF_01
				BF_02 <- 1/BF_02
				BF_12 <- 1/BF_12
			}
		}

    if (options$bayesFactorType == "BF01") {
        if (options$hypothesis == "equalNotEqual") {
            row <- list(Variable=variable, "hypothesis[type1]" = gettext("H0: Equal"), "BF[type1]"=BF_0u, "pmp[type1]" = PMP_0,
                                "hypothesis[type2]" = gettext("H1: Not equal"), "BF[type2]" = "", "pmp[type2]" = PMP_u)
        } else if (options$hypothesis == "equalBigger") {
            row <-list(Variable=variable, "hypothesis[type1]" = gettext("H0: Equal"),"BF[type1]"=BF_01, "pmp[type1]" = PMP_0,
                               "hypothesis[type2]" = gettext("H1: Bigger"), "BF[type2]" = "", "pmp[type2]" = PMP_1)
        } else if (options$hypothesis == "equalSmaller") {
            row <-list(Variable=variable, "hypothesis[type1]" = gettext("H0: Equal"), "BF[type1]"=BF_01, "pmp[type1]" = PMP_0,
                               "hypothesis[type2]" = gettext("H1: Smaller"), "BF[type2]" = "", "pmp[type2]" = PMP_1)
        } else if (options$hypothesis == "biggerSmaller") {
            row <-list(Variable=variable, "hypothesis[type1]" = gettext("H1: Bigger"), "BF[type1]"=BF_01, "pmp[type1]" = PMP_0,
                               "hypothesis[type2]" = gettext("H2: Smaller"), "BF[type2]" = "", "pmp[type2]" = PMP_1)
        } else if (options$hypothesis == "equalBiggerSmaller") {
            row <-list(Variable=variable,
                               "type[equal]" = gettext("H0: Equal"),
                               "BF[equal]"= "",
                               "pmp[equal]" = PMP_0,
                               "type[greater]" = gettext("H1: Bigger"),
                               "BF[greater]" = BF_01,
                               "pmp[greater]" = PMP_1,
                               "type[less]" = gettext("H2: Smaller"),
                               "BF[less]" = BF_02,
                               "pmp[less]" = PMP_2)
        }
    } else if (options$bayesFactorType == "BF10") {
        if (options$hypothesis == "equalNotEqual") {
            row <- list(Variable=variable, "hypothesis[type1]" = gettext("H0: Equal"),"BF[type1]"="", "pmp[type1]" = PMP_0,
                                "hypothesis[type2]" = gettext("H1: Not equal"), "BF[type2]" = BF_0u, "pmp[type2]" = PMP_u)
        } else if (options$hypothesis == "equalBigger") {
            row <-list(Variable=variable, "hypothesis[type1]" = gettext("H0: Equal"),"BF[type1]"="", "pmp[type1]" = PMP_0,
                               "hypothesis[type2]" = gettext("H1: Bigger"), "BF[type2]" = BF_01, "pmp[type2]" = PMP_1)
        } else if (options$hypothesis == "equalSmaller") {
            row <-list(Variable=variable, "hypothesis[type1]" = gettext("H0: Equal"), "BF[type1]"="", "pmp[type1]" = PMP_0,
                               "hypothesis[type2]" = gettext("H1: Smaller"), "BF[type2]" = BF_01, "pmp[type2]" = PMP_1)
        } else if (options$hypothesis == "biggerSmaller") {
            row <-list(Variable=variable, "hypothesis[type1]" = gettext("H1: Bigger"), "BF[type1]"= "", "pmp[type1]" = PMP_0,
                               "hypothesis[type2]" = gettext("H2: Smaller"), "BF[type2]" = BF_01, "pmp[type2]" = PMP_1)
        } else if (options$hypothesis == "equalBiggerSmaller") {
            row <-list(Variable=variable,
                               "type[equal]" = gettext("H0: Equal"),
                               "BF[equal]"= "",
                               "pmp[equal]" = PMP_0,
                               "type[greater]"= gettext("H1: Bigger"),
                               "BF[greater]" = BF_01,
                               "pmp[greater]" = PMP_1,
                               "type[less]" = gettext("H2: Smaller"),
                               "BF[less]" = BF_02,
                               "pmp[less]" = PMP_2)
        }
    }
    bainTable$addRows(row, rowNames=variable)
    progressbarTick()
  }
}

.bainIndependentSamplesDescriptivesTable <- function(dataset, options, bainContainer, ready, position) {

	if (!is.null(bainContainer[["descriptivesTable"]]) || !options[["descriptives"]]) return() 

  descriptivesTable <- createJaspTable(gettext("Descriptive Statistics"))
	descriptivesTable$dependOn(options =c("variables", "descriptives", "credibleInterval"))
	descriptivesTable$position <- position

  descriptivesTable$addColumnInfo(name="v",                    title = "",               type="string")
  descriptivesTable$addColumnInfo(name="group",                title = gettext("Group"), type="string")
  descriptivesTable$addColumnInfo(name="N",                    title = gettext("N"),     type="integer")
  descriptivesTable$addColumnInfo(name="mean",                 title = gettext("Mean"),  type="number")
  descriptivesTable$addColumnInfo(name="sd",                   title = gettext("SD"),    type="number")
  descriptivesTable$addColumnInfo(name="se",                   title = gettext("SE"),    type="number")

  overTitle <- gettextf("%.0f%% Credible Interval", 100 * options[["credibleInterval"]])
  descriptivesTable$addColumnInfo(name="lowerCI",              title = gettext("Lower"), type="number", overtitle = overTitle)
  descriptivesTable$addColumnInfo(name="upperCI",              title = gettext("Upper"), type="number", overtitle = overTitle)
	
	bainContainer[["descriptivesTable"]] <- descriptivesTable

	if (!ready)
		return()

	descriptivesTable$setExpectedSize(length(options[["variables"]]) * 2)

	levels <- base::levels(dataset[[ .v(options[["groupingVariable"]]) ]])
	if (length(levels) != 2) {
		g1 <- "1"
		g2 <- "2"
	} else {
		g1 <- levels[1]
		g2 <- levels[2]
	}

	for (variable in options[["variables"]]) {
		
		bainAnalysis <- .bainTwoSampleState(variable, options, dataset, bainContainer)

		if(isTryError(bainAnalysis)){

			descriptivesTable$addRows(data.frame(v=variable), rowNames=variable)
      descriptivesTable$addFootnote(message=gettextf("Results not computed: %s", .extractErrorMessage(bainAnalysis)), colNames="v", rowNames=variable)

		} else {

			bainSummary <- summary(bainAnalysis, ci = options[["credibleInterval"]])

			# Descriptives from bain, sd calculated manually
			N <- bainSummary[["n"]]
			mu <- bainSummary[["Estimate"]]
			CiLower <- bainSummary[["lb"]]
			CiUpper <- bainSummary[["ub"]]
			sd <- aggregate(dataset[, .v(variable)], list(dataset[, .v(options[["groupingVariable"]])]), sd)[, 2]
			se <- sqrt(diag(bainAnalysis[["posterior"]]))

			row <- data.frame(v = variable, group = g1, N = N[1], mean = mu[1], sd = sd[1], se = se[1], lowerCI = CiLower[1], upperCI = CiUpper[1])
			descriptivesTable$addRows(row)
			row <- data.frame(v = "", group = g2, N = N[2], mean = mu[2], sd = sd[2], se = se[2], lowerCI = CiLower[2], upperCI = CiUpper[2])
			descriptivesTable$addRows(row)

		}
	}
}

.bainIndependentSamplesDescriptivesPlots <- function(dataset, options, bainContainer, ready, position) {
	
	if (!is.null(bainContainer[["descriptivesPlots"]]) || !options[["descriptivesPlots"]]) return()

  descriptivesPlots <- createJaspContainer(gettext("Descriptive Plots"))
	descriptivesPlots$dependOn(options = c("variables", "descriptivesPlots", "credibleInterval"))
	descriptivesPlots$position <- position

	bainContainer[["descriptivesPlots"]] <- descriptivesPlots

	if (!ready)
		return()

	for (variable in options[["variables"]]) {

		if (is.null(bainContainer[["descriptivesPlots"]][[variable]])){

			bainAnalysis <- .bainTwoSampleState(variable, options, dataset, bainContainer)

			if(isTryError(bainAnalysis)){

				descriptivesPlots[[variable]] <- createJaspPlot(plot=NULL, title = variable)
				descriptivesPlots[[variable]]$dependOn(optionContainsValue=list("variables" = variable))
				descriptivesPlots[[variable]]$setError(gettextf("Results not computed: %s", .extractErrorMessage(bainAnalysis)))

			} else {

				bainSummary <- summary(bainAnalysis, ci = options[["credibleInterval"]])
				levels <- base::levels(dataset[[ .v(options[["groupingVariable"]]) ]])
				
				# Descriptive statistics from bain
				N <- bainSummary[["n"]]
				mu <- bainSummary[["Estimate"]]
				CiLower <- bainSummary[["lb"]]
				CiUpper <- bainSummary[["ub"]]

				yBreaks <- JASPgraphs::getPrettyAxisBreaks(c(CiLower, CiUpper), min.n = 4)
				d <- data.frame(v = levels, N = N, mean = mu, lowerCI = CiLower, upperCI = CiUpper, index = 1:length(levels))

				p <- ggplot2::ggplot(d, ggplot2::aes(x=index, y=mean)) +
						ggplot2::geom_errorbar(ggplot2::aes(ymin=lowerCI, ymax=upperCI), colour="black", width=.2, position = ggplot2::position_dodge(.2)) +
						ggplot2::geom_line(position=ggplot2::position_dodge(.2), size = .7) +
						ggplot2::geom_point(position=ggplot2::position_dodge(.2), size=4) +
						ggplot2::ylab(variable) +
						ggplot2::xlab(options[["groupingVariable"]]) +
						ggplot2::scale_x_continuous(breaks = 1:length(levels), labels = as.character(levels)) +
						ggplot2::scale_y_continuous(breaks = yBreaks, labels = yBreaks, limits = range(yBreaks))
				p <- JASPgraphs::themeJasp(p)

				descriptivesPlots[[variable]] <- createJaspPlot(plot=p, title = variable)
				descriptivesPlots[[variable]]$dependOn(optionContainsValue=list("variables" = variable))
			}

		}
	}
}
