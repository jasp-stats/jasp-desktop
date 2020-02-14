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

BainTTestBayesianPairedSamples <- function(jaspResults, dataset, options, ...) {

  ### READY ###
  ready <- !is.null(unlist(options[["pairs"]]))
  
  ### READ DATA ###
  readList <- .readDataBainPairedSamples(options, dataset)
  dataset <- readList[["dataset"]]
  missingValuesIndicator <- readList[["missingValuesIndicator"]]

  .bainPairedSamplesErrorCheck(dataset, options)
  
  bainContainer <- .bainGetContainer(jaspResults, deps=c("seed"))

  ### RESULTS ###
  .bainPairedSamplesResultsTable(dataset, options, bainContainer, missingValuesIndicator, ready, position = 1)
  
  ### DESCRIPTIVES ###
  .bainPairedSamplesDescriptivesTable(dataset, options, bainContainer, ready, position = 2)

  ### BAYES FACTOR PLOTS ###
  .bainTTestFactorPlots(dataset, options, bainContainer, ready, type = "pairedSamples", position = 3)
  
  ### DESCRIPTIVES PLOTS ###
  .bainPairedSamplesDescriptivesPlots(dataset, options, bainContainer, ready, position = 4)
}

.readDataBainPairedSamples <- function(options, dataset) {

    all.variables <- unique(unlist(options[["pairs"]]))
    all.variables <- all.variables[all.variables != ""]
    pairs <- options[["pairs"]]

    if (is.null(dataset)) {
            trydata <- .readDataSetToEnd(columns.as.numeric=all.variables)
            missingValuesIndicator <- .unv(names(which(apply(trydata, 2, function(x) { any(is.na(x))} ))))
            dataset <- .readDataSetToEnd(columns.as.numeric=all.variables, exclude.na.listwise=all.variables)
    }

    readList <- list()
    readList[["dataset"]] <- dataset
    readList[["missingValuesIndicator"]] <- missingValuesIndicator
    return(readList)
}

.bainPairedSamplesErrorCheck <- function(dataset, options){
  all.variables <- unique(unlist(options[["pairs"]]))
  all.variables <- all.variables[all.variables != ""]
  .hasErrors(dataset, type=c("infinity", "variance", "observations"),
            all.target=all.variables, observations.amount="< 3",
            exitAnalysisIfErrors = TRUE)
}

.bainPairedSampleState <- function(pair, options, dataset, bainContainer){

  currentPair <- paste(pair, collapse=" - ")

  if(!is.null(bainContainer[[currentPair]]))
    return(bainContainer[[currentPair]]$object)

  type <- base::switch(options[["hypothesis"]],
                    "equalNotEqual"       = 1,
                    "equalBigger"         = 2,
                    "equalSmaller"        = 3,
                    "biggerSmaller"       = 4,
                    "equalBiggerSmaller"  = 5)
    
  if (pair[[2]] != "" && pair[[1]] != pair[[2]] && pair[[1]] != "") {

    subDataSet <- subset(dataset, select=c(.v(pair[[1]]), .v(pair[[2]])) )
    c1 <- subDataSet[[ .v(pair[[1]]) ]]
    c2 <- subDataSet[[ .v(pair[[2]]) ]]

    p <- try({
      # Call bain from package
      bain:::bain_ttest_cran(x = c1, y = c2, type = type, paired = TRUE, seed = options[["seed"]])
    })
    bainContainer[[currentPair]] <- createJaspState(p, dependencies = c("hypothesis", "seed"))
    bainContainer[[currentPair]]$dependOn(optionContainsValue=list("pairs" = pair))
  }

  return(bainContainer[[currentPair]]$object)
}

#could probably be merged with other *very similar* functions of other BAIN analyses
.bainPairedSamplesResultsTable <- function(dataset, options, bainContainer, missingValuesIndicator, ready, position) {
    
    if (!is.null(bainContainer[["bainTable"]])) return()

    bainTable <- createJaspTable(gettext("Bain Paired Samples T-Test"))
    bainTable$dependOn(options = c("pairs", "hypothesis", "bayesFactorType"))
    bainTable$position <- position

    bf.type <- options$bayesFactorType
    BFH1H0 <- FALSE
    bf.title <- gettext("BF")

    if (options$hypothesis == "equalBiggerSmaller") {
            bainTable$addColumnInfo(name="Variable",      type="string",                title="")
            bainTable$addColumnInfo(name="type[equal]",   type="string",                title=gettext("Hypothesis"))
            bainTable$addColumnInfo(name="BF[equal]",     type="number",                title=bf.title)
            bainTable$addColumnInfo(name="pmp[equal]",    type="number", format="dp:3", title=gettext("Posterior probability"))
            bainTable$addColumnInfo(name="type[greater]", type="string",                title=gettext("Hypothesis"))
            bainTable$addColumnInfo(name="BF[greater]",   type="number",                title=bf.title)
            bainTable$addColumnInfo(name="pmp[greater]",  type="number", format="dp:3", title=gettext("Posterior probability"))
            bainTable$addColumnInfo(name="type[less]",    type="string",                title=gettext("Hypothesis"))
            bainTable$addColumnInfo(name="BF[less]",      type="number",                title=bf.title)
            bainTable$addColumnInfo(name="pmp[less]",     type="number", format="dp:3", title=gettext("Posterior probability"))
    } else {
            bainTable$addColumnInfo(name="Variable",          type="string",                title="")
            bainTable$addColumnInfo(name="hypothesis[type1]", type="string",                title=gettext("Hypothesis"))
            bainTable$addColumnInfo(name="BF[type1]",         type="number",                title=bf.title)
            bainTable$addColumnInfo(name="pmp[type1]",        type="number", format="dp:3", title=gettext("Posterior probability"))
            bainTable$addColumnInfo(name="hypothesis[type2]", type="string",                title=gettext("Hypothesis"))
            bainTable$addColumnInfo(name="BF[type2]",         type="number",                title=bf.title)
            bainTable$addColumnInfo(name="pmp[type2]",        type="number", format="dp:3", title=gettext("Posterior probability"))
    }
    
    type <- base::switch(options[["hypothesis"]],
                            "equalNotEqual"     = 1,
                            "equalBigger"       = 2,
                            "equalSmaller"      = 3,
                            "biggerSmaller"     = 4,
                            "equalBiggerSmaller"= 5)
    message <- base::switch(options[["hypothesis"]],
                              "equalNotEqual"       = gettext("The alternative hypothesis H1 specifies that the mean of variable 1 is unequal to the mean of variable 2. The posterior probabilities are based on equal prior probabilities."),
                              "equalBigger"         = gettext("The alternative hypothesis H1 specifies that the mean of variable 1 is bigger than the mean of variable 2. The posterior probabilities are based on equal prior probabilities."),
                              "equalSmaller"        = gettext("The alternative hypothesis H1 specifies that the mean of variable 1 is smaller than the mean of variable 2. The posterior probabilities are based on equal prior probabilities."),
                              "biggerSmaller"       = gettext("The hypothesis H1 specifies that the mean of variable 1 is bigger than the mean of variable 2, while the hypothesis H2 specifies that it is smaller. The posterior probabilities are based on equal prior probabilities."),
                              "equalBiggerSmaller"  = gettext("The null hypothesis H0 with equal means is tested against the other hypotheses. The alternative hypothesis H1 states that the mean of variable 1 is bigger than the mean of variable 2. The alternative hypothesis H2 states that the mean of variable 1 is smaller than the mean of variable 2. The posterior probabilities are based on equal prior probabilities."))
    
    bainTable$addFootnote(message = message)
    bainTable$addCitation(.bainGetCitations())
    
    bainContainer[["bainTable"]] <- bainTable

    if (!ready)
      return()

    bainTable$setExpectedSize(length(options[["pairs"]]))

    startProgressbar(length(options[["pairs"]]))

    for (pair in options[["pairs"]]){

      currentPair <- paste(pair, collapse=" - ")

      if(pair[[1]] != "" || pair[[2]] != ""){
        
        bainAnalysis <- .bainPairedSampleState(pair, options, dataset, bainContainer)

        if (isTryError(bainAnalysis)) {
          bainTable$addRows(list(Variable=currentPair), rowNames=currentPair)
          bainTable$addFootnote(message=gettextf("Results not computed: %s", .extractErrorMessage(bainAnalysis)), colNames="Variable", rowNames=currentPair)
          progressbarTick()
          next
        } 
              
        if (any(pair %in% missingValuesIndicator)) {
          i <- which(pair %in% missingValuesIndicator)
          if (length(i) > 1) {
            message <- gettext("Both variables contain missing values, the rows containing these values are removed in the analysis.")
          } else {
            message <- gettextf("The variable %s contains missing values, the rows containing these values are removed in the analysis.", pair[i])
          }
          bainTable$addFootnote(message=message, colNames="Variable", rowNames=currentPair)
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
                row <- list(Variable=currentPair, "hypothesis[type1]" = gettext("H0: Equal"), "BF[type1]"=BF_0u, "pmp[type1]" = PMP_0,
                                    "hypothesis[type2]" = gettext("H1: Not equal"), "BF[type2]" = "", "pmp[type2]" = PMP_u)
            }
            if (options$hypothesis == "equalSmaller") {
                row <-list(Variable=currentPair, "hypothesis[type1]" = gettext("H0: Equal"), "BF[type1]"= BF_01, "pmp[type1]" = PMP_0,
                                   "hypothesis[type2]" = gettext("H1: Smaller"), "BF[type2]" = "", "pmp[type2]" = PMP_1)
            }
            if (options$hypothesis == "equalBigger") {
                row <-list(Variable=currentPair, "hypothesis[type1]" = gettext("H0: Equal"), "BF[type1]"= BF_01, "pmp[type1]" = PMP_0,
                                   "hypothesis[type2]" = gettext("H1: Bigger"), "BF[type2]" = "", "pmp[type2]" = PMP_1)
            }
            if (options$hypothesis == "biggerSmaller") {
                row <-list(Variable=currentPair, "hypothesis[type1]" = gettext("H1: Bigger"), "BF[type1]"= BF_01, "pmp[type1]" = PMP_0,
                                   "hypothesis[type2]" = gettext("H2: Smaller"), "BF[type2]" = "", "pmp[type2]" = PMP_1)
            }
            if (options$hypothesis == "equalBiggerSmaller") {
                row <-list(Variable=currentPair,
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
        } else if (options$bayesFactorType == "BF10") {
            if (options$hypothesis == "equalNotEqual") {
                row <- list(Variable=currentPair, "hypothesis[type1]" = gettext("H0: Equal"),"BF[type1]"="", "pmp[type1]" = PMP_0,
                                    "hypothesis[type2]" = gettext("H1: Not equal"), "BF[type2]" = BF_0u, "pmp[type2]" = PMP_u)
            }
            if (options$hypothesis == "equalSmaller") {
                row <-list(Variable=currentPair, "hypothesis[type1]" = gettext("H0: Equal"), "BF[type1]"= "", "pmp[type1]" = PMP_0,
                                   "hypothesis[type2]" = gettext("H1: Smaller"), "BF[type2]" = BF_01, "pmp[type2]" = PMP_1)
            }
            if (options$hypothesis == "equalBigger") {
                row <-list(Variable=currentPair, "hypothesis[type1]" = gettext("H0: Equal"), "BF[type1]"= "", "pmp[type1]" = PMP_0,
                                   "hypothesis[type2]" = gettext("H1: Bigger"), "BF[type2]" = BF_01, "pmp[type2]" = PMP_1)
            }
            if (options$hypothesis == "biggerSmaller") {
                row <-list(Variable=currentPair, "hypothesis[type1]" = gettext("H1: Bigger"), "BF[type1]"= "", "pmp[type1]" = PMP_0,
                                   "hypothesis[type2]" = gettext("H2: Smaller"), "BF[type2]" = BF_01, "pmp[type2]" = PMP_1)
            }
            if (options$hypothesis == "equalBiggerSmaller") {
                row <-list(Variable=currentPair,
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
    } else {
      if (options$hypothesis == "equalBiggerSmaller") {
            row <- list(Variable=currentPair, "type[equal]" = ".", "BF[equal]"= ".", "pmp[equal]" = ".",
                               "type[greater]"= ".", "BF[greater]" = ".", "pmp[greater]" = ".",
                               "type[less]" = ".", "BF[less]" = ".", "pmp[less]" = ".")
      } else {
            row <- list(Variable=currentPair, "hypothesis[type1]" = ".", "BF[type1]"= ".", "pmp[type1]" = ".",
                               "hypothesis[type2]" = ".", "BF[type2]" = ".", "pmp[type2]" = ".")
      }
    }
    bainTable$addRows(row, rowNames = currentPair)

    if(pair[[1]] == pair[[2]]){
      bainTable$addFootnote(message=gettext("Results not computed: The variables in this pair are the same."), colNames="Variable", rowNames=currentPair)
    }
    if(pair[[1]] == "" || pair[[2]] == ""){
      bainTable$addFootnote(message=gettext("Results not computed: The pair is incomplete."), colNames="Variable", rowNames=currentPair)
    }

    progressbarTick()
  }
}

.bainPairedSamplesDescriptivesTable <- function(dataset, options, bainContainer, ready, position) {

    if (!is.null(bainContainer[["descriptivesTable"]]) || !options[["descriptives"]]) return() 
      
    descriptivesTable <- createJaspTable(gettext("Descriptive Statistics"))
    descriptivesTable$dependOn(options =c("pairs", "descriptives", "credibleInterval"))
    descriptivesTable$position <- position

    descriptivesTable$addColumnInfo(name="v",                    title = "",              type="string")
    descriptivesTable$addColumnInfo(name="N",                    title = gettext("N"),    type="integer")
    descriptivesTable$addColumnInfo(name="mean",                 title = gettext("Mean"), type="number")
    descriptivesTable$addColumnInfo(name="sd",                   title = gettext("SD"),   type="number")
    descriptivesTable$addColumnInfo(name="se",                   title = gettext("SE"),   type="number")

    overTitle <- gettextf("%.0f%% Credible Interval", 100 * options[["credibleInterval"]])
    descriptivesTable$addColumnInfo(name="lowerCI",              title = gettext("Lower"), type="number", overtitle = overTitle)
    descriptivesTable$addColumnInfo(name="upperCI",              title = gettext("Upper"), type="number", overtitle = overTitle)
    
    bainContainer[["descriptivesTable"]] <- descriptivesTable
    
    if (!ready)
      return()

    descriptivesTable$setExpectedSize(length(options[["pairs"]]))

    for (pair in options[["pairs"]]) {

      if (pair[[2]] != "" && pair[[1]] != pair[[2]]) {

      subDataSet <- subset(dataset, select=c(.v(pair[[1]]), .v(pair[[2]])))

      c1 <- subDataSet[[ .v(pair[[1]]) ]]
      c2 <- subDataSet[[ .v(pair[[2]]) ]]
      difference <- c1 - c2

      currentPair <- paste(pair, collapse=" - ")

      bainAnalysis <- .bainPairedSampleState(pair, options, dataset, bainContainer)

      if(isTryError(bainAnalysis)){

      descriptivesTable$addRows(data.frame(v=currentPair), rowNames=currentPair)
      descriptivesTable$addFootnote(message=gettextf("Results not computed: %s", .extractErrorMessage(bainAnalysis)), colNames="v", rowNames=currentPair)
    
      } else {

        bainSummary <- summary(bainAnalysis, ci = options[["credibleInterval"]])

        # Descriptive statistics from bain, sd calculated manually
        N <- bainSummary[["n"]]
        mu <- bainSummary[["Estimate"]]
        CiLower <- bainSummary[["lb"]]
        CiUpper <- bainSummary[["ub"]]
        se <- sqrt(diag(bainAnalysis[["posterior"]]))
        sd <- sd(difference)

        row <- list(v = currentPair, N = N, mean = mu, sd = sd, se = se, lowerCI = CiLower, upperCI = CiUpper)
        descriptivesTable$addRows(row)
      }
    }
  }
}

.bainPairedSamplesDescriptivesPlots <- function(dataset, options, bainContainer, ready, position) {
  
  if (!is.null(bainContainer[["descriptivesPlots"]]) || !options[["descriptivesPlots"]]) return()

  descriptivesPlots <- createJaspContainer(gettext("Descriptive Plots"))
  descriptivesPlots$dependOn(options =c("pairs", "descriptivesPlots", "credibleInterval"))
  descriptivesPlots$position <- position

  bainContainer[["descriptivesPlots"]] <- descriptivesPlots

  if (!ready)
    return()

  for (pair in options[["pairs"]]) {

    currentPair <- paste(pair, collapse=" - ")

    if (is.null(bainContainer[["descriptivesPlots"]][[currentPair]]) && pair[[2]] != "" && pair[[1]] != pair[[2]]){

      bainAnalysis <- .bainPairedSampleState(pair, options, dataset, bainContainer)

      if(isTryError(bainAnalysis)){

      descriptivesPlots[[currentPair]] <- createJaspPlot(plot=NULL, title = currentPair)
      descriptivesPlots[[currentPair]]$dependOn(optionContainsValue=list("variables" = currentPair))
			descriptivesPlots[[currentPair]]$setError(.extractErrorMessage(bainAnalysis))

		  } else {

        bainSummary <- summary(bainAnalysis, ci = options[["credibleInterval"]])

        # Descriptive statistics from bain
        N <- bainSummary[["n"]]
        mu <- bainSummary[["Estimate"]]
        CiLower <- bainSummary[["lb"]]
        CiUpper <- bainSummary[["ub"]]

        yBreaks <- JASPgraphs::getPrettyAxisBreaks(c(0, CiLower, CiUpper), min.n = 4)
        d <- data.frame(v = gettext("Difference"), N = N, mean = mu, lowerCI = CiLower, upperCI = CiUpper, index = 1)

        p <- ggplot2::ggplot(d, ggplot2::aes(x=index, y=mean)) +
              ggplot2::geom_segment(ggplot2::aes(x = -Inf, xend = Inf, y = 0, yend = 0), linetype = 2, color = "darkgray") +
              ggplot2::geom_errorbar(ggplot2::aes(ymin=lowerCI, ymax=upperCI), colour="black", width=.1, position = ggplot2::position_dodge(.2)) +
              ggplot2::geom_point(position=ggplot2::position_dodge(.2), size=4) +
              ggplot2::ylab("") +
              ggplot2::xlab("") +
              ggplot2::scale_y_continuous(breaks = yBreaks, labels = yBreaks, limits = range(yBreaks)) +
              ggplot2::scale_x_continuous(breaks = 0:2, labels = NULL)
        p <- JASPgraphs::themeJasp(p, xAxis = FALSE) + ggplot2::theme(axis.ticks.x = ggplot2::element_blank())
        
        descriptivesPlots[[currentPair]] <- createJaspPlot(plot=p, title = currentPair)
        descriptivesPlots[[currentPair]]$dependOn(optionContainsValue=list("pairs" = pair))
      }
    }
  }
}
