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

BainTTestBayesianOneSample <- function(jaspResults, dataset, options, ...) {

  ### READY ###
  ready <- length(options[["variables"]][options[["variables"]] != ""] > 0)
  
  ### READ DATA ###
  readList								<- .readDataBainOneSample(options, dataset)
  dataset									<- readList[["dataset"]]
  missingValuesIndicator	<- readList[["missingValuesIndicator"]]
  
  ### RESULTS ###
  .bainOneSampleResultsTable(dataset, options, jaspResults, missingValuesIndicator, ready)
  
  ### DESCRIPTIVES ###
  .bainOneSampleDescriptivesTable(dataset, options, jaspResults, ready)
  
  ### BAYES FACTOR PLOTS ###
  .bainTTestFactorPlots(dataset, options, jaspResults, ready, "oneSample")
  
  ### DESCRIPTIVES PLOTS ###
  .bainOneSampleDescriptivesPlot(dataset, options, jaspResults, ready)
}

.readDataBainOneSample <- function(options, dataset) {
    if (is.null(dataset)) {
            trydata									<- .readDataSetToEnd(columns.as.numeric = options[["variables"]])
            missingValuesIndicator	<- .unv(names(which(apply(trydata, 2, function(x) { any(is.na(x))} ))))
            dataset									<- .readDataSetToEnd(columns.as.numeric = options[["variables"]], exclude.na.listwise = options[["variables"]])
    }
    .hasErrors(dataset, perform, type=c("infinity", "variance", "observations"),
                all.target = options[["variables"]], observations.amount="< 3",
                exitAnalysisIfErrors = TRUE)
    readList <- list()
    readList[["dataset"]] <- dataset
    readList[["missingValuesIndicator"]] <- missingValuesIndicator
    return(readList)
}

.bainOneSampleResultsTable <- function(dataset, options, jaspResults, missingValuesIndicator, ready) {

  if (!is.null(jaspResults[["bainTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

  bainTable                      <- createJaspTable("Bain One Sample T-test")
  jaspResults[["bainTable"]]     <- bainTable
  bainTable$position <- 1
  bainTable$dependOn(options =c("testValue", "variables", "hypothesis", "bayesFactorType"))

  bf.type <- options[["bayesFactorType"]]
  BFH1H0 <- FALSE
  bf.title <- "BF"

  if (options$hypothesis == "allTypes") {
    bainTable$addColumnInfo(name="Variable",      type="string", title="")
    bainTable$addColumnInfo(name="type[equal]",   type="string", title="Hypothesis")
    bainTable$addColumnInfo(name="BF[equal]",     type="number", title=bf.title)
    bainTable$addColumnInfo(name="pmp[equal]",    type="number", format="dp:3", title="Posterior probability")
    bainTable$addColumnInfo(name="type[greater]", type="string", title="Hypothesis")
    bainTable$addColumnInfo(name="BF[greater]",   type="number", title="bf.title")
    bainTable$addColumnInfo(name="pmp[greater]",  type="number", format="dp:3", title="Posterior probability")
    bainTable$addColumnInfo(name="type[less]",    type="string", title="Hypothesis")
    bainTable$addColumnInfo(name="BF[less]",      type="number", title=bf.title)
    bainTable$addColumnInfo(name="pmp[less]",     type="number", format="dp:3", title="Posterior probability")
  } else {
    bainTable$addColumnInfo(name="Variable",          type="string", title="")
    bainTable$addColumnInfo(name="hypothesis[type1]", type="string", title="Hypothesis")
    bainTable$addColumnInfo(name="BF[type1]",         type="number", title=bf.title)
    bainTable$addColumnInfo(name="pmp[type1]",        type="number", format="dp:3", title="Posterior probability")
    bainTable$addColumnInfo(name="hypothesis[type2]", type="string", title="Hypothesis")
    bainTable$addColumnInfo(name="BF[type2]",         type="number", title=bf.title)
    bainTable$addColumnInfo(name="pmp[type2]",        type="number", format="dp:3", title="Posterior probability")
  }
  
  type <- base::switch(options[["hypothesis"]],
                        "notEqualToTestValue"   = 1,
                        "greaterThanTestValue"  = 2,
                        "lessThanTestValue"     = 3,
                        "_4type"                = 4,
                        "allTypes"              = 5)
  note <- base::switch(options[["hypothesis"]],
                        "notEqualToTestValue"   = "The alternative hypothesis H1 specifies that the mean is unequal to ",
                        "greaterThanTestValue"  = "The alternative hypothesis H1 specifies that the mean is bigger than ",
                        "lessThanTestValue"     = "The alternative hypothesis H1 specifies that the mean is smaller than ",
                        "_4type"                = NULL,
                        "allTypes"              = "The null hypothesis H0 with test value ")
  message <- base::switch(options[["hypothesis"]],
                            "notEqualToTestValue"   = paste0(note, options[["testValue"]], "."," The posterior probabilities are based on equal prior probabilities."),
                            "greaterThanTestValue"  = paste0(note, options[["testValue"]], "."," The posterior probabilities are based on equal prior probabilities."),
                            "lessThanTestValue"     = paste0(note, options[["testValue"]], "."," The posterior probabilities are based on equal prior probabilities."),
                            "_4type"                = paste0("The hypothesis H1 specifies that the mean is bigger than ",options[["testValue"]]," and the hypothesis H2 specifies that the mean is smaller than ",options[["testValue"]],". The posterior probabilities are based on equal prior probabilities."),
                            "allTypes"              = paste0(note, options[["testValue"]], " is tested against the other hypotheses. H1 states that the mean is bigger than ", options[["testValue"]]," and H2 states that the mean is smaller than ",options[["testValue"]],". The posterior probabilities are based on equal prior probabilities."))
  bainTable$addFootnote(message=message)

  bainTable$addCitation(.bainGetCitations())

  if (!ready)
    return()

  startProgressbar(length(options[["variables"]]))
  bainResult <- list()
  
  for (variable in options[["variables"]]) {

    variableData <- dataset[[ .v(variable) ]]
    variableData <- variableData[ ! is.na(variableData) ]

    p <- try({
      bainAnalysis <- Bain::Bain_ttestData(variableData, nu = options$testValue, type = type)
      bainResult[[variable]] <- bainAnalysis
    })

    if (isTryError(p)) {
			bainTable$addRows(list(Variable=variable), rowNames=variable)
			bainTable$addFootnote(message=paste0("Results not computed: ", .extractErrorMessage(p)), colNames="Variable", rowNames=variable)
			progressbarTick()
			next
		}
		
		if (variable %in% missingValuesIndicator) {
			bainTable$addFootnote(message= paste0("Variable contains missing values, the rows containing these values are removed in the analysis."), colNames="Variable", rowNames=variable)
		}

    if (type == 1) {
        BF_0u <- bainAnalysis$BF_0u
        PMP_u <- bainAnalysis$PMP_u
        PMP_0 <- bainAnalysis$PMP_0
        if (options$bayesFactorType == "BF10")
          BF_0u <- 1/BF_0u
    }
    if (type == 2) {
        BF_01 <- bainAnalysis$BF_01
        PMP_1 <- bainAnalysis$PMP_1
        PMP_0 <- bainAnalysis$PMP_0
        if (options$bayesFactorType == "BF10")
            BF_01 <- 1/BF_01
    }
    if (type == 3) {
        BF_01 <- bainAnalysis$BF_01
        PMP_0 <- bainAnalysis$PMP_0
        PMP_1 <- bainAnalysis$PMP_1
        if (options$bayesFactorType == "BF10")
            BF_01 <- 1/BF_01
    }
     if (type == 4) {
        BF_01 <- bainAnalysis$BF_12
        PMP_0 <- bainAnalysis$PMP_1
        PMP_1 <- bainAnalysis$PMP_2
        if (options$bayesFactorType == "BF10")
            BF_01 <- 1/BF_01
    }
     if (type == 5) {
        BF_01 <- bainAnalysis$BF_01
        BF_02 <- bainAnalysis$BF_02
        BF_12 <- bainAnalysis$BF_12
        PMP_0 <- bainAnalysis$PMP_0
        PMP_1 <- bainAnalysis$PMP_1
        PMP_2 <- bainAnalysis$PMP_2
        if (options$bayesFactorType == "BF10")
        {
            BF_01 <- 1/BF_01
            BF_02 <- 1/BF_02
            BF_12 <- 1/BF_12
        }
    }

    if (options$bayesFactorType == "BF01") {
        if (options$hypothesis == "notEqualToTestValue") {
            row <- list(Variable=variable, "hypothesis[type1]" = "H0: Equal","BF[type1]"=BF_0u, "pmp[type1]" = PMP_0,
                                            "hypothesis[type2]" = "H1: Not equal", "BF[type2]" = "", "pmp[type2]" = PMP_u)
        } else if (options$hypothesis == "greaterThanTestValue") {
            row <-list(Variable=variable, "hypothesis[type1]" = "H0: Equal","BF[type1]"=BF_01, "pmp[type1]" = PMP_0,
                                            "hypothesis[type2]" = "H1: Bigger", "BF[type2]" = "", "pmp[type2]" = PMP_1)
        } else if (options$hypothesis == "lessThanTestValue") {
            row <-list(Variable=variable, "hypothesis[type1]" = "H0: Equal", "BF[type1]"=BF_01, "pmp[type1]" = PMP_0,
                                            "hypothesis[type2]" = "H1: Smaller", "BF[type2]" = "", "pmp[type2]" = PMP_1)
        } else if (options$hypothesis == "_4type") {
            row <-list(Variable=variable, "hypothesis[type1]" = "H1: Bigger", "BF[type1]"=BF_01, "pmp[type1]" = PMP_0,
                                            "hypothesis[type2]" = "H2: Smaller", "BF[type2]" = "", "pmp[type2]" = PMP_1)
        } else if (options$hypothesis == "allTypes") {
            row <-list(Variable=variable, "type[equal]" = "H0: Equal", "BF[equal]"= "", "pmp[equal]" = PMP_0, 
                                            "type[greater]" = "H1: Bigger", "BF[greater]" = BF_01, "pmp[greater]" = PMP_1,
                                            "type[less]" = "H2: Smaller", "BF[less]" = BF_02, "pmp[less]" = PMP_2)
        }
    } else if (options$bayesFactorType == "BF10") {
        if (options$hypothesis == "notEqualToTestValue") {
            row <- list(Variable=variable, "hypothesis[type1]" = "H0: Equal","BF[type1]"="", "pmp[type1]" = PMP_0,
                                              "hypothesis[type2]" = "H1: Not equal", "BF[type2]" = BF_0u, "pmp[type2]" = PMP_u)
        } else if (options$hypothesis == "greaterThanTestValue") {
            row <-list(Variable=variable, "hypothesis[type1]" = "H0: Equal","BF[type1]"="", "pmp[type1]" = PMP_0,
                                            "hypothesis[type2]" = "H1: Bigger", "BF[type2]" = BF_01, "pmp[type2]" = PMP_1)
        } else if (options$hypothesis == "lessThanTestValue") {
            row <-list(Variable=variable, "hypothesis[type1]" = "H0: Equal", "BF[type1]"="", "pmp[type1]" = PMP_0,
                                            "hypothesis[type2]" = "H1: Smaller", "BF[type2]" = BF_01, "pmp[type2]" = PMP_1)
        } else if (options$hypothesis == "_4type") {
            row <-list(Variable=variable, "hypothesis[type1]" = "H1: Bigger", "BF[type1]"= "", "pmp[type1]" = PMP_0,
                                            "hypothesis[type2]" = "H2: Smaller", "BF[type2]" = BF_01, "pmp[type2]" = PMP_1)
        } else if (options$hypothesis == "allTypes") {
            row <-list(Variable=variable, "type[equal]" = "H0: Equal", "BF[equal]"= "", "pmp[equal]" = PMP_0,
                                            "type[greater]"= "H1: Bigger", "BF[greater]" = BF_01, "pmp[greater]" = PMP_1,
                                            "type[less]" = "H2: Smaller", "BF[less]" = BF_02, "pmp[less]" = PMP_2)
        }
    }
    bainTable$addRows(row, rowNames=variable)
    progressbarTick()
  }
  jaspResults[["bainResult"]] <- createJaspState(bainResult)
  jaspResults[["bainResult"]]$dependOn(optionsFromObject =bainTable)
}

.bainOneSampleDescriptivesTable <- function(dataset, options, jaspResults, ready) {

  if (!is.null(jaspResults[["descriptivesTable"]])) return() #The options for this table didn't change so we don't need to rebuild it
    if (options[["descriptives"]]) {
      
      descriptivesTable <- createJaspTable("Descriptive Statistics")
      descriptivesTable$dependOn(options =c("variables", "descriptives", "descriptivesPlotsCredibleInterval"))
      descriptivesTable$position <- 2

      descriptivesTable$addColumnInfo(name="v",                    title = "", type="string")
      descriptivesTable$addColumnInfo(name="N",                    title = "N", type="integer")
      descriptivesTable$addColumnInfo(name="mean",                 title = "Mean", type="number")
      descriptivesTable$addColumnInfo(name="sd",                   title = "SD", type="number")
      descriptivesTable$addColumnInfo(name="se",                   title = "SE", type="number")

      interval <- 100 * options[["descriptivesPlotsCredibleInterval"]]
      overTitle <- paste0(interval, "% Credible Interval")
      descriptivesTable$addColumnInfo(name="lowerCI",              title = "Lower", type="number", overtitle = overTitle)
      descriptivesTable$addColumnInfo(name="upperCI",              title = "Upper", type="number", overtitle = overTitle)
      
      jaspResults[["descriptivesTable"]] <- descriptivesTable

      if (!ready)
        return()

      for (variable in options[["variables"]]) {
          data <- na.omit(dataset[[ .v(variable) ]])
          if (class(data) != "factor") { # TODO: Fix this...
            posteriorSummary <- .posteriorSummaryGroupMean(variable=data, descriptivesPlotsCredibleInterval=options$descriptivesPlotsCredibleInterval)
            ciLower <- round(posteriorSummary$ciLower,3)
            ciUpper <- round(posteriorSummary$ciUpper,3)
            n    <- length(data)
            mean <- round(mean(data),3)
            stdDeviation <- round(sd(data),3)
            stdErrorMean <- round((sd(data)/sqrt(length(data))),3)
            row <- list(v=variable, N=n, mean=mean, sd=stdDeviation, se=stdErrorMean, lowerCI = ciLower, upperCI = ciUpper)
          } else {
            n <- length(data)
            row <- list(v=variable, N=n, mean="", sd="", se="", lowerCI="", upperCI="")
        }
        descriptivesTable$addRows(row)
      }
    }
}

.bainOneSampleDescriptivesPlot <- function(dataset, options, jaspResults, ready) {
  if (options[["descriptivesPlots"]] && ready) {
      if (is.null(jaspResults[["descriptivesPlots"]])) {
      jaspResults[["descriptivesPlots"]] <- createJaspContainer("Descriptive Plots")
      jaspResults[["descriptivesPlots"]]$dependOn(options =c("descriptivesPlots", "descriptivesPlotsCredibleInterval"))
      jaspResults[["descriptivesPlots"]]$position <- 4
      }
      for (variable in unlist(options[["variables"]])) {
          if (is.null(jaspResults[["descriptivesPlots"]][[variable]]))
          {
            variableData <- dataset[[ .v(variable) ]]
            variableData <- variableData[ ! is.na(variableData) ]
            ggplotObj    <- .plotGroupMeanBayesOneSampleTtest(variable=variableData, variableName=variable, testValueOpt=options[["testValue"]],
                            descriptivesPlotsCredibleInterval=options$descriptivesPlotsCredibleInterval)
            jaspResults[["descriptivesPlots"]][[variable]]        <- createJaspPlot(plot=ggplotObj, title = variable)
            jaspResults[["descriptivesPlots"]][[variable]]        $dependOn(optionContainsValue=list("variables" = variable))
          }
      }
  } else if (options[["descriptivesPlots"]]) {
    emptyPlot <- createJaspPlot(plot = NULL, title = "Descriptives Plots")
    jaspResults[["descriptivesPlots"]] <- emptyPlot
    jaspResults[["descriptivesPlots"]]$dependOn(options =c("variables", "descriptivesPlots"))
    jaspResults[["descriptivesPlots"]]$position <- 4
  }
}

.bainTTestFactorPlots <- function(dataset, options, jaspResults, ready, type) {
  if (!options[["bayesFactorPlot"]]) return()

  if (is.null(jaspResults[["BFplots"]])) {
    BFplots <- createJaspContainer("Bayes Factor Comparison")
    BFplots$dependOn(options=c("testValue", "hypothesis", "bayesFactorPlot", "groupingVariable"))
    BFplots$position <- 3
    jaspResults[["BFplots"]] <- BFplots
  } else {
    BFplots <- jaspResults[["BFplots"]]
  }

  if (!ready) {
    BFplots[["placeHolder"]] <- createJaspPlot(plot = NULL, title = "", height = 400, width = 600, dependencies=c("pairs", "variables"))
    return()
  }
  
  bainResult <- jaspResults[["bainResult"]]$object
  
  if (type == "pairedSamples") {
    option <- "pairs"
    dependencies <- options$pairs
    variables <- unlist(lapply(options$pairs, paste, collapse=" - "))
  } else {
    option <- "variables"
    variables <- dependencies <- options$variables
  }
  
  for (i in seq_along(variables)) {
    variable <- variables[i]
    if (!is.null(BFplots[[variable]]))
      next

    plot <- createJaspPlot(plot = NULL, title = variable, height = 400, width = 600)

    dependency <- list()
    dependency[[option]] <- dependencies[[i]]
    plot$dependOn(optionContainsValue=dependency)

    if (!is.null(bainResult[[variable]]))
      plot$plotObject <- .plot.BainT(bainResult[[variable]])
    else
      plot$setError("Plotting not possible: the results for this variable were not computed.")
      
    BFplots[[variable]] <- plot
  }
}

.plot.BainT <- function(x, y, ...) {
    if (length(x) == 4 && names(x)[1] == "BF_0u") {
        PMP <- x$PMP_0
        lab = c("H0", "H1")
        PMPb = c(PMP, 1 - PMP)
        ggdata <- data.frame(lab = lab, PMPb = PMPb)
        p <- ggplot2::ggplot(data = ggdata, mapping = ggplot2::aes(x = "", y = PMPb,
                            fill = lab)) +
            ggplot2::geom_bar(stat = "identity", width = 1e10, color = "black", size = 1) +
            ggplot2::geom_col()
        pp <- p + ggplot2::coord_polar(theta = "y", direction = -1) +
                  ggplot2::labs(x = "", y = "", title = "PMP") +
                  ggplot2::theme(panel.grid = ggplot2::element_blank(),
                                legend.position = "none") +
                  ggplot2::scale_y_continuous(
                    breaks = cumsum(rev(PMPb)) - rev(PMPb)/2,
                    labels = rev(lab))
        pp <- pp + ggplot2::theme(panel.background = ggplot2::element_blank(),
                                  axis.text=ggplot2::element_text(size=17, color = "black"),
                                  plot.title = ggplot2::element_text(size=18, hjust = .5),
                                  axis.ticks.y = ggplot2::element_blank())
        pp <- pp + ggplot2::scale_fill_brewer(palette="Set1")
    }
    if (length(x) == 4 && names(x)[1] == "BF_01") {
        PMP <- x$PMP_0
        lab = c("H0", "H1")
        PMPb = c(PMP, 1 - PMP)
        ggdata <- data.frame(lab = lab, PMPb = PMPb)
        p <- ggplot2::ggplot(data = ggdata, mapping = ggplot2::aes(x = "", y = PMPb,
                              fill = lab)) +
            ggplot2::geom_bar(stat = "identity", width = 1e10, color = "black", size = 1) +
            ggplot2::geom_col()
        pp <- p + ggplot2::coord_polar(theta = "y", direction = -1) +
            ggplot2::labs(x = "", y = "", title = "PMP") +
            ggplot2::theme(panel.grid = ggplot2::element_blank(),
                            legend.position = "none") +
            ggplot2::scale_y_continuous(
              breaks = cumsum(rev(PMPb)) - rev(PMPb)/2,
              labels = rev(lab))
        pp <- pp + ggplot2::theme(panel.background = ggplot2::element_blank(),
                                  axis.text=ggplot2::element_text(size=17, color = "black"),
                                  plot.title = ggplot2::element_text(size=18, hjust = .5),
                                  axis.ticks.y = ggplot2::element_blank())
        pp <- pp + ggplot2::scale_fill_brewer(palette="Set1")
    }
    if (length(x) == 4 && names(x)[1] == "BF_12") {
        PMP <- x$PMP_1
        lab = c("H1", "H2")
        PMPb = c(PMP, 1 - PMP)
        ggdata <- data.frame(lab = lab, PMPb = PMPb)
        p <- ggplot2::ggplot(data = ggdata, mapping = ggplot2::aes(x = "", y = PMPb,
                              fill = lab)) +
            ggplot2::geom_bar(stat = "identity", width = 1e10, color = "black", size = 1) +
            ggplot2::geom_col()
        pp <- p + ggplot2::coord_polar(theta = "y", direction = -1) +
            ggplot2::labs(x = "", y = "", title = "PMP") +
            ggplot2::theme(panel.grid = ggplot2::element_blank(),
                            legend.position = "none") +
            ggplot2::scale_y_continuous(
              breaks = cumsum(rev(PMPb)) - rev(PMPb)/2,
              labels = rev(lab))
        pp <- pp + ggplot2::theme(panel.background = ggplot2::element_blank(),
                                  axis.text=ggplot2::element_text(size=17, color = "black"),
                                  plot.title = ggplot2::element_text(size=18, hjust = .5),
                                  axis.ticks.y = ggplot2::element_blank())
        pp <- pp + ggplot2::scale_fill_brewer(palette="Set1")
    }
    if (length(x) == 7) {
        PMP <- c(x$PMP_0, x$PMP_1, x$PMP_2)
        lab = c("H0", "H1", "H2")
        ggdata <- data.frame(lab = lab, PMP = PMP)
        p <- ggplot2::ggplot(data = ggdata, mapping = ggplot2::aes(x = "", y = PMP,
                              fill = lab)) +
            ggplot2::geom_bar(stat = "identity", width = 1e10, color = "black", size = 1) +
            ggplot2::geom_col()
        pp <- p + ggplot2::coord_polar(theta = "y", direction = -1) +
            ggplot2::labs(x = "", y = "", title = "PMP") +
            ggplot2::theme(panel.grid = ggplot2::element_blank(),
                            legend.position = "none") +
            ggplot2::scale_y_continuous(breaks = cumsum(rev(PMP)) - rev(PMP)/2, labels = rev(lab))
        pp <- pp + ggplot2::theme(panel.background = ggplot2::element_blank(),
                                  axis.text=ggplot2::element_text(size=17, color = "black"),
                                  plot.title = ggplot2::element_text(size=18, hjust = .5),
                                  axis.ticks.y = ggplot2::element_blank())
        pp <- pp + ggplot2::scale_fill_brewer(palette="Set1")
    }
    return(pp)
}
