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
  print(unlist(options[["pairs"]]))
  ready <- !(""%in%unlist(options[["pairs"]])) && !is.null(unlist(options[["pairs"]])) # TODO: Fix this
  
  ### READ DATA ###
  readList <- .readDataBainPairedSamples(options, dataset)
  dataset <- readList[["dataset"]]
  missingValuesIndicator <- readList[["missingValuesIndicator"]]
  
  ### RESULTS ###
  .bainPairedSamplesResultsTable(dataset, options, jaspResults, missingValuesIndicator, ready)
  
  ### DESCRIPTIVES ###
  .bainPairedSamplesDescriptivesTable(dataset, options, jaspResults, ready)
  
  ### BAYES FACTOR PLOTS ###
  .bainPairedSamplesBayesFactorPlots(dataset, options, jaspResults, ready)
  
  ### DESCRIPTIVES PLOTS ###
  .bainPairedSamplesDescriptivesPlots(dataset, options, jaspResults, ready)
}

.bainPairedSamplesResultsTable <- function(dataset, options, jaspResults, missingValuesIndicator, ready) {
    if(!is.null(jaspResults[["bainTable"]])) return()

    bainTable                      <- createJaspTable("Bain Paired Samples T-Test Result")
    jaspResults[["bainTable"]]     <- bainTable
    bainTable$dependOn(options =c("pairs", "hypothesis", "bayesFactorType"))
    bainTable$position <- 1

    bf.type <- options$bayesFactorType
    BFH1H0 <- FALSE
    bf.title <- "BF"

    if(options$hypothesis == "allTypes"){
            bainTable$addColumnInfo(name="Variable", type="string", title="")
            bainTable$addColumnInfo(name = "type[equal]", type = "string", title = "Hypothesis")
            bainTable$addColumnInfo(name="BF[equal]", type="number", format="sf:4;dp:3", title=bf.title)
            bainTable$addColumnInfo(name="pmp[equal]", type="number", format="dp:3", title="Posterior probability")
            bainTable$addColumnInfo(name = "type[greater]", type = "string", title = "Hypothesis")
            bainTable$addColumnInfo(name="BF[greater]", type="number", format="sf:4;dp:3", title="bf.title")
            bainTable$addColumnInfo(name="pmp[greater]", type="number", format="dp:3", title="Posterior probability")
            bainTable$addColumnInfo(name = "type[less]", type = "string", title = "Hypothesis")
            bainTable$addColumnInfo(name = "BF[less]", type = "number", format="sf:4;dp:3", title = bf.title)
            bainTable$addColumnInfo(name="pmp[less]", type="number", format="dp:3", title="Posterior probability")
    } else {
            bainTable$addColumnInfo(name="Variable", type="string", title="")
            bainTable$addColumnInfo(name = "hypothesis[type1]", type = "string", title = "Hypothesis")
            bainTable$addColumnInfo(name="BF[type1]", type="number", format="sf:4;dp:3", title=bf.title)
            bainTable$addColumnInfo(name="pmp[type1]", type="number", format="dp:3", title="Posterior probability")
            bainTable$addColumnInfo(name = "hypothesis[type2]", type = "string", title = "Hypothesis")
            bainTable$addColumnInfo(name="BF[type2]", type="number", format="sf:4;dp:3", title=bf.title)
            bainTable$addColumnInfo(name="pmp[type2]", type="number", format="dp:3", title="Posterior probability")
    }
    
    type <- base::switch(options[["hypothesis"]],
                            "groupsNotEqual"    = 1,
                            "groupOneGreater"   = 2,
                            "groupTwoGreater"   = 3,
                            "_4type"            = 4,
                            "allTypes"          = 5)
    message <- base::switch(options[["hypothesis"]],
                              "groupsNotEqual"    = "The alternative hypothesis H1 specifies that the mean of variable 1 is unequal to the mean of variable 2. The posterior probabilities are based on equal prior probabilities.",
                              "groupOneGreater"   = "The alternative hypothesis H1 specifies that the mean of variable 1 is bigger than the mean of variable 2. The posterior probabilities are based on equal prior probabilities.",
                              "groupTwoGreater"   = "The alternative hypothesis H1 specifies that the mean of variable 1 is smaller than the mean of variable 2. The posterior probabilities are based on equal prior probabilities.",
                              "_4type"            = "The hypothesis H1 specifies that the mean of variable 1 is bigger than the mean of variable 2, while the hypothesis H2 specifies that it is smaller. The posterior probabilities are based on equal prior probabilities.",
                              "allTypes"          = "The null hypothesis H0 with equal means is tested against the other hypotheses. The alternative hypothesis H1 states that the mean of variable 1 is bigger than the mean of variable 2. The alternative hypothesis H2 states that the mean of variable 1 is smaller than the mean of variable 2. The posterior probabilities are based on equal prior probabilities.")
    bainTable$addFootnote(message = message, symbol = "<i>Note.</i>")

    bainTable$addCitation("Gu, X., Mulder, J., and Hoijtink, H. (2017). Approximate adjusted fractional Bayes factors: A general method for testing informative hypotheses. British Journal of Mathematical and Statistical Psychology. DOI:10.1111/bmsp.12110")
    bainTable$addCitation("Hoijtink, H., Mulder, J., van Lissa, C., and Gu, X. (2018). A Tutorial on testing hypotheses using the Bayes factor. Psychological Methods.")
    bainTable$addCitation("Hoijtink, H., Gu, X., and Mulder, J. (2018). Bayesian evaluation of informative hypotheses for multiple populations. Britisch Journal of Mathematical and Statistical Psychology. DOI: 10.1111/bmsp.12145")

    if(!ready)
      return()

    jaspResults$startProgressbar(length(options[["pairs"]]))
    bainResult <- list()

    for (pair in options[["pairs"]]){

      if(any(pair %in% missingValuesIndicator)){
        i <- which(pair %in% missingValuesIndicator)
          if(length(i) > 1){
            bainTable$addFootnote(message= paste0("The variables ", pair[1], " and ", pair[2], " contain missing values, the rows containing these values are removed in the analysis."), symbol="<b>Warning.</b>")
          } else if (length(i) == 1){
            bainTable$addFootnote(message= paste0("The variable ", pair[i], " contains missing values, the rows containing these values are removed in the analysis."), symbol="<b>Warning.</b>")
          }
      }

        currentPair <- paste(pair, collapse=" - ")

        if(length(pair) > 0 && pair[[2]] != "" && pair[[1]] != pair[[2]]){

            subDataSet <- subset(dataset, select=c(.v(pair[[1]]), .v(pair[[2]])) )
            subDataSet <- na.omit(subDataSet)
            c1 <- subDataSet[[ .v(pair[[1]]) ]]
            c2 <- subDataSet[[ .v(pair[[2]]) ]]

            p <- try({
                analysisPerformed <- TRUE
                bainAnalysis <- Bain::Bain_ttestData(c1, c2, paired = TRUE, type = type)
                bainResult[[currentPair]] <- bainAnalysis
            })

        } else {
            analysisPerformed <- FALSE
        }

        if(analysisPerformed){

            if(class(p) == "try-error"){
              bainTable$setError("An error occurred in the analysis. Please double check your variables.")
              return()
            }

            if(type == 1){
                BF_0u <- bainAnalysis$BF_0u
                PMP_u <- bainAnalysis$PMP_u
                PMP_0 <- bainAnalysis$PMP_0
                if(options$bayesFactorType == "BF10")
                  BF_0u <- 1/BF_0u
            }
            if(type == 2){
                BF_01 <- bainAnalysis$BF_01
                PMP_1 <- bainAnalysis$PMP_1
                PMP_0 <- bainAnalysis$PMP_0
                if(options$bayesFactorType == "BF10")
                    BF_01 <- 1/BF_01
            }
            if(type == 3){
                BF_01 <- bainAnalysis$BF_01
                PMP_0 <- bainAnalysis$PMP_0
                PMP_1 <- bainAnalysis$PMP_1
                if(options$bayesFactorType == "BF10")
                    BF_01 <- 1/BF_01
            }
             if (type == 4){
                BF_01 <- bainAnalysis$BF_12
                PMP_0 <- bainAnalysis$PMP_1
                PMP_1 <- bainAnalysis$PMP_2
                if(options$bayesFactorType == "BF10")
                    BF_01 <- 1/BF_01
            }
             if (type == 5){
                BF_01 <- bainAnalysis$BF_01
                BF_02 <- bainAnalysis$BF_02
                BF_12 <- bainAnalysis$BF_12
                PMP_0 <- bainAnalysis$PMP_0
                PMP_1 <- bainAnalysis$PMP_1
                PMP_2 <- bainAnalysis$PMP_2
                if(options$bayesFactorType == "BF10")
                {
                    BF_01 <- 1/BF_01
                    BF_02 <- 1/BF_02
                    BF_12 <- 1/BF_12
                }
            }

        if(options$bayesFactorType == "BF01"){
            if(options$hypothesis == "groupsNotEqual"){
                row <- list(Variable=currentPair, "hypothesis[type1]" = "H0: Equal","BF[type1]"=.clean(BF_0u), "pmp[type1]" = .clean(PMP_0),
                                    "hypothesis[type2]" = "H1: Not equal", "BF[type2]" = "", "pmp[type2]" = .clean(PMP_u))
            }
            if(options$hypothesis == "groupTwoGreater"){
                row <-list(Variable=currentPair, "hypothesis[type1]" = "H0: Equal","BF[type1]"= .clean(BF_01), "pmp[type1]" = .clean(PMP_0),
                                   "hypothesis[type2]" = "H1: Smaller", "BF[type2]" = "", "pmp[type2]" = .clean(PMP_1))
            }
            if(options$hypothesis == "groupOneGreater"){
                row <-list(Variable=currentPair, "hypothesis[type1]" = "H0: Equal", "BF[type1]"= .clean(BF_01), "pmp[type1]" = .clean(PMP_0),
                                   "hypothesis[type2]" = "H1: Bigger", "BF[type2]" = "", "pmp[type2]" = .clean(PMP_1))
            }
            if(options$hypothesis == "_4type"){
                row <-list(Variable=currentPair, "hypothesis[type1]" = "H1: Bigger", "BF[type1]"= .clean(BF_01), "pmp[type1]" = .clean(PMP_0),
                                   "hypothesis[type2]" = "H2: Smaller", "BF[type2]" = "", "pmp[type2]" = .clean(PMP_1))
            }
            if(options$hypothesis == "allTypes"){
                row <-list(Variable=currentPair,
                                   "type[equal]" = "H0: Equal",
                                   "BF[equal]"= "",
                                   "pmp[equal]" = .clean(PMP_0),
                                   "type[greater]"= "H1: Bigger",
                                   "BF[greater]" = .clean(BF_01),
                                   "pmp[greater]" = .clean(PMP_1),
                                   "type[less]" = "H2: Smaller",
                                   "BF[less]" = .clean(BF_02),
                                   "pmp[less]" = .clean(PMP_2))
            }
        } else if (options$bayesFactorType == "BF10"){
            if(options$hypothesis == "groupsNotEqual"){
                row <- list(Variable=currentPair, "hypothesis[type1]" = "H0: Equal","BF[type1]"="", "pmp[type1]" = .clean(PMP_0),
                                    "hypothesis[type2]" = "H1: Not equal", "BF[type2]" = .clean(BF_0u), "pmp[type2]" = .clean(PMP_u))
            }
            if(options$hypothesis == "groupTwoGreater"){
                row <-list(Variable=currentPair, "hypothesis[type1]" = "H0: Equal","BF[type1]"= "", "pmp[type1]" = .clean(PMP_0),
                                   "hypothesis[type2]" = "H1: Smaller", "BF[type2]" = .clean(BF_01), "pmp[type2]" = .clean(PMP_1))
            }
            if(options$hypothesis == "groupOneGreater"){
                row <-list(Variable=currentPair, "hypothesis[type1]" = "H0: Equal", "BF[type1]"= "", "pmp[type1]" = .clean(PMP_0),
                                   "hypothesis[type2]" = "H1: Bigger", "BF[type2]" = .clean(BF_01), "pmp[type2]" = .clean(PMP_1))
            }
            if(options$hypothesis == "_4type"){
                row <-list(Variable=currentPair, "hypothesis[type1]" = "H1: Bigger", "BF[type1]"= "", "pmp[type1]" = .clean(PMP_0),
                                   "hypothesis[type2]" = "H2: Smaller", "BF[type2]" = .clean(BF_01), "pmp[type2]" = .clean(PMP_1))
            }
            if(options$hypothesis == "allTypes"){
                row <-list(Variable=currentPair,
                                   "type[equal]" = "H0: Equal",
                                   "BF[equal]"= "",
                                   "pmp[equal]" = .clean(PMP_0),
                                   "type[greater]"= "H1: Bigger",
                                   "BF[greater]" = .clean(BF_01),
                                   "pmp[greater]" = .clean(PMP_1),
                                   "type[less]" = "H2: Smaller",
                                   "BF[less]" = .clean(BF_02),
                                   "pmp[less]" = .clean(PMP_2))
            }
        }
    } else {
        if(options$hypothesis == "allTypes"){
            row <- list(Variable=currentPair, "type[equal]" = ".", "BF[equal]"= ".", "pmp[equal]" = ".",
                               "type[greater]"= ".", "BF[greater]" = ".", "pmp[greater]" = ".",
                               "type[less]" = ".", "BF[less]" = ".", "pmp[less]" = ".")
        } else {
            row <- list(Variable=currentPair, "hypothesis[type1]" = ".", "BF[type1]"= ".", "pmp[type1]" = ".",
                               "hypothesis[type2]" = ".", "BF[type2]" = ".", "pmp[type2]" = ".")
        }
    }
    bainTable$addRows(row)
    jaspResults$progressbarTick()
  }
  jaspResults[["bainResult"]] <- createJaspState(bainResult)
  jaspResults[["bainResult"]]$dependOn(optionsFromObject =bainTable)
}

.bainPairedSamplesDescriptivesTable <- function(dataset, options, jaspResults, ready){

    if(!is.null(jaspResults[["descriptivesTable"]])) return() #The options for this table didn't change so we don't need to rebuild it  
      if(options[["descriptives"]]){
      
    descriptivesTable                                            <- createJaspTable("Descriptive Statistics")
    jaspResults[["descriptivesTable"]]                           <- descriptivesTable
    descriptivesTable$dependOn(options =c("pairs", "descriptives", "descriptivesPlotsCredibleInterval"))
    descriptivesTable$position <- 2

    descriptivesTable$addColumnInfo(name="v",                    title = "", type="string")
    descriptivesTable$addColumnInfo(name="N",                    title = "N", type="integer")
    descriptivesTable$addColumnInfo(name="mean",                 title = "Mean", type="number", format="sf:4;dp:3")
    descriptivesTable$addColumnInfo(name="sd",                   title = "sd", type="number", format="sf:4;dp:3")
    descriptivesTable$addColumnInfo(name="se",                   title = "se", type="number", format="sf:4;dp:3")

    interval <- 100 * options[["descriptivesPlotsCredibleInterval"]]
    overTitle <- paste0(interval, "% Credible Interval")
    descriptivesTable$addColumnInfo(name="lowerCI",              title = "lowerCI", type="number", format="sf:4;dp:3", overtitle = overTitle)
    descriptivesTable$addColumnInfo(name="upperCI",              title = "upperCI", type="number", format="sf:4;dp:3", overtitle = overTitle)
    
    if(!ready)
      return()

    for(variable in unique(unlist(options[["pairs"]]))){

        if(variable == "")
            next

        variableData <- dataset[[.v(variable)]]
        variableDataOm <- na.omit(variableData)

        posteriorSummary <- .posteriorSummaryGroupMean(variable=variableDataOm, descriptivesPlotsCredibleInterval=options$descriptivesPlotsCredibleInterval)
        ciLower<- round(posteriorSummary$ciLower,3)
        ciLower <- .clean(ciLower)
        ciUpper <- round(posteriorSummary$ciUpper,3)
        ciUpper <- .clean(ciUpper)

        n <- .clean(as.numeric(length(variableDataOm)))
        m <- .clean(as.numeric(mean(variableDataOm)))
        std <- .clean(as.numeric(sd(variableDataOm)))

        if(is.numeric(std)){
            se <- .clean(round((as.numeric(std/sqrt(n))),3))
        } else {
            se <- .clean(NaN)
        }

        row <- list(v=variable, N=n, mean=m, sd=std, se=se, lowerCI=ciLower, upperCI=ciUpper)
        descriptivesTable$addRows(row)
    }

    for (i in .indices(options[["pairs"]])) {

    pair <- options[["pairs"]][[i]]

    if (!(pair[[1]] == "" || pair[[2]] == "" || pair[[1]] == pair[[2]])) {

    subDataSet <- subset(dataset, select=c(.v(pair[[1]]), .v(pair[[2]])))
    subDataSet <- na.omit(subDataSet)

    c1 <- subDataSet[[ .v(pair[[1]]) ]]
    c2 <- subDataSet[[ .v(pair[[2]]) ]]

    currentPair <- paste(pair, collapse=" - ")
    diff <- c1-c2
    meandiff <- mean(diff)
    sd <- sd(diff)
    se <- sqrt(var(c1) + var(c2) - (2*cor(c1,c2)*sd(c1)*sd(c2)))/sqrt(length(diff))
    N <- length(diff)

    ciLower <- round(meandiff - 1.96*se,3)
    ciUpper <- round(meandiff + 1.96*se,3)

    row <- list(v=currentPair, N=.clean(N), mean=.clean(meandiff), sd=.clean(sd), se=.clean(se), lowerCI=.clean(ciLower), upperCI=.clean(ciUpper))
    descriptivesTable$addRows(row)
    }
  }
}
}

.bainPairedSamplesDescriptivesPlots <- function(dataset, options, jaspResults, ready){
  if(options[["descriptivesPlots"]] && ready){
      if(is.null(jaspResults[["descriptivesPlots"]])){
      jaspResults[["descriptivesPlots"]]          <- createJaspContainer("Descriptive Plots")
      jaspResults[["descriptivesPlots"]]          $dependOn(options =c("variables", "testValue", "descriptivesPlots", "descriptivesPlotsCredibleInterval"))
      jaspResults[["descriptivesPlots"]]			    $position <- 4
      }
      for (pair in options[["pairs"]]){
          if(is.null(jaspResults[["descriptivesPlots"]][[paste(pair, collapse=" - ")]]) && length(pair)==2)
          {
            subDataSet <- subset(dataset, select=c(.v(pair[[1]]), .v(pair[[2]])) )
            subDataSet <- na.omit(subDataSet)
            c1 <- subDataSet[[ .v(pair[[1]]) ]]
            c2 <- subDataSet[[ .v(pair[[2]]) ]]
            difference <- c1 - c2        
            ggplotObj <- .plotGroupMeanBayesOneSampleTtest(variable=difference, variableName=paste0(pair[[1]]," - ", pair[[2]]),
                            testValueOpt=0, descriptivesPlotsCredibleInterval=options$descriptivesPlotsCredibleInterval)
            jaspResults[["descriptivesPlots"]][[paste(pair, collapse=" - ")]]        <- createJaspPlot(plot=ggplotObj, title = paste(pair, collapse=" - "))
            jaspResults[["descriptivesPlots"]][[paste(pair, collapse=" - ")]]        $dependOn(optionContainsValue=list("pairs" = paste(pair, collapse=" - ")))
          }
      }
  } else if(options[["descriptivesPlots"]]){
    errorPlot <- createJaspPlot(plot = NULL, title = "Descriptives Plots")
    errorPlot$setError("Plotting not possible: No analysis has been run.")
    jaspResults[["descriptivesPlots"]] <- errorPlot
    jaspResults[["descriptivesPlots"]]$dependOn(options =c("variables", "descriptivesPlots"))
    jaspResults[["descriptivesPlots"]]$position <- 4
  }
}

.readDataBainPairedSamples <- function(options, dataset){
    all.variables                                                       <- unique(unlist(options$pairs))
    all.variables                                                       <- all.variables[all.variables != ""]
    pairs                                                               <- options$pairs
    # Read in data
    if (is.null(dataset)) {
            trydata                                                     <- .readDataSetToEnd(columns.as.numeric=all.variables)
            missingValuesIndicator                                      <- .unv(names(which(apply(trydata, 2, function(x){ any(is.na(x))} ))))
            dataset                                                     <- .readDataSetToEnd(columns.as.numeric=all.variables, exclude.na.listwise=all.variables)
    }
    .hasErrors(dataset, perform, type=c("infinity", "variance", "observations"),
                all.target=all.variables, message="short", observations.amount="< 3",
                exitAnalysisIfErrors = TRUE)
    readList <- list()
    readList[["dataset"]] <- dataset
    readList[["missingValuesIndicator"]] <- missingValuesIndicator
    return(readList)
}

.bainPairedSamplesBayesFactorPlots <- function(dataset, options, jaspResults, ready){
  bainResult <- jaspResults[["bainResult"]]$object
  if(options[["bayesFactorPlot"]] && ready){
      if(is.null(jaspResults[["BFplots"]])){
      jaspResults[["BFplots"]]                    <- createJaspContainer("Bayes Factor Comparison")
      jaspResults[["BFplots"]]                    $dependOn(options =c("variables", "testValue", "hypothesis", "bayesFactorPlot"))
      jaspResults[["BFplots"]]					          $position <- 3
      }
      for (pair in options[["pairs"]]){
          if(is.null(jaspResults[["BFplots"]][[paste(pair, collapse=" - ")]])){
              if(is.null(bainResult[[paste(pair, collapse=" - ")]])){
                errorPlot <- createJaspPlot(plot = NULL, title = paste(pair, collapse=" - "), height = 400, width = 600)
                errorPlot$setError("Plotting not possible: No analysis has been run.")
                jaspResults[["BFplots"]][[paste(pair, collapse=" - ")]] <- errorPlot
                jaspResults[["BFplots"]][[paste(pair, collapse=" - ")]]$dependOn(optionContainsValue=list("variables" = paste(pair, collapse=" - ")))
              } else{
                p <- .plot.BainT(bainResult[[paste(pair, collapse=" - ")]])
                jaspResults[["BFplots"]][[paste(pair, collapse=" - ")]] <- createJaspPlot(plot = p, title = paste(pair, collapse=" - "), height = 400, width = 600)
                jaspResults[["BFplots"]][[paste(pair, collapse=" - ")]]$dependOn(optionContainsValue=list("variables" = paste(pair, collapse=" - ")))
              }
          }
      }
  } else if(options[["bayesFactorPlot"]]){
    errorPlot <- createJaspPlot(plot = NULL, title = "Bayes Factor Comparison", height = 400, width = 600)
    errorPlot$setError("Plotting not possible: No analysis has been run.")
    jaspResults[["BFplots"]] <- errorPlot
    jaspResults[["BFplots"]]$dependOn(options =c("variables", "bayesFactorPlot"))
    jaspResults[["BFplots"]]$position <- 3
  }
}
