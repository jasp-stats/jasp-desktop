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

BainTTestBayesianOneSample <- function(jaspResults, dataset, options, state=NULL) {
    # Read in data and check for errors
    dataset                                         <- .readDataBainOneSample(options, dataset)
    # Null state
    if(is.null(state))
      state                                         <- list()
    # Pass the title
    jaspResults$title                               <- "Bayesian Informative One Sample T-Test"
    # Create the main results table
    .bainOneSampleTable(dataset, options, jaspResults)
    # Save analysis result in state
    bainResult                                      <- jaspResults[["bainResult"]]$object
    # Descriptive statistics
    if (options$descriptives)
    {
        if(is.null(jaspResults[["descriptives"]]))
            .bainDescriptivesOneSampleTable(dataset, options, jaspResults)        
    }
    # Bayes factor plots
    if(options$plotPriorAndPosterior)
    {
        if(is.null(jaspResults[["BFplots"]]))
        {
        jaspResults[["BFplots"]]                    <- createJaspContainer("Bayes Factor Plots") 
        jaspResults[["BFplots"]]                    $dependOnOptions(c("variables", "testValue", "hypothesis", "plotPriorAndPosterior"))
        }    
        bayesFactorPlots                            <- jaspResults[["BFplots"]]    
        for (variable in unlist(options$variables))
        {
            if(is.null(bayesFactorPlots[[variable]]))
            {
                bayesFactorPlots[[variable]]        <- .bainOneSampleBFplots(dataset, options, bainResult[[variable]], variable)
                bayesFactorPlots[[variable]]        $setOptionMustContainDependency("variables", variable)
            }
        }                
        if(bayesFactorPlots$length == 0)
            jaspResults[["BFplots"]]                <- NULL
    }
    # Descriptive plots
    if(options$descriptivesPlots)
    {
        if(is.null(jaspResults[["descriptivesPlots"]]))
        {
        jaspResults[["descriptivesPlots"]]          <- createJaspContainer("Descriptive Plots") 
        jaspResults[["descriptivesPlots"]]          $dependOnOptions(c("variables", "testValue", "descriptivesPlots", "descriptivesPlotsCredibleInterval"))
        }        
        descriptivePlots                            <- jaspResults[["descriptivesPlots"]]
        for (variable in unlist(options$variables))
        {
            if(is.null(descriptivePlots[[variable]]))
            {
                descriptivePlots[[variable]]        <- .bainOneSampleDescriptivesPlot(dataset, options, variable)
                descriptivePlots[[variable]]        $setOptionMustContainDependency("variables", variable)
            }
        }                
        if(descriptivePlots$length == 0)
            jaspResults[["descriptivePlots"]]       <- NULL
    }
    # Save the state
    state[["options"]]                              <- options
    return(state)    
}

.bainOneSampleTable <- function(dataset, options, jaspResults) {

  if(!is.null(jaspResults[["bainTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

  variables                      <- unlist(options$variables)
  
  testValue                      <- options$testValue
  hypothesis                     <- options$hypothesis
  bainTable                      <- createJaspTable("One Sample T-test Result")
  jaspResults[["bainTable"]]     <- bainTable

  bainTable$dependOnOptions(c("testValue", "variables", "hypothesis", "bayesFactorType", "logscale"))
  
  bf.type <- options$bayesFactorType
  if (bf.type == "BF10"){
    BFH1H0 <- TRUE
    bf.title <- "BF\u2081\u2080"
  } else {
    BFH1H0 <- FALSE  
    bf.title <- "BF\u2080\u2081" 
  } 
  
  if(options$logscale == "logBF")
      bf.title <- paste0("Log(",bf.title,")")
      
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

  if (options$hypothesis == "notEqualToTestValue")
  {
    type <- 1
    note <- "The alternative hypothesis H1 specifies that the mean is unequal to "
    message <- paste0(note, options$testValue, "."," The posterior probabilities are based on equal prior probabilities.")
  }
  if (options$hypothesis == "greaterThanTestValue"){
    type <- 2
    note <- "The alternative hypothesis H1 specifies that the mean is bigger than "
    message <- paste0(note, options$testValue, "."," The posterior probabilities are based on equal prior probabilities.")
  }
  if (options$hypothesis == "lessThanTestValue"){
    type <- 3
    note <- "The alternative hypothesis H1 specifies that the mean is smaller than "
    message <- paste0(note, options$testValue, "."," The posterior probabilities are based on equal prior probabilities.")
  }
  if(options$hypothesis == "_4type"){      
    type <- 4
    message <- paste0("The null hypothesis H0 specifies that the mean is bigger than ",options$testValue," and the alternative hypothesis H1 specifies that the mean is smaller than ",options$testValue,". The posterior probabilities are based on equal prior probabilities.")             
  }
  if(options$hypothesis == "allTypes"){
      type <- 5
      note <- "The null hypothesis H0 with test value "
      message <- paste0(note, options$testValue, " is tested against the other hypotheses. H1 states that the mean is bigger than ", options$testValue," and H2 states that the mean is smaller than ",options$testValue,". The posterior probabilities are based on equal prior probabilities.")
  }
        
  bainTable$addFootnote(message=message, symbol="<i>Note.</i>")
  
  bainTable$addCitation("Gu, X., Mulder, J., and Hoijtink, H. (2018). Approximate adjusted fractional Bayes factors: A general method for testing informative hypotheses. British Journal of Mathematical and Statistical Psychology, 71, 229-261. DOI: 10.1111/bmsp.12110")
  
  jaspResults$startProgressbar(length(variables))
  
  bainResult <- list()
  
  for (variable in variables){
      
      variableData <- dataset[[ .v(variable) ]]
      variableData <- variableData[ ! is.na(variableData) ]      
      variableData <- variableData - options$testValue # subtract test value from data points
      
      p <- try({
          bainAnalysis <- Bain::Bain_ttestData(variableData, type = type)
          bainResult[[variable]] <- bainAnalysis
      })
      
      if(class(p) == "try-error"){          
          message <- "An error occurred in the analysis. Please double check your variables."	
          bainTable$errorMessage <- message
          bainTable$error <- "badData"
          return()       
      }
      
    if(type == 1){
        BF_0u <- bainAnalysis$BF_0u
        PMP_u <- bainAnalysis$PMP_u
        PMP_0 <- bainAnalysis$PMP_0
        if(options$bayesFactorType == "BF10")
          BF_0u <- 1/BF_0u
        if(options$logscale == "logBF")
          BF_0u <- log(BF_0u)
    }
    if(type == 2){
        BF_01 <- bainAnalysis$BF_01
        PMP_1 <- bainAnalysis$PMP_1
        PMP_0 <- bainAnalysis$PMP_0
        if(options$bayesFactorType == "BF10")
            BF_01 <- 1/BF_01
        if(options$logscale == "logBF")
            BF_01 <- log(BF_01)
    }        
    if(type == 3){
        BF_01 <- bainAnalysis$BF_01
        PMP_0 <- bainAnalysis$PMP_0
        PMP_1 <- bainAnalysis$PMP_1
        if(options$bayesFactorType == "BF10")
            BF_01 <- 1/BF_01
        if(options$logscale == "logBF")
            BF_01 <- log(BF_01)
    }
     if (type == 4){
        BF_01 <- bainAnalysis$BF_12
        PMP_0 <- bainAnalysis$PMP_1
        PMP_1 <- bainAnalysis$PMP_2
        if(options$bayesFactorType == "BF10")
            BF_01 <- 1/BF_01
        if(options$logscale == "logBF")
            BF_01 <- log(BF_01)
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
        if(options$logscale == "logBF")
        {
            BF_01 <- log(BF_01)
            BF_02 <- log(BF_02)
            BF_12 <- log(BF_12)
        }
    }
            
            if(options$bayesFactorType == "BF01"){
                
                if(options$hypothesis == "notEqualToTestValue"){
                    row <- list(Variable=variable, "hypothesis[type1]" = "H0: Equal","BF[type1]"=.clean(BF_0u), "pmp[type1]" = .clean(PMP_0),
                                        "hypothesis[type2]" = "H1: Not equal", "BF[type2]" = "", "pmp[type2]" = .clean(PMP_u))
                } else if(options$hypothesis == "greaterThanTestValue"){
                    row <-list(Variable=variable, "hypothesis[type1]" = "H0: Equal","BF[type1]"=.clean(BF_01), "pmp[type1]" = .clean(PMP_0),
                                       "hypothesis[type2]" = "H1: Bigger", "BF[type2]" = "", "pmp[type2]" = .clean(PMP_1))
                } else if(options$hypothesis == "lessThanTestValue"){
                    row <-list(Variable=variable, "hypothesis[type1]" = "H0: Equal", "BF[type1]"=.clean(BF_01), "pmp[type1]" = .clean(PMP_0),
                                       "hypothesis[type2]" = "H1: Smaller", "BF[type2]" = "", "pmp[type2]" = .clean(PMP_1))
                } else if (options$hypothesis == "_4type"){
                    row <-list(Variable=variable, "hypothesis[type1]" = "H0: Bigger", "BF[type1]"=.clean(BF_01), "pmp[type1]" = .clean(PMP_0),
                                       "hypothesis[type2]" = "H1: Smaller", "BF[type2]" = "", "pmp[type2]" = .clean(PMP_1))
                } else if (options$hypothesis == "allTypes"){
                    row <-list(Variable=variable, 
                                       "type[equal]" = "H0: Equal",
                                       "BF[equal]"= "", 
                                       "pmp[equal]" = .clean(PMP_0),
                                       "type[greater]" = "H1: Bigger",
                                       "BF[greater]" = .clean(BF_01), 
                                       "pmp[greater]" = .clean(PMP_1),
                                       "type[less]" = "H2: Smaller",
                                       "BF[less]" = .clean(BF_02),
                                       "pmp[less]" = .clean(PMP_2)) 
                }
                
            } else if (options$bayesFactorType == "BF10"){
                
                if(options$hypothesis == "notEqualToTestValue"){
                    row <- list(Variable=variable, "hypothesis[type1]" = "H0: Equal","BF[type1]"="", "pmp[type1]" = .clean(PMP_0),
                                        "hypothesis[type2]" = "H1: Not equal", "BF[type2]" = .clean(BF_0u), "pmp[type2]" = PMP_u)
                } else if(options$hypothesis == "greaterThanTestValue"){
                    row <-list(Variable=variable, "hypothesis[type1]" = "H0: Equal","BF[type1]"="", "pmp[type1]" = .clean(PMP_0),
                                       "hypothesis[type2]" = "H1: Bigger", "BF[type2]" = .clean(BF_01), "pmp[type2]" = .clean(PMP_1))
                } else if(options$hypothesis == "lessThanTestValue"){
                    row <-list(Variable=variable, "hypothesis[type1]" = "H0: Equal", "BF[type1]"="", "pmp[type1]" = .clean(PMP_0),
                                       "hypothesis[type2]" = "H1: Smaller", "BF[type2]" = .clean(BF_01), "pmp[type2]" = .clean(PMP_1))
                } else if (options$hypothesis == "_4type"){
                    row <-list(Variable=variable, "hypothesis[type1]" = "H0: Bigger", "BF[type1]"= "", "pmp[type1]" = .clean(PMP_0),
                                       "hypothesis[type2]" = "H1: Smaller", "BF[type2]" = .clean(BF_01), "pmp[type2]" = .clean(PMP_1))
                } else if (options$hypothesis == "allTypes"){
                    row <-list(Variable=variable, 
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
  
      bainTable$addRows(row)
      jaspResults$progressbarTick()
  }

  jaspResults[["bainResult"]] <- createJaspState(bainResult)
  jaspResults[["bainResult"]]$copyDependenciesFromJaspObject(bainTable)
}

.bainDescriptivesOneSampleTable <- function(dataset, options, jaspResults) {
    
    if(!is.null(jaspResults[["descriptives"]])) return() #The options for this table didn't change so we don't need to rebuild it

    variables                                               <- unlist(options$variables)
    descriptives                                            <- createJaspTable("Descriptive Statistics")
    jaspResults[["descriptives"]]                           <- descriptives
    descriptives$dependOnOptions(c("variables", "descriptives", "descriptivesPlotsCredibleInterval"))
    
    descriptives$addColumnInfo(name="v",                    title = "", type="string")
    descriptives$addColumnInfo(name="N",                    title = "N", type="Integer")
    descriptives$addColumnInfo(name="mean",                 title = "Mean", type="number", format="sf:4;dp:3")
    descriptives$addColumnInfo(name="sd",                   title = "sd", type="number", format="sf:4;dp:3")
    descriptives$addColumnInfo(name="se",                   title = "se", type="number", format="sf:4;dp:3")
    
    interval <- 100 * options$descriptivesPlotsCredibleInterval
    overTitle <- paste0(interval, "% Credible Interval")
    descriptives$addColumnInfo(name="lowerCI",              title = "lowerCI", type="number", format="sf:4;dp:3", overtitle = overTitle)
    descriptives$addColumnInfo(name="upperCI",              title = "upperCI", type="number", format="sf:4;dp:3", overtitle = overTitle)
      
    for(variable in variables){
        
        data <- na.omit(dataset[[ .v(variable) ]])
        if (class(data) != "factor")
        {
          posteriorSummary <- .posteriorSummaryGroupMean(variable=data, descriptivesPlotsCredibleInterval=options$descriptivesPlotsCredibleInterval)
          ciLower <- .clean(round(posteriorSummary$ciLower,3))
          ciUpper <- .clean(round(posteriorSummary$ciUpper,3))
          n    <- .clean(length(data))
          mean <- .clean(round(mean(data),3))
          stdDeviation <- .clean(round(sd(data),3))
          stdErrorMean <- .clean(round((sd(data)/sqrt(length(data))),3))
          row <- list(v=variable, N=n, mean=mean, sd=stdDeviation, se=stdErrorMean, lowerCI = ciLower, upperCI = ciUpper)
        } else {
          n <- .clean(length(data))
          row <- list(v=variable, N=n, mean="", sd="", se="", lowerCI="", upperCI="")
      }
      
    descriptives$addRows(row)   
    } 
}

.bainOneSampleBFplots <- function(dataset, options, bainResult, variable){ 
    if(is.null(bainResult))
      return(createJaspPlot(error="badData", errorMessage="Plotting is not possible: No analysis has been run."))   
    BFplot <- createJaspPlot(plot=function() { Bain::plot.BainT(bainResult) }, title=variable, width = options$plotWidth, height = options$plotHeight)    
    return(BFplot)
}

.bainOneSampleDescriptivesPlot <- function(dataset, options, variable){
    variableData <- dataset[[ .v(variable) ]]
    variableData <- variableData[ ! is.na(variableData) ]
    ggplotObj    <- .plotGroupMeanBayesOneSampleTtest(variable=variableData, variableName=variable, testValueOpt=options$testValue,
                    descriptivesPlotsCredibleInterval=options$descriptivesPlotsCredibleInterval)
                    
    return(createJaspPlot(plot=ggplotObj, title = variable, width = options$plotWidth, height = options$plotHeight))
}

.readDataBainOneSample <- function(options, dataset){
    
    all.variables                                   <- unlist(options$variables)
    
    if (is.null(dataset)){   
        if (options$missingValues == "excludeListwise") {
            dataset                                 <- .readDataSetToEnd(columns.as.numeric=all.variables, exclude.na.listwise=all.variables)
        } else {            
            dataset                                 <- .readDataSetToEnd(columns.as.numeric=all.variables)
        }
    }
    
    .hasErrors(dataset, perform, type=c("infinity", "variance", "observations"),
                all.target=all.variables, message="short", observations.amount="< 3", 
                exitAnalysisIfErrors = TRUE)
    
    return(dataset)
    
}
