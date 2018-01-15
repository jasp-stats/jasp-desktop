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

BainTTestBayesianIndependentSamples <- function(jaspResults, dataset, options, state=NULL) {
		# Read in data and check for errors
		readList											<- .readDataBainTwoSample(options, dataset)
		dataset                       <- readList[["dataset"]]
		missingValuesIndicator        <- readList[["missingValuesIndicator"]]
    # Null state
    if(is.null(state))
      state 										<- list()
    # Pass the title
    jaspResults$title 								<- "Bain Independent Samples Welch's T-Test"
    # Create the main results table
    .bainTwoSampleTable(dataset, options, jaspResults, missingValuesIndicator)
	# Save analysis result in state
    bainResult 										<- jaspResults[["bainResult"]]$object
    # Descriptive statistics
    if (options$descriptives)
    {
        if(is.null(jaspResults[["descriptives"]]))
            .bainDescriptivesTwoSampleTable(dataset, options, jaspResults)        
    }
	# Bayes factor plots
    if(options$plotPriorAndPosterior)
    {
        if(is.null(jaspResults[["BFplots"]]))
        {
        jaspResults[["BFplots"]] 					<- createJaspContainer("Bayes Factor Comparison") 
        jaspResults[["BFplots"]]					$dependOnOptions(c("variables", "testValue", "hypothesis", "plotPriorAndPosterior"))
				jaspResults[["BFplots"]]					$position <- 3
        }    
        bayesFactorPlots <- jaspResults[["BFplots"]]
        for (variable in unlist(options$variables))
        {
            if(is.null(bayesFactorPlots[[variable]]))
            {
                bayesFactorPlots[[variable]] 		<- .bainTwoSampleBFplots(dataset, options, bainResult[[variable]], variable)
                bayesFactorPlots[[variable]]		$setOptionMustContainDependency("variables", variable)
            }
        }
        if(bayesFactorPlots$length == 0)
            jaspResults[["BFplots"]] 				<- NULL
    }
	# Descriptive plots
    if(options$descriptivesPlots)
    {
        if(is.null(jaspResults[["descriptivesPlots"]]))
        {
        jaspResults[["descriptivesPlots"]] 			<- createJaspContainer("Descriptive Plots") 
        jaspResults[["descriptivesPlots"]]			$dependOnOptions(c("variables", "testValue", "descriptivesPlots", "descriptivesPlotsCredibleInterval"))
				jaspResults[["descriptivesPlots"]]			$position <- 4
        }    
        descriptivePlots 							<- jaspResults[["descriptivesPlots"]]
        for (variable in unlist(options$variables))
        {
            if(is.null(descriptivePlots[[variable]]))
            {
                descriptivePlots[[variable]] 		<- .bainTwoSampleDescriptivesPlot(dataset, options, variable)
                descriptivePlots[[variable]]		$setOptionMustContainDependency("variables", variable)
			}
        }            
        if(descriptivePlots$length == 0)
            jaspResults[["descriptivePlots"]] 		<- NULL
    }
    # Save the state
    state[["options"]] <- options
    return(state)    
}

.bainTwoSampleTable <- function(dataset, options, jaspResults, missingValuesIndicator) {

  if(!is.null(jaspResults[["bainTable"]])) return() #The options for this table didn't change so we don't need to rebuild it
	
  variables                      <- unlist(options$variables)
  hypothesis                     <- options$hypothesis
  bainTable                      <- createJaspTable("Bain Independent Samples Welch's T-Test Result")
  jaspResults[["bainTable"]]     <- bainTable

  bainTable$dependOnOptions(c("variables", "hypothesis", "bayesFactorType", "logscale", "groupingVariable"))
  
  bf.type <- options$bayesFactorType
  BFH1H0 <- FALSE  
  bf.title <- "BF\u2080\u2081" 
	
	if(options$hypothesis == "allTypes")
		bf.title <- bf.title <- "BF\u2080." 
  
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
			
	bainTable$position <- 1

  if (options$hypothesis == "groupsNotEqual")
  {
    type <- 1
    message <- "The alternative hypothesis H1 specifies that the mean of group 1 is unequal to the mean of group 2. The posterior probabilities are based on equal prior probabilities."
  }
  if (options$hypothesis == "groupTwoGreater"){
  type <- 2
  message <- "The alternative hypothesis H1 specifies that the mean of group 1 is smaller than the mean of group 2. The posterior probabilities are based on equal prior probabilities."
  }
  if (options$hypothesis == "groupOneGreater"){
    type <- 3
    message <- "The alternative hypothesis H1 specifies that mean of group 1 is bigger than the mean of group 2. The posterior probabilities are based on equal prior probabilities."
  }
  if(options$hypothesis == "_4type"){      
    type <- 4
    message <- "The null hypothesis H1 specifies that the mean of group 1 is bigger than the mean of group 2. The alternative hypothesis H2 specifies that the mean in group 1 is smaller than the mean in group 2. The posterior probabilities are based on equal prior probabilities."        
  }
  if(options$hypothesis == "allTypes"){
      type <- 5
	  message <- "The null hypothesis H0 (equal group means) is tested against H1 (first mean larger than second mean) and H2 (first mean smaller than second mean). The posterior probabilities are based on equal prior probabilities."
  }
        
  bainTable$addFootnote(message=message, symbol="<i>Note.</i>")
  
	bainTable$addCitation("Gu, X., Mulder, J., and Hoijtink, H. (2017). Approximate adjusted fractional Bayes factors: A general method for testing informative hypotheses. British Journal of Mathematical and Statistical Psychology. DOI:10.1111/bmsp.12110")
	bainTable$addCitation("Hoijtink, H., Mulder, J., van Lissa, C., and Gu, X. (2018). A Tutorial on testing hypotheses using the Bayes factor. Psychological Methods.")
	bainTable$addCitation("Hoijtink, H., Gu, X., and Mulder, J. (2018). Bayesian evaluation of informative hypotheses for multiple populations. Britisch Journal of Mathematical and Statistical Psychology. DOI: 10.1111/bmsp.12145")
	
  if(options$groupingVariable == "") return()
  
  jaspResults$startProgressbar(length(variables))
  
  bainResult <- list()
  
  levels <- base::levels(dataset[[ .v(options$groupingVariable) ]])
  if (length(levels) != 2) {
	  g1 <- "1"
	  g2 <- "2"
  } else {
	  g1 <- levels[1]
	  g2 <- levels[2]
  }
  
  for (variable in variables){
		
		if(variable %in% missingValuesIndicator){
			bainTable$addFootnote(message= paste0("The variable ", variable, " contains missing values, the rows containing these values are removed in the analysis."), symbol="<b>Warning.</b>")
		}

	  subDataSet <- dataset[, c(.v(variable), .v(options$groupingVariable))]
	  subDataSet <- na.omit(subDataSet)
	  group2 <- subDataSet[subDataSet[[.v(options$groupingVariable)]]== g1,.v(variable)]
	  group1 <- subDataSet[subDataSet[[.v(options$groupingVariable)]]== g2,.v(variable)]
	  
	  p <- try({
		  
      	bainAnalysis <- Bain::Bain_ttestData(group1, group2, type = type)
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
        if(options$bayesFactorType == "BF01")
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
                
                if(options$hypothesis == "groupsNotEqual"){
                    row <- list(Variable=variable, "hypothesis[type1]" = "H0: Equal","BF[type1]"=.clean(BF_0u), "pmp[type1]" = .clean(PMP_0),
                                        "hypothesis[type2]" = "H1: Not equal", "BF[type2]" = "", "pmp[type2]" = .clean(PMP_u))
                } else if(options$hypothesis == "groupOneGreater"){
                    row <-list(Variable=variable, "hypothesis[type1]" = "H0: Equal","BF[type1]"=.clean(BF_01), "pmp[type1]" = .clean(PMP_0),
                                       "hypothesis[type2]" = "H1: Bigger", "BF[type2]" = "", "pmp[type2]" = .clean(PMP_1))
                } else if(options$hypothesis == "groupTwoGreater"){
                    row <-list(Variable=variable, "hypothesis[type1]" = "H0: Equal", "BF[type1]"=.clean(BF_01), "pmp[type1]" = .clean(PMP_0),
                                       "hypothesis[type2]" = "H1: Smaller", "BF[type2]" = "", "pmp[type2]" = .clean(PMP_1))
                } else if (options$hypothesis == "_4type"){
                    row <-list(Variable=variable, "hypothesis[type1]" = "H1: Bigger", "BF[type1]"=.clean(BF_01), "pmp[type1]" = .clean(PMP_1),
                                       "hypothesis[type2]" = "H2: Smaller", "BF[type2]" = "", "pmp[type2]" = .clean(PMP_0))
                } else if (options$hypothesis == "allTypes"){
                    row <-list(Variable=variable, 
                                       "type[equal]" = "H0: Equal",
                                       "BF[equal]"= "", 
                                       "pmp[equal]" = .clean(PMP_0),
                                       "type[greater]" = "H1: Bigger",
                                       "BF[greater]" = .clean(BF_02), 
                                       "pmp[greater]" = .clean(PMP_2),
                                       "type[less]" = "H2: Smaller",
                                       "BF[less]" = .clean(BF_01),
                                       "pmp[less]" = .clean(PMP_1)) 
                }
                
            } else if (options$bayesFactorType == "BF10"){
                
                if(options$hypothesis == "groupsNotEqual"){
                    row <- list(Variable=variable, "hypothesis[type1]" = "H0: Equal","BF[type1]"="", "pmp[type1]" = .clean(PMP_0),
                                        "hypothesis[type2]" = "H1: Not equal", "BF[type2]" = .clean(BF_0u), "pmp[type2]" = PMP_u)
                } else if(options$hypothesis == "groupOneGreater"){
                    row <-list(Variable=variable, "hypothesis[type1]" = "H0: Equal","BF[type1]"="", "pmp[type1]" = .clean(PMP_0),
                                       "hypothesis[type2]" = "H1: Bigger", "BF[type2]" = .clean(BF_01), "pmp[type2]" = .clean(PMP_1))
                } else if(options$hypothesis == "groupTwoGreater"){
                    row <-list(Variable=variable, "hypothesis[type1]" = "H0: Equal", "BF[type1]"="", "pmp[type1]" = .clean(PMP_0),
                                       "hypothesis[type2]" = "H1: Smaller", "BF[type2]" = .clean(BF_01), "pmp[type2]" = .clean(PMP_1))
                } else if (options$hypothesis == "_4type"){
                    row <-list(Variable=variable, "hypothesis[type1]" = "H1: Bigger", "BF[type1]"= "", "pmp[type1]" = .clean(PMP_1),
                                       "hypothesis[type2]" = "H2: Smaller", "BF[type2]" = .clean(BF_01), "pmp[type2]" = .clean(PMP_0))
                } else if (options$hypothesis == "allTypes"){
                    row <-list(Variable=variable, 
                                       "type[equal]" = "H0: Equal",
                                       "BF[equal]"= "", 
                                       "pmp[equal]" = .clean(PMP_0),
                                       "type[greater]"= "H1: Bigger",
                                       "BF[greater]" = .clean(BF_02), 
                                       "pmp[greater]" = .clean(PMP_2),
                                       "type[less]" = "H2: Smaller",
                                       "BF[less]" = .clean(BF_01),
                                       "pmp[less]" = .clean(PMP_1)) 
                }
                
            }
  
      bainTable$addRows(row)
	  jaspResults$progressbarTick()
  }

  jaspResults[["bainResult"]] <- createJaspState(bainResult)
  jaspResults[["bainResult"]]$copyDependenciesFromJaspObject(bainTable)
}

.bainDescriptivesTwoSampleTable <- function(dataset, options, jaspResults) {
    
    if(!is.null(jaspResults[["descriptives"]])) return() #The options for this table didn't change so we don't need to rebuild it

    variables                                               <- unlist(options$variables)
    descriptives                                            <- createJaspTable("Descriptive Statistics")
    jaspResults[["descriptives"]]                           <- descriptives
    descriptives$dependOnOptions(c("variables", "descriptives", "descriptivesPlotsCredibleInterval"))
    
    descriptives$addColumnInfo(name="v",                    title = "", type="string")
		descriptives$addColumnInfo(name="group",                title = "Group", type="string")
    descriptives$addColumnInfo(name="N",                    title = "N", type="Integer")
    descriptives$addColumnInfo(name="mean",                 title = "Mean", type="number", format="sf:4;dp:3")
    descriptives$addColumnInfo(name="sd",                   title = "sd", type="number", format="sf:4;dp:3")
    descriptives$addColumnInfo(name="se",                   title = "se", type="number", format="sf:4;dp:3")
		
		descriptives$position <- 2
    
		interval <- 100 * options$descriptivesPlotsCredibleInterval
		overTitle <- paste0(interval, "% Credible Interval")
		descriptives$addColumnInfo(name="lowerCI",              title = "lowerCI", type="number", format="sf:4;dp:3", overtitle = overTitle)
    descriptives$addColumnInfo(name="upperCI",              title = "upperCI", type="number", format="sf:4;dp:3", overtitle = overTitle)
	
	if(options$groupingVariable == "") return()
      
	levels <- base::levels(dataset[[ .v(options$groupingVariable) ]])
	if (length(levels) != 2) {
		g1 <- "1"
		g2 <- "2"
	} else {
		g1 <- levels[1]
		g2 <- levels[2]
	}
	
	for(variable in variables){
	  
		for (i in 1:2) {

	  	level <- levels[i]
	  	variableData <- dataset[[.v(variable)]]
		groupingData <- dataset[[.v(options$groupingVariable)]]
		groupData <- variableData[groupingData == level]
		groupDataOm <- na.omit(groupData)
		
		if (class(groupDataOm) != "factor") {
			
			posteriorSummary <- .posteriorSummaryGroupMean(variable=groupDataOm, descriptivesPlotsCredibleInterval=options$descriptivesPlotsCredibleInterval)
			ciLower <- .clean(round(posteriorSummary$ciLower,3))
			ciUpper <- .clean(round(posteriorSummary$ciUpper,3))
			n <- .clean(length(groupDataOm))
		  	mean <- .clean(mean(groupDataOm))
		  	std <- .clean(sd(groupDataOm))
		  	sem <- .clean(sd(groupDataOm) / sqrt(length(groupDataOm)))
			if(i == 1)
				row <- list(v = variable, group = level, N = n, mean = mean, sd = std, se = sem, lowerCI = ciLower, upperCI = ciUpper)
			if(i == 2)
				row <- list(v = "", group = level, N = n, mean = mean, sd = std, se = sem, lowerCI = ciLower, upperCI = ciUpper)
				
		} else {
			n <- .clean(length(groupDataOm))
			row <- list(v = variable, group = "", N = n, mean = "", sd = "", se = "", lowerCI = "", upperCI = "")
		}
		
		descriptives$addRows(row)		
	}	
  } 
}

.bainTwoSampleBFplots <- function(dataset, options, bainResult, variable){    
	if(is.null(bainResult))
	  return(createJaspPlot(error="badData", errorMessage="Plotting is not possible: No analysis has been run."))
	p <- .plot.BainT(bainResult)
  BFplot <- createJaspPlot(plot=p, title=variable, width = options$plotWidth, height = options$plotHeight)    
  return(BFplot)
}

.bainTwoSampleDescriptivesPlot <- function(dataset, options, variable){
	
	levels <- base::levels(dataset[[ .v(options$groupingVariable) ]])
	if (length(levels) != 2) {
		g1 <- "1"
		g2 <- "2"
	} else {
		g1 <- levels[1]
		g2 <- levels[2]
	}
	subDataSet <- dataset[, c(.v(variable), .v(options$groupingVariable))]
	subDataSet <- na.omit(subDataSet)
	group2 <- subDataSet[subDataSet[[.v(options$groupingVariable)]]== g1,.v(variable)]
	group1 <- subDataSet[subDataSet[[.v(options$groupingVariable)]]== g2,.v(variable)]
	
	ggplotObj <- .plot2GroupMeansBayesIndTtest(v1 = group2, v2 = group1, nameV1 = g1, nameV2 = g2, 
				groupingName = options$groupingVariable, dependentName = variable, 
				descriptivesPlotsCredibleInterval=options$descriptivesPlotsCredibleInterval)
	
    return(createJaspPlot(plot=ggplotObj, title = variable, width = options$plotWidth, height = options$plotHeight))
}

.readDataBainTwoSample <- function(options, dataset){
	
	all.variables 									<- unlist(options$variables)
	grouping   										<- options$groupingVariable
	read.variables 									<- c(all.variables, grouping)
	if (grouping == "")
		grouping <- NULL
		
	if (is.null(dataset)){  
		
						trydata                	<- .readDataSetToEnd(columns.as.numeric=all.variables)
						missingValuesIndicator 	<- .unv(names(which(apply(trydata, 2, function(x){ any(is.na(x))} ))))
		           
            dataset 								<- .readDataSetToEnd(columns.as.numeric=all.variables, columns.as.factor=grouping, exclude.na.listwise=read.variables)
    }
	
	.hasErrors(dataset=dataset, perform=perform, type="factorLevels",
			   factorLevels.target=grouping, factorLevels.amount = "!= 2",
			   exitAnalysisIfErrors = TRUE)   
	
	.hasErrors(dataset, perform, type=c("infinity", "variance", "observations"),
				all.target=all.variables, message="short", observations.amount="< 3", 
				exitAnalysisIfErrors = TRUE)
				
	readList <- list()
  readList[["dataset"]] <- dataset 
  readList[["missingValuesIndicator"]] <- missingValuesIndicator
	
	return(readList)
	
}
