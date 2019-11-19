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

  bainContainer <- .bainGetContainer(jaspResults, deps=c("variables", "testValue", "seed"))
  
  ### RESULTS ###
  .bainOneSampleResultsTable(dataset, options, bainContainer, missingValuesIndicator, ready, position = 1)
  
  ### DESCRIPTIVES ###
  .bainOneSampleDescriptivesTable(dataset, options, bainContainer, ready, position = 2)

  ### BAYES FACTOR PLOTS ###
  .bainTTestFactorPlots(dataset, options, bainContainer, ready, type = "oneSample", position = 3)

  ### DESCRIPTIVES PLOTS ###
  .bainOneSampleDescriptivesPlot(dataset, options, bainContainer, ready, position = 4)
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

.bainOneSampleResultsTable <- function(dataset, options, bainContainer, missingValuesIndicator, ready, position) {

  if (!is.null(bainContainer[["bainTable"]])) return()

  bainTable <- createJaspTable("Bain One Sample T-test")
  bainTable$position <- position
  bainTable$dependOn(options =c("hypothesis", "bayesFactorType"))

  bf.type <- options[["bayesFactorType"]]
  BFH1H0 <- FALSE
  bf.title <- "BF"

  if (options$hypothesis == "equalBiggerSmaller") {
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
                        "equalNotEqual"       = 1,
                        "equalBigger"         = 2,
                        "equalSmaller"        = 3,
                        "biggerSmaller"       = 4,
                        "equalBiggerSmaller"  = 5)
  note <- base::switch(options[["hypothesis"]],
                        "equalNotEqual"       = "The alternative hypothesis H1 specifies that the mean is unequal to ",
                        "equalBigger"         = "The alternative hypothesis H1 specifies that the mean is bigger than ",
                        "equalSmaller"        = "The alternative hypothesis H1 specifies that the mean is smaller than ",
                        "biggerSmaller"       = NULL,
                        "equalBiggerSmaller"  = "The null hypothesis H0 with test value ")
  message <- base::switch(options[["hypothesis"]],
                            "equalNotEqual"     = paste0(note, options[["testValue"]], "."," The posterior probabilities are based on equal prior probabilities."),
                            "equalBigger"       = paste0(note, options[["testValue"]], "."," The posterior probabilities are based on equal prior probabilities."),
                            "equalSmaller"      = paste0(note, options[["testValue"]], "."," The posterior probabilities are based on equal prior probabilities."),
                            "biggerSmaller"     = paste0("The hypothesis H1 specifies that the mean is bigger than ",options[["testValue"]]," and the hypothesis H2 specifies that the mean is smaller than ",options[["testValue"]],". The posterior probabilities are based on equal prior probabilities."),
                            "equalBiggerSmaller"= paste0(note, options[["testValue"]], " is tested against the other hypotheses. H1 states that the mean is bigger than ", options[["testValue"]]," and H2 states that the mean is smaller than ",options[["testValue"]],". The posterior probabilities are based on equal prior probabilities."))
  bainTable$addFootnote(message=message)

  bainTable$addCitation(.bainGetCitations())

  bainContainer[["bainTable"]] <- bainTable

  if (!ready)
    return()

  startProgressbar(length(options[["variables"]]))
  bainResult <- list()
  
  for (variable in options[["variables"]]) {

    variableData <- dataset[[ .v(variable) ]]

    p <- try({
      bainAnalysis <- .bain_ttest_cran(x = variableData, nu = options[["testValue"]], type = type, seed = options[["seed"]])
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
            row <- list(Variable=variable, "hypothesis[type1]" = "H0: Equal","BF[type1]"=BF_0u, "pmp[type1]" = PMP_0,
                                            "hypothesis[type2]" = "H1: Not equal", "BF[type2]" = "", "pmp[type2]" = PMP_u)
        } else if (options$hypothesis == "equalBigger") {
            row <-list(Variable=variable, "hypothesis[type1]" = "H0: Equal","BF[type1]"=BF_01, "pmp[type1]" = PMP_0,
                                            "hypothesis[type2]" = "H1: Bigger", "BF[type2]" = "", "pmp[type2]" = PMP_1)
        } else if (options$hypothesis == "equalSmaller") {
            row <-list(Variable=variable, "hypothesis[type1]" = "H0: Equal", "BF[type1]"=BF_01, "pmp[type1]" = PMP_0,
                                            "hypothesis[type2]" = "H1: Smaller", "BF[type2]" = "", "pmp[type2]" = PMP_1)
        } else if (options$hypothesis == "biggerSmaller") {
            row <-list(Variable=variable, "hypothesis[type1]" = "H1: Bigger", "BF[type1]"=BF_01, "pmp[type1]" = PMP_0,
                                            "hypothesis[type2]" = "H2: Smaller", "BF[type2]" = "", "pmp[type2]" = PMP_1)
        } else if (options$hypothesis == "equalBiggerSmaller") {
            row <-list(Variable=variable, "type[equal]" = "H0: Equal", "BF[equal]"= "", "pmp[equal]" = PMP_0, 
                                            "type[greater]" = "H1: Bigger", "BF[greater]" = BF_01, "pmp[greater]" = PMP_1,
                                            "type[less]" = "H2: Smaller", "BF[less]" = BF_02, "pmp[less]" = PMP_2)
        }
    } else if (options$bayesFactorType == "BF10") {
        if (options$hypothesis == "equalNotEqual") {
            row <- list(Variable=variable, "hypothesis[type1]" = "H0: Equal","BF[type1]"="", "pmp[type1]" = PMP_0,
                                              "hypothesis[type2]" = "H1: Not equal", "BF[type2]" = BF_0u, "pmp[type2]" = PMP_u)
        } else if (options$hypothesis == "equalBigger") {
            row <-list(Variable=variable, "hypothesis[type1]" = "H0: Equal","BF[type1]"="", "pmp[type1]" = PMP_0,
                                            "hypothesis[type2]" = "H1: Bigger", "BF[type2]" = BF_01, "pmp[type2]" = PMP_1)
        } else if (options$hypothesis == "equalSmaller") {
            row <-list(Variable=variable, "hypothesis[type1]" = "H0: Equal", "BF[type1]"="", "pmp[type1]" = PMP_0,
                                            "hypothesis[type2]" = "H1: Smaller", "BF[type2]" = BF_01, "pmp[type2]" = PMP_1)
        } else if (options$hypothesis == "biggerSmaller") {
            row <-list(Variable=variable, "hypothesis[type1]" = "H1: Bigger", "BF[type1]"= "", "pmp[type1]" = PMP_0,
                                            "hypothesis[type2]" = "H2: Smaller", "BF[type2]" = BF_01, "pmp[type2]" = PMP_1)
        } else if (options$hypothesis == "equalBiggerSmaller") {
            row <-list(Variable=variable, "type[equal]" = "H0: Equal", "BF[equal]"= "", "pmp[equal]" = PMP_0,
                                            "type[greater]"= "H1: Bigger", "BF[greater]" = BF_01, "pmp[greater]" = PMP_1,
                                            "type[less]" = "H2: Smaller", "BF[less]" = BF_02, "pmp[less]" = PMP_2)
        }
    }
    bainTable$addRows(row, rowNames = variable)
    progressbarTick()
  }
  bainContainer[["bainResult"]] <- createJaspState(bainResult)
  bainContainer[["bainResult"]]$dependOn(optionsFromObject = bainTable)
}

.bainOneSampleDescriptivesTable <- function(dataset, options, bainContainer, ready, position) {

  if (!is.null(bainContainer[["descriptivesTable"]]) || !options[["descriptives"]]) return()
      
  descriptivesTable <- createJaspTable("Descriptive Statistics")
  descriptivesTable$dependOn(options = c("descriptives", "credibleInterval"))
  descriptivesTable$position <- position

  descriptivesTable$addColumnInfo(name="v",                    title = "", type="string")
  descriptivesTable$addColumnInfo(name="N",                    title = "N", type="integer")
  descriptivesTable$addColumnInfo(name="mean",                 title = "Mean", type="number")
  descriptivesTable$addColumnInfo(name="sd",                   title = "SD", type="number")
  descriptivesTable$addColumnInfo(name="se",                   title = "SE", type="number")

  interval <- 100 * options[["credibleInterval"]]
  overTitle <- paste0(interval, "% Credible Interval")
  descriptivesTable$addColumnInfo(name="lowerCI",              title = "Lower", type="number", overtitle = overTitle)
  descriptivesTable$addColumnInfo(name="upperCI",              title = "Upper", type="number", overtitle = overTitle)
  
  bainContainer[["descriptivesTable"]] <- descriptivesTable

  if (!ready || bainContainer$getError())
    return()

  bainResult <- bainContainer[["bainResult"]]$object

  for (variable in options[["variables"]]) {

    bainResult_tmp <- bainResult[[variable]]
    bainSummary <- summary(bainResult_tmp, ci = options[["credibleInterval"]])

    N <- bainSummary[["n"]]
    mu <- bainSummary[["Estimate"]]
    CiLower <- bainSummary[["lb"]]
    CiUpper <- bainSummary[["ub"]]
    sd <- sd(dataset[, .v(variable)])
    se <- sqrt(diag(bainResult_tmp[["posterior"]]))

    row <- list(v = variable, N = N, mean = mu, sd = sd, se = se, lowerCI = CiLower, upperCI = CiUpper)
    descriptivesTable$addRows(row)
  }
}

.bainOneSampleDescriptivesPlot <- function(dataset, options, bainContainer, ready, position) {
  
  if (!is.null(bainContainer[["descriptivesPlots"]]) || !options[["descriptivesPlots"]]) return()

	descriptivesPlots <- createJaspContainer("Descriptive Plots")
	descriptivesPlots$dependOn(options =c("descriptivesPlots", "credibleInterval"))
	descriptivesPlots$position <- position

  bainContainer[["descriptivesPlots"]] <- descriptivesPlots

	if (!ready || bainContainer$getError())
		return()

	bainResult <- bainContainer[["bainResult"]]$object

  for (variable in options[["variables"]]) {

    if (is.null(bainContainer[["descriptivesPlots"]][[variable]])){

      bainSummary <- summary(bainResult[[variable]], ci = options[["credibleInterval"]])

      N <- bainSummary[["n"]]
      mu <- bainSummary[["Estimate"]]
      CiLower <- bainSummary[["lb"]]
      CiUpper <- bainSummary[["ub"]]

      yBreaks <- JASPgraphs::getPrettyAxisBreaks(c(options[["testValue"]], CiLower, CiUpper), min.n = 4)
      d <- data.frame(v = variable, N = N, mean = mu, lowerCI = CiLower, upperCI = CiUpper, index = 1)

      p <- ggplot2::ggplot(d, ggplot2::aes(x=index, y=mean)) +
            ggplot2::geom_segment(ggplot2::aes(x = -Inf, xend = Inf, y = options[["testValue"]], yend = options[["testValue"]]), linetype = 2, color = "darkgray") +
            ggplot2::geom_errorbar(ggplot2::aes(ymin=lowerCI, ymax=upperCI), colour="black", width=.1, position = ggplot2::position_dodge(.2)) +
            ggplot2::geom_point(position=ggplot2::position_dodge(.2), size=4) +
            ggplot2::ylab("") +
            ggplot2::xlab("") +
            ggplot2::scale_y_continuous(breaks = yBreaks, labels = yBreaks) +
            ggplot2::scale_x_continuous(breaks = 0:2, labels = NULL)
      p <- JASPgraphs::themeJasp(p, xAxis = FALSE) + ggplot2::theme(axis.ticks.x = ggplot2::element_blank())
      
      descriptivesPlots[[variable]] <- createJaspPlot(plot=p, title = variable)
      descriptivesPlots[[variable]]$dependOn(optionContainsValue=list("variables" = variable))
    }
  }
}

.bainTTestFactorPlots <- function(dataset, options, bainContainer, ready, type, position) {

  if (!is.null(bainContainer[["bayesFactorPlots"]]) || !options[["bayesFactorPlot"]]) return()

	bayesFactorPlots <- createJaspContainer("Posterior Probabilities")
	bayesFactorPlots$dependOn(options = c("bayesFactorPlot", "hypothesis"))
	bayesFactorPlots$position <- position

  bainContainer[["bayesFactorPlots"]] <- bayesFactorPlots

  if (!ready || bainContainer$getError())
		return()

  bainResult <- bainContainer[["bainResult"]]$object
  
  if (type == "pairedSamples") {
    option <- "pairs"
    dependencies <- options[["pairs"]]
    variables <- unlist(lapply(options[["pairs"]], paste, collapse=" - "))
  } else {
    option <- "variables"
    variables <- dependencies <- options[["variables"]]
  }

  analysisType <- base::switch(options[["hypothesis"]],
                      "equalNotEqual"		= 1,
                      "equalBigger"		  = 2,
                      "equalSmaller"		= 3,
                      "biggerSmaller"		= 4,
                      "equalBiggerSmaller"= 5)
  
  for (i in seq_along(variables)) {
    variable <- variables[i]
    if (!is.null(bayesFactorPlots[[variable]]))
      next

    plot <- createJaspPlot(plot = NULL, title = variable, height = 300, width = 400)

    dependency <- list()
    dependency[[option]] <- dependencies[[i]]
    plot$dependOn(optionContainsValue=dependency)

    if (!is.null(bainResult[[variable]])){
      p <- try({
          plot$plotObject <- .plot_bain_ttest_cran(bainResult[[variable]], type = analysisType)
      })
      if(isTryError(p)){
        plot$setError(paste0("Plotting not possible: ", .extractErrorMessage(p)))
      }
    } else {
      plot$setError("Plotting not possible: the results for this variable were not computed.")
    }
      
    bayesFactorPlots[[variable]] <- plot
  }
}

.plot_bain_ttest_cran <- function(x, type){

    if(type == 1 || type == 2 || type == 3){
      labs <- c("H0", "H1")
    }
    if(type == 4){
      labs <- c("H1", "H2")
    }
    if(type == 5){
      labs <- c("H0", "H1", "H2")
    }
    labels <- rev(labs)
    if(type == 1){
      values <- x$fit$PMPb
    } else {
      values <- na.omit(x$fit$PMPa)
    }

    ggdata <- data.frame(lab = labs, PMP = values)

    p <- ggplot2::ggplot(data = ggdata, mapping = ggplot2::aes(x = "", y = PMP, fill = lab)) +
          ggplot2::geom_bar(stat = "identity", width = 1e10, color = "black", size = 1) +
          ggplot2::geom_col() +
          ggplot2::coord_polar(theta = "y", direction = -1) +
          ggplot2::labs(x = "", y = "") +
          ggplot2::theme(panel.grid = ggplot2::element_blank(), legend.position = "none") +
          ggplot2::scale_y_continuous(breaks = cumsum(rev(values)) - rev(values)/2, labels = labels) +
          ggplot2::theme(panel.background = ggplot2::element_blank(),
                              axis.text=ggplot2::element_text(size=17, color = "black"),
                              plot.title = ggplot2::element_text(size=18, hjust = .5),
                              axis.ticks.y = ggplot2::element_blank()) +
          ggplot2::scale_fill_brewer(palette="Set1")

    return(p)
}

# This function is not from JASP and will be migrated to the bain CRAN package in time
.bain_ttest_cran <- function(x, y = NULL, nu = 0, type = 1, paired = FALSE, seed){

  set.seed(seed)

  # ONE GROUP
  if(is.null(y) && !paired){
    
    tres <- bain::t_test(x)  
    
    if(type == 1){
      c3 <- paste0("result <- bain::bain(tres,\"x=",nu,"\")")  
      result <- eval(parse(text = c3))
    }
    if(type == 2){
      c3 <- paste0("result <- bain::bain(tres,\"x=",nu,"; x>",nu,"\")")  
      result <- eval(parse(text = c3))
    }
    if(type == 3){
      c3 <- paste0("result <- bain::bain(tres,\"x=",nu,"; x<",nu,"\")")  
      result <- eval(parse(text = c3))
    }
    if(type == 4){
      c3 <- paste0("result <- bain::bain(tres,\"x>",nu,"; x<",nu,"\")")  
      result <- eval(parse(text = c3))
    }
    if(type == 5){
      c3 <- paste0("result <- bain::bain(tres,\"x=",nu,"; x>",nu,"; x<",nu,"\")")  
      result <- eval(parse(text = c3))
    }

  }

  # INDEPENDENT SAMPLES
  if(!is.null(y) && !paired){
    
    tres <- bain::t_test(x, y, paired = FALSE, var.equal = FALSE)
    
    if(type == 1){
      result <- bain::bain(tres,"x=y")
    } 
    if(type == 2){
      result <- bain::bain(tres,"x=y; x>y")
    }
    if(type == 3){
      result <- bain::bain(tres,"x=y; x<y")
    }
    if(type == 4){
      result <- bain::bain(tres,"x>y; x<y")
    }
    if(type == 5){
      result <- bain::bain(tres,"x=y; x>y; x<y")
      }
  }

  # PAIRED SAMPLE
  if(!is.null(y) && paired){
    
    tres <- bain::t_test(x, y, paired = TRUE)

    if(type == 1){
      result <- bain::bain(tres,"difference=0")
    }
    if(type == 2){
      result <- bain::bain(tres,"difference=0;difference>0")
    }
    if(type == 3){
      result <- bain::bain(tres,"difference=0;difference<0")
    }
    if(type == 4){
      result <- bain::bain(tres,"difference>0;difference<0")
    }
    if(type == 5){
      result <- bain::bain(tres,"difference=0;difference>0;difference<0")
    }
  }

  return(invisible(result))
}