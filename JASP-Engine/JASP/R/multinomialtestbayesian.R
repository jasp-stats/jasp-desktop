#
# Copyright (C) 2018 University of Amsterdam
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

MultinomialTestBayesian <- function (dataset = NULL, options, perform = "run",
                             callback = function(...) 0,  ...) {

  # First, we load the variables into the R environment
  fact <- NULL
  asnum <- NULL
  if (options$factor != "") {
    fact <- options$factor
    if (options$counts != "") {
      asnum <- options$counts
      if (options$exProbVar != "") {
        asnum <- c(asnum, options$exProbVar)
      }
    }
  }

  if (is.null(dataset)) {
    if (perform == "run") {
      dataset <- .readDataSetToEnd(columns.as.numeric = asnum, columns.as.factor = fact,
                                   exclude.na.listwise = NULL)
    } else {
      dataset <- .readDataSetHeader(columns.as.numeric = asnum, columns.as.factor = fact)
    }
  } else {
    dataset <- .vdf(dataset, columns.as.numeric = asnum, columns.as.factor = fact)
  }

  results <- list() # Initialise results object

  # Then, we retrieve the state and initialise the output objects
  state <- .retrieveState()

  multBayesResults <- NULL # result of the chi-square test
  descriptivesTable <- NULL # expected versus observed
  descriptivesPlot <- NULL # barplot of factor levels

  # Then, we can fill the output objects with old info if its option did not
  # change.
  if (!is.null(state)) {
    diff <- .diff(options, state$options) # a list of TRUE/FALSE

    if (is.list(diff)){

      if (!any(diff[["factor"]], diff[["counts"]],
               diff[["credibleIntervalInterval"]],
               diff[["bayesFactorType"]],
               diff[["hypothesis"]], diff[["exProbVar"]],
               diff[["expectedProbs"]], diff[["simulatepval"]],
               (options[["hypothesis"]] == "expectedProbs" &&
               diff[["tableWidget"]])
              )) {

        multBayesResults <- state[["multBayesResults"]]

        # the following depend on multBayesResults so in same if-statement
        if (!any(diff[["credibleInterval"]],
                 diff[["countProp"]])) {
          descriptivesTable <- state[["descriptivesTable"]]
        }

        if (!any(diff[["descriptivesPlotCredibleInterval"]],
                 diff[["countProp"]], diff[["plotWidth"]],
                 diff[["plotHeight"]])) {
          descriptivesPlot <- state[["descriptivesPlot"]]
        }
      }

      #... etcetera
      # TODO
    }
  }

  # Meta information
  results[["title"]] <- "Bayesian Multinomial Test"
  results[[".meta"]] <- list(list(name = "chisq", type = "table"),
                             list(name = "descriptivesTable", type = "table"),
                             list(name = "descriptivesPlot", type = "image"))

  # chi-square Table
  # Generate results
  if (is.null(multBayesResults)) {
    multBayesResults <- .multinomialBayesTest(dataset, options, fact, perform)
  }

  results[["chisq"]] <- .multinomialBayesTable(multBayesResults, options, perform)

  # Descriptives Table
  if (options[["descriptives"]]) {
    # Generate descriptives table
    if (is.null(descriptivesTable)) {
      descriptivesTable <- .multinomialBayesDescriptives(multBayesResults, fact, options, perform)
    }

    results[["descriptivesTable"]] <- descriptivesTable
  } else {

    results[["descriptivesTable"]] <- NULL
  }

  # Bayesian Multinomial Descriptives Plot
  if (options[["descriptivesPlot"]]) {
    # Generate descriptives plots
    if (is.null(descriptivesPlot)) {
      descriptivesPlot <- .multinomialBayesDescriptivesPlot(multBayesResults, options, perform)
    }

    plotPath <- list(descriptivesPlot$data) # for keep later

    results[["descriptivesPlot"]] <- descriptivesPlot
  } else {

    results[["descriptivesPlot"]] <- NULL
    plotPath <- list()
  }

  if (perform == "run") {

    state <- list()
    state[["options"]] <- options
    state[["multBayesResults"]] <- multBayesResults
    state[["descriptivesTable"]] <- descriptivesTable
    state[["descriptivesPlot"]] <- descriptivesPlot

    return(list(results=results, status="complete", state=state,
                keep = plotPath))
  } else {
    return(list(results=results, status="inited", state=state,
                keep = plotPath))
  }
}


.multinomialBayesTest <- function(dataset, options, fact, perform) {
  # Run chi-square test and return object
  #
  # Args:
  #   dataset: input dataset
  #   options: user options
  #   fact
  #   perform: init or run
  #
  # Return:
  #   Object (chisquare test)

  multBayesResults <- NULL
  
  if (perform == "run" && !is.null(fact)) {
    # first determine the hypotheses
    f <- dataset[[.v(fact)]]
    f <- f[!is.na(f)]

    if (options$counts != "") {
      # convert to "regular" fact
      c <- dataset[[.v(options$counts)]]
      if (length(c) != length(levels(f))) {
        .quitAnalysis("Invalid counts: the number of counts does not equal the number of categories. Check your count dataset!")
      }
      f <- factor(rep(f, c), levels = levels(f))
    }
    
    t <- table(f)
    nlev <- nlevels(f)

    hyps <- .multinomialHypotheses(dataset, options, nlev)
    
    # create a named list with as values the chi-square result objects
    multBayesResults <- lapply(hyps, function(h) {
      # catch warning message and append to object if necessary
      csr <- NULL
      warn <- NULL
      resultsObject <- .bayesFactorExactEquality(counts = t, thetas = h, options)
      
      csr <- list(
        BF = resultsObject[["BF"]],
        warn = resultsObject[["warn"]],
        expected = resultsObject[["expected"]]
      )
      csr[["observed"]] <- t
      csr[["levels"]] <- nlev
      return(csr)
    })
  }

  return(multBayesResults)  # return the out object
}


.multinomialBayesTable <- function(multBayesResults, options, perform) {
  # Transform chi-square test object into table for JASP
  # multBayesResults = list(H1 = obj, H2 = obj, ....)
  #
  # Args:
  #   multBayesResults:
  #   options: input options
  #   perform: init or run
  #
  # Return:
  #   Chi square table
  table <- list()
  footnotes <- .newFootnotes()
  table[["title"]] <- "Bayesian Multinomial Test"
  
  # Bayes factor type
  if (options$bayesFactorType == "BF01") {
    bf.title <- "BF\u2080\u2081"
  } else if (options$bayesFactorType == "BF10") {
    bf.title <- "BF\u2081\u2080"
  } else if (options$bayesFactorType == "LogBF10") {
    bf.title <- "Log(\u0042\u0046\u2081\u2080)"
  }

  # include fields
  fields <- list(
    list(name="case", title="", type="string", combine=TRUE),
    list(name="levels", title="Levels", type="integer"),
    list(name="BF", title = bf.title, type="number", format="sf:4;dp:3")
    )

  # include footnotes
  table[["schema"]] <- list(fields = fields)

  message <- list()

  for (r in 1:length(multBayesResults)) {

    if (!is.null(multBayesResults[[r]][["warn"]])) {
      .addFootnote(footnotes, symbol = "<em>Note.</em>", text = multBayesResults[[r]][["warn"]])
    }
  }

  table[["footnotes"]] <- as.list(footnotes)

  # fill in results one row at a time
  if (!is.null(multBayesResults)) {

    for (r in 1:length(multBayesResults)) {
      table[["data"]][[r]] <- list(case = names(multBayesResults)[r],
                                   levels = multBayesResults[[r]][["levels"]],
                                   BF = multBayesResults[[r]][["BF"]])
      table[["status"]] <- "complete"
    }
  } else {
    # init state?
      data <- list(list(case = ".", levels = ".", BF = ".",
                        lowerCI = ".", upperCI = "."))

    table[["data"]] <- data
  }

  return(table)
}


.multinomialBayesDescriptives <- function(multBayesResults, fact, options, perform) {
  # Create multinomial descriptives table
  #
  # Args:
  #   multBayesResults:
  #   fact:
  #   options: user options
  #   perform: init or run
  #
  # Return:
  #   Descriptives table
  footnotes <- .newFootnotes()

  if (options[["countProp"]]=="descCounts"){
    numberType = list(type="integer")
  } else {
    numberType = list(type="number", format="sf:4;dp:3")
  }

  # Expected vs. Observed table
  table <- list("title" = "Descriptives")

  if (is.null(fact)) {
    # If we have no variable init table with generic name
    fields <- list(
      list(name="factor", title="Factor", type = "string"),
      c(list(name="observed", title="Observed"), numberType),
      c(list(name="expected", title="Expected"), numberType)
    )
    if (options$credibleInterval){
      interval <- 100 * options$credibleIntervalInterval
      title <- paste0(interval, "% Credible Interval")
      fields[[length(fields)+1]] <- list(name="lowerCI",
                                         title="Lower",
                                         type = "number",
                                         format = "sf:4;dp:3",
                                         overTitle = title)
      fields[[length(fields)+1]] <- list(name="upperCI",
                                        title="Upper",
                                        type = "number",
                                        format = "sf:4;dp:3",
                                        overTitle = title)
      .addFootnote(footnotes, symbol = "<em>Note.</em>", "Credible intervals are based on marginal beta distributions.")
    }

    rows <- list(list(factor = ".", observed = ".", expected = "."))
  } else if (perform != "run") {
    # If we have a variable then init table with factor name

    fields <- list(
      list(name="factor", title=fact, type = "string"),
      c(list(name="observed", title="Observed"), numberType),
      c(list(name="expected", title="Expected"), numberType)
    )
    if (options$credibleInterval){
      interval <- 100 * options$credibleIntervalInterval
      title <- paste0(interval, "% Credible Interval")
      fields[[length(fields)+1]] <- list(name="lowerCI",
                                         title="Lower",
                                         type = "number",
                                         format = "sf:4;dp:3",
                                         overTitle = title)
      fields[[length(fields)+1]] <- list(name="upperCI",
                                        title="Upper",
                                        type = "number",
                                        format = "sf:4;dp:3",
                                        overTitle = title)
      .addFootnote(footnotes, symbol = "<em>Note.</em>", "Credible intervals are based on marginal beta distributions.")
    }
    rows <- list(list(factor = ".", observed = ".", expected = "."))

  } else {
    # now  we want to create the full table
    # First we create the correct columns
    fields <- list(
      list(name="factor", title=fact, type = "string"),
      c(list(name="observed", title="Observed"), numberType)
    )
    footnotes <- .newFootnotes()

    nms <- names(multBayesResults)

    if (length(nms) == 1) {
      fields[[length(fields)+1]] <- c(list(name="expected",
                                           title = paste0("Expected: ", nms)),
                                      numberType)
    } else {
      for (i in 1:length(nms)) {
        fields[[length(fields)+1]] <- c(list(name=nms[i],
                                             title = nms[i]),
                                        numberType,
                                        overTitle = "Expected")
      }
    }

    if (options$credibleInterval){
      interval <- 100 * options$credibleIntervalInterval
      title <- paste0(interval, "% Credible Interval")
      fields[[length(fields)+1]] <- list(name="lowerCI",
                                         title="Lower",
                                         type = "number",
                                         format = "sf:4;dp:3",
                                         overTitle = title)
      fields[[length(fields)+1]] <- list(name="upperCI",
                                        title="Upper",
                                        type = "number",
                                        format = "sf:4;dp:3",
                                        overTitle = title)
      .addFootnote(footnotes, symbol = "<em>Note.</em>", "Credible intervals are based on marginal beta distributions.")
    }

    # Then we fill the columns with the information
    if (options[["countProp"]]=="descCounts"){
      div <- 1
    } else {
      div <- sum(multBayesResults[[1]][["observed"]])
    }

    tableFrame <- data.frame(
      factor = names(multBayesResults[[1]][["observed"]]),
      observed = as.integer(multBayesResults[[1]][["observed"]])/div,
      stringsAsFactors = FALSE
    )

    # Add expected counts (i.e. Binomial median)
    for (r in multBayesResults){
      tableFrame <- cbind(tableFrame, as.integer(r[["expected"]])/div)
    }

    if (length(nms) == 1) {
      colnames(tableFrame)[-(1:2)] <- "expected"
    } else {
      colnames(tableFrame)[-(1:2)] <- nms
    }
    
    # Add credibleInterval for observed counts
    if (options$credibleInterval){
      n <- sum(multBayesResults[[1]][["observed"]])
      # compute cis
      ci <- .multMedianAndCredibleIntervals(multBayesResults[[1]][["observed"]], 
                                            options$credibleIntervalInterval)
      ci <- ci * n # on the count scale

      # add these to the tableFrame
      ciDf <- data.frame(ci)
      colnames(ciDf) <- c("lowerCI", "upperCI")
      tableFrame <- cbind(tableFrame, ciDf/div)
    }

    rows <- list()

    for (i in 1:nrow(tableFrame)){
      rows[[i]] <- as.list(tableFrame[i,])
    }
    table[["status"]] <- "complete"
  }

  table[["schema"]] <- list(fields = fields)
  table[["data"]] <- rows
  table[["footnotes"]] <- as.list(footnotes)

  return(table)
}


.multinomialBayesDescriptivesPlot <- function(multBayesResults, options, perform) {
  # Create Bayesian multinomial descriptives plot
  #
  # Args:
  #   multBayesResults:
  #   options: user options
  #   perform: init or run
  #
  # Return:
  #   Descriptives plot

  # init output object
  descriptivesPlot <- list("title" = "Descriptives plot")
  descriptivesPlot[["width"]] <- options$plotWidth
  descriptivesPlot[["height"]] <- options$plotHeight
  descriptivesPlot[["custom"]] <- list(width = "plotWidth",
                                      height = "plotHeight")

  if (perform == "run" && options$factor != "") {
    # Generate the plot

    # Counts or props
    if (options[["countProp"]] == "descCounts") {
      div <- 1
      yname <- "Observed counts"
    } else {
      div <- sum(multBayesResults[[1]][["observed"]])
      yname <- "Observed proportions"
    }

    # we need to reverse the factor's levels because of the coord_flip later
    f <- names(multBayesResults[[1]][["observed"]])
    plotFrame <- data.frame(
      factor = factor(f, levels = rev(f)),
      obs = as.numeric(multBayesResults[[1]][["observed"]])/div
    )

    # Calculate confidence interval
    cl <- options$descriptivesPlotCredibleInterval
    n <- sum(multBayesResults[[1]][["observed"]])
    ci <- .multMedianAndCredibleIntervals(multBayesResults[[1]][["observed"]], 
                                          options$credibleIntervalInterval)
    ci <- ci * n # on the count scale
    ciDf <- data.frame(data.frame(ci))/div
    colnames(ciDf) <- c("lowerCI", "upperCI")

    # Define custom y axis function
    base_breaks_y <- function(x){
      b <- pretty(c(0,x))
      d <- data.frame(x=-Inf, xend=-Inf, y=min(b), yend=max(b))
      list(ggplot2::geom_segment(data=d, ggplot2::aes(x=x, y=y,
                                                      xend=xend, yend=yend),
                                 size = 0.75,
                                 inherit.aes=FALSE),
           ggplot2::scale_y_continuous(breaks=b))
    }

    # Create plot
    p <- ggplot2::ggplot(data = plotFrame,
                         mapping = ggplot2::aes(x = factor, y = obs)) +
      ggplot2::geom_bar(stat = "identity", size = 0.75, colour="black",
                        fill = "grey") +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = ciDf$lowerCI,
                                          ymax = ciDf$upperCI),
                             size = 0.75, width = 0.3) +
      base_breaks_y(ciDf$upperCI) +
      ggplot2::xlab(options$factor) +
      ggplot2::ylab(yname) +
      ggplot2::coord_flip() +
      ggplot2::theme_bw() +
      ggplot2::theme(
        panel.grid.minor = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(size = 18),
        panel.grid.major = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_text(size = 18, vjust=0.1),
        axis.title.y = ggplot2::element_text(size = 18, vjust=0.9),
        axis.text.x = ggplot2::element_text(size = 15),
        axis.text.y = ggplot2::element_text(size = 15),
        panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
        plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
        legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
        panel.border = ggplot2::element_blank(),
        axis.line =  ggplot2::element_blank(),
        legend.key = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_line(size = 0.5),
        axis.ticks.length = grid::unit(3, "mm"),
        axis.ticks.margin = grid::unit(1,"mm"),
        plot.margin = grid::unit(c(0.1, 0.1, 0.6, 0.6), "cm"),
        legend.position = "none")

    # create plot object
    content <- .writeImage(width = options$plotWidth,
                           height = options$plotHeight,
                           plot = p, obj = TRUE)

    descriptivesPlot[["convertible"]] <- TRUE
    descriptivesPlot[["obj"]] <- content[["obj"]]
    descriptivesPlot[["data"]] <- content[["png"]]
    descriptivesPlot[["status"]] <- "complete"

  } else {
    descriptivesPlot[["data"]] <- ""
  }

  return(descriptivesPlot)
}


.bayesFactorExactEquality <- function(counts, options, thetas){
  # Function calculates the Bayes factor for the Bayesian multinomial test
  # alphas: vector with alpha parameters (To-Do)
  # counts: vector of observed data
  # thetas: vector of test values
  #
  # output: list consisting of
  #         - Bayes factor in favor of alternative hypothesis
  #         - warning message if parameters were rescaled
  #         - expected counts
  alphas <- rep(1, length(counts))

  warn <- NULL
  if(sum(thetas) != 1){
    thetas <- thetas/sum(thetas)
    warn   <- "Parameters have been rescaled."
  }
  
  # expected counts under the null hypothesis (Binomial median)
  expected <- sum(counts)*thetas
  
  lbeta.xa <- sum(lgamma(alphas + counts)) - lgamma(sum(alphas + counts))
  lbeta.a  <- sum(lgamma(alphas)) - lgamma(sum(alphas))
  
  if (any(rowSums(cbind(thetas, counts)) == 0)) {
    
    # in this case, counts*log(thetas) should be zero, omit to avoid numerical issue with log(0)
    
    logBF10 <- (lbeta.xa-lbeta.a) 
    
  } else {
    
    logBF10 <- (lbeta.xa-lbeta.a) + (0 - sum(counts * log(thetas)))
    
  }
  if (options$bayesFactorType == "LogBF10") {
    BF <- logBF10
  } else if (options$bayesFactorType == "BF10") {
    BF <- exp(logBF10)
  } else if(options$bayesFactorType == "BF01") {
    BF <- 1/exp(logBF10)
  }
  
  return(list(BF = BF,
              warn = warn,
              expected = expected))
  
}


.multMedianAndCredibleIntervals <- function(counts, credibleIntervalInterval, computeMedian = FALSE){
  # Compute median (if specified) and credible intervals for proportions
  # alphas: vector with alpha parameters (To-Do!)
  # counts: vector of observed data
  # credibleIntervalInterval: value of the credible interval (e.g., 0.95)
  #
  # output: matrix with median (if specified), lower and upper bound of CI 
  #         for each parameters estimate (based on marginal beta distributions)
  
  N <- sum(counts)
  alphas <- rep(1, length(counts))
  
  lower <- (1 - credibleIntervalInterval)/2
  upper <- 1 - lower
  
  # median and credible intervals
  medianCI <- matrix(NA, ncol = 2, nrow = length(counts))
  colnames(medianCI) <- c('lowerCI', 'upperCI')
  
  for(i in seq_along(counts)){
    k <- counts[i]
    a <- alphas[i]
    b <- sum(alphas[-i])
    medianCI[i, ] <- qbeta(c(lower, upper), a + k, b + N - k) 
  
  if(computeMedian == TRUE){
    medianCI <- cbind(medianCI, qbeta(c(0.5), a + k, b + N - k))
    colnames(medianCI)[3] <- c('median')
  }
  }
  
  return(medianCI)
}
