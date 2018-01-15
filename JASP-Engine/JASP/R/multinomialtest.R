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

MultinomialTest <- function (dataset = NULL, options, perform = "run",
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

  chisqResults <- NULL # result of the chi-square test
  descriptivesTable <- NULL # expected versus observed
  descriptivesPlot <- NULL # barplot of factor levels

  # Then, we can fill the output objects with old info if its option did not
  # change.
  if (!is.null(state)) {
    diff <- .diff(options, state$options) # a list of TRUE/FALSE

    if (is.list(diff)){

      if (!any(diff[["factor"]], diff[["counts"]],
               diff[["confidenceIntervalInterval"]],
               diff[["hypothesis"]], diff[["exProbVar"]],
               diff[["expectedProbs"]], diff[["simulatepval"]],
               (options[["hypothesis"]] == "expectedProbs" &&
               diff[["tableWidget"]])
              )) {

        chisqResults <- state[["chisqResults"]]

        # the following depend on chisqResults so in same if-statement
        if (!any(diff[["confidenceInterval"]],
                 diff[["countProp"]])) {
          descriptivesTable <- state[["descriptivesTable"]]
        }

        if (!any(diff[["descriptivesPlotConfidenceInterval"]],
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
  results[["title"]] <- "Multinomial Test"
  results[[".meta"]] <- list(list(name = "chisq", type = "table"),
                             list(name = "descriptivesTable", type = "table"),
                             list(name = "descriptivesPlot", type = "image"))

  # chi-square Table
  # Generate results
  if (is.null(chisqResults)) {
    chisqResults <- .chisquareTest(dataset, options, fact, perform)
  }

  results[["chisq"]] <- .chisqTable(chisqResults, options, perform)

  # Descriptives Table
  if (options[["descriptives"]]) {
    # Generate descriptives table
    if (is.null(descriptivesTable)) {
      descriptivesTable <- .multinomialDescriptives(chisqResults, fact, options, perform)
    }

    results[["descriptivesTable"]] <- descriptivesTable
  } else {

    results[["descriptivesTable"]] <- NULL
  }

  # Multinomial Descriptives Plot
  if (options[["descriptivesPlot"]]) {
    # Generate descriptives plots
    if (is.null(descriptivesPlot)) {
      descriptivesPlot <- .multinomialDescriptivesPlot(chisqResults, options, perform)
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
    state[["chisqResults"]] <- chisqResults
    state[["descriptivesTable"]] <- descriptivesTable
    state[["descriptivesPlot"]] <- descriptivesPlot

    return(list(results=results, status="complete", state=state,
                keep = plotPath))
  } else {
    return(list(results=results, status="inited", state=state,
                keep = plotPath))
  }
}


.chisquareTest <- function(dataset, options, fact, perform) {
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

  chisqResults <- NULL

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
    chisqResults <- lapply(hyps, function(h) {
      # catch warning message and append to object if necessary
      csr <- NULL
      warn <- NULL
      csr <- withCallingHandlers(
        chisq.test(x = t, p = h, rescale.p = TRUE,
                   simulate.p.value = options$simulatepval),
        warning = function(w) {
         warn <<- w$message
        }
      )
      csr[["warn"]] <- warn
      return(csr)
    })
  }

  return(chisqResults)  # return the out object
}


.chisqTable <- function(chisqResults, options, perform) {
  # Transform chi-square test object into table for JASP
  # chisqResults = list(H1 = obj, H2 = obj, ....)
  #
  # Args:
  #   chisqResults:
  #   options: input options
  #   perform: init or run
  #
  # Return:
  #   Chi square table
  table <- list()
  footnotes <- .newFootnotes()
  table[["title"]] <- "Multinomial Test"

  # include fields
  fields <- list(
    list(name="case", title="", type="string", combine=TRUE),
    list(name="chisquare", title="\u03C7\u00B2", type = "number", format = "sf:4;dp:3"),
    list(name="df", title="df", type="integer"),
    list(name="p", title="p", type="number", format="dp:3;p:.001")
    )

  # include Vovk-Selke p-ratio as columns
  if (options$VovkSellkeMPR) {
    .addFootnote(footnotes, symbol = "\u002A", text = "Vovk-Sellke Maximum
    <em>p</em>-Ratio: Based the <em>p</em>-value, the maximum
    possible odds in favor of H\u2081 over H\u2080 equals
    1/(-e <em>p</em> log(<em>p</em>)) for <em>p</em> \u2264 .37
    (Sellke, Bayarri, & Berger, 2001).")
    fields[[length(fields) + 1]] <- list(name = "VovkSellkeMPR",
                                        title = "VS-MPR\u002A",
                                        type = "number",
                                        format = "sf:4;dp:3")
  }

  # include footnotes
  table[["schema"]] <- list(fields = fields)

  message <- list()

  for (r in 1:length(chisqResults)) {

    if (!is.null(chisqResults[[r]][["warn"]])) {
      .addFootnote(footnotes, symbol = "<em>Note.</em>", text = chisqResults[[r]][["warn"]])
    }
  }

  table[["footnotes"]] <- as.list(footnotes)

  # fill in results one row at a time
  if (!is.null(chisqResults)) {

    for (r in 1:length(chisqResults)) {
      df <- chisqResults[[r]][["parameter"]][["df"]]
      if (is.na(df)) df <- "-" # This happens when the monte carlo option is checked
      table[["data"]][[r]] <- list(case = names(chisqResults)[r],
                                   chisquare = .clean(chisqResults[[r]][["statistic"]][["X-squared"]]),
                                   df = .clean(df),
                                   p = chisqResults[[r]][["p.value"]])

      if (options$VovkSellkeMPR){
        for (row in 1:length(table[["data"]])){
          table[["data"]][[row]][["VovkSellkeMPR"]] <- .VovkSellkeMPR(table[["data"]][[row]][["p"]])
        }
      }
      table[["status"]] <- "complete"
    }
  } else {
    # init state?
    if (options$VovkSellkeMPR){
      data <- list(list(case = ".", chisquare = ".", df = ".", p = ".",
                        VovkSellkeMPR = ".", lowerCI = ".", upperCI = "."))
    } else {
      data <- list(list(case = ".", chisquare = ".", df = ".", p = ".",
                        lowerCI = ".", upperCI = "."))
    }

    table[["data"]] <- data
  }

  return(table)
}


.multinomialDescriptives <- function(chisqResults, fact, options, perform) {
  # Create multinomial descriptives table
  #
  # Args:
  #   chisqResults:
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
    if (options$confidenceInterval){
      interval <- 100 * options$confidenceIntervalInterval
      title <- paste0(interval, "% Confidence Interval")
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
      .addFootnote(footnotes, symbol = "<em>Note.</em>", "Confidence intervals are based on independent binomial distributions.")
    }

    rows <- list(list(factor = ".", observed = ".", expected = "."))
  } else if (perform != "run") {
    # If we have a variable then init table with factor name

    fields <- list(
      list(name="factor", title=fact, type = "string"),
      c(list(name="observed", title="Observed"), numberType),
      c(list(name="expected", title="Expected"), numberType)
    )
    if (options$confidenceInterval){
      interval <- 100 * options$confidenceIntervalInterval
      title <- paste0(interval, "% Confidence Interval")
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
      .addFootnote(footnotes, symbol = "<em>Note.</em>", "Confidence intervals are based on independent binomial distributions.")
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

    nms <- names(chisqResults)

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

    if (options$confidenceInterval){
      interval <- 100 * options$confidenceIntervalInterval
      title <- paste0(interval, "% Confidence Interval")
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
      .addFootnote(footnotes, symbol = "<em>Note.</em>", "Confidence intervals are based on independent binomial distributions.")
    }

    # Then we fill the columns with the information
    if (options[["countProp"]]=="descCounts"){
      div <- 1
    } else {
      div <- sum(chisqResults[[1]][["observed"]])
    }

    tableFrame <- data.frame(
      factor = names(chisqResults[[1]][["observed"]]),
      observed = as.integer(chisqResults[[1]][["observed"]])/div,
      stringsAsFactors = FALSE
    )

    for (r in chisqResults){
      tableFrame <- cbind(tableFrame, as.integer(r[["expected"]])/div)
    }

    if (length(nms) == 1) {
      colnames(tableFrame)[-(1:2)] <- "expected"
    } else {
      colnames(tableFrame)[-(1:2)] <- nms
    }

    # Add confidenceInterval
    if (options$confidenceInterval){
      n <- sum(chisqResults[[1]][["observed"]])
      # make a list of cis
      ci <- lapply(chisqResults[[1]][["observed"]], function(l) {
        bt <- binom.test(l,n,conf.level = options$confidenceIntervalInterval)
        return(bt$conf.int * n) # on the count scale
      })

      # add these to the tableFrame
      ciDf <- t(data.frame(ci))
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


.multinomialDescriptivesPlot <- function(chisqResults, options, perform) {
  # Create multinomial descriptives plot
  #
  # Args:
  #   chisqResults:
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
      div <- sum(chisqResults[[1]][["observed"]])
      yname <- "Observed proportions"
    }

    # we need to reverse the factor's levels because of the coord_flip later
    f <- names(chisqResults[[1]][["observed"]])
    plotFrame <- data.frame(
      factor = factor(f, levels = rev(f)),
      obs = as.numeric(chisqResults[[1]][["observed"]])/div
    )

    # Calculate confidence interval
    cl <- options$descriptivesPlotConfidenceInterval
    n <- sum(chisqResults[[1]][["observed"]])
    ci <- lapply(chisqResults[[1]][["observed"]], function(l) {
      bt <- binom.test(l, n, conf.level = cl)
      return(bt$conf.int * n) # on the count scale
    })
    ciDf <- data.frame(t(data.frame(ci)))/div
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
      ggplot2::ylab(yname)
      
    p <- JASPgraphs::themeJasp(p, horizontal = TRUE, xAxis = FALSE)
    
    

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


.multinomialHypotheses <- function(dataset, options, nlevels) {
  # Transform input into a list of hypotheses
  # This function transforms the input into a list of hypotheses
  #
  # Args:
  #   dataset: input dataset
  #   options: user options
  #   nlevels:
  #
  # Return:
  #   hypotheses

  hyps <- list()
  if (options$exProbVar == "" && options$hypothesis == "multinomialTest") {
    # Expected probabilities are simple now
    hyps[["Multinomial"]] <- rep(1/nlevels, nlevels)
  } else {
    # First, generate a table with expected probabilities based on input
    expectedDf <- .generateExpectedProps(dataset, options, nlevels)
    # assign each hypothesis to the hyps object
    hyps <- as.list(expectedDf)
  }
  return(hyps)
}


.generateExpectedProps <- function(dataset, options, nlevels) {
  # Parse expected probabilities/counts
  # This function returns a data frame with in each named column the expected
  # probabilities. The column names are the hypothesis names.
  #
  # Args:
  #   dataset: input dataset
  #   options: user options
  #   nlevels:
  #
  # Return:
  #   expected Probabilities

  if (options$exProbVar != "") {
    if (options$counts == "") {
      .quitAnalysis("Expected counts not supported without observed counts!")
    }
    # use only exProbVar
    fact <- dataset[[.v(options$factor)]]
    eProps <- data.frame(dataset[[.v(options$exProbVar)]])
    rownames(eProps) <- fact

    # Reorder to match factor levels
    eProps <- data.frame(eProps[levels(fact),])
    colnames(eProps) <- options$exProbVar
    rownames(eProps) <- levels(fact)

    if (nlevels != nrow(eProps)) {
      stop("Expected counts do not match number of levels of factor!")
    }

    return(eProps)

  } else if (length(options$tableWidget) > 0) {
    eProps <- sapply(options$tableWidget, function(x) {
      vals <- unlist(x$values)
      if (sum(vals) == 0) {
        vals <- rep(1, length(vals))
      } else {
        vals
      }
    })

    colnames(eProps) <- sapply(seq_along(options$tableWidget), function(x) paste0("H\u2080 (", letters[x], ")"))
    rownames(eProps) <- options$tableWidget[[1]]$levels

    return(as.data.frame(eProps))
  } else {

    stop("No expected counts entered!")
  }
}
