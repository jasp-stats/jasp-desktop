#
# Copyright (C) 2013-2018 University of Amsterdam
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


ABTestBayesian <- function(
    dataset = NULL,
    options,
    perform = "run",
    callback = function(...) list(status = "ok"),
    state = NULL,
    ...
) {

    # FIXME:
    # 3. Data reading function is inneficient
    # 4. Code repetition in plotting functions

    # Configure the state
    variableOpts <- c("n1", "y1", "n2", "y2")
    priorOpts <- c("normal_mu", "normal_sigma")
    modelOpts <- c(variableOpts, "normal_mu", "normal_sigma", "orEqualTo1Prob", "orLessThan1Prob",
                   "orGreaterThan1Prob", "orNotEqualTo1Prob", "numSamples")

    stateKey <- list(
        ab_obj = modelOpts,
        descriptives = variableOpts,
        plotPosterior = c(modelOpts, "plotPosteriorType", "plotPriorAndPosterior"),
        plotSequentialAnalysis = c(modelOpts, "plotSequentialAnalysis"),
        plotPrior = c(priorOpts, "plotPriorType", "plotPriorOnly")
    )

    # Initialize the variables
    ab_obj <- state$ab_obj
    descriptives <- state$descriptives
    plotPosterior <- state$plotPosterior
    plotSequentialAnalysis <- state$plotSequentialAnalysis
    plotPrior <- state$plotPrior
    status <- state$status

    # Read the selected columns
    if (options$n1 != "" && options$y1 != "" && options$n2 != "" && options$y2 != "") {
        dataset <- .readData.abTest(perform, options)
    }

    # Set the status
    if (is.null(ab_obj)) { # status can only change in ab_obj
        status <- .setStatus.abTest(dataset, perform, options)
    }

    # Calculate the necessary components
    if (perform == "run" && status$ready) {
        # get the ab object
        if (is.null(ab_obj)) {
            ab_output <- .calcModel.abTest(
                dataset = dataset,
                status = status,
                options = options
            )

            status <- ab_output$status
            ab_obj <- ab_output$ab_obj
        }
    }

    # Populate the output table
    abTableData <- NULL
    if (!is.null(ab_obj)) {
        abTableData <- .calcDataTable.abTest(
            ab_obj = ab_obj,
            status = status,
            options = options
        )
    }

    if (options$descriptives && is.null(descriptives)) {
        descriptives <- .descriptivesTable.abTest(dataset, status, perform, options)
    }

    if (options[["plotPriorAndPosterior"]] && is.null(plotPosterior)) {
        plotPosterior <- .plotPosterior.abTest(ab_obj = ab_obj, status = status, perform = perform,
                                               posteriorPlotType = options[["plotPosteriorType"]])
    }

    if (options$plotSequentialAnalysis && is.null(plotSequentialAnalysis)) {
        plotSequentialAnalysis <- .plotSequentialAnalysis.abTest(ab_obj = ab_obj, status = status, perform = perform)
    }

    if (options[["plotPriorOnly"]] && is.null(plotPrior)) {
        plotPrior <- .plotPrior.abTest(options = options, status = status, perform = perform,
                                       priorPlotType = options[["plotPriorType"]])
    }

    # Assign to results
    results <- list()
    results[[".meta"]] <- .createMeta.abTest()
    results[["title"]] <- "Bayesian A/B Test"

    results[["abTestTable"]] <- .fillABTable.abTest(data = abTableData, status = status, perform = perform,
                                                    options = options)

    if (options$descriptives) {
        results[["descriptivesTable"]] <- descriptives
    }

    if (options[["plotPriorAndPosterior"]] || options$plotSequentialAnalysis || options[["plotPriorOnly"]]) {

        results[["inferentialPlots"]] <- list(title = "Inferential Plots", PosteriorPlot = plotPosterior,
                                              SequentialAnalysisPlot = plotSequentialAnalysis, PriorPlot = plotPrior)
    }

    # Set keep and the state
    if (perform == "init") {
        statusAnalysis <- "inited"
        keep <- state$keep
    } else { #run
        statusAnalysis <- "complete"
        keep <- c(plotPosterior$data, plotSequentialAnalysis$data, plotPrior$data)

        state <- list(
            options = options,
            ab_obj = ab_obj,
            descriptives = descriptives,
            plotPosterior = plotPosterior,
            plotSequentialAnalysis = plotSequentialAnalysis,
            plotPrior = plotPrior,
            status = status,
            keep = keep
        )
    }

    if (!is.null(state))
        attr(state, "key") <- stateKey

    return(list(
        keep = keep,
        results = results,
        status = statusAnalysis,
        state = state)
    )
}


.createMeta.abTest <- function() {
    # Creates and returns the 'meta' list required as a template to populate the output
    #
    # Return:
    #   A meta object

    meta <- list()
    meta[[1]] <- list(name = "abTestTable", type = "table")
    meta[[2]] <- list(name = "descriptivesTable", type = "table")
    meta[[3]] <- list(
        name="inferentialPlots",
        type="object",
        meta=list(
            list(name = "PosteriorPlot", type = "image"),
            list(name = "SequentialAnalysisPlot", type = "image"),
            list(name = "PriorPlot", type = "image")
        )
    )

    return(meta)
}


.fillABTable.abTest <- function(data = NULL, status, perform, options) {
    # Fills and returns the complete main table
    #
    # Args:
    #   data: list with footnotes and data rows calculated by .calcDataRegTable.basReg()
    #   status: current status of the analysis
    #   perform: 'run' or 'init'
    #   options: a list of user options
    #
    # Return:
    #   List with completed table; may be inserted in results as is

    if (options$bayesFactorType == "BF10") {
        bf.title <- "BF<sub>10</sub>"
    } else if (options$bayesFactorType == "BF01") {
        bf.title <- "BF<sub>01</sub>"
    } else if (options$bayesFactorType == "LogBF10") {
        bf.title <- "Log(BF<sub>10</sub>)"
    }

    fields <- list(
        list(name = "Models", type = "string"),
        list(name = "P(M)", type = "number", format = "sf:4;dp:3"),
        list(name = "P(M|data)", type = "number", format = "sf:4;dp:3"),
        list(name = "BF", type = "number", format = "sf:4;dp:3", title = paste(bf.title, sep = ""))
    )

    table <- list()
    table[["title"]] <- "Model Comparison"
    table[["citation"]] <- list()
    table[["schema"]] <- list(fields = fields)

    if (! is.null(data) && ! status$error) {
        table[["data"]] <- data$rows
        table[["footnotes"]] <- as.list(data$notes)
    } else {
        names <- sapply(fields, function(x) x$name)
        table[["data"]][[1]] <- setNames(as.list(rep(".", length(names))), names)
    }

    if (status$error) {
        table[["error"]] <- list(errorType = "badData", errorMessage = status$error.message)
    }

    return(table)
}


.readData.abTest <- function(perform, options) {
    # Get the relevant data
    #
    # Args:
    #   perform: 'run' or 'init'
    #   options: a list of user options
    #
    # Return:
    #   The (numeric) columns given as dependent/covariates/wlsWeights

    vars <- c(options$n1, options$y1, options$n2, options$y2)

    if (perform == "run") {
        n1 <- .readDataSetToEnd(columns.as.numeric = c(options$n1), exclude.na.listwise = c(options$n1))
        y1 <- .readDataSetToEnd(columns.as.numeric = c(options$y1), exclude.na.listwise = c(options$y1))
        n2 <- .readDataSetToEnd(columns.as.numeric = c(options$n2), exclude.na.listwise = c(options$n2))
        y2 <- .readDataSetToEnd(columns.as.numeric = c(options$y2), exclude.na.listwise = c(options$y2))

        dataset = list()
        dataset[["n1"]] <- n1[[.v(options$n1)]]
        dataset[["y1"]] <- y1[[.v(options$y1)]]
        dataset[["n2"]] <- n2[[.v(options$n2)]]
        dataset[["y2"]] <- y2[[.v(options$y2)]]
    } else {
        # ??
        dataset <- .readDataSetHeader(columns.as.numeric = vars)
    }

    return(dataset)
}


.setStatus.abTest <- function(dataset, perform, options) {
    # Create status object and do error checking; exit the analysis if any errors are found
    #
    # Args:
    #   dataset: dataset input by user
    #   perform: 'run' or 'init'
    #   options: a list of user options
    #
    # Return:
    #   A status object containing "ready", "error", "error.message"

    status <- list(ready = TRUE, error = FALSE, error.message = NULL)

    if (options$n1 == "" || options$n2 == "" || options$y1 == "" || options$y2 == "") {
        status$ready <- FALSE
    }

    # customChecks <- list()
    #
    # .hasErrors(dataset = dataset, perform = perform,
    # 	type=c("infinity", "observations", "variance"), custom = customChecks,
    # 	infinity.target = c(options$covariates, options$dependent, options$wlsWeight),
    # 	observations.target = options$dependent, observations.amount = paste("<", length(options$modelTerms) + 1),
    # 	variance.target = c(options$covariates, options$dependent),
    # 	exitAnalysisIfErrors = TRUE)

    return(status)
}


.calcModel.abTest <- function(dataset, status, options) {
    # Conducts A/B Test
    #
    # Args:
    #   dataset: dataset input by user
    #   status: current status of the analysis
    #   options: a list of user options
    #
    # Return:
    #   list containing the ab object

    prior_par <- list(mu_psi = options$normal_mu, sigma_psi = options$normal_sigma, mu_beta = 0, sigma_beta = 1)

    # Normalize prior probabilities
    sum_logor = options$orGreaterThan1Prob + options$orLessThan1Prob + options$orEqualTo1Prob + options$orNotEqualTo1Prob
    orGreaterThan1Prob <- options$orGreaterThan1Prob / sum_logor
    orLessThan1Prob    <- options$orLessThan1Prob / sum_logor
    orEqualTo1Prob     <- options$orEqualTo1Prob / sum_logor
    orNotEqualTo1Prob  <- options$orNotEqualTo1Prob / sum_logor

    prior_prob <- c(orNotEqualTo1Prob, orGreaterThan1Prob, orLessThan1Prob, orEqualTo1Prob)
    names(prior_prob) <- c("H1", "H+", "H-", "H0")

    ab <- try(abtest::ab_test(data = dataset, prior_par = prior_par, prior_prob = prior_prob,
                              posterior = TRUE, nsamples = options$numSamples))

    if (isTryError(ab)) {
        status$ready <- FALSE
        status$error <- TRUE
    }

    return(list(ab_obj = ab, status = status))
}


.calcDataTable.abTest <-function(ab_obj, status, options) {
    # Calculate the data needed for the main table
    #
    # Args:
    #   ab_obj: ab test object (including nuisanceTerms entry)
    #   status: current status of the analysis
    #   options: a list of user options
    #
    # Return:
    #   list with table footnotes and data rows (ready to insert in 'data' of table)

    if (status$error == TRUE) {
        return(NULL)
    }

    # if (options$bayesFactorType == "BF10") {
    #     bf10 <- 1
    #     bfplus0 <- ab_obj$bf[["bfplus0"]]
    #     bfminus0 <- ab_obj$bf[["bfminus0"]]
    # } else if (options$bayesFactorType == "BF01") {
    #     bf10 <- 1
    #     bfplus0 <- 1 / ab_obj$bf[["bfplus0"]]
    #     bfminus0 <- 1 / ab_obj$bf[["bfminus0"]]
    # } else if (options$bayesFactorType == "LogBF10") {
    #     bf10 <- 0
    #     bfplus0 <- ab_obj$logbf[["bfplus0"]]
    #     bfminus0 <- ab_obj$logbf[["bfminus0"]]
    # }

    # Normalize prior probabilities
    sum_logor = options$orGreaterThan1Prob + options$orLessThan1Prob + options$orEqualTo1Prob + options$orNotEqualTo1Prob
    orGreaterThan1Prob <- options$orGreaterThan1Prob / sum_logor
    orLessThan1Prob    <- options$orLessThan1Prob / sum_logor
    orEqualTo1Prob     <- options$orEqualTo1Prob / sum_logor
    orNotEqualTo1Prob  <- options$orNotEqualTo1Prob / sum_logor

    output.rows <- list()

    output.rows[[1]] <- list(
        "Models" = "Log odds ratio = 0",
        "BF" = .clean(1.00),
        "P(M|data)" = ab_obj$post_prob[["H0"]],
        "P(M)" = .clean(orEqualTo1Prob)
    )

    output.rows[[2]] <- list(
        "Models" = "Log odds ratio > 0",
        "BF" = .clean(ab_obj$bf[["bfplus0"]]),
        "P(M|data)" = ab_obj$post_prob[["H+"]],
        "P(M)" = .clean(orGreaterThan1Prob)
    )

    output.rows[[3]] <- list(
        "Models" = "Log odds ratio < 0",
        "BF" = .clean(ab_obj$bf[["bfminus0"]]),
        "P(M|data)" = ab_obj$post_prob[["H-"]],
        "P(M)" = .clean(orLessThan1Prob)
    )

    if (orNotEqualTo1Prob > 0) {
        output.rows[[4]] <- list(
            "Models" = "Log odds ratio ≠ 0",
            "BF" = .clean(ab_obj$bf[["bf10"]]),
            "P(M|data)" = ab_obj$post_prob[["H1"]],
            "P(M)" = .clean(orNotEqualTo1Prob)
        )
    }

    if (options$bayesFactorOrder == "bestModelTop") {
        ordered <- output.rows[order(sapply(output.rows, "[[", "P(M|data)"), decreasing = TRUE)]

        best_model_bf <- ordered[[1]]$BF
        ordered[[1]]$BF <- .clean(ordered[[1]]$BF / best_model_bf)
        ordered[[2]]$BF <- .clean(ordered[[2]]$BF / best_model_bf)
        ordered[[3]]$BF <- .clean(ordered[[3]]$BF / best_model_bf)
        if (orNotEqualTo1Prob > 0) {
            ordered[[4]]$BF <- .clean(ordered[[4]]$BF / best_model_bf)
        }

        output.rows <- ordered
    }

    if (options$bayesFactorType == "BF01") {
        output.rows[[1]]$BF <-.clean(1 / output.rows[[1]]$BF)
        output.rows[[2]]$BF <-.clean(1 / output.rows[[2]]$BF)
        output.rows[[3]]$BF <-.clean(1 / output.rows[[3]]$BF)
        if (orNotEqualTo1Prob > 0) {
            output.rows[[4]]$BF <-.clean(1 / output.rows[[4]]$BF)
        }

        # output.rows <- lapply(output.rows, function(x) {x$BF <- .clean(1 / x$BF); return(x)})
    } else if (options$bayesFactorType == "LogBF10") {
        output.rows[[1]]$BF <-.clean(base::log(output.rows[[1]]$BF))
        output.rows[[2]]$BF <-.clean(base::log(output.rows[[2]]$BF))
        output.rows[[3]]$BF <-.clean(base::log(output.rows[[3]]$BF))
        if (orNotEqualTo1Prob > 0) {
            output.rows[[4]]$BF <-.clean(base::log(output.rows[[4]]$BF))
        }
    }

    # Add footnotes if necessary
    footnotes <- NULL

    return(list(rows=output.rows, notes=footnotes))
}


.descriptivesTable.abTest <- function(dataset, status, perform, options) {
    # Generate a descriptives table (counts, total, proportion)
    #
    # Args:
    #   dataset: data read by .readData.basReg()
    #   status: current status of the analysis
    #   perform: 'run' or 'init'
    #   options: a list of user options
    #
    # Return:
    #   List with completed descriptives table; may be inserted in results as is

    descriptives <- list()
    descriptives[["title"]] <- "Descriptives"

    fields <- list(
        list(name="group",      title="",           type="string" ),
        list(name="counts",     title="Counts",     type="integer"),
        list(name="total",      title="Total",      type="integer"),
        list(name="proportion", title="Proportion", type="number", format="sf:4;dp:3")
    )

    descriptives[["schema"]] <- list(fields=fields)

    if (status$ready && perform == "run") {
        rows <- list()

        num_rows = length(dataset$y1)
        counts = dataset$y1[num_rows]
        total = dataset$n1[num_rows]
        rows[[1]] <- list(group = "Group 1", counts = counts, total = total, proportion = counts / total)

        counts = dataset$y2[num_rows]
        total = dataset$n2[num_rows]
        rows[[2]] <- list(group = "Group 2", counts = counts, total = total, proportion = counts / total)

        descriptives[["data"]] <- rows

    } else {
       descriptives[["data"]][[1]] <- list(group = ".", counts = ".", total = ".", proportion = ".")
   }

    return(descriptives)
}


.makeEmptyPlot.abTest <- function(xlab = NULL, ylab = NULL, title = NULL, status) {
    # Convenience function to create empty x and y-axes
    #
    # Args:
    #   xlab: label for x-axis
    #   ylab: label for y-axis
    #   title: title of the plot in the meta list
    #   status: current status of the analysis
    #
    # Return:
    #   list with an empty plot (if there are errors in the analysis it is a badData plot)

    plot <- list()
    plot[["title"]] <- title
    plot[["status"]] <- "waiting"

    plotFunc <- function() {
        plot(1, type = "n", xlim = 0:1, ylim = 0:1, bty = "n", axes = FALSE, xlab = "", ylab = "")

        axis(1, at = 0:1, labels = FALSE, cex.axis = 1.6, lwd = 2, xlab = "")
        axis(2, at = 0:1, labels = FALSE, cex.axis = 1.6, lwd = 2, ylab = "")

        if (is.null(ylab)) {
            mtext(text = ylab, side = 2, las = 0, cex = 1.6, line = 3.25)
        }

        if (is.null(xlab)) {
            mtext(xlab, side = 1, cex = 1.5, line = 2.6)
        }
    }

    content <- .writeImage(width = 530, height = 400, plot = plotFunc)

    plot[["obj"]] <- content[["obj"]]
    plot[["data"]] <- content[["png"]]
    plot[["width"]] <- 530
    plot[["height"]] <- 400
    plot[["status"]] <- "complete"

    if (status$error) {
        plot[["error"]] <- "badData"
    }

    return(plot)
}


.plotPosterior.abTest <- function(ab_obj, status, perform, posteriorPlotType) {
    # Plot the posterior for different models
    #
    # Args:
    #   ab_obj: bas object
    #   status: current status of the analysis
    #   perform: 'run' or 'init'
    #   posteriorPlotType
    #
    # Return:
    #   list with plot data

    title <- "Prior and Posterior"
    emptyPlot <- .makeEmptyPlot.abTest(title = title, status = status)

    if (!(status$ready && perform == "run")) {
        emptyPlot[["title"]]  <- title
        emptyPlot[["status"]] <- "waiting"
        return (emptyPlot)
    }

    what <- switch(
        posteriorPlotType,
        "LogOddsRatio" = "logor",
        "OddsRatio" = "or",
        "RelativeRisk" = "rrisk",
        "AbsoluteRisk" = "arisk",
        "p1&p2" = "p1p2"
    )

    plot <- list()
    plot[["title"]]  <- title
    plot[["status"]] <- "waiting"

    p <- try(silent = FALSE, expr = {

        plotFunc <- function() {
            abtest::plot_posterior(x = ab_obj, what = what, hypothesis = "H1")
        }
        # plotObj <- .plotImage.basReg(bas_obj) # to be implemented later
        content <- .writeImage(width = 530, height = 400, plot = plotFunc)

        plot[["convertible"]] <- TRUE
        plot[["obj"]] <- content[["obj"]]
        plot[["data"]] <- content[["png"]]
        plot[["width"]] <- 530
        plot[["height"]] <- 400
        plot[["status"]] <- "complete"
    })

    if (class(p) == "try-error") {
        errorMessage <- .extractErrorMessage(p)
        plot[["error"]] <- list(error="badData",
                        errorMessage = paste("Plotting is not possible: ", errorMessage))
    }

    plot[["status"]] <- "complete"

    return (plot)
}


.plotPrior.abTest <- function(options, status, perform, priorPlotType) {
    # Plot the prior
    #
    # Args:
    #   options
    #   status: current status of the analysis
    #   perform: 'run' or 'init'
    #   priorPlotType
    #
    # Return:
    #   list with plot data

    prior_par <- list(mu_psi = options$normal_mu, sigma_psi = options$normal_sigma, mu_beta = 0, sigma_beta = 1)

    title <- "Prior"
    emptyPlot <- .makeEmptyPlot.abTest(title = title, status = status)

    if (!(perform == "run")) {
        emptyPlot[["title"]]  <- title
        emptyPlot[["status"]] <- "waiting"
        return (emptyPlot)
    }

    what <- switch(
        priorPlotType,
        "LogOddsRatio" = "logor",
        "OddsRatio" = "or",
        "RelativeRisk" = "rrisk",
        "AbsoluteRisk" = "arisk",
        "p1&p2" = "p1p2",
        "p1" = "p1",
        "p2" = "p2"
    )

    plot <- list()
    plot[["title"]]  <- title
    plot[["status"]] <- "waiting"

    p <- try(silent = FALSE, expr = {

        plotFunc <- function() {
            abtest::plot_prior(prior_par = prior_par, what = what, hypothesis = "H1")
        }
        # plotObj <- .plotImage.basReg(bas_obj) # to be implemented later
        content <- .writeImage(width = 530, height = 400, plot = plotFunc)

        plot[["convertible"]] <- TRUE
        plot[["obj"]] <- content[["obj"]]
        plot[["data"]] <- content[["png"]]
        plot[["width"]] <- 530
        plot[["height"]] <- 400
        plot[["status"]] <- "complete"
    })

    if (class(p) == "try-error") {
        errorMessage <- .extractErrorMessage(p)
        plot[["error"]] <- list(error="badData",
                        errorMessage = paste("Plotting is not possible: ", errorMessage))
    }

    plot[["status"]] <- "complete"

    return (plot)
}


.plotSequentialAnalysis.abTest <- function(ab_obj, status, perform) {
    # Plot the posterior for different models
    #
    # Args:
    #   ab_obj: bas object
    #   status: current status of the analysis
    #   perform: 'run' or 'init'
    #
    # Return:
    #   list with plot data

    title <- "Sequential Analysis"
    emptyPlot <- .makeEmptyPlot.abTest(title = title, status = status)

    if (!(status$ready && perform == "run")) {
        emptyPlot[["title"]]  <- title
        emptyPlot[["status"]] <- "waiting"
        return (emptyPlot)
    }

    plot <- list()
    plot[["title"]]  <- title
    plot[["status"]] <- "waiting"

    p <- try(silent = FALSE, expr = {

        plotFunc <- function() {
            abtest::plot_sequential(x = ab_obj)
        }
        # plotObj <- .plotImage.basReg(bas_obj) # to be implemented later
        content <- .writeImage(width = 530, height = 400, plot = plotFunc)

        plot[["convertible"]] <- TRUE
        plot[["obj"]] <- content[["obj"]]
        plot[["data"]] <- content[["png"]]
        plot[["width"]] <- 530
        plot[["height"]] <- 400
        plot[["status"]] <- "complete"
    })

    if (class(p) == "try-error") {
        errorMessage <- .extractErrorMessage(p)
        plot[["error"]] <- list(error="badData",
                        errorMessage = paste("Plotting is not possible: ", errorMessage))
    }

    plot[["status"]] <- "complete"

    return (plot)
}
