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

.ttestBayesianRunAnalysis <- function(jaspResults, dataset, options,
                                      analysis = c("independent", "paired", "one-sample")) {

  # the main workhorse of the Bayesian t-tests.
  #
  # Structure of the Bayesian T-tests:
  #
  # |- ttestContainer (dummy container)
  #     |-dependOn(c("effectSizeStandardized", "groupingVariable", "hypothesis",
  #     |            "informativeCauchyLocation", "informativeCauchyScale", "informativeNormalMean",
  #     |            "informativeNormalStd", "informativeStandardizedEffectSize",
  #     |            "informativeTDf", "informativeTLocation", "informativeTScale",
  #     |            "missingValues", "priorWidth", "testStatistic", "wilcoxonSamplesNumber",
  #     |            "testValue"
  #     | ))
  #     |-ttestResults (main table with BF for each test)
  #
  #     |-Inferential plots
  #       |-for var in variables
  #          |-priorAndPosteriorPlots
  #            |-dependOn("plotPriorAndPosteriorAdditionalInfo", optionContainsValue = var)
  #
  #          |-robustnessPlots
  #            |-dependOn("plotBayesFactorRobustnessAdditionalInfo", optionContainsValue = var)
  #
  #          |-sequentialPlots
  #            |-dependOn("plotSequentialAnalysisRobustness", optionContainsValue = var)
  #
  #     |-delta
  #       |-dependOn(c("priorWidth", "groupingVariable", "missingValues", "wilcoxonSamplesNumber"))
  #
  # |- descriptivesContainer
  #     |-dependOn(c("groupingVariable", "missingValues", "descriptivesPlotsCredibleInterval", "descriptivesPlots"))
  #

  analysis <- match.arg(analysis)
	dataset <- .ttestBayesianReadData(dataset, options)
	errors  <- .ttestBayesianGetErrorsPerVariable(dataset, options, analysis)

	# Main analysis
	ttestResults <- .ttestBayesian(jaspResults, dataset, options, errors, analysis)

	# create descriptives table and plots
  .ttestBayesianDescriptives(jaspResults, dataset, options, ttestResults, errors)

  # create inferential plots
	.ttestBayesianInferentialPlots(jaspResults, dataset, options, ttestResults, errors)

	return()

}

.ttestBayesian <- function(jaspResults, dataset, options, errors, analysis) {

  derivedOptions <- .ttestBayesianDerivedOptions(jaspResults, options, analysis)

  if (is.null(jaspResults[["ttestContainer"]])) {
    ttestContainer <- createJaspContainer("")
    jaspResults[["ttestContainer"]] <- ttestContainer
    ttestContainer$dependOn(c(
      "effectSizeStandardized", "groupingVariable", "hypothesis",
      "informativeCauchyLocation", "informativeCauchyScale", "informativeNormalMean",
      "informativeNormalStd", "informativeStandardizedEffectSize",
      "informativeTDf", "informativeTLocation", "informativeTScale",
      "missingValues", "priorWidth", "testStatistic", "wilcoxonSamplesNumber",
      "testValue"
    ))
    ttestContainer$position <- 1L
  } else {
	  ttestContainer <- jaspResults[["ttestContainer"]]
  }

  # check if we actually need to compute things
  if (!is.null(ttestContainer[["ttestTable"]]) && !derivedOptions[["anyNewVariables"]]) {
    obj <- ttestContainer[["stateTTestResults"]]$object
    obj[["derivedOptions"]] <- derivedOptions
    return(obj)
  }

  ttestState <- ttestContainer[["stateTTestResults"]]$object
  # recompute the analysis / table
  ttestResults <- switch(analysis,
    "independent" = .ttestBISTTest(ttestContainer, dataset, options, derivedOptions, errors, ttestState),
    "one-sample"  = .ttestBOSTTest(ttestContainer, dataset, options, derivedOptions, errors, ttestState),
    "paired"      = .ttestBPSTTest(ttestContainer, dataset, options, derivedOptions, errors, ttestState)
  )

  tmp <- createJaspState(ttestResults)
  ttestContainer[["stateTTestResults"]] <- tmp

	return(ttestResults)

}

.ttestBayesianReadData <- function(dataset = NULL, options) {

  if (is.null(dataset)) {
    missing <- options[["missingValues"]]
    if (is.null(options[["variables"]])) {
      dependents <- unique(unlist(options[["pairs"]] ))
      dependents <- dependents[dependents != ""]
    } else {
      dependents <- unlist(options[["variables"]])
    }
    grouping <- options[["groupingVariable"]]
    if (identical(grouping, ""))
      grouping <- NULL

    excl <- grouping
    if (missing == "excludeListwise")
      excl <- c(excl, dependents)

    if (length(dependents)) {
      dataset <- .readDataSetToEnd(columns = c(dependents, grouping), exclude.na.listwise = excl)
      if (!is.null(grouping))
        dataset[[.v(grouping)]] <- as.factor(dataset[[.v(grouping)]])

      # 100% required if we fully switch to columns = ... , but also allow the QMl iunterface to be not strict in terms of input,
      # so factors can be entered in scale boxes. Joris probably has more ideas about this
      for (var in .v(dependents))
        dataset[[var]] <- as.numeric(dataset[[var]])

    }
  }
  return(dataset)
}

.ttestBayesianGetErrorsPerVariable <- function(dataset, options, analysis) {

  errors <- list()
  if (analysis == "independent") {

    dependents <- unlist(options[["variables"]])
    grouping   <- options[["groupingVariable"]]

    # analysis breaking errors
    if (grouping != "") {
      .hasErrors(dataset, "run", type = c('factorLevels', 'variance'),
                 factorLevels.target = grouping, factorLevels.amount = '!= 2',
                 variance.target = dependents, variance.grouping = grouping,
                 exitAnalysisIfErrors = TRUE)
    } else {
      grouping <- NULL
    }

    for (var in dependents) {

      errors[[var]] <- .hasErrors(dataset, perform = "run", message = 'short',
                                  type = c('infinity','observations','variance'),
                                  all.target = var, observations.amount = "< 2",
                                  all.grouping = grouping)
    }



  } else if (analysis == "one-sample") {

    dependents <- unlist(options[["variables"]])
    for (var in dependents) {
      errors[[var]] <- .hasErrors(dataset, perform = "run", message = 'short',
                                  type = c('infinity','observations','variance'),
                                  all.target = var, observations.amount = "< 2")
    }

  } else {

    nms <- sapply(options[["pairs"]], paste, collapse = " - ")
    for (i in seq_along(options[["pairs"]])) {
      pair <- options[["pairs"]][[i]]
      var <- nms[i]

      if (pair[[1L]] == "" || pair[[2L]] == "") {

        errors[[var]] <- list(message = sprintf("Please provide another variable."))

      } else if (identical(pair[[1L]], pair[[2L]])) {

        errors[[var]] <- list(message = sprintf("Variables %s and %s are the same!",
                                                pair[[1L]], pair[[2L]]))

      } else {

        errors[[var]] <- .hasErrors(dataset, perform = "run", message = 'short',
                                    type = c('infinity','observations','variance'),
                                    all.target = c(pair[[1L]],pair[[2L]]), observations.amount = "< 2")
      }
    }
  }
  return(errors)
}

.ttestBayesianInitBayesFactorPackageOptions <- function() {

  # sets all options required for the BayesFactor package.
  # note that options$... / options[[...]] and options(...) are two COMPLETELY DIFFERENT things!!
  # also, options(...) is, by R's default, global so this works

  if(is.null(options("BFMaxModels")))         options(BFMaxModels = 50000)
  if(is.null(options("BFpretestIterations"))) options(BFpretestIterations = 100)
  if(is.null(options("BFapproxOptimizer")))   options(BFapproxOptimizer = "optim")
  if(is.null(options("BFapproxLimits")))      options(BFapproxLimits = c(-15,15))
  if(is.null(options("BFprogress")))          options(BFprogress = interactive())
  if(is.null(options("BFfactorsMax")))        options(BFfactorsMax = 5)

}

.ttestBayesianDerivedOptions <- function(jaspResults, options, analysis) {

  # initialize derived options: takes user input and determines:
  #
  # - can any analysis be done?
  # - which variables will need to be computed for plots?
  #
  # in addition:
  #
  # - unifies the hypothesis type (larger, smaller or equal) inside options[["oneSided"]]
  # - defines the dependencies for all objects and puts this inside options[["stateKey"]]

  derivedOptions <- list(ttestType = analysis)
  if (analysis == "independent") {

    dependents <- unlist(options[["variables"]])

    derivedOptions[["variables"]]    <- dependents
    derivedOptions[["ready"]] <- length(dependents) > 0L && options[["groupingVariable"]] != ""
    derivedOptions[["wilcoxTest"]] <- options[["testStatistic"]] == "Wilcoxon"
    derivedOptions[["oneSided"]] <- switch(
      options[["hypothesis"]],
      "groupOneGreater" = "right",
      "groupTwoGreater" = "left",
      FALSE
    )
    AtTheEndResetPlotRobustnessSequential <- NULL
    if (derivedOptions[["wilcoxTest"]]) {

      # when a user requests robustness/ sequential plots first and then selects wilcoxTest
      # jasp will still provide these as TRUE, but they shouldn't be.
      AtTheEndResetPlotRobustnessSequential <- options[c("plotBayesFactorRobustness", "plotSequentialAnalysis")]
      derivedOptions[["plotBayesFactorRobustness"]] <- FALSE
      derivedOptions[["plotSequentialAnalysis"]] <- FALSE

    }

  } else if (analysis == "one-sample") { # one-sample

    dependents <- unlist(options[["variables"]])
    derivedOptions[["variables"]]    <- dependents
    derivedOptions[["ready"]] <- length(dependents) > 0L
    derivedOptions[["wilcoxTest"]] <- FALSE

    derivedOptions[["oneSided"]] <- switch(
      options[["hypothesis"]],
      "greaterThanTestValue" = "right",
      "lessThanTestValue" = "left",
      FALSE
    )

  } else { # paired

    # this needs to be decided for each pair individually, which is done inside .ttestBPSTTest
    derivedOptions[["ready"]] <- TRUE
    derivedOptions[["wilcoxTest"]] <- FALSE

    dependents <- sapply(options[["pairs"]], paste, collapse = " - ")
    derivedOptions[["variables"]]    <- dependents
    derivedOptions[["pairs"]]        <- options[["pairs"]]
    names(derivedOptions[["pairs"]]) <- dependents

    derivedOptions[["oneSided"]] <- switch(
      options[["hypothesis"]],
      "groupOneGreater" = "right",
      "groupTwoGreater" = "left",
      FALSE
    )
  }

  derivedOptions[["nullInterval"]] <- switch(
    as.character(derivedOptions[["oneSided"]]),
    "right" = c(0, Inf),
    "left"  = c(-Inf, 0),
    c(-Inf, Inf)
  )

	oldDependents <- jaspResults[["stateVariables"]]$object

	# TODO: check if we can do without this
	if (!is.null(dependents)) {
	  tmp <- createJaspState(object = dependents)
	  tmp$dependOn("missingValues") # <-- check if necessary!
	  if (analysis == "independent")
	    tmp$dependOn("groupingVariable") # <-- check if necessary!
	  jaspResults[["stateVariables"]] <- tmp
	}

	idx <- !(dependents %in% oldDependents)
	derivedOptions[["anyNewVariables"]] <- any(idx)
	derivedOptions[["newVariables"]] <- dependents[idx]

  return(derivedOptions)

}

.ttestBayesianGetBFTitle <- function(bfType = c("BF10", "BF01", "LogBF10"),
                                     hypothesis = c("equal", "greater", "smaller")) {

  bfType <- match.arg(bfType)
  hypothesis <- match.arg(hypothesis)

  if (bfType == "BF10") {
	  if (hypothesis == "equal") {
	    bfTitle <- "BF\u2081\u2080"
	  } else if (hypothesis == "greater") {
	    bfTitle <- "BF\u208A\u2080"
	  } else {
	    bfTitle <- "BF\u208B\u2080"
	  }
	} else if (bfType == "LogBF10") {
	  if (hypothesis == "equal") {
	    bfTitle <- "Log(\u0042\u0046\u2081\u2080)"
	  } else if (hypothesis == "greater") {
	    bfTitle <- "Log(\u0042\u0046\u208A\u2080)"
	  } else {
	    bfTitle <- "Log(\u0042\u0046\u208B\u2080)"
	  }
	} else if (bfType == "BF01") {
	  if (hypothesis == "equal") {
	    bfTitle <- "BF\u2080\u2081"
	  } else if (hypothesis == "greater") {
	    bfTitle <- "BF\u2080\u208A"
	  } else {
	    bfTitle <- "BF\u2080\u208B"
	  }
	}
  return(bfTitle)
}

.ttestBayesianEmptyObject <- function(options, derivedOptions, ttestState = NULL) {

  # construct a t-test object based on supplied options
  nvar <- length(options[["variables"]])
  isPaired <- derivedOptions[["ttestType"]] == "paired"

  # exception for paired t-test
  if (nvar == 1L && options[["variables"]] == " - " && isPaired)
    nvar <- 0L

  if (!is.null(ttestState)) {
    obj <- ttestState
  } else {
    # TODO make this a data.frame so that the state can be efficiently reused
    obj <- list(
      status          = rep("ok", nvar),
      BF10post        = numeric(nvar),
      tValue          = rep(NA, nvar),
      n1              = rep(NA, nvar),
      n2              = rep(NA, nvar),
      plottingError   = vector("list", nvar),
      errorFootnotes  = rep("no", nvar),
      delta           = vector("list", nvar)
    )

  }
  
  obj[["BFH1H0"]]          <- !options[["bayesFactorType"]] == "BF01"
  obj[["grouping"]]        <- options[["groupingVariable"]]
  obj[["paired"]]          <- isPaired
  obj[["analysis"]]        <- derivedOptions[["ttestType"]]
  obj[["derivedOptions"]]  <- derivedOptions
  obj[["bayesFactorType"]] <- options[["bayesFactorType"]]

  if (nvar > 0L) {
    idx <- lengths(obj) == nvar
    # so we can index everything by name
    for (i in which(idx)) {
      names(obj[[i]]) <- derivedOptions[["variables"]]
    }
  }

  if (obj[["analysis"]] != "independent")
    obj[["n2"]] <- NULL

  return(obj)

}

.ttestBayesianCreateTtestRows <- function(dependents, options, derivedOptions, ttestState) {

  # create empty object for the table
  if (length(dependents) == 0L)
    return(NULL)

  ttestRows <- data.frame(row.names = dependents)
  if (is.null(options[["pairs"]])) {
      ttestRows[, "variable"] <- dependents
  } else {
    ttestRows[, "separator"] <- "-"
    nms <- unlist(options[["pairs"]])
    ttestRows[, "variable1"] <- nms[seq(1, length(nms), 2)]
    ttestRows[, "variable2"] <- nms[seq(2, length(nms), 2)]
  }
  ttestRows[,  c("BF", "error")] <- NA_real_
  if (isTRUE(derivedOptions[["wilcoxTest"]]))
    ttestRows[, "rHat"] <- NA_real_

  stateTtestRows <- ttestState[["ttestRows"]]
  if (!is.null(stateTtestRows)) {
    # fill table with data computed before
    idx <- match(ttestRows[["variable"]], stateTtestRows[["variable"]], nomatch = 0L)
    ttestRows[idx != 0, ] <- stateTtestRows[idx, ]

    # ensure that BF type is correct (e.g., BF01 to BF10/ log(BF01))
    ttestRows[["BF"]] <-
      .recodeBFtype(bfOld     = ttestRows[["BF"]],
                    newBFtype = options[["bayesFactorType"]],
                    oldBFtype = ttestState[["bayesFactorType"]]
      )
  }
  return(ttestRows)
}

.ttestBayesianSetupWilcoxProgressBar <- function(nvar, ttestState, noSamples) {
  # all variables minus the ones we sampled before
  todo <- nvar
  if (!is.null(ttestState[["delta"]]))
    todo <- todo - sum(sapply(ttestState[["delta"]], Negate(is.null)))
  if (todo > 0L) {
    # times 5 since there are 5 chains by default
    startProgressbar(5L * todo * noSamples, 100)
  }
}

# descriptives ----
.ttestBayesianDescriptives <- function(jaspResults, dataset, options, ttestResults, errors) {

  if (!(options[["descriptives"]] || options[["descriptivesPlots"]]))
    return()

  if (is.null(jaspResults[["descriptivesContainer"]])) {
    descriptivesContainer <- createJaspContainer("")
    jaspResults[["descriptivesContainer"]] <- descriptivesContainer
    descriptivesContainer$dependOn(c(
      "groupingVariable", "missingValues", "descriptivesPlotsCredibleInterval", "descriptivesPlots",
      "pairs"
    ))
    descriptivesContainer$position <- 2L
  } else {
    descriptivesContainer <- jaspResults[["descriptivesContainer"]]
  }

  derivedOptions <- ttestResults[["derivedOptions"]]
  dependents     <- derivedOptions[["variables"]]
  grouping       <- options[["groupingVariable"]]
  canDoAnalysis  <- derivedOptions[["ready"]]

  if (options[["descriptives"]]) {
    if (is.null(descriptivesContainer[["table"]])) {
      descriptivesTable <- createJaspTable(title = "Descriptives")
      descriptivesTable$dependOn(c("descriptives", "variables", "pairs"))
      descriptivesTable$position <- 1L

      .ttestBayesianDescriptivesTable(
        descriptives           = descriptivesTable,
        dataset                = dataset,
        dependents             = dependents,
        grouping               = grouping,
        CRI                    = options[["descriptivesPlotsCredibleInterval"]],
        canRun                 = canDoAnalysis,
        pairs                  = options[["pairs"]]
      )
      descriptivesContainer[["table"]] <- descriptivesTable
    }
  }

  if (options[["descriptivesPlots"]]) {
    if (is.null(descriptivesContainer[["plots"]])) {

      descriptivesPlots <- createJaspContainer(title = "Descriptives Plots")
      descriptivesPlots$position <- 2L
      descriptivesContainer[["plots"]] <- descriptivesPlots
      runDescriptives <- TRUE

    } else {
      descriptivesPlots <- descriptivesContainer[["plots"]]
      runDescriptives   <- derivedOptions[["anyNewVariables"]]
    }

    if (runDescriptives) {
      .ttestBayesianDescriptivesPlots(
        descriptivePlots = descriptivesPlots,
        dataset          = dataset,
        dependents       = dependents,
        errors           = errors,
        grouping         = grouping,
        CRI              = options[["descriptivesPlotsCredibleInterval"]],
        canRun           = canDoAnalysis,
        testValueOpt     = options[["testValue"]],
        pairs            = derivedOptions[["pairs"]]
      )
    }
  }
  return()
}

.ttestBayesianDescriptivesTable <- function(descriptives, dataset, dependents,
                                       grouping = NULL, CRI = NULL, canRun = FALSE,
                                       dependencies = NULL, stateDescriptivesTable = NULL,
                                       pairs = NULL) {

  hasGrouping <- !is.null(grouping)
	hasCRI <- !is.null(CRI)
	if (!is.null(pairs)) {
	  dependents <- unique(unlist(pairs))
	  dependents <- dependents[dependents != ""]
	}

	descriptives$addColumnInfo(name = "variable", title = "",      type = "string", combine = TRUE)
	if (hasGrouping)
	  descriptives$addColumnInfo(name = "group",    title = "Group", type = "string")
	descriptives$addColumnInfo  (name = "N",        title = "N",     type = "number")
	descriptives$addColumnInfo  (name = "mean",     title = "Mean",  type = "number")
	descriptives$addColumnInfo  (name = "sd",       title = "SD",    type = "number")
	descriptives$addColumnInfo  (name = "se",       title = "SE",    type = "number")

	if (hasCRI) {
		interval <- 100 * CRI
		title <- paste0(interval, "% Credible Interval")
		descriptives$addColumnInfo(name = "lowerCI", type = "number", format = "sf:4;dp:3", title = "Lower", overtitle = title)
		descriptives$addColumnInfo(name = "upperCI", type = "number", format = "sf:4;dp:3", title = "Upper", overtitle = title)
	}

	nvar <- length(dependents)
	if (nvar == 0) dependents <- "."

	if (nvar == 0 || nrow(dataset) == 0 || !canRun) {

		if (hasGrouping) {

			tmp <- rep(dependents, each = 2)
			tmp[seq(2, length(tmp), 2)] <- ""
			dat <- data.frame(variable = tmp)

		} else {

			dat <- data.frame(variable = dependents)

		}
		descriptives$setData(dat)

	} else {
		if (hasGrouping) {

			levels <- levels(dataset[[ .v(grouping) ]])
			nlevels <- length(levels)
			groupingData <- dataset[[.v(grouping)]]
		} else {
			levels <- NULL
			nlevels <- 1
			groupingData <- NULL

		}

		for (var in dependents) {
			if (is.null(stateDescriptivesTable[[var]])) {
				for (i in seq_len(nlevels)) {

					if (hasGrouping) {
						level <- levels[i]
						groupData <- dataset[groupingData == level, .v(var)]
					} else {
						groupData <- dataset[[.v(var)]]
					}
					groupDataOm <- groupData[!is.na(groupData)]

					if (class(groupDataOm) != "factor") {

						posteriorSummary <- .posteriorSummaryGroupMean(variable=groupDataOm,
																													 descriptivesPlotsCredibleInterval=CRI)
						ciLower <- .clean(posteriorSummary[["ciLower"]])
						ciUpper <- .clean(posteriorSummary[["ciUpper"]])

						n <- .clean(length(groupDataOm))
						mean <- .clean(mean(groupDataOm))
						std <- .clean(sd(groupDataOm))
						sem <- .clean(sd(groupDataOm) / sqrt(length(groupDataOm)))

						row <- list(variable = var,
												N = n, mean = mean, sd = std, se = sem)

						if (hasGrouping)
							row[["group"]] <- level

						if (hasCRI)
							row[c("lowerCI", "upperCI")] <- list(ciLower, ciUpper)

					} else {

						n <- .clean(length(groupDataOm))
						row <- list(variable = var, N = n,
												mean = "", sd = "", se = "")

						if (hasGrouping)
							row[["group"]] <- ""
						if (hasCRI)
							row[c("lowerCI", "upperCI")] <- list("", "")
					}

					descriptives$addRows(row)
					stateDescriptivesTable[[var]][[i]] <- row

				}
			} else { # reuse state
				for (i in seq_len(nlevels)) {
					descriptives$addRows(stateDescriptivesTable[[var]][[i]])
				}
			}
		}
	}

	return()
}

.ttestBayesianDescriptivesPlots <- function(descriptivePlots, dataset, dependents, errors,
                                            grouping = NULL, CRI = .95, canRun = FALSE,
                                            testValueOpt = NULL, pairs = NULL) {
  # just pass t-test type?
  hasGrouping <- !is.null(grouping)
  paired <- !is.null(pairs)

  if (hasGrouping) {
    groupingData <- dataset[[.v(grouping)]]
    levels   <- base::levels(dataset[[.v(grouping)]])
    canDoDescriptives <- length(dependents) >= 1 && !(is.null(grouping) || grouping == "")
  } else if (paired) {
    grouping <- "group"
    groupingData <- factor(rep(0:1, each = nrow(dataset)))
    canDoDescriptives <- length(pairs) >= 1
  } else {
    canDoDescriptives <- length(dependents) >= 1
  }

  for (var in dependents) {

    if (is.null(descriptivePlots[[var]])) {

      plot <- createJaspPlot(title = var, width = 530, height = 400)
      if (canDoDescriptives) {
        if (isFALSE(errors[[var]])) {

          if (paired) {
            pair <- pairs[[var]]
            levels <- c(pair[[1]], pair[[2]])

            dat <- c(dataset[[.v(pair[[1]])]], dataset[[.v(pair[[2]])]])
            dat <- data.frame(
              value = dat,
              group = groupingData
            )
            dat <- dat[!is.na(dat[[1]]), ]

          } else {
            idxC <- !is.na(dataset[[.v(var)]])
            dat <- dataset[idxC, .v(c(var, grouping))]
          }

          obj <- try(.ttestBayesianPlotKGroupMeans(
            data         = dat,
            var          = var,
            grouping     = grouping,
            paired       = paired,
            groupNames   = levels,
            CRI          = CRI,
            testValueOpt = testValueOpt
          ))
          if (isTryError(obj)) {
            plot$setError(.extractErrorMessage(obj))
          } else {
            plot$plotObject <- obj
          }
        } else {
          plot$setError(errors[[var]][["message"]])
        }
      }
      if (paired) {
        plot$dependOn(optionContainsValue = list(pairs = unname(pairs[var])))
      } else {
        plot$dependOn(optionContainsValue = list(variables = var))
      }
      descriptivePlots[[var]] <- plot
    }
  }
  return()
}

# inferential plots ----
.ttestBayesianInferentialPlots <- function(jaspResults, dataset, options, ttestResults, errors) {

  opts <- c("plotPriorAndPosterior", "plotBayesFactorRobustness", "plotSequentialAnalysis")
  if (!any(unlist(options[opts])))
    return()

  if (ttestResults[["paired"]]) {
    dependents <- ttestResults[["derivedOptions"]][["variables"]]
    grouping   <- NULL
    pairs      <- ttestResults[["derivedOptions"]][["pairs"]]
  } else {
    dependents <- unlist(options[["variables"]])
    grouping   <- options[["groupingVariable"]]
    pairs <- NULL
  }

  if (is.null(jaspResults[["ttestContainer"]][["inferentialPlots"]])) {
    inferentialPlotsCollection <- createJaspContainer("Inferential Plots")
    inferentialPlotsCollection$dependOn(c("hypothesis", "testStatistic"))
    jaspResults[["ttestContainer"]][["inferentialPlots"]] <- inferentialPlotsCollection
  } else {
    inferentialPlotsCollection <- jaspResults[["ttestContainer"]][["inferentialPlots"]]
  }
  
  # for the independent samples t-test, some plots cannot be shown for the Wilcoxon test
  if (is.null(options[["testStatistic"]]) || options[["testStatistic"]] == "Student") {
    whichPlotTitles <- which(unlist(options[unlist(opts)]))
  } else { # Wilcoxon
    # only show prior and posterior plot (even if others are checked)
    whichPlotTitles <- which(unlist(options[unlist(opts[1L])]))
	# ensure prior is Cauchy, not informed (people can hack this in and it would destroy the results)
	options[["effectSizeStandardized"]]        <- "default"
	options[["defaultStandardizedEffectSize"]] <- "cauchy"
  }
  

  # create all empty plots and containers before filling them in one-by-one, to avoid the screen from flashing
  dependencies <- list(
    c("plotPriorAndPosterior",     "plotPriorAndPosteriorAdditionalInfo"),
    c("plotBayesFactorRobustness", "plotBayesFactorRobustnessAdditionalInfo", "bayesFactorType"),
    c("plotSequentialAnalysis",    "plotSequentialAnalysisRobustness",        "bayesFactorType")
  )

  plotTitles <- c("Prior and Posterior", "Bayes Factor Robustness Check", "Sequential Analysis")
  jaspTitles <- c("plotPriorAndPosterior", "plotRobustness", "plotSequential")
  for (var in dependents) { # was there a container for these plots for this variable?
    if (is.null(inferentialPlotsCollection[[var]])) {
      container <- createJaspContainer(title = var)
      inferentialPlotsCollection[[var]] <- container
      if (ttestResults[["paired"]]) {
        container$dependOn(optionContainsValue = list(pairs = unname(pairs[var])))
      } else {
        container$dependOn(optionContainsValue = list(variables = var))
      }
    } else {
      container <- inferentialPlotsCollection[[var]]
    }

    for (i in whichPlotTitles) { # add empty plot in the container for this variable
      if (is.null(container[[jaspTitles[i]]])) {
        plot <- createJaspPlot(title = plotTitles[i], width = 530, height = 400)
        plot$dependOn(options = dependencies[[i]])
        plot$position <- i
        container[[jaspTitles[i]]] <- plot
      }
    }
  }

  if (!ttestResults[["derivedOptions"]][["ready"]])
    return()

  if (options[["plotPriorAndPosterior"]]) {
		.ttestBayesianPlotPriorAndPosterior(
  		collection             = inferentialPlotsCollection,
  		dependents             = dependents,
  		errors                 = errors,
  		pairs                  = pairs,
  		t                      = ttestResults[["tValue"]],
  		n1                     = ttestResults[["n1"]],
  		n2                     = ttestResults[["n2"]],
  		BF                     = ttestResults[["BF10post"]],
  		BFH1H0                 = ttestResults[["BFH1H0"]],
  		delta                  = ttestResults[["delta"]],
  		plottingError          = ttestResults[["plottingError"]],
  		paired                 = ttestResults[["paired"]],
  		oneSided               = ttestResults[["derivedOptions"]][["oneSided"]],
  		wilcoxTest             = ttestResults[["derivedOptions"]][["wilcoxTest"]],
  		rscale                 = options[["priorWidth"]],
  		addInformation         = options[["plotPriorAndPosteriorAdditionalInfo"]],
  		options                = options
  	)
  }

  if (options[["plotBayesFactorRobustness"]]) {
  	.ttestBayesianPlotRobustness(
      collection             = inferentialPlotsCollection,
      dependents             = dependents,
      errors                 = errors,
      dataset                = dataset,
      grouping               = grouping,
      pairs                  = pairs,
      BF10post               = ttestResults[["BF10post"]],
      BFH1H0                 = ttestResults[["BFH1H0"]],
      paired                 = ttestResults[["paired"]],
      plottingError          = ttestResults[["plottingError"]],
      oneSided               = ttestResults[["derivedOptions"]][["oneSided"]],
      nullInterval           = ttestResults[["derivedOptions"]][["nullInterval"]],
      rscale                 = options[["priorWidth"]],
      additionalInformation  = options[["plotBayesFactorRobustnessAdditionalInfo"]],
      effectSizeStandardized = options[["effectSizeStandardized"]],
      options                = options
  	)
  }

  if (options[["plotSequentialAnalysis"]]) {
  	.ttestBayesianPlotSequential(
  		collection             = inferentialPlotsCollection,
  		dependents             = dependents,
  		errors                 = errors,
  		dataset                = dataset,
  		grouping               = grouping,
  		pairs                  = pairs,
  		BF10post               = ttestResults[["BF10post"]],
  		BFH1H0                 = ttestResults[["BFH1H0"]],
  		paired                 = ttestResults[["paired"]],
  		plottingError          = ttestResults[["plottingError"]],
  		oneSided               = ttestResults[["derivedOptions"]][["oneSided"]],
  		nullInterval           = ttestResults[["derivedOptions"]][["nullInterval"]],
  		rscale                 = options[["priorWidth"]],
  		effectSizeStandardized = options[["effectSizeStandardized"]],
  		plotDifferentPriors    = options[["plotSequentialAnalysisRobustness"]],
  		options                = options
  	)
  }
}

.ttestBayesianPlotPriorAndPosterior <- function(collection, dependents, errors,
                                                tValue, n1, n2 = NULL,
                                                BF, BFH1H0, oneSided = FALSE,
                                                rscale = "medium", delta = NULL,
                                                addInformation = TRUE,
                                                plottingError = NULL,
                                                paired = FALSE, pairs = NULL,
                                                wilcoxTest = FALSE, options, ...) {

  for (var in dependents) {
    if (is.null(collection[[var]][["plotPriorAndPosterior"]]$plotObject)) {

      plot <- collection[[var]][["plotPriorAndPosterior"]]
      plot$status <- "running"

      if (isFALSE(errors[[var]]) && is.null(plottingError[[var]])) {

        obj <- try(.plotPriorPosterior(
          t                      = tValue[var],
          n1                     = n1[var],
          n2                     = n2[var],
          paired                 = paired,
          oneSided               = oneSided,
          BF                     = BF[var],
          BFH1H0                 = BFH1H0,
          rscale                 = rscale,
          delta                  = delta[[var]],
          addInformation         = addInformation,
          wilcoxTest             = wilcoxTest,
          options                = options,
          ...
        ))

        if (isTryError(obj)) {
          plot$setError(.extractErrorMessage(obj))
        } else {
          plot$plotObject <- obj
        }
      } else {
        if (!isFALSE(errors[[var]])) {
          err <- errors[[var]][["message"]]
        } else {
          err <- plottingError[[var]]
        }
        plot$setError(err)
      }
    }
  }
}

.ttestBayesianPlotRobustness <- function(collection, dependents, errors, dataset,
                                         grouping = NULL, BF10post, BFH1H0,
                                         rscale = "medium", paired = FALSE,
                                         plottingError = NULL,
                                         oneSided = FALSE, nullInterval = c(-Inf, Inf),
                                         additionalInformation = TRUE,
                                         effectSizeStandardized, pairs = NULL,
                                         options, ...) {
  hasGrouping <- !is.null(grouping)
  if (hasGrouping) {
    levels <- levels(dataset[[.v(grouping)]])
    g1 <- levels[1]
    g2 <- levels[2]
    idxG1 <- dataset[[.v(grouping)]] == g1
    idxG2 <- dataset[[.v(grouping)]] == g2
  } else {
    g1 <- NULL
    g2 <- NULL
    group2 <- NULL
  }

  for (var in dependents) {
    if (is.null(collection[[var]][["plotRobustness"]]$plotObject)) {
      plot <- collection[[var]][["plotRobustness"]]
      if (effectSizeStandardized == "informative") {
        plot$setError("Bayes factor robustness check plot currently not supported for informed prior.")
      } else if (isFALSE(errors[[var]]) && is.null(plottingError[[var]])) {

        plot$status <- "running"

        if (paired) {
          pair <- pairs[[var]]
          group1 <- dataset[, .v(pair[[1]])]
          group2 <- dataset[, .v(pair[[2]])]
          idxC <- !(is.na(group1) | is.na(group2))
          group1 <- group1[idxC]
          group2 <- group2[idxC]
        } else {
          idxC <- !is.na(dataset[[.v(var)]])
          if (hasGrouping) {
            group1 <- dataset[idxG1 & idxC, .v(var)]
            group2 <- dataset[idxG2 & idxC, .v(var)]
          } else {
            group1 <- dataset[idxC, .v(var)]
            group1 <- group1 - options[["testValue"]]
          }
        }

        obj <- try(.plotBF.robustnessCheck.ttest2(
          x                     = group1,
          y                     = group2,
          BF10post              = BF10post[var],
          paired                = paired,
          oneSided              = oneSided,
          nullInterval          = nullInterval,
          rscale                = rscale,
          BFH1H0                = BFH1H0,
          additionalInformation = additionalInformation,
          ...
        ))
        if (isTryError(obj)) {
          plot$setError(.extractErrorMessage(obj))
        } else {
          plot$plotObject <- obj
        }
      } else {
        if (!isFALSE(errors[[var]])) {
          err <- errors[[var]][["message"]]
        } else {
          err <- plottingError[[var]]
        }
        plot$setError(err)
      }
    }
  }
}

.ttestBayesianPlotSequential <- function(collection, dependents, errors, dataset,
                                         grouping = NULL, BF10post, BFH1H0,
                                         rscale = "medium", paired = FALSE,
                                         plottingError = NULL,
                                         oneSided = FALSE, nullInterval = c(-Inf, Inf),
                                         plotDifferentPriors = FALSE,
                                         effectSizeStandardized,
                                         testValue = NULL, pairs = NULL,
                                         options, ...) {
  hasGrouping <- !is.null(grouping)
  if (hasGrouping) {
    levels <- levels(dataset[[.v(grouping)]])
    g1 <- levels[1L]
    g2 <- levels[2L]
    idxG1 <- dataset[[.v(grouping)]] == g1
	  idxG2 <- dataset[[.v(grouping)]] == g2
  } else {
    g1 <- NULL
    g2 <- NULL
    group2 <- NULL
    subDataSet <- NULL
  }

  for (var in dependents) {
    if (is.null(collection[[var]][["plotSequential"]]$plotObject)) {
      plot <- collection[[var]][["plotSequential"]]

      if (effectSizeStandardized == "informative") {
        plot$setError("Sequential analysis robustness check plot currently not supported for informed prior.")
      } else if (isFALSE(errors[[var]]) && is.null(plottingError[[var]])) {

        plot$status <- "running"

        if (paired) {
          pair <- pairs[[var]]
          group1 <- dataset[, .v(pair[[1L]])]
          group2 <- dataset[, .v(pair[[2L]])]
          idxC <- !(is.na(group1) | is.na(group2))
          group1 <- group1[idxC]
          group2 <- group2[idxC]
        } else {
          idxC <- !is.na(dataset[[.v(var)]])
          if (hasGrouping) {
            group1 <- dataset[idxG1 & idxC, .v(var)]
            group2 <- dataset[idxG2 & idxC, .v(var)]
            subDataSet <- dataset[idxC, .v(c(var, grouping))]
          } else {
            group1 <- dataset[idxC, .v(var)]
            group1 <- group1 - options$testValue
          }
        }

        obj <- try(.plotSequentialBF.ttest2(
          x                   = group1,
          y                   = group2,
          oneSided            = oneSided,
          rscale              = rscale,
          BFH1H0              = BFH1H0,
          BF10post            = BF10post[var],
          paired              = paired,
          plotDifferentPriors = plotDifferentPriors,
          subDataSet          = subDataSet,
          level1              = g1,
          level2              = g2,
          nullInterval        = nullInterval,
          options             = options,
          ...
        ))
        if (isTryError(obj)) {
          plot$setError(.extractErrorMessage(obj))
        } else {
          plot$plotObject <- obj
        }
      } else {
        if (!isFALSE(errors[[var]])) {
          err <- errors[[var]][["message"]]
        } else {
          err <- plottingError[[var]]
        }
        plot$setError(err)
      }
    }
  }
}

# plot functions  ----
.addPlotToJaspObj0 <- function(title, var, obj = NULL, errors = "",  w = 530, h = 400) {

	# convenience function
	if (is.null(obj)) {
		plot <- createJaspPlot(title = title, width = w, height = h,
													 error = "badData", errorMessage = errors)
	} else if (identical(obj, "empty")) {
		plot <- createJaspPlot(title = title, width = w, height = h)
	} else if (isTryError(obj)) {
		plot <- createJaspPlot(title = title, width = w, height = h,
													 error = "badData", errorMessage = .extractErrorMessage(obj))
	} else {
		plot <- createJaspPlot(title = title, width = w, height = h, plot = obj)
	}
	return(plot)
}

.ttestBayesianPlotKGroupMeans <- function(data, var, grouping = NULL,
																					groupNames = NULL, CRI = .95,
																					testValueOpt = NULL, paired = FALSE) {

	# to be remade in jaspGraphs
	hasGrouping <- !is.null(grouping)
	if (hasGrouping) {

		summaryStat <- tapply(data[[1L]], data[[2L]], function(x) {
			.posteriorSummaryGroupMean(variable = x, descriptivesPlotsCredibleInterval = CRI)
		})
		summaryStat <- do.call(rbind.data.frame, summaryStat)
		summaryStat$groupingVariable <- factor(groupNames)
		mapping <- ggplot2::aes(x=groupingVariable, y=median, group=group)

	} else {

		summaryStat <- as.data.frame(.posteriorSummaryGroupMean(data, descriptivesPlotsCredibleInterval = CRI))
		summaryStat$groupingVariable <- var
		mapping <- ggplot2::aes(x=groupingVariable, y=median, group=group)
		testValue <- data.frame("testValue" = testValueOpt) # default zero

	}

	if (hasGrouping && !paired) {
		ylab <- ggplot2::ylab(var)
		xlab <- ggplot2::xlab(grouping)
	} else {
	  ylab <- ggplot2::ylab(NULL)
	  xlab <- ggplot2::xlab(NULL)
	}

	summaryStat$group <- 1

	pd <- ggplot2::position_dodge(.2)

	p <-	ggplot2::ggplot(summaryStat, mapping = mapping) +
			ggplot2::geom_errorbar(ggplot2::aes(ymin=ciLower, ymax=ciUpper), colour="black", width=.2, position=pd) +
			ggplot2::geom_line(position=pd, size = .7) +
			ggplot2::geom_point(position=pd, size=4) +
			xlab + ylab +
			ggplot2::theme_bw() +
			ggplot2::theme(panel.grid.minor=ggplot2::element_blank(), plot.title = ggplot2::element_text(size=18),
				panel.grid.major=ggplot2::element_blank(),
				axis.title.x = ggplot2::element_text(size=18,vjust=-.2), axis.title.y = ggplot2::element_text(size=18,vjust=-1),
				axis.text.x = ggplot2::element_text(size=15), axis.text.y = ggplot2::element_text(size=15),
				panel.background = ggplot2::element_rect(fill = 'transparent', colour = NA),
				plot.background = ggplot2::element_rect(fill = 'transparent', colour = NA),
				legend.background = ggplot2::element_rect(fill = 'transparent', colour = NA),
				panel.border = ggplot2::element_blank(), axis.line = ggplot2::element_blank(),
				legend.key = ggplot2::element_blank(),
				legend.title = ggplot2::element_text(size=12),
				legend.text = ggplot2::element_text(size = 12),
				axis.ticks = ggplot2::element_line(size = 0.5),
				# axis.ticks.margin = grid::unit(1,"mm"),
				axis.ticks.length = grid::unit(3, "mm"),
				plot.margin = grid::unit(c(.5,0,.5,.5), "cm")) +
				.base_breaks_y4(summaryStat, testValueOpt) +
				.base_breaks_x(summaryStat$groupingVariable)

	if (!is.null(testValueOpt))
		p <- p + ggplot2::geom_hline(data = testValue, ggplot2::aes(yintercept=testValue), linetype="dashed")

	return(p)

}

.base_breaks_y4 <- function(x, testValue) {

  values <- c(testValue, x$ciLower, x$ciUpper)
  ci.pos <- c(min(values), max(values))
  b <- pretty(ci.pos)
  d <- data.frame(x=-Inf, xend=-Inf, y=min(b), yend=max(b))
  list(ggplot2::geom_segment(data=d, ggplot2::aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE, size = 1),
       ggplot2::scale_y_continuous(breaks=c(min(b), testValue, max(b))))
}

.base_breaks_x <- function(x) {

	b <- unique(as.numeric(x))
	d <- data.frame(y=-Inf, yend=-Inf, x=min(b), xend=max(b))
	list(ggplot2::geom_segment(data=d, ggplot2::aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE, size = 1))

}

.ttestBayesianGetBFnamePlots <- function(BFH1H0, nullInterval) {

  if (BFH1H0) {
	  if (identical(nullInterval, c(-Inf, Inf))) {
	    bfTitle <- "BF[1][0]"
	  } else if (identical(nullInterval, c(0, Inf))) {
	    bfTitle <- "BF['+'][0]"
	  } else {
	    bfTitle <- "BF['-'][0]"
	  }
	} else {
	  if (identical(nullInterval, c(-Inf, Inf))) {
	    bfTitle <- "BF[0][1]"
	  } else if (identical(nullInterval, c(0, Inf))) {
	    bfTitle <- "BF[0]['+']"
	  } else {
	    bfTitle <- "BF[0]['-']"
	  }
	}
  return(bfTitle)
}

.ttestBayesianGetRScale <- function(rscale) {

  if (is.numeric(rscale)) {
		return(rscale)
	} else if (is.character(rscale)) {
	  return(switch(
	    rscale,
	    "medium"    = sqrt(2) / 2,
	    "wide"      = 1.0,
	    "ultrawide" = sqrt(2)
	  ))
	} else {
		.quitAnalysis(message = sprintf("Expected numeric or character rscale but got %s.", class(rscale)))
	}
}

.plotBF.robustnessCheck.ttest2 <- function(
  x = NULL, y = NULL, paired = FALSE, BF10post, nullInterval, formula = NULL, data = NULL, rscale = 1, oneSided = FALSE,
  BFH1H0 = TRUE, additionalInformation = FALSE) {

  r <- .ttestBayesianGetRScale(rscale)

  if (r > 1.5) {
    rValues <- seq(0.0005, 2.0, length.out = 535)
  } else {
    rValues <- seq(0.0005, 1.5, length.out = 400)
  }

  # BF10
  BF10 <- vector("numeric", length(rValues))
  if (!isFALSE(oneSided)) {
    for (i in seq_along(rValues)) {
      BF10[i] <- .oneSidedTtestBFRichard(x = x, y = y, paired = paired, oneSided = oneSided, r = rValues[i])
    }
  } else {
    for (i in seq_along(rValues)) {
      BF <- BayesFactor::ttestBF(x = x, y = y, paired = paired, nullInterval = nullInterval, rscale = rValues[i])
      BF10[i] <- BayesFactor::extractBF(BF, logbf = FALSE, onlybf = FALSE)[1, "bf"]
    }
  }

  # maximum BF value
  idx <- which.max(BF10)
  maxBF10 <- BF10[idx]
  maxBFrVal <- rValues[idx]

  if (isFALSE(oneSided)) {

    # BF10 "medium" prior
    BF10m     <- BayesFactor::ttestBF(x = x, y = y, paired = paired, nullInterval = nullInterval, rscale = "medium")
    BF10w     <- BayesFactor::ttestBF(x = x, y = y, paired = paired, nullInterval = nullInterval, rscale = "wide")
    BF10ultra <- BayesFactor::ttestBF(x = x, y = y, paired = paired, nullInterval = nullInterval, rscale = "ultrawide")

    BF10m     <- BayesFactor::extractBF(BF10m,     logbf = FALSE, onlybf = FALSE)[1L, "bf"]
    BF10w     <- BayesFactor::extractBF(BF10w,     logbf = FALSE, onlybf = FALSE)[1L, "bf"]
    BF10ultra <- BayesFactor::extractBF(BF10ultra, logbf = FALSE, onlybf = FALSE)[1L, "bf"]

  } else {

    # BF10 "medium" prior
    BF10m     <- .oneSidedTtestBFRichard(x = x, y = y, paired = paired, oneSided = oneSided, r = "medium")
    BF10w     <- .oneSidedTtestBFRichard(x = x, y = y, paired = paired, oneSided = oneSided, r = "wide")
    BF10ultra <- .oneSidedTtestBFRichard(x = x, y = y, paired = paired, oneSided = oneSided, r = "ultrawide")
  }

  # BF10 user prior
  # BF10user <- if (BFH1H0) BF10post else 1 / BF10post

  dfLines <- data.frame(
    x = rValues,
    y = log(BF10)
  )
  
  BF10user <- BF10post
  if (BFH1H0) {
    bfType <- "BF10"
  } else {
    bfType <- "BF01"
    dfLines$y <- -dfLines$y
    BF10user  <- 1 / BF10user
    maxBF10   <- 1 / maxBF10
    BF10w     <- 1 / BF10w
    BF10ultra <- 1 / BF10ultra
  }
  
  BFsubscript <- .ttestBayesianGetBFnamePlots(BFH1H0, nullInterval)

  # to mimic old behavior  
  # getBFSubscript <- function(x) .ttestBayesianGetBFnamePlots(x <= 1, nullInterval)
  # getBFValue     <- function(x) if (x <= 1) 1 / x else x
  dfPoints <- data.frame(
    x = c(maxBFrVal, r, 1, sqrt(2)),
    y = log(c(maxBF10, BF10user, BF10w, BF10ultra)),
    g = JASPgraphs::parseThis(c(
      sprintf("paste(max, ~%s, ':',   phantom(phollll), %s, ~at, ~'r'==%s)", BFsubscript, format(maxBF10,   digits = 4), format(maxBFrVal, digits = 4)),
      sprintf("paste(user~prior, ':', phantom(phll[0]), ~%s==%s)",           BFsubscript, format(BF10user,  digits = 4)),
      sprintf("paste(wide~prior, ':', phantom(ph[0][0]), ~%s==%s)",          BFsubscript, format(BF10w,     digits = 4)),
      sprintf("paste(ultrawide~prior, ':', ~%s==%s)",                        BFsubscript, format(BF10ultra, digits = 4))
    )),
    stringsAsFactors = FALSE
  )
  
  hypothesis <- switch(oneSided,
    "right" = "greater",
    "left"  = "smaller",
    "equal"
  )

  plot <- JASPgraphs::PlotRobustnessSequential(
    dfLines      = dfLines,
    dfPoints     = dfPoints,
    pointLegend  = additionalInformation,
    xName        = "Cauchy prior width",
    hypothesis   = hypothesis,
    bfType       = bfType
  )

  return(plot)

}

.plotSequentialBF.ttest2 <- function(
	x = NULL, y = NULL, paired = FALSE, BF10post, formula = NULL, data = NULL, rscale = 1, oneSided = FALSE,
	plotDifferentPriors = FALSE, BFH1H0 = TRUE, dontPlotData = FALSE, level1 = NULL, level2 = NULL,
	subDataSet = NULL, nullInterval = c(-Inf, Inf), options) {

	r <- .ttestBayesianGetRScale(rscale)
  evidenceText <- !plotDifferentPriors
  BFsubscript <- .ttestBayesianGetBFnamePlots(BFH1H0, nullInterval)

  if (is.null(y) || paired) {

    BF10 <- vector("numeric", max(length(x), length(y)))
    BF10w <- vector("numeric", max(length(x), length(y)))
    BF10u <- vector("numeric", max(length(x), length(y)))

    idData <- 1

    if (is.null(y)) {

      ind <- which(x == x[1])
      idData <- sum((ind+1)-(1:(length(ind))) == 1)

    } else {

      idData <- 1

      for (i in 2:(min(c(length(x), length(y))))) {

        previous  <- c(x[i-1], y[i-1])

        if (all(c(x[i], y[i]) == previous)) {

          idData <- idData + 1

        } else if (x[i] == y[i]) {

          idData <- idData + 1

        } else {

          break
        }
      }
    }

    BF10[1:idData] <- 1
    BF10w[1:idData] <- 1
    BF10u[1:idData] <- 1


    if (idData < length(x)) {

      i <- idData + 1

    } else {

      i <- idData

    }

    if (idData < length(y)) {

      j <- idData + 1

    } else {

      j <- idData

    }

    k <- idData + 1


    while ((i <= length(x) | j <= length(y)) & k <= length(BF10)) {

      bfObject <- .generalTtestBF(x = x[1:i], y = y[1:j], paired = paired, oneSided = oneSided, options = options)
      BF10[k] <- bfObject[["bf"]]
      # if (oneSided == FALSE) {
      #
      # 	BF <- BayesFactor::ttestBF(x = x[1:i], y= y[1:j], paired = paired, rscale=r, nullInterval = nullInterval)
      # 	BF10[k] <- BayesFactor::extractBF(BF, logbf = FALSE, onlybf = F)[1, "bf"]
      #
      # } else {
      #
      # 	BF10[k] <- .oneSidedTtestBFRichard(x = x[1:i], y= y[1:j], paired = paired, r=r, oneSided=oneSided)
      # }

      k <- k + 1

      if (i < length(x)) {

        i <- i + 1
      }
      if (j < length(y)) {

        j <- j + 1
      }

      if ( ! .shouldContinue(callback()))
        return()
    }


    BF10 <- BF10[is.finite(BF10)]

    if ( ! .shouldContinue(callback()))
      return()

    if (plotDifferentPriors) {

      if (idData < length(x)) {

        i <- idData + 1

      } else {

        i <- idData

      }

      if (idData < length(y)) {

        j <- idData + 1

      } else {

        j <- idData

      }

      k <- idData + 1


      while ((i <= length(x) | j <= length(y)) & k <= length(BF10u)) {

        if (oneSided == FALSE) {

          BF <- BayesFactor::ttestBF(x = x[1:i], y= y[1:j], paired = paired, rscale="ultrawide", nullInterval = nullInterval)
          BF10u[k] <- BayesFactor::extractBF(BF, logbf = FALSE, onlybf = F)[1, "bf"]

        } else {

          BF10u[k] <- .oneSidedTtestBFRichard(x = x[1:i], y= y[1:j], paired = paired, r="ultrawide", oneSided=oneSided)
        }

        k <- k + 1

        if (i < length(x)) {

          i <- i + 1
        }
        if (j < length(y)) {

          j <- j + 1
        }

        if ( ! .shouldContinue(callback()))
          return()
      }


      BF10u <- BF10u[is.finite(BF10u)]

      if ( ! .shouldContinue(callback()))
        return()


      if (idData < length(x)) {

        i <- idData + 1

      } else {

        i <- idData

      }

      if (idData < length(y)) {

        j <- idData + 1

      } else {

        j <- idData

      }

      k <- idData + 1


      while ((i <= length(x) | j <= length(y)) & k <= length(BF10w)) {

        if (oneSided == FALSE) {

          BF <- BayesFactor::ttestBF(x = x[1:i], y= y[1:j], paired = paired, rscale= "wide", nullInterval = nullInterval)
          BF10w[k] <- BayesFactor::extractBF(BF, logbf = FALSE, onlybf = F)[1, "bf"]

        } else {

          BF10w[k] <- .oneSidedTtestBFRichard(x = x[1:i], y= y[1:j], paired = paired, r="wide", oneSided=oneSided)
        }

        k <- k + 1

        if (i < length(x)) {

          i <- i + 1
        }
        if (j < length(y)) {

          j <- j + 1
        }

        if ( ! .shouldContinue(callback()))
          return()
      }

      BF10w <- BF10w[is.finite(BF10w)]

      if ( ! .shouldContinue(callback()))
        return()

    }

  } else if (!is.null(y) && !paired) {

    idData <- 1

    xx <- numeric()
    yy <- numeric()

    BF10 <- vector("numeric", nrow(subDataSet))
    BF10w <- vector("numeric", nrow(subDataSet))
    BF10u <- vector("numeric", nrow(subDataSet))

    # # this loop is an alternative to calling sd(x) > 0 && sd(y) > 0 during each iteration in the loop below
    # xx <- subDataSet[subDataSet[, 2L] == level1, 1L]
    # yy <- subDataSet[subDataSet[, 2L] == level2, 1L]
    #
    # diffX <- diffY <- TRUE
    # for (i in 2:nrow(subDataSet)) {
    #   diffX <- diffX && (xx[i] != xx[1L])
    #   if (diffX)
    #     break
    # }
    # idx <- i
    # for (i in 2:length(yy)) {
    #   diffY <- diffY && (yy[i] != yy[1L])
    #   if (diffY)
    #     break
    # }
    # idxy <- i
    #
    #
    # sp <- split(sub)
    #
    # iStart <- i
    # BF10[1:iStart] <- 1.0
    #
    # for (i in iStart:nrow(subDataSet)) {
    #   bfObject <- .generalTtestBF(x = subDataSet[1:i, 1L], y = subDataSet[1:i, 2L], paired = paired, oneSided = oneSided, options = options)
    #   BF10[i] <- bfObject[["bf"]]
    # }
    #
    # if (plotDifferentPriors) {
    #
    #   if (oneSided == FALSE) {
    #     for (i in iStart:nrow(subDataSet)) {
    #       BFtmpu <- BayesFactor::ttestBF(x = xx, y = yy, paired = paired, rscale = "ultrawide", nullInterval = nullInterval)
    #       BFtmpw <- BayesFactor::ttestBF(x = xx, y = yy, paired = paired, rscale = "wide",      nullInterval = nullInterval)
    #
    #       BF10u[i] <- BayesFactor::extractBF(BFtmpu, logbf = FALSE, onlybf = FALSE)[1L, "bf"]
    #       BF10w[i] <- BayesFactor::extractBF(BFtmpw, logbf = FALSE, onlybf = FALSE)[1L, "bf"]
    #
    #     }
    #   } else if (oneSided == "right") {
    #     for (i in iStart:nrow(subDataSet)) {
    #       BF10u[i] <- .oneSidedTtestBFRichard(xx, yy, oneSided = oneSided, r = "ultrawide")
    #       BF10w[i] <- .oneSidedTtestBFRichard(xx, yy, oneSided = oneSided, r = "wide")
    #     }
    #   }
    # }

    for (i in seq_len(nrow(subDataSet))) {

      if (subDataSet[i, 2] == level1) {

        xx <- c(xx, subDataSet[i, 1])

      } else if (subDataSet[i, 2] == level2) {

        yy <- c(yy, subDataSet[i, 1])

      }

      if (length(xx) > 1 && length(yy) > 1 && (sd(xx) > 0 || sd(yy) > 0)) {

        bfObject <- .generalTtestBF(x = xx, y = yy, paired = paired, oneSided = oneSided, options = options)
        BF10[i] <- bfObject[["bf"]]
        # if (oneSided == FALSE) {
        #
        # 	BF <- BayesFactor::ttestBF(x = xx, y= yy, paired = paired, rscale= r, nullInterval = nullInterval)
        # 	BF10[i] <- BayesFactor::extractBF(BF, logbf = FALSE, onlybf = F)[1, "bf"]
        #
        # } else if (oneSided == "right") {
        #
        # 	BF10[i] <- .oneSidedTtestBFRichard(xx, yy, oneSided= "right", r=r)
        #
        # } else if (oneSided == "left") {
        #
        # 	BF10[i] <- .oneSidedTtestBFRichard(xx, yy, oneSided= "left", r=r)
        # }

      } else {

        BF10[i] <- 1
      }
    }

    if (plotDifferentPriors) {

      xx <- numeric()
      yy <- numeric()

      for (i in seq_len(nrow(subDataSet))) {

        if (subDataSet[i, 2] == level1) {

          xx <- c(xx, subDataSet[i, 1])

        } else if (subDataSet[i, 2] == level2) {

          yy <- c(yy, subDataSet[i, 1])

        }

        if (length(xx) > 1 && length(yy) > 1 && (sd(xx) > 0 || sd(yy) > 0)) {

          if (oneSided == FALSE) {

            BF <- BayesFactor::ttestBF(x = xx, y= yy, paired = paired, rscale= "ultrawide", nullInterval = nullInterval)
            BF10u[i] <- BayesFactor::extractBF(BF, logbf = FALSE, onlybf = F)[1, "bf"]

          } else if (oneSided == "right") {

            BF10u[i] <- .oneSidedTtestBFRichard(xx, yy, oneSided= "right", r="ultrawide")

          } else if (oneSided == "left") {

            BF10u[i] <- .oneSidedTtestBFRichard(xx, yy, oneSided= "left", r="ultrawide")
          }

        } else {

          BF10u[i] <- 1
        }
      }

      xx <- numeric()
      yy <- numeric()

      for (i in seq_len(nrow(subDataSet))) {

        if (subDataSet[i, 2] == level1) {

          xx <- c(xx, subDataSet[i, 1])

        } else if (subDataSet[i, 2] == level2) {

          yy <- c(yy, subDataSet[i, 1])

        }

        if (length(xx) > 1 && length(yy) > 1 && (sd(xx) > 0 || sd(yy) > 0)) {

          if (oneSided == FALSE) {

            BF <- BayesFactor::ttestBF(x = xx, y= yy, paired = paired, rscale= "wide", nullInterval = nullInterval)
            BF10w[i] <- BayesFactor::extractBF(BF, logbf = FALSE, onlybf = F)[1, "bf"]

          } else if (oneSided == "right") {

            BF10w[i] <- .oneSidedTtestBFRichard(xx, yy, oneSided= "right", r="wide")

          } else if (oneSided == "left") {

            BF10w[i] <- .oneSidedTtestBFRichard(xx, yy, oneSided= "left", r="wide")
          }

        } else {

          BF10w[i] <- 1
        }
      }
    }

  }

  if (plotDifferentPriors) {
    dfLines <- data.frame(
      x = seq_along(BF10),
      y = c(BF10, BF10u, BF10w),
      g = factor(rep(c("user", "ultrawide", "wide"), c(length(BF10), length(BF10u), length(BF10w))),
                 levels = c("user", "wide", "ultrawide"))
    )
  } else {
    dfLines <- data.frame(
      x = seq_along(BF10),
      y = BF10
    )
  }

  hypothesis <- switch(oneSided,
    "right" = "greater",
    "left"  = "smaller",
    "equal"
  )

  BF <- BF10[length(BF10)]
  if (BFH1H0) {
    bftype <- "BF10"
  } else {
    dfLines$y <- 1 / dfLines$y
    bftype <- "BF01"
  }
  dfLines$y <- log(dfLines$y)
  
  plotLineOrPoint <- if (length(BF10) <= 60) "point" else "line"

  plot <- JASPgraphs::PlotRobustnessSequential(
    dfLines         = dfLines,
    xName           = "n",
    BF              = BF,
    bfType          = bftype,
    hypothesis      = hypothesis,
    plotLineOrPoint = plotLineOrPoint
  )
  return(plot)
}

.plotPriorPosterior <- function(
  t = NULL, n1 = NULL, n2 = NULL, paired = FALSE, oneSided = FALSE, BF, BFH1H0, iterations = 10000, rscale = "medium",
  addInformation = TRUE, delta = NULL, nullInterval = c(-Inf, Inf),
  wilcoxTest = FALSE, options = NULL) {

  r <- .ttestBayesianGetRScale(rscale)

  if (BFH1H0) {
    BF10 <- BF
    BF01 <- 1 / BF10
  } else {
    BF01 <- BF
    BF10 <- 1 / BF01
  }

  if (options[["effectSizeStandardized"]] == "informative" && !wilcoxTest) {
    # informative prior
    xlim <- vector("numeric", 2)
    if (options[["informativeStandardizedEffectSize"]] == "cauchy") {
      ci99PlusMedian <- .ciPlusMedian_t(t = t, n1 = n1, n2 = n2, independentSamples = ! paired && !is.null(n2),
                                        prior.location = options[["informativeCauchyLocation"]],
                                        prior.scale = options[["informativeCauchyScale"]],
                                        prior.df = 1, ci = .99, oneSided = oneSided)
      priorLower <- .qShiftedT(.15, parameters = c(options[["informativeCauchyLocation"]],
                                                    options[["informativeCauchyScale"]],
                                                    1), oneSided = oneSided)
      priorUpper <- .qShiftedT(.85, parameters = c(options[["informativeCauchyLocation"]],
                                                    options[["informativeCauchyScale"]],
                                                    1), oneSided = oneSided)
      # compute 95% credible interval & median:
      ci95PlusMedian <- .ciPlusMedian_t(t = t, n1 = n1, n2 = n2, independentSamples = ! paired && !is.null(n2),
                                        prior.location = options[["informativeCauchyLocation"]],
                                        prior.scale = options[["informativeCauchyScale"]],
                                        prior.df = 1, ci = .95, oneSided = oneSided)
      CIlow <- ci95PlusMedian[["ciLower"]]
      CIhigh <- ci95PlusMedian[["ciUpper"]]
      medianPosterior <- ci95PlusMedian[["median"]]

    } else if (options[["informativeStandardizedEffectSize"]] == "t") {
      ci99PlusMedian <- .ciPlusMedian_t(t = t, n1 = n1, n2 = n2, independentSamples = ! paired && !is.null(n2),
                                        prior.location = options[["informativeTLocation"]],
                                        prior.scale = options[["informativeTScale"]],
                                        prior.df = options[["informativeTDf"]],
                                        ci = .99, oneSided = oneSided)
      priorLower <- .qShiftedT(.15, parameters = c(options[["informativeTLocation"]],
                                                    options[["informativeTScale"]],
                                                    options[["informativeTDf"]]),
                               oneSided = oneSided)
      priorUpper <- .qShiftedT(.85, parameters = c(options[["informativeTLocation"]],
                                                    options[["informativeTScale"]],
                                                    options[["informativeTDf"]]),
                               oneSided = oneSided)
      # compute 95% credible interval & median:
      ci95PlusMedian <- .ciPlusMedian_t(t = t, n1 = n1, n2 = n2, independentSamples = ! paired && !is.null(n2),
                                        prior.location = options[["informativeTLocation"]],
                                        prior.scale = options[["informativeTScale"]],
                                        prior.df = options[["informativeTDf"]], ci = .95, oneSided = oneSided)
      CIlow <- ci95PlusMedian[["ciLower"]]
      CIhigh <- ci95PlusMedian[["ciUpper"]]
      medianPosterior <- ci95PlusMedian[["median"]]
    } else if (options[["informativeStandardizedEffectSize"]] == "normal") {
      ci99PlusMedian <- .ciPlusMedian_normal(t = t, n1 = n1, n2 = n2, independentSamples = ! paired && !is.null(n2),
                                             prior.mean = options[["informativeNormalMean"]],
                                             prior.variance = options[["informativeNormalStd"]]^2,
                                             ci = .99, oneSided = oneSided)

      priorAreaSmaller0 <- pnorm(0, options[["informativeNormalMean"]], options[["informativeNormalStd"]])
      if (oneSided == "right") {
        lowerp <- priorAreaSmaller0 + (1 - priorAreaSmaller0)*0.15
        upperp <- priorAreaSmaller0 + (1 - priorAreaSmaller0)*0.85
      } else if (oneSided == "left") {
        lowerp <- priorAreaSmaller0*0.15
        upperp <- priorAreaSmaller0*0.85
      } else {
        lowerp <- 0.15
        upperp <- 0.85
      }

      priorLower <- qnorm(lowerp, options[["informativeNormalMean"]], options[["informativeNormalStd"]])
      priorUpper <- qnorm(upperp, options[["informativeNormalMean"]], options[["informativeNormalStd"]])

      # compute 95% credible interval & median:
      ci95PlusMedian <- .ciPlusMedian_normal(t = t, n1 = n1, n2 = n2, independentSamples = ! paired && !is.null(n2),
                                             prior.mean = options[["informativeNormalMean"]],
                                             prior.variance = options[["informativeNormalStd"]]^2,
                                             ci = .95, oneSided = oneSided)
      CIlow <- ci95PlusMedian[["ciLower"]]
      CIhigh <- ci95PlusMedian[["ciUpper"]]
      medianPosterior <- ci95PlusMedian[["median"]]

    }

    xlim[1] <- min(-2, ci99PlusMedian[["ciLower"]], priorLower)
    xlim[2] <- max(2, ci99PlusMedian[["ciUpper"]], priorUpper)
    xticks <- pretty(xlim)

    ylim <- vector("numeric", 2)

    ylim[1] <- 0
    dmax1 <- optimize(function(x).dposterior_informative(x, t = t, n1 = n1, n2 = n2, paired = paired,
                                                        oneSided = oneSided, options = options),
                     interval = range(xticks),
                     maximum = TRUE)$objective
    dmax2 <- optimize(function(x).dprior_informative(x, oneSided = oneSided, options = options),
                     interval = range(xticks),
                     maximum = TRUE)$objective
    dmax <- max(c(dmax1, dmax2))

    # get maximum density
    # ylim[2] <- stretch * max(c(dmax, dmax2))

    # calculate position of "nice" tick marks and create labels
    # yticks <- pretty(ylim)
    xlabels <- formatC(xticks, 1, format= "f")
    # ylabels <- formatC(yticks, 1, format= "f")

    xxx <- seq(min(xticks), max(xticks), length.out = 1000)
    priorLine <- .dprior_informative(xxx, oneSided = oneSided, options = options)
    posteriorLine <- .dposterior_informative(xxx, t = t, n1 = n1, n2 = n2, paired = paired,
                                             oneSided = oneSided, options = options)

    xlim <- c(min(CIlow,range(xticks)[1]), max(range(xticks)[2], CIhigh))

  } else {
    # sample from delta posterior
    if (!wilcoxTest) {
      bfObject <- BayesFactor::meta.ttestBF(t = t, n1 = n1, n2 = n2, rscale = r)
      # library(BayesFactor)
      samples <- BayesFactor::posterior(model = bfObject, iterations = iterations,
                                        index = 1)
      delta <- samples[,"delta"]
    }

    # fit shifted t distribution
    if (is.null(n2) || paired) {
      deltaHat <- t * sqrt(1 / n1)
      N <- n1
      df <- N - 1
      sigmaStart <- 1 / N
    } else if (!is.null(n2) && !paired) {
      deltaHat <- t * sqrt((n1 + n2) / (n1 * n2))
      df <- n1 + n2 - 2
      sigmaStart <- sqrt((n1 * n2) / (n1 + n2))
    }

    if (sigmaStart < .01) {
      sigmaStart <- .01
    }

    parameters <- try(silent = TRUE,
                      expr = optim(par = c(deltaHat, sigmaStart, df),
                                   fn =.likelihoodShiftedT, data = delta,
                                   method = "BFGS")$par)

    if (isTryError(parameters)) {
      parameters <- try(silent = TRUE,
                        expr = optim(par = c(deltaHat, sigmaStart, df),
                                     fn = .likelihoodShiftedT, data = delta,
                                     method ="Nelder-Mead")$par)
    }

    # set limits plot
    xlim <- vector("numeric", 2)

    if (oneSided == FALSE) {
      xlim[1] <- min(-2, quantile(delta, probs = 0.01)[[1]])
      xlim[2] <- max(2, quantile(delta, probs = 0.99)[[1]])
    }

    if (oneSided == "right") {
      # if (length(delta[delta >= 0]) < 10)
      #	return("Plotting is not possible: To few posterior samples in tested interval")

      xlim[1] <- min(-2, quantile(delta[delta >= 0], probs = 0.01)[[1]])
      xlim[2] <- max(2, quantile(delta[delta >= 0], probs = 0.99)[[1]])

      if (any(is.na(xlim))) {
        xlim[1] <- min(-2, .qShiftedT(0.01, parameters, oneSided="right"))
        xlim[2] <- max(2, .qShiftedT(0.99, parameters, oneSided="right"))
      }
    }

    if (oneSided == "left") {
      #if (length(delta[delta <= 0]) < 10)
      #	return("Plotting is not possible: To few posterior samples in tested interval")

      xlim[1] <- min(-2, quantile(delta[delta <= 0], probs = 0.01)[[1]])
      xlim[2] <- max(2, quantile(delta[delta <= 0], probs = 0.99)[[1]])

      if (any(is.na(xlim))) {
        xlim[1] <-  min(-2, .qShiftedT(0.01, parameters, oneSided="left"))
        xlim[2] <- max(2,.qShiftedT(0.99, parameters, oneSided="left"))
      }
    }

    xticks <- pretty(xlim)

    ylim <- vector("numeric", 2)

    ylim[1] <- 0
    dmax <- optimize(function(x).dposteriorShiftedT(x, parameters = parameters,
                                                    oneSided = oneSided), interval = range(xticks),
                     maximum = TRUE)$objective
    # get maximum density
    # ylim[2] <- max(stretch * .dprior(0,r, oneSided= oneSided), stretch * dmax)

    # calculate position of "nice" tick marks and create labels
    # yticks <- pretty(ylim)
    xlabels <- formatC(xticks, 1, format= "f")
    # ylabels <- formatC(yticks, 1, format= "f")

    # compute 95% credible interval & median:
    if (oneSided == FALSE) {
      CIlow <- quantile(delta, probs = 0.025)[[1]]
      CIhigh <- quantile(delta, probs = 0.975)[[1]]
      medianPosterior <- median(delta)

      if (any(is.na(c(CIlow, CIhigh, medianPosterior)))) {
        CIlow <- .qShiftedT(0.025, parameters, oneSided=FALSE)
        CIhigh <- .qShiftedT(0.975, parameters, oneSided=FALSE)
        medianPosterior <- .qShiftedT(0.5, parameters, oneSided=FALSE)
      }
    }

    if (oneSided == "right") {
      CIlow <- quantile(delta[delta >= 0], probs = 0.025)[[1]]
      CIhigh <- quantile(delta[delta >= 0], probs = 0.975)[[1]]
      medianPosterior <- median(delta[delta >= 0])

      if (any(is.na(c(CIlow, CIhigh, medianPosterior)))) {
        CIlow <- .qShiftedT(0.025, parameters, oneSided="right")
        CIhigh <- .qShiftedT(0.975, parameters, oneSided="right")
        medianPosterior <- .qShiftedT(0.5, parameters, oneSided="right")
      }
    }

    if (oneSided == "left") {
      CIlow <- quantile(delta[delta <= 0], probs = 0.025)[[1]]
      CIhigh <- quantile(delta[delta <= 0], probs = 0.975)[[1]]
      medianPosterior <- median(delta[delta <= 0])

      if (any(is.na(c(CIlow, CIhigh, medianPosterior)))) {
        CIlow <- .qShiftedT(0.025, parameters, oneSided="left")
        CIhigh <- .qShiftedT(0.975, parameters, oneSided="left")
        medianPosterior <- .qShiftedT(0.5, parameters, oneSided="left")
      }
    }

    priorLine <- .dprior(seq(min(xticks), max(xticks),length.out = 1000), r=r, oneSided= oneSided)
    posteriorLine <- .dposteriorShiftedT(x = seq(min(xticks), max(xticks),
                                                 length.out = 1000), parameters = parameters,
                                         oneSided = oneSided)

    # xlim <- c(min(CIlow,range(xticks)[1]), max(range(xticks)[2], CIhigh))

  }

  if ("effectSizeStandardized" %in% names(options) && options$effectSizeStandardized == "informative") {
	  heightPriorAtZero <- .dprior_informative(0, oneSided = oneSided, options = options)
	  heightPosteriorAtZero <- .dposterior_informative(0, t = t, n1 = n1, n2 = n2, paired = paired,
	                                                   oneSided = oneSided, options = options)
	} else {
	  heightPriorAtZero <- .dprior(0, r, oneSided = oneSided)

	  if (oneSided == FALSE) {
	    heightPosteriorAtZero <- .dposteriorShiftedT(0, parameters=parameters, oneSided=oneSided)
	  } else if (oneSided == "right") {
	    posteriorLineLargerZero <- posteriorLine[posteriorLine > 0]
	    heightPosteriorAtZero <- posteriorLineLargerZero[1]
	  } else if (oneSided == "left") {
	    posteriorLineLargerZero <- posteriorLine[posteriorLine > 0]
	    heightPosteriorAtZero <- posteriorLineLargerZero[length(posteriorLineLargerZero)]
	  }
	}

  dfLines <- data.frame(
    x = seq(min(xticks), max(xticks),length.out = 1000L),
    y = c(posteriorLine, priorLine),
    g = factor(rep(c("Posterior", "Prior"), each = 1000L)) # 1000 is apparently a fixed number
  )
  dfPoints <- data.frame(
    x = 0.0,
    y = c(heightPosteriorAtZero, heightPriorAtZero),
    g = c("Posterior", "Prior")
  )
  CRI <- c(CIlow, CIhigh)
  median <- medianPosterior

  BFsubscript <- .ttestBayesianGetBFnamePlots(BFH1H0, nullInterval)
  if (!addInformation) {
    BF01 <- NULL
    median <- NULL
    CRI <- NULL
  }
  
  if (BFH1H0) {
    bfType <- "BF10"
  } else {
    BF <- 1 / BF01
    bfType <- "BF01"
  }
  hypothesis <- switch(oneSided,
    "right" = "greater",
    "left"  = "smaller",
    "equal"
  )

  plot <- JASPgraphs::PlotPriorAndPosterior(
    dfLines    = dfLines,
    dfPoints   = dfPoints,
    BF         = BF,
    CRI        = CRI,
    bfType     = bfType,
    hypothesis = hypothesis,
    median     = median,
    xName      = expression(paste("Effect size", ~delta))
  )
  return(plot)

}


# helper functions ----
.ttestBayesianCheckBFPlot <- function(bf) {
  # test is a Bayes factor is suitable for plotting
  if (is.na(bf)) {
    return("Bayes factor could not be calculated")
  } else if (is.infinite(bf)) {
    return("Bayes factor is infinite")
  } else if (is.infinite(1 / bf)) {
    return("The Bayes factor is too small")
  } else {
    return(NULL)
  }
}
# citations ----
.ttestBayesianCitations <- c(
  "MoreyEtal2015"    = "Morey, R. D., & Rouder, J. N. (2015). BayesFactor (Version 0.9.11-3)[Computer software].",
	"RouderEtal2009"   = "Rouder, J. N., Speckman, P. L., Sun, D., Morey, R. D., & Iverson, G. (2009). Bayesian t tests for accepting and rejecting the null hypothesis. Psychonomic Bulletin & Review, 16, 225–237.",
  "vanDoornEtal2018" = "van Doorn, J., Ly, A., Marsman, M., & Wagenmakers, E. J. (2018). Bayesian Latent-Normal Inference for the Rank Sum Test, the Signed Rank Test, and Spearman's rho. Manuscript submitted for publication and uploaded to arXiv: https://arxiv.org/abs/1703.01805",
  "GronauEtal2017"   = "Gronau, Q. F., Ly, A., & Wagenmakers, E.-J. (2017). Informed Bayesian T-Tests. Manuscript submitted for publication and uploaded to arXiv: https://arxiv.org/abs/1704.02479"
)
