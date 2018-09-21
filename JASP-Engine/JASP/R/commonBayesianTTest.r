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

.ttestBayesianReadData <- function(dataset = NULL,
                                   dependents, grouping = NULL,
                                   missing = c("excludeListwise", "excludeAnalysisByAnalysis")) {

  # read in dataset
  #
  # Arguments:
  #
  # dataset    : Deprecated, should be NULL.
  # dependents : Either a list of variables, or a list with pairs of variables.
  # grouping   : An optional grouping variable. Specify "" to indicate there should be a
  #              grouping variable but none was supplied
  # missing    : Should missing values be excluded listwise? Always removes rows with
  #              missings in the grouping variable (if it was supplied).
  #
  # returns:
  #
  # A dataframe

  missing <- match.arg(missing)

  if (is.list(dependents)) {
    dependents = unique(unlist(dependents))
    dependents <- dependents[dependents != ""]
  }
  if (identical(grouping, ""))
    grouping <- NULL

  if (is.null(dataset)) {
    excl <- grouping
    if (missing == "excludeListwise") {
      excl <- c(excl, dependents)
    }
    dataset <- .readDataSetToEnd(columns.as.numeric=dependents, columns.as.factor=grouping, exclude.na.listwise=excl)
  }
  return(dataset)
}

.ttestBayesianGetErrorsPerVariable <- function(options, dataset) {

	# does all error handling
	#
	# return:
	#
	# - if no analysis can be done (bad data) returns NULL
	# - if an analysis breaking error occurs, calls stop
	# - otherwise returns a named list where each name is an entry of dependents:
	#   |- element is FALSE if no errors are found
	#   |- element is a list with a message if errors are found

	if (options[["canDoAnalysis"]]) {

		dependents <- unlist(options$variables)
		grouping   <- options$groupingVariable
		errors <- list()

		if (!is.null(grouping)) {

		  # analysis breaking errors
		  .hasErrors(dataset, "run", type = c('factorLevels', 'variance'),
		             factorLevels.target = grouping, factorLevels.amount = '!= 2',
		             variance.target = dependents,
		             variance.grouping = grouping,
		             exitAnalysisIfErrors = TRUE)

		  for (var in dependents) {

		    errors[[var]] <- .hasErrors(dataset, perform = "run", message = 'short',
		                                type = c('infinity','observations','variance'),
		                                all.target = var, observations.amount = "< 2", all.grouping = grouping)
		  }

		} else {

	  	  # analysis breaking errors
		  # .hasErrors(dataset, "run", type = 'variance',
		  #            variance.target = dependents, exitAnalysisIfErrors = TRUE)

		  if (is.null(options[["pairs"]])) {
		    for (var in dependents) {
		      errors[[var]] <- .hasErrors(dataset, perform = "run", message = 'short',
		                                  type = c('infinity','observations','variance'),
		                                  all.target = var, observations.amount = "< 2")
		    }
		  } else {

		    for (var in dependents) {
		      pair <- options[["pairs"]][[var]]

		      if (pair[[1]] == "" || pair[[2]] == "") {

		        errors[[var]] <- list(message = sprintf("Please provide another variable."))

		      } else if (identical(pair[[1]], pair[[2]])) {

		        errors[[var]] <- list(message = sprintf("Variables %s and %s are the same!",
		                                                pair[[1]], pair[[2]]))

		      } else {

		        errors[[var]] <- .hasErrors(dataset, perform = "run", message = 'short',
		                                    type = c('infinity','observations','variance'),
		                                    all.target = c(pair[[1]],pair[[2]]), observations.amount = "< 2")
		      }
		    }
		  }
		}

		return(errors)
	} else {
		return(NULL)
	}
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

.ttestBayesianInitOptions <- function(jaspResults, options, analysis = c("independent", "paired", "one-sample")) {

	# initialize options: takes user input and determines:
	#
	# - can any analysis be done?
	# - which variables will need to be computed for plots?
	#
  # in addition:
  #
  # - unifies the hypothesis type (larger, smaller or equal) inside options[["oneSided"]]
  # - defines the dependencies for all objects and adds this to options[["stateKey"]]
  #

  analysis <- match.arg(analysis)
  options[["ttestType"]] <- analysis

  	# dependencies specified here to pass as depends
	defaults <- c(
	  "effectSizeStandardized", "groupingVariable", "hypothesis",
	  "informativeCauchyLocation", "informativeCauchyScale", "informativeNormalMean",
	  "informativeNormalStd", "informativeStandardizedEffectSize",
	  "informativeTDf", "informativeTLocation", "informativeTScale",
	  "missingValues", "priorWidth", "testStatistic", "wilcoxonSamplesNumber"
	)
  options[["stateKey"]] <- list(
    ttestResults           = defaults,
    descriptives           = c("groupingVariable", "missingValues", "descriptivesPlotsCredibleInterval",
                               "descriptivesPlots"),
    priorAndPosteriorPlots = c(defaults, "plotPriorAndPosteriorAdditionalInfo"),
    robustnessPlots        = c(defaults, "plotBayesFactorRobustnessAdditionalInfo"),
    sequentialPlots        = c(defaults, "plotSequentialAnalysisRobustness"),
    delta                  = c("priorWidth", "groupingVariable", "missingValues", "wilcoxonSamplesNumber")
  )

	if (analysis == "independent") {

	  dependents <- unlist(options[["variables"]])

	  options[["canDoAnalysis"]] <- length(dependents) > 0 && options$groupingVariable != ""
	  # options is updated to avoid computing these in multiple functions
	  options[["wilcoxTest"]] <- options$testStatistic ==  "Wilcoxon"
	  options[["oneSided"]] <- switch(options[["hypothesis"]],
	                                  "groupOneGreater" = "right",
	                                  "groupTwoGreater" = "left",
	                                  FALSE
	  )
	  AtTheEndResetPlotRobustnessSequential <- NULL
	  if (options[["wilcoxTest"]]) {

	    # when a user requests robustness/ sequential plots first and then selects wilcoxTest
	    # jasp will still provide these as TRUE, but they shouldn't be.
	    AtTheEndResetPlotRobustnessSequential <- options[c("plotBayesFactorRobustness", "plotSequentialAnalysis")]
	    options[["plotBayesFactorRobustness"]] <- FALSE
	    options[["plotSequentialAnalysis"]] <- FALSE

	  }

	} else if (analysis == "one-sample") { # one-sample

	  dependents <- unlist(options[["variables"]])
	  options[["canDoAnalysis"]] <- length(dependents) > 0
	  options[["wilcoxTest"]] <- FALSE

	  options[["oneSided"]] <- switch(options[["hypothesis"]],
	                                  "greaterThanTestValue" = "right",
	                                  "lessThanTestValue" = "left",
	                                  FALSE
	  )

	} else { # paired

	  # this needs to be decided for each pair individually, which is done inside .ttestBPSTTest
	  options[["canDoAnalysis"]] <- TRUE
	  options[["wilcoxTest"]] <- FALSE

	  dependents <- sapply(options[["pairs"]], paste, collapse = " - ")
	  options[["variables"]]    <- dependents
	  names(options[["pairs"]]) <- dependents

	  options[["oneSided"]] <- switch(options[["hypothesis"]],
	                                  "groupOneGreater" = "right",
	                                  "groupTwoGreater" = "left",
	                                  FALSE
	  )

	  options[["stateKey"]][["ttestResults"]] <-
	    c(options[["stateKey"]][["ttestResults"]], "pairs")
	  options[["stateKey"]][["descriptives"]] <-
	    c(options[["stateKey"]][["descriptives"]], "pairs")

	}

	options[["nullInterval"]] <- switch(options[["oneSided"]],
    "right" = c(0, Inf),
    "left"  = c(-Inf, 0),
    c(-Inf, Inf)
  )

	oldDependents <- NULL
	if (!is.null(jaspResults[["stateVariables"]]))
	  oldDependents <- jaspResults[["stateVariables"]]$object

	if (!is.null(dependents)) {
	  tmp <- createJaspState(object = dependents, title = "dependents")
	  tmp$dependOnOptions("missingValues")
	  if (analysis == "independent")
	    tmp$dependOnOptions("groupingVariable")
	  jaspResults[["stateVariables"]] <- tmp
	}

	idx <- !(dependents %in% oldDependents)
	options[["anyNewVariables"]] <- any(idx)
	options[["newVariables"]] <- dependents[idx]

	.ttestBayesianInitBayesFactorPackageOptions()

  return(options)

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

.ttestBayesianEmptyObject <- function(options) {

  # construct a t-test object based on supplied options
  nvar <- length(options[["variables"]])

  # exception for paired t-test
  if (nvar == 1L && options[["variables"]] == " - " && options[["ttestType"]] == "paired")
    nvar <- 0L

  obj <- list(
    status          = rep("ok", nvar),
    BF10post        = numeric(nvar),
    tValue          = rep(NA, nvar),
    n_group1        = rep(NA, nvar),
    n_group2        = rep(NA, nvar),
    plottingError   = vector("list", nvar),
    errorFootnotes  = rep("no", nvar),
    delta           = vector("list", nvar),
    grouping        = options[["groupingVariable"]],
    paired          = !is.null(options[["pairs"]]),
    bayesFactorType = options[["bayesFactorType"]],
    analysis        = options[["ttestType"]]
  )
  if (nvar > 0L) {
    idx <- lengths(obj) == nvar
    obj[idx] <- lapply(obj[idx], stats::setNames, nm = options[["variables"]])
  }
  return(obj)

}
# descriptives ----
.ttestBayesianDescriptives <- function(jaspResults, dataset, options, errors) {

  if (is.null(jaspResults[["Descriptives"]])) {

    print("remake descriptivesCollection")
    descriptivesCollection <- createJaspContainer("Descriptives")
    descriptivesCollection$dependOnOptions(options[["stateKey"]][["descriptives"]])
    jaspResults[["Descriptives"]] <- descriptivesCollection

  } else {
    print("descriptivesCollection from state")
    descriptivesCollection <- jaspResults[["Descriptives"]]
  }

  dependents <- options[["variables"]]
  grouping <- options[["groupingVariable"]]
  canDoAnalysis <- options[["canDoAnalysis"]]

  if (options[["descriptives"]]) {
    if (is.null(descriptivesCollection[["table"]])) {
      print("remake descriptivesTable")
      descriptivesTable <- createJaspTable(title = "Descriptives")
      descriptivesTable$dependOnOptions("descriptives")
      descriptivesTable$position <- 1L
      descriptivesCollection[["table"]] <- descriptivesTable

      if (options$descriptivesPlots) {
        CRI <- options$descriptivesPlotsCredibleInterval
      } else {
        CRI <- NULL
      }

      .ttestBayesianDescriptivesTable(
        descriptives           = descriptivesTable,
        dataset                = dataset,
        dependents             = dependents,
        grouping               = grouping,
        CRI                    = CRI,
        canRun                 = canDoAnalysis,
        pairs                  = options[["pairs"]]
      )
    } else {
      print("descriptivesTable from state")
    }
  }

  if (options[["descriptivesPlots"]]) {
    if (is.null(descriptivesCollection[["plots"]])) {
      print("remake descriptivesPlots")
      descriptivesPlots <- createJaspContainer(title = "Descriptives Plots")
      descriptivesPlots$copyDependenciesFromJaspObject(descriptivesCollection)
      descriptivesPlots$position <- 2L
      descriptivesCollection[["plots"]] <- descriptivesPlots
      runDescriptives <- TRUE

    } else {
      print("descriptivesPlots from state")
      descriptivesPlots <- descriptivesCollection[["plots"]]
      runDescriptives <- options[["anyNewVariables"]]
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
        pairs            = options[["pairs"]]
      )
    }
  }
}

.ttestBayesianDescriptivesTable <- function(descriptives, dataset, dependents,
                                       grouping = NULL, CRI = NULL, canRun = FALSE,
                                       dependencies = NULL, stateDescriptivesTable = NULL,
                                       pairs = NULL) {

  # general function to make a descriptives tables
  #
  # Arguments
  #
  # descriptivesTable      : a jaspTable
  # dataset                : dataset (duh)
  # dependents             : variables in dataset to get descriptives for
  # grouping               : an optional grouping variable. If should be a grouping variable but there
  #                          is none supplied, pass ""
  # CRI                    : credible interval (between 0 and 1)
  # canRun                 : FALSE implies an empty table is generated, TRUE means it will get filled
  # pairs                  : a list where each element is a sublist indicating pairs of variables
  # dependencies           : dependencies available in options
  # stateDescriptivesTable : the results from previous time, either jaspState or jaspState$object
  #
  # Details:
  #
  # Does about everything to make a descriptives table. The only things that need to be set by a user
  # are the dependencies of the table and state.
  #
  # Returns
  #
  # jaspState

	# descriptives <- createJaspTable(title = if (is.null(grouping) "Group Descriptives" else "Descriptives"))
	# jaspCollection[["descriptivesTable"]] <- descriptives
	# descriptives$dependOnOptions(c(dependencies))

  hasGrouping <- !is.null(grouping)
	hasCRI <- !is.null(CRI)
	if (!is.null(pairs)) {
	  dependents <- unique(unlist(pairs))
	  dependents <- dependents[dependents != ""]
	}

	descriptives$addColumnInfo(name = "variable", title = "",      type = "string", combine = TRUE)
	if (hasGrouping)
	  descriptives$addColumnInfo(name = "group",    title = "Group", type = "string")
	descriptives$addColumnInfo(name = "N",        title = "N",     type = "number")
	descriptives$addColumnInfo(name = "mean",     title = "Mean",  type = "number", format = "sf:4;dp:3")
	descriptives$addColumnInfo(name = "sd",       title = "SD",    type = "number", format = "sf:4;dp:3")
	descriptives$addColumnInfo(name = "se",       title = "SE",    type = "number", format = "sf:4;dp:3")

	if (hasCRI) {
		interval <- 100 * CRI
		title <- paste0(interval, "% Credible Interval")
		descriptives$addColumnInfo(name = "lowerCI", type = "number", format = "sf:4;dp:3", title = "Lower", overtitle = title)
		descriptives$addColumnInfo(name = "upperCI", type = "number", format = "sf:4;dp:3", title = "Upper", overtitle = title)
	}

	if (!is.list(stateDescriptivesTable)) {
	  stateDescriptivesTable <- stateDescriptivesTable$object
	}

	nvar <- length(dependents)
	if (nvar == 0) dependents <- "."

	if (nvar == 0 || nrow(dataset) == 0 || !canRun) {

	  if (hasGrouping) {

  		tmp <- rep(dependents, each = 2)
  		tmp[seq(2, length(tmp), 2)] <- ""
  		dat <- data.frame(variable = tmp)

	  } else {

  		dat <- data.frame(variable = tmp)

	  }
    descriptives$setData(dat)

	} else {
	  if (hasGrouping) {

	    levels <- base::levels(dataset[[ .v(grouping) ]])
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
	            ciLower <- .clean(posteriorSummary$ciLower)
	            ciUpper <- .clean(posteriorSummary$ciUpper)

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
	descriptives$status <- "complete"

	return(createJaspState(object = stateDescriptivesTable, title = "stateDescriptivesTable"))

}

.ttestBayesianDescriptivesPlots <- function(descriptivePlots, dataset, dependents, errors,
																						grouping = NULL, CRI = .95, canRun = FALSE,
																						testValueOpt = NULL, pairs = NULL) {

  # general function to make a descriptives plots
  #
  # Arguments
  #
  # descriptivePlots  :  a jaspCollection
  # dataset           :  dataset (duh)
  # dependents        :  variables in dataset to get descriptives for
  # errors            :  optional list of errors (from .hasErrors). Should be a
  #                      list(dependent = list(message = ..., ,error = ...))
  # grouping          :  an optional grouping variable. If should be a grouping variable but there
  #                      is none supplied, pass ""
  # CRI               :  credible interval (between 0 and 1)
  # canRun            :  FALSE implies an empty table is generated, TRUE means it will get filled
  # testValueOpt      :  test value for Null-hypothesis. Omitted if NULL.
  # pairs             :  a list where each element is a sublist indicating pairs of variables
  #
  # Details:
  #
  # Does about everything to make descriptives plots The only things that need to be set by a user
  # are the dependencies of the descriptivePlots. IMPORTANT: Will automatically make each plot
  # depend on whether the corresponding variable is present in options[["variables"]].
  # general dependencies of the plots should be put on the collection
  #
  # Returns
  #
  # Nothing

  hasGrouping <- !is.null(grouping)
  paired <- !is.null(pairs)
	if (hasGrouping) {
		groupingData <- dataset[[.v(grouping)]]
		levels   <- base::levels(dataset[[.v(grouping)]])
	} else if (paired) {
	  grouping <- "group"
	  groupingData <- factor(rep(0:1, each = nrow(dataset)))
	}

	for (var in dependents) {

		if (is.null(descriptivePlots[[var]])) {
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
				plot <- .addPlotToJaspObj0(title, var, obj)
			} else {
				plot <- .addPlotToJaspObj0(title, var, NULL, errors[[var]][["message"]])
			}
  	  if (paired) {
  	    plot$setOptionMustContainDependency("pairs", pairs[[var]])
  	  } else {
  	    plot$setOptionMustContainDependency("variables",  var)
  	  }
			descriptivePlots[[var]] <- plot
		}
	}

	return()
}

# inferential plots ----
.ttestBayesianInferentialPlots <- function(jaspResults, dataset, options, results, errors) {

  dependents <- unlist(options$variables)
  grouping <- options[["groupingVariable"]]
  pairs    <- options[["pairs"]]

  if (is.null(jaspResults[["inferentialPlots"]])) {
    print("remake inferentialPlotsCollection")
    inferentialPlotsCollection <- createJaspContainer("Inferential Plots")
    inferentialPlotsCollection$dependOnOptions("hypothesis")
    jaspResults[["inferentialPlots"]] <- inferentialPlotsCollection
  } else {
    print("inferentialPlotsCollection from state")
    inferentialPlotsCollection <- jaspResults[["inferentialPlots"]]
  }

  # make subcontainers for each variable
  for (var in dependents) {
  	if (is.null(inferentialPlotsCollection[[var]])) {
  		print(sprintf("inferentialPlotsCollection[[%s]] remade", var))
  		container <- createJaspContainer(var)
  		container$dependOnOptions("hypothesis")
  		inferentialPlotsCollection[[var]] <- container
  	} else {
  		print(sprintf("inferentialPlotsCollection[[%s]] from state", var))
  	}
  }

  if (options[["plotPriorAndPosterior"]]) {
		.ttestBayesianPlotPriorAndPosterior(
  		collection     = inferentialPlotsCollection,
  		dependents     = dependents,
  		errors         = errors,
  		t              = results[["tValue"]],
  		n1             = results[["n1"]],
  		n2             = results[["n2"]],
  		BF             = results[["BF10post"]],
  		BFH1H0         = results[["BFH1H0"]],
  		delta          = results[["delta"]],
  		plottingError  = results[["plottingError"]],
  		paired         = results[["paired"]],
  		oneSided       = options[["oneSided"]],
  		rscale         = options[["priorWidth"]],
  		addInformation = options[["plotPriorAndPosteriorAdditionalInfo"]],
  		pairs          = pairs,
  		options        = options,
      dependencies   = c(options[["stateKey"]][["priorAndPosteriorPlots"]], "plotPriorAndPosterior")
  	)
  }

  if (options[["plotBayesFactorRobustness"]]) {
  	.ttestBayesianPlotRobustness(
  		collection             = inferentialPlotsCollection,
  		dependents             = dependents,
  		errors                 = errors,
  		dataset                = dataset,
  		grouping               = grouping,
  		BF10post               = results[["BF10post"]],
  		BFH1H0                 = results[["BFH1H0"]],
  		rscale                 = options[["priorWidth"]],
  		paired                 = results[["paired"]],
  		oneSided               = options[["oneSided"]],
  		additionalInformation  = options[["plotBayesFactorRobustnessAdditionalInfo"]],
  		effectSizeStandardized = options[["effectSizeStandardized"]],
  		pairs                  = pairs,
  		options                = options,
      dependencies           = c(options[["stateKey"]][["robustnessPlots"]], "plotSequentialAnalysis")
  	)
  }

  if (options[["plotSequentialAnalysis"]]) {
  	.ttestBayesianPlotSequential(
  		collection             = inferentialPlotsCollection,
  		dependents             = dependents,
  		errors                 = errors,
  		dataset                = dataset,
  		grouping               = grouping,
  		BF10post               = results[["BF10post"]],
  		BFH1H0                 = results[["BFH1H0"]],
  		paired                 = results[["paired"]],
  		oneSided               = options[["oneSided"]],
  		rscale                 = options[["priorWidth"]],
  		effectSizeStandardized = options[["effectSizeStandardized"]],
  		pairs                  = pairs,
  		options                = options,
      dependencies           = c(options[["stateKey"]][["sequentialPlots"]], "plotSequentialAnalysis")
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
                                                options, dependencies = NULL, ...) {
  title <- "Prior and Posterior Plot"
  for (var in dependents) {
    if (is.null(collection[[var]][["plotPriorAndPosterior"]])) {
      if (isFALSE(errors[[var]]) && is.null(plottingError[[var]])) {

        obj <- function() {.plotPosterior.summarystats.ttest(
          t              = tValue[var],
          n1             = n1[var],
          n2             = n2[var],
          paired         = paired,
          oneSided       = oneSided,
          BF             = BF[var],
          BFH1H0         = BFH1H0,
          rscale         = rscale,
          options        = options,
          delta          = delta[[var]],
          addInformation = addInformation,
          ...
        )}

        plot <- .addPlotToJaspObj0(title, var, obj)
      } else {
        if (!isFALSE(errors[[var]])) {
          err <- errors[[var]][["message"]]
        } else {
          err <- plottingError[[var]]
        }
        plot <- .addPlotToJaspObj0(title, var, NULL, err)
      }
      if (paired) {
        plot$setOptionMustContainDependency("pairs", pairs[[var]])
      } else {
        plot$setOptionMustContainDependency("variables",  var)
      }
    	plot$position <- 1L
      plot$dependOnOptions(dependencies)
      collection[[var]][["plotPriorAndPosterior"]] <- plot
    }
  }
}

.ttestBayesianPlotRobustness <- function(collection, dependents, errors, dataset,
                                         grouping = NULL, BF10post, BFH1H0,
                                         rscale = "medium", paired = FALSE,
                                         oneSided = FALSE,
                                         additionalInformation = TRUE,
                                         effectSizeStandardized, pairs = NULL,
                                         options, dependencies = NULL, ...) {
  title <- "Bayes Factor Robustness Plot"
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
    if (is.null(collection[[var]][["plotRobustness"]])) {
      if (effectSizeStandardized == "informative") {
        plot <- createJaspPlot(title = var, width = 480, height = 320, error = "badData",
                               errorMessage = "Bayes factor robustness check plot currently not supported for informed prior.")
      } else if (isFALSE(errors[[var]])) {

        if (paired) {
          pair <- options[["pairs"]][[var]]
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
            group1 <- group1 - options$testValue
          }
        }

        obj <- function() {.plotBF.robustnessCheck.ttest(
          x                     = group1,
          y                     = group2,
          BF10post              = BF10post[var],
          paired                = paired,
          oneSided              = oneSided,
          rscale                = rscale,
          BFH1H0                = BFH1H0,
          additionalInformation = additionalInformation,
          ...
        )}
        plot <- .addPlotToJaspObj0(title, var, obj)
      } else {
        plot <- .addPlotToJaspObj0(title, var, NULL, error[[var]][["message"]])
      }
      if (paired) {
        plot$setOptionMustContainDependency("pairs", pairs[[var]])
      } else {
        plot$setOptionMustContainDependency("variables",  var)
      }
    	plot$position <- 2L
      plot$dependOnOptions(dependencies)
      collection[[var]][["plotRobustness"]] <- plot
    }
  }
}

.ttestBayesianPlotSequential <- function(collection, dependents, errors, dataset,
                                         grouping = NULL, BF10post, BFH1H0,
                                         rscale = "medium", paired = FALSE,
                                         oneSided = FALSE,
                                         plotDifferentPriors = FALSE,
                                         effectSizeStandardized,
                                         testValue = NULL, pairs = NULL,
                                         options, dependencies = NULL, ...) {
  title <- "Sequential Analysis Plot"
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
    if (is.null(collection[[var]][["plotSequential"]])) {
      if (effectSizeStandardized == "informative") {
        plot <- createJaspPlot(title = var, width = 480, height = 320, error = "badData",
                               errorMessage = "Sequential analysis robustness check plot currently not supported for informed prior.")
      } else if (isFALSE(errors[[var]])) {

        if (paired) {
          pair <- options[["pairs"]][[var]]
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

        obj <- function() {.plotSequentialBF.ttest(
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
          options             = options,
          ...
        )}

        plot <- .addPlotToJaspObj0(title, var, obj)
      } else {
        plot <- .addPlotToJaspObj0(title, var, NULL, error[[var]][["message"]])
      }

      if (paired) {
        plot$setOptionMustContainDependency("pairs", pairs[[var]])
      } else {
        plot$setOptionMustContainDependency("variables",  var)
      }
      plot$dependOnOptions(dependencies)
			plot$position <- 3L
      collection[[var]][["plotSequential"]] <- plot
    }
  }
}

# plot functions  ----
.addPlotToJaspObj0 <- function(title, var, obj = NULL, errors = "",  w = 480, h = 320) {

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

  #

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

