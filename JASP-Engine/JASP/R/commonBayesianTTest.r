.ttestBayesianReadData <- function(options, dataset = NULL) {

	dependents <- unlist(options$variables)
  grouping   <- options$groupingVariable
  if (identical(grouping, ""))
    grouping <- NULL

  if (is.null(dataset)) {
    excl <- grouping
    if (options$missingValues == "excludeListwise") {
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
		  .hasErrors(dataset, "run", type = 'variance', factorLevels.amount = '!= 2',
		             variance.target = dependents, exitAnalysisIfErrors = TRUE)
		  
		  for (var in dependents) {
		    
		    errors[[var]] <- .hasErrors(dataset, perform = "run", message = 'short',
		                                type = c('infinity','observations','variance'),
		                                all.target = var, observations.amount = "< 2")
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

  if(is.null(options("BFMaxModels"))) options(BFMaxModels = 50000)
  if(is.null(options("BFpretestIterations"))) options(BFpretestIterations = 100)
  if(is.null(options("BFapproxOptimizer"))) options(BFapproxOptimizer = "optim")
  if(is.null(options("BFapproxLimits"))) options(BFapproxLimits = c(-15,15))
  if(is.null(options("BFprogress"))) options(BFprogress = interactive())
  if(is.null(options("BFfactorsMax"))) options(BFfactorsMax = 5)

}

.ttestBayesianInitAnalysisOptions <- function(jaspResults, options, which = c("independent", "paired", "one-sample")) {

	# initialize options: takes user input and determines:
	#
	# - can any analysis be done?
	# - which variables will need to be computed for plots?
	#
	# also defines the dependencies for all objects and adds this to options[["stateKey"]]

  which <- match.arg(which)
	dependents <- unlist(options$variables)
	oldDependents <- NULL
	if (!is.null(jaspResults[["stateVariables"]]))
		oldDependents <- jaspResults[["stateVariables"]]$object

	if (!is.null(dependents)) {
		tmp <- createJaspState(object = dependents, title = "dependents")
		if (which == "independent")
		  tmp$dependOnOptions("groupingVariable")
		jaspResults[["stateVariables"]] <- tmp
	}

	options[["anyNewVariables"]] <- any(!dependents %in% oldDependents)
	options[["newVariables"]] <- dependents[!dependents %in% oldDependents]

	if (which == "independent") {
	  
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
	  
	  # dependencies specified here to pass as depends
	  defaults <- c("priorWidth", "hypothesis", "groupingVariable", "missingValues",
	                "effectSizeStandardized", "informativeStandardizedEffectSize",
	                "informativeCauchyLocation", "informativeCauchyScale", "informativeTLocation",
	                "informativeTScale", "informativeTDf", "informativeNormalMean",
	                "informativeNormalStd", "testStatistic", "wilcoxonSamplesNumber")
	  options[["stateKey"]] <- list(
	    ttestResults           = defaults,
	    descriptives           = c("groupingVariable", "missingValues", "descriptivesPlotsCredibleInterval"),
	    priorAndPosteriorPlots = c(defaults, "plotHeight", "plotWidth", "plotPriorAndPosteriorAdditionalInfo"),
	    robustnessPlots        = c(defaults, "plotHeight", "plotWidth", "plotBayesFactorRobustnessAdditionalInfo"),
	    sequentialPlots        = c(defaults, "plotHeight", "plotWidth", "plotSequentialAnalysisRobustness"),
	    delta                  = c("priorWidth", "groupingVariable", "missingValues", "wilcoxonSamplesNumber"),
	    descriptivesData       = c("descriptives", "groupingVariable", "missingValues", "descriptivesPlotsCredibleInterval")
	  )
	  
	} else if (which == "one-sample") { # one-sample
	  
	  options[["canDoAnalysis"]] <- length(dependents) > 0
	  options[["wilcoxTest"]] <- FALSE
	  
	  options[["oneSided"]] <- switch(options[["hypothesis"]],
	                                  "greaterThanTestValue" = "right",
	                                  "lessThanTestValue" = "left",
	                                  FALSE
	  )
	  
	  # dependencies specified here to pass as depends
	  defaults <- c("priorWidth", "hypothesis", "groupingVariable", "missingValues",
	                "effectSizeStandardized", "informativeStandardizedEffectSize",
	                "informativeCauchyLocation", "informativeCauchyScale", "informativeTLocation",
	                "informativeTScale", "informativeTDf", "informativeNormalMean",
	                "informativeNormalStd", "testStatistic", "wilcoxonSamplesNumber")
	  options[["stateKey"]] <- list(
	    ttestResults = defaults,
	    descriptives = c("groupingVariable", "missingValues", "descriptivesPlotsCredibleInterval"),
	    priorAndPosteriorPlots = c(defaults, "plotHeight", "plotWidth", "plotPriorAndPosteriorAdditionalInfo"),
	    robustnessPlots = c(defaults, "plotHeight", "plotWidth", "plotBayesFactorRobustnessAdditionalInfo"),
	    sequentialPlots = c(defaults, "plotHeight", "plotWidth", "plotSequentialAnalysisRobustness"),
	    delta = c("priorWidth", "groupingVariable", "missingValues", "wilcoxonSamplesNumber"),
	    descriptivesData = c("descriptives", "groupingVariable", "missingValues", "descriptivesPlotsCredibleInterval")
	  )
	  
	} else { # paired
	  
	}
	
	options[["nullInterval"]] <- switch(options[["oneSided"]],
    "right" = c(0, Inf),
    "left"  = c(-Inf, 0),
    c(-Inf, Inf)
  )

  return(options)

}

.ttestBayesianDescriptives <- function(descriptives, dataset, dependents, 
                                       grouping = NULL, CRI = NULL, canRun = FALSE, 
                                       dependencies = NULL, stateDescriptivesTable = NULL) {
  
  # general function to make a descriptives tables
  #
  # Arguments
  # 
  # descriptivesTable      : a jaspTable
  # dataset                : dataset (duh)
  # dependents             : variables in dataset to get descriptives for
  # grouping               : an optional grouping variable. If should be a grouping variable but there is none supplied, pass ""
  # CRI                    : credible interval (between 0 and 1)
  # canRun                 : FALSE implies an empty table is generated, TRUE means it will get filled
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
	    
	    tmp <- rep(dependents, each = 2)
  		tmp[seq(2, length(tmp), 2)] <- ""
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
																						grouping = NULL, CRI = NULL, canRun = FALSE,
																						testValueOpt = NULL) {

  hasGrouping <- !is.null(grouping)

	if (hasGrouping) {
		grouping <- options$groupingVariable
		groupingData <- dataset[[.v(grouping)]]
		levels   <- base::levels(dataset[[.v(grouping)]])
		
	}

	for (var in dependents) {

		if (is.null(descriptivesPlotCollection[[var]])) {
			if (isFALSE(errors[[var]])) {

				idxC <- !is.na(dataset[[.v(var)]])

				obj <- try({.plot2GroupMeansBayesIndTtest(
					data = dataset[idxC, c(.v(var), grouping)], var = var, grouping = grouping
					groupNames = levels, CRI = CRI, testValueOpt = testValueOpt)
				})
				plot <- .addPlotToJaspObj0(var, errors, obj, dependencies)
			} else {
				plot <- .addPlotToJaspObj0(var, errors, NULL, dependencies)
			}
			descriptivesPlotCollection[[var]] <- plot
		}
	}
	
	return()
}

.addPlotToJaspObj0 <- function(var, errors, obj = NULL, w = 480, h = 320) {

	# convenience function
	if (is.null(obj)) {
		plot <- createJaspPlot(title = var, width = w, height = h,
													 error = "badData", errorMessage = errors[[var]][["message"]])
	} else if (identical(obj, "empty")) {
		plot <- createJaspPlot(title = var, width = w, height = h)
	} else if (isTryError(obj)) {
		plot <- createJaspPlot(title = var, width = w, height = h,
													 error = "badData", errorMessage = .extractErrorMessage(obj))
	} else {
		plot <- createJaspPlot(title = var, width = w, height = h, plot = obj)
	}
	plot$setOptionMustContainDependency("variables",  var)
	return(plot)
}

.ttestBayesianPlotKGroupMeans <- function(data, var, grouping = NULL, 
																					groupNames = NULL, CRI = .95,
																					testValueOpt = NULL) {

	hasGrouping <- !is.null(grouping)
	if (hasGrouping) {
		
		summaryStat <- tapply(data[[1L]], data[[2L]], function(x) {
			.posteriorSummaryGroupMean(variable = x, descriptivesPlotsCredibleInterval = CRI)
		})
		summaryStat <- do.call(rbind, summaryStat)
		summaryStat$groupingVariable <- groupNames
		mapping <- ggplot2::aes(x=groupingVariable, y=dependent, group=group)
		ylab <- ggplot2::ylab(var)
		xlab <- ggplot2::xlab(grouping)
		
	} else {
		
		summaryStat <- as.data.frame(.posteriorSummaryGroupMean(data[[1L]], descriptivesPlotsCredibleInterval = CRI))
		mapping <- ggplot2::aes(x=groupingVariable, y=dependent, group=group)
		testValue <- data.frame("testValue" = testValueOpt) # default zero
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
				axis.ticks.margin = grid::unit(1,"mm"),
				axis.ticks.length = grid::unit(3, "mm"),
				plot.margin = grid::unit(c(.5,0,.5,.5), "cm")) +
				.base_breaks_y4(summaryStat, testValueOpt) +
				.base_breaks_x(summaryStat$groupingVariable)

	if (!is.null(testValueOpt))
		p <- p + ggplot2::geom_hline(data = testValue, ggplot2::aes(yintercept=testValue), linetype="dashed")

	return(p)

}

.base_breaks_y4 <- function(x, testValue){

  values <- c(testValue, x$ciLower, x$ciUpper)
  ci.pos <- c(min(values), max(values))
  b <- pretty(ci.pos)
  d <- data.frame(x=-Inf, xend=-Inf, y=min(b), yend=max(b))
  list(ggplot2::geom_segment(data=d, ggplot2::aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE, size = 1),
       ggplot2::scale_y_continuous(breaks=c(min(b), testValue, max(b))))
}