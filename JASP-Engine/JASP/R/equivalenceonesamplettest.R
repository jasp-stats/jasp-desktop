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

EquivalenceOneSampleTTest <- function(jaspResults, dataset, options) {
  
  ready <- (length(options$variables) > 0)

  # Read dataset and error checking
  if (ready) {
    dataset <- .ttestReadData(dataset, options, "one-sample") 
    .ttestCheckErrors(dataset, options, "one-sample")
    errors <- .ttestBayesianGetErrorsPerVariable(dataset, options, "one-sample")
  }
  
  # Compute the results
  if(options[['equivalenceRegion']] == "lower"){
    options$lowerbound <- -Inf
    options$upperbound <- options$lower_max
  } else if(options[['equivalenceRegion']] == "upper"){
    options$lowerbound <- options$upper_min
    options$upperbound <- Inf
  }
  equivalenceOneTTestResults <- .equivalenceOneTTestComputeResults(jaspResults, dataset, options, ready, errors)
  
  # Output tables and plots
  if(is.null(jaspResults[["equivalenceOneTTestTable"]]))
    .equivalenceOneTTestTableMain(jaspResults, dataset, options, equivalenceOneTTestResults, ready)
  
  if(is.null(jaspResults[["equivalenceOneBoundsTable"]]))
    .equivalenceOneTTestTableEqb(jaspResults, dataset, options, equivalenceOneTTestResults, ready)
  
  if(options$descriptives && is.null(jaspResults[["equivalenceOneDescriptivesTable"]]))
    .equivalenceOneTTestTableDescriptives(jaspResults, dataset, options, equivalenceOneTTestResults, ready)
  
  if(options$equivalenceboundsplot)
    .equivalenceOnePlot(jaspResults, dataset, options, equivalenceOneTTestResults, ready)
  
  return()
}

.equivalenceOneTTestComputeResults <- function(jaspResults, dataset, options, ready, errors) {
  
  if (!ready)
    return(list())
  
  if (!is.null(jaspResults[["stateEquivalenceOneTTestResults"]]))
    return(jaspResults[["stateEquivalenceOneTTestResults"]]$object)
  
  results <- list()
  
  for (variable in options$variables) {
    
    results[[variable]] <- list()
    
    if(!isFALSE(errors[[variable]])) {
      errorMessage <- errors[[variable]]$message
      results[[variable]][["status"]] <- "error"
      results[[variable]][["errorFootnotes"]] <- errorMessage
      
    } else {
      tableResults <- try(TOSTER::dataTOSTone(data         = dataset, 
                                              vars         = .v(variable), 
                                              mu           = options$mu,
                                              low_eqbound  = options$lowerbound,
                                              high_eqbound = options$upperbound,
                                              eqbound_type = ifelse(options$boundstype == "raw", "raw", "d"),  # bounds type is raw or cohen's d
                                              alpha        = options$alpha,                                    # default = 0.05
                                              desc         = TRUE))
      
      if (isTryError(tableResults)) {
        
        errorMessage <- .extractErrorMessage(tableResults)
        results[[variable]][["status"]]         <- "error"
        results[[variable]][["errorFootnotes"]] <- errorMessage
        
      } else {
        
        dat <- na.omit(dataset[[ .v(variable) ]])
        n   <- length(dat)
        
        confIntEffSize <- c(0,0)
        ciEffSize <- 1 - 2 * options$alpha
        alphaLevel <- 1 - (ciEffSize + 1) / 2
        confIntEffSize <- .confidenceLimitsEffectSizes(ncp = tableResults$tost$asDF$`t[0]`, 
                                                       df = tableResults$tost$asDF$`df[0]`, 
                                                       alpha.lower = alphaLevel,
                                                       alpha.upper = alphaLevel)[c(1, 3)]
        confIntEffSize <- unlist(confIntEffSize) / sqrt(n)
        confIntEffSize <- sort(confIntEffSize)
        
        results[[variable]] <- list(
          ttestTvalue = tableResults$tost$asDF$`t[0]`, 
          ttestDf     = tableResults$tost$asDF$`df[0]`,
          ttestP      = tableResults$tost$asDF$`p[0]`,
          upperTvalue = tableResults$tost$asDF$`t[1]`, 
          upperDf     = tableResults$tost$asDF$`df[1]`,
          upperP      = tableResults$tost$asDF$`p[1]`,
          lowerTvalue = tableResults$tost$asDF$`t[2]`, 
          lowerDf     = tableResults$tost$asDF$`df[2]`,
          lowerP      = tableResults$tost$asDF$`p[2]`, 
          lowCohen    = tableResults$eqb$asDF$`low[cohen]`,
          highCohen   = tableResults$eqb$asDF$`high[cohen]`,
          cilCohen    = as.numeric(confIntEffSize[1]),
          ciuCohen    = as.numeric(confIntEffSize[2]),
          lowRaw      = tableResults$eqb$asDF$`low[raw]`,
          highRaw     = tableResults$eqb$asDF$`high[raw]`,
          cilRaw      = tableResults$eqb$asDF$`cil[raw]`,
          ciuRaw      = tableResults$eqb$asDF$`ciu[raw]`, 
          desc        = as.data.frame(tableResults$desc))
      }
    }
  }
  
  # Save results to state
  jaspResults[["stateEquivalenceOneTTestResults"]] <- createJaspState(results)
  jaspResults[["stateEquivalenceOneTTestResults"]]$dependOn(c("variables", "mu", "equivalenceRegion", "lower", "upper",
                                                              "region", "lowerbound", "upperbound", "lower_max", "upper_min", "boundstype", "alpha", "missingValues")) 
  
  return(results)
}

.equivalenceOneTTestTableMain <- function(jaspResults, dataset, options, equivalenceOneTTestResults, ready) {
  
  # Create table
  equivalenceOneTTestTable <- createJaspTable(title = gettext("Equivalence One Sample T-Test"))
  equivalenceOneTTestTable$dependOn(c("variables", "mu", "equivalenceRegion", "lower", "upper",
                                      "region", "lowerbound", "upperbound", "lower_max", "upper_min", "boundstype", "alpha"))

  # Add Columns to table
  equivalenceOneTTestTable$addColumnInfo(name = "variable",   title = " ",                   type = "string", combine = TRUE)
  equivalenceOneTTestTable$addColumnInfo(name = "statistic",  title = gettext("Statistic"),  type = "string")
  equivalenceOneTTestTable$addColumnInfo(name = "t",          title = gettext("t"),          type = "number")
  equivalenceOneTTestTable$addColumnInfo(name = "df",         title = gettext("df"),         type = "number")
  equivalenceOneTTestTable$addColumnInfo(name = "p",          title = gettext("p"),          type = "pvalue")
  
  equivalenceOneTTestTable$showSpecifiedColumnsOnly <- TRUE
  
  if (ready)
    equivalenceOneTTestTable$setExpectedSize(length(options$variables))
  
  jaspResults[["equivalenceOneTTestTable"]] <- equivalenceOneTTestTable
  
  if (!ready)
    return()
  
  .equivelanceOneTTestFillTableMain(equivalenceOneTTestTable, dataset, options, equivalenceOneTTestResults)
  
  return()
}

.equivelanceOneTTestFillTableMain <- function(equivalenceOneTTestTable, dataset, options, equivalenceOneTTestResults) {
  
  for (variable in options$variables) {
    
    results <- equivalenceOneTTestResults[[variable]]
    
    if (!is.null((results$status))) { 
      equivalenceOneTTestTable$addFootnote(message = results$errorFootnotes, rowNames = variable, colNames = "statistic")
      equivalenceOneTTestTable$addRows(list(variable = variable, statistic = NaN), rowNames = variable)
    } else {
      # TOST T-test
      equivalenceOneTTestTable$addRows(list(variable  = variable,
                                            statistic = gettext("T-Test"),
                                            t         = results$ttestTvalue,
                                            df        = results$ttestDf,
                                            p         = results$ttestP))
      
      # TOST upper:
      equivalenceOneTTestTable$addRows(list(variable  = variable,
                                            statistic = gettext("Upper bound"),
                                            t         = results$upperTvalue,
                                            df        = results$upperDf,
                                            p         = results$upperP))
      
      # TOST lower:
      equivalenceOneTTestTable$addRows(list(variable  = variable,
                                            statistic = gettext("Lower bound"),
                                            t         = results$lowerTvalue,
                                            df        = results$lowerDf,
                                            p         = results$lowerP))
    }
    }
  
  return()
}

.equivalenceOneTTestTableEqb <- function(jaspResults, dataset, options, equivalenceOneTTestResults, ready) {
  
  # Create table
  equivalenceOneBoundsTable <- createJaspTable(title = gettext("Equivalence Bounds"))
  equivalenceOneBoundsTable$dependOn(c("variables", "mu", "equivalenceRegion", "lower", "upper",
                                       "region", "lowerbound", "upperbound", "lower_max", "upper_min",
                                       "boundstype", "alpha", "missingValues"))

  # Add Columns to table
  equivalenceOneBoundsTable$addColumnInfo(name = "variable",   title = " ",                     type = "string", combine = TRUE)
  equivalenceOneBoundsTable$addColumnInfo(name = "statistic",  title = gettext("Bounds type"),  type = "string")
  equivalenceOneBoundsTable$addColumnInfo(name = "low",        title = gettext("Low"),          type = "number")
  equivalenceOneBoundsTable$addColumnInfo(name = "high",       title = gettext("High"),         type = "number")
  
  title <- gettextf("%s%% Confidence Interval", 100 - options$alpha * 100 * 2)
  equivalenceOneBoundsTable$addColumnInfo(name = "lower", type = "number", format = "sf:4;dp:3", title = gettext("Lower"), overtitle = title)
  equivalenceOneBoundsTable$addColumnInfo(name = "upper", type = "number", format = "sf:4;dp:3", title = gettext("Upper"), overtitle = title)
  
  
  equivalenceOneBoundsTable$showSpecifiedColumnsOnly <- TRUE
  
  jaspResults[["equivalenceOneBoundsTable"]] <- equivalenceOneBoundsTable
  
  if (ready)
    equivalenceOneBoundsTable$setExpectedSize(length(options$variables))
  
  if (!ready)
    return()
  
  .equivalenceOneTTestFillTableEqb(equivalenceOneBoundsTable, dataset, options, equivalenceOneTTestResults)

  return()
}

.equivalenceOneTTestFillTableEqb <- function(equivalenceOneBoundsTable, dataset, options, equivalenceOneTTestResults) {
  
  for (variable in options$variables) {
    
    results <- equivalenceOneTTestResults[[variable]]
    
    if (!is.null((results$status))) { 
      equivalenceOneBoundsTable$addFootnote(message = results$errorFootnotes, rowNames = variable, colNames = "statistic")
      equivalenceOneBoundsTable$addRows(list(variable = variable, statistic = NaN), rowNames = variable)
    } else {
    
    equivalenceOneBoundsTable$addRows(list(variable     = variable,
                                           statistic    = gettext("Cohen's d"),
                                           low          = results$lowCohen,
                                           high         = results$highCohen,
                                           lower        = results$cilCohen,
                                           upper        = results$ciuCohen))
    
    
    equivalenceOneBoundsTable$addRows(list(variable     = variable,
                                           statistic    = gettext("Raw"),
                                           low          = results$lowRaw,
                                           high         = results$highRaw,
                                           lower        = results$cilRaw, 
                                           upper        = results$ciuRaw))
    }
  }
}

.equivalenceOneTTestTableDescriptives <- function(jaspResults, dataset, options, equivalenceOneTTestResults, ready) {
  
  # Create table
  equivalenceOneDescriptivesTable <- createJaspTable(title = gettext("Descriptives"))
  equivalenceOneDescriptivesTable$dependOn(c("variables", "mu", "descriptives", "missingValues"))

  # Add Columns to table
  equivalenceOneDescriptivesTable$addColumnInfo(name = "variable",   title = " ",                  type = "string")
  equivalenceOneDescriptivesTable$addColumnInfo(name = "N",          title = gettext("N"),         type = "integer")
  equivalenceOneDescriptivesTable$addColumnInfo(name = "mean",       title = gettext("Mean"),      type = "number")
  equivalenceOneDescriptivesTable$addColumnInfo(name = "sd",         title = gettext("SD"),        type = "number")
  equivalenceOneDescriptivesTable$addColumnInfo(name = "se",         title = gettext("SE"),        type = "number")
  
  equivalenceOneDescriptivesTable$showSpecifiedColumnsOnly <- TRUE
  
  if (ready)
    equivalenceOneDescriptivesTable$setExpectedSize(length(options$variables))
  
  jaspResults[["equivalenceOneDescriptivesTable"]] <- equivalenceOneDescriptivesTable
  
  if (!ready)
    return()
  
  .equivalenceOneFillDescriptivesTable(equivalenceOneDescriptivesTable, dataset, options, equivalenceOneTTestResults)
  
  return()
}

.equivalenceOneFillDescriptivesTable <- function(equivalenceOneDescriptivesTable, dataset, options, equivalenceOneTTestResults) {
  
  for (variable in options$variables) {
    
    results <- equivalenceOneTTestResults[[variable]]
    
    if (!is.null((results$status))) { 
      equivalenceOneDescriptivesTable$addFootnote(message = results$errorFootnotes, rowNames = variable, colNames = "N")
      equivalenceOneDescriptivesTable$addRows(list(variable = variable, N = NaN), rowNames = variable)
    } else {
      results <- results$desc
      equivalenceOneDescriptivesTable$addRows(list(variable      = variable,
                                                   N             = results$n,
                                                   mean          = results$m,
                                                   sd            = results$sd, 
                                                   se            = results$se))
    }
  }
}

.equivalenceOnePlot <- function(jaspResults, dataset, options, equivalenceOneTTestResults, ready) {
  
  equivalenceOneBoundsContainer <- createJaspContainer(title = gettext("Equivalence Bounds Plots"))
  equivalenceOneBoundsContainer$dependOn(c("boundstype", "missingValues", "equivalenceRegion", "lower", "upper",
                                           "region", "lowerbound", "upperbound", "lower_max", "upper_min", "equivalenceboundsplot"))
  jaspResults[["equivalenceOneBoundsContainer"]] <- equivalenceOneBoundsContainer
  
  if (!ready)
    return()
  
  for (variable in options$variables) {
    
    # Check if the plot for this variable was already created previously
    if (!is.null(equivalenceOneBoundsContainer[[variable]]))
      next
    
    equivalenceOneTTestPlot <- createJaspPlot(title = variable, width = 480, height = 320)
    equivalenceOneTTestPlot$dependOn(optionContainsValue = list("variables" = variable))

    # Get results
    results <- equivalenceOneTTestResults[[variable]]
    
    if (!is.null((results$status))) { 
      equivalenceOneTTestPlot$setError(results$errorFootnotes)
    } else {
      # Calculate mean of group difference
      m1  <- results$desc$m
      m2  <- options$mu
      dif <- (m1 - m2)
      
      # Make plot:
      plot <- ggplot2::ggplot(data = dataset, ggplot2::aes_string(x = 0, y = dif)) +
        ggplot2::annotate("rect", xmin = -20, xmax = 20, ymin = results$lowRaw, ymax = results$highRaw, alpha = .5) + 
        ggplot2::geom_errorbar(ggplot2::aes_string(x = 0, ymin = results$cilRaw, ymax = results$ciuRaw, width = .4), size = .8, colour = "black") +
        ggplot2::geom_point(ggplot2::aes_string(x = 0, y = dif), shape = 21, fill = "black", size = 3, colour = "black") +
        ggplot2::labs(x = '', y = variable) +
        ggplot2::expand_limits(x = c(-2, 4), y = 0)
      plot <- JASPgraphs::themeJasp(plot)
      plot <- plot + ggplot2::theme(axis.text.x  = ggplot2::element_blank(),
                                    axis.title.x = ggplot2::element_blank(),
                                    axis.ticks.x = ggplot2::element_blank(),
                                    axis.line.x  = ggplot2::element_blank(),) + ggplot2::scale_x_discrete(limits = c("", "", "", "", "", "", ""))
      
      equivalenceOneTTestPlot$plotObject <- plot
    }
    
    equivalenceOneBoundsContainer[[variable]] <- equivalenceOneTTestPlot
  }
  
  return()
}
