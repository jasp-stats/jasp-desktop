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

TTestIndependentSamples <- function(jaspResults, dataset = NULL, options, ...) {
  #at least one variable and one grouping variable
  ready <- length(options$variables) > 0 && options$groupingVariable != ""
  type <- "independent"
  if(ready) {
    dataset <- .ttestReadData(dataset, options, type)
    .ttestCheckErrors(        dataset, options, type)
  }
  # Output tables (each calls its own results function)
  .ttestMainTable(  jaspResults, dataset, options, ready, type)
  .ttestNormalTable(jaspResults, dataset, options, ready, type)
  .ttestIndependentEqVarTable(jaspResults, dataset, options, ready)
  # Descriptives
  .ttestDescriptivesTable(jaspResults, dataset, options, ready, type)
  .ttestIndependentDescriptivesPlot(jaspResults, dataset, options, ready)
  
  return()
}

# Tables
.ttestIndependentEqVarTable <- function(jaspResults, dataset, options, ready){
  # Container
  .ttestAssumptionCheckContainer(jaspResults, options, type)
  container <- jaspResults[["AssumptionChecks"]]
  if (!options$equalityOfVariancesTests || !is.null(container[["equalityVariance"]])) 
    return()
  # Create table
  equalityVariance <- createJaspTable(title = "Test of Equality of Variances (Levene's)")
  #dependList <- c()
  #ttestMainTable$dependOn(dependList)
  equalityVariance$showSpecifiedColumnsOnly <- TRUE
  equalityVariance$addColumnInfo(name = "variable", type = "string", title = "")
  equalityVariance$addColumnInfo(name = "F",  type = "number")
  equalityVariance$addColumnInfo(name = "df", type = "integer")
  equalityVariance$addColumnInfo(name = "p",  type = "pvalue")
  
  container[["equalityVariance"]] <- equalityVariance
  res <- try(.ttestIndependentEqVarFill(container, dataset, options, ready))
  .ttestSetError(res, equalityVariance)
}

# Table fill
.ttestIndependentEqVarFill <- function(container, dataset, options, ready){

	data <- list()
	variables <- options$variables
	groups <- options$groupingVariable
	if (length(variables) == 0) variables <- "."
  
	for (variable in variables)
		data[[length(data) + 1]] <- list(variable = variable)

	if (groups != "") {

		levels <- levels(dataset[[ .v(groups) ]])

		for (variable in variables) {

			result <- try(silent = TRUE, expr = {

				levene <- car::leveneTest(dataset[[ .v(variable) ]],
																	dataset[[ .v(groups) ]], "mean")

				F  <- levene[1, "F value"]
				df <- levene[1, "Df"]
				p  <- levene[1, "Pr(>F)"]

				row <- list(variable = variable, F = F, df = df, p = p)

				if (is.na(levene[1, "F value"])) {
					note <- "F-statistic could not be calculated"
					container[["equalityVariance"]]$addFootnote(note)
				}
				row
			})

			if (isTryError(result))
				result <- list(variable = variable, F = "", df = "", p = "")

			container[["equalityVariance"]]$addRows(result)
		}
	}
}

# Plot
.ttestIndependentDescriptivesPlot <- function(jaspResults, dataset, options, ready) {
  if(!options$descriptivesPlots)
    return()
  .ttestDescriptivesContainer(jaspResults, options)
  container <- jaspResults[["ttestDescriptives"]]
  container[["title"]] <- createJaspContainer("Descriptives Plot")
  subcontainer <- container[["title"]]
  for(variable in options$variables) {
    title <- variable
    descriptivesPlot      <- createJaspPlot(title = title, width = 480, height = 320)
    descriptivesPlot$dependOn(optionContainsValue = list(variables = variable))
    subcontainer[[title]] <- descriptivesPlot
    if(ready){
      p <- try(.ttestIndependentDescriptivesPlotFill(dataset, options, variable))
      if(isTryError(p))
        descriptivesPlot$setError(.extractErrorMessage(p))
      else
        descriptivesPlot$plotObject <- p
    }
  }
  return()
}

.ttestIndependentDescriptivesPlotFill <- function(dataset, options, variable) {

	groups <- options$groupingVariable

	descriptivesPlotList <- list()

	base_breaks_x <- function(x) {
		b <- unique(as.numeric(x))
		d <- data.frame(y = -Inf, yend = -Inf, x = min(b), xend = max(b))
		list(ggplot2::geom_segment(data = d, ggplot2::aes(x = x, y = y, xend = xend,
																											yend = yend), inherit.aes = FALSE, size = 1))
	}
	
	base_breaks_y <- function(x) {
		ci.pos <- c(x[, "dependent"] - x[, "ci"], x[, "dependent"] + x[, "ci"])
		b <- pretty(ci.pos)
		d <- data.frame(x = -Inf, xend = -Inf, y = min(b), yend = max(b))
		list(ggplot2::geom_segment(data = d, ggplot2::aes(x = x, y = y,xend = xend,
																											yend = yend), inherit.aes = FALSE, size = 1),
				 ggplot2::scale_y_continuous(breaks = c(min(b), max(b))))
	}
	
	dataset <- na.omit(dataset)
	ci <- options$descriptivesPlotsConfidenceInterval
	summaryStat <- .summarySE(as.data.frame(dataset), 
	                          measurevar = .v(variable),
														groupvars = .v(groups), 
														conf.interval = ci, na.rm = TRUE, .drop = FALSE)
	
	colnames(summaryStat)[which(colnames(summaryStat) == .v(variable))] <- "dependent"
	colnames(summaryStat)[which(colnames(summaryStat) == .v(groups))] <- "groupingVariable"
	
	pd <- ggplot2::position_dodge(0.2)
	
	p <- ggplot2::ggplot(summaryStat, ggplot2::aes(x = groupingVariable,
																								 y = dependent, group = 1)) + 
	  ggplot2::geom_errorbar(ggplot2::aes(ymin = ciLower, ymax = ciUpper), 
	                         colour = "black", width = 0.2, position = pd) +
		ggplot2::geom_line(position = pd, size = 0.7) + 
	  ggplot2::geom_point(position = pd, size = 4) + 
	  ggplot2::ylab(unlist(variable)) + 
	  ggplot2::xlab(options$groupingVariable) +
		base_breaks_y(summaryStat) + base_breaks_x(summaryStat$groupingVariable)
  
	p <- JASPgraphs::themeJasp(p)
	
	return(p)
}
