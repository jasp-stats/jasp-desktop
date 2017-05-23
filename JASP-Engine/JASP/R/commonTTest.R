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

#' The plan of this file is to successively abstract functionality
#' from the individual t-tests into a common interface to reduce clutter
#'
#' returns the result object and the dataset
.initializeTTest <- function(dataset, options, perform, type = 'independent-samples') {
	
	if (type == "one-sample") {
		analysisTitle <- "One Sample T-Test"
	} else if (type == "paired") {
		analysisTitle <- "Paired Samples T-Test"
	} else {
		analysisTitle <- "Independent Samples T-Test"
	}
	
	groups <- options$groupingVariable
	depvars <- unlist(options$variables)
	
	## a paired t-test works with pairs ...
	if (type == 'paired') {
		depvars <- unlist(options$pairs)
		depvars <- depvars[depvars != ""]
	}
	
	## !is.null(groups) is there to avoid errors for paired and one-sample t-test
	if (!is.null(groups) && groups == "") groups <- NULL

	if (is.null(dataset)) {
		## if we are ready to run, read in the dataset
		if (perform == "run") {
			
			if (options$missingValues == "excludeListwise") {
				exclude <- depvars
			} else {
				exclude <- NULL
			}
				
			dataset <- .readDataSetToEnd(columns.as.numeric = depvars,
										 columns.as.factor = groups,
										 exclude.na.listwise = exclude)
			
		## else just read in the headers (and create an empty table)
		} else {
			dataset <- .readDataSetHeader(columns.as.numeric = depvars,
										  columns.as.factor = groups)
		}
	} 
	
	## this is the main object; we add stuff to it and return it
	results <- list("title" = analysisTitle)

	#### META
	meta <- list()
	meta[[1]] <- list(name = "title", type = "title")
	meta[[2]] <- list(name = "ttest", type = "table")
	meta[[3]] <- list(name="assumptionChecks", type="object", meta=list(list(name="shapiroWilk", type="table"), list(name="levene", type="table")))
	meta[[4]] <- list(name="descriptives", type="object", meta=list(list(name="descriptivesTable", type="table"), list(name = "descriptivesPlots", type = "collection", meta="image")))
	
	results[[".meta"]] <- meta
	list("results" = results, "dataset" = dataset)
}
