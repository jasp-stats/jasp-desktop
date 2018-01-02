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

CorrelationPartial <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {

	if (is.null(dataset))
	{
		if (perform == "run") {
			dataset <- .readDataSetToEnd()
		} else {
			dataset <- .readDataSetHeader()
		}
	}

	results <- list()
	
	correlations <- list()
	
	correlations[["title"]] <- "Correlation"
	correlations[["cases"]] <- c("line1", "line2", "line3")
	
	fields <- list(
		list(id="col1", type="number", format="sf:4"),
		list(id="col2", type="number", format="sf:4"),
		list(id="col3", type="number", format="dp:4;p:.001"))
		
	schema <- list(fields=fields)
	
	correlations[["schema"]] <- schema
	
	results[["correlations"]] <- correlations

	results
}

