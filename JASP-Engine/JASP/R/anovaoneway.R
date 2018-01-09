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

AnovaOneWay <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {

	if (is.null(dataset))
	{
		if (perform == "run") {
			dataset <- .readDataSetToEnd()
		} else {
			dataset <- .readDataSetHeader()
		}
	}

	results <- list()
	
	anova.table <- list()


	anova.table[["title"]] <- "ANOVA"
	anova.table[["cases"]] <- I(options$variables)

	fields <- list(
		list(id="bruce"))

	anova.table[["schema"]] <- list(fields=fields)

	results[["anova"]] <- anova.table 

	results
}

