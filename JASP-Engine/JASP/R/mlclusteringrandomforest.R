#
# Copyright (C) 2017 University of Amsterdam
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

MLClusteringRandomForest <- function(dataset = NULL, options, perform = "run",
									 callback = function(...) 0, ...) {
									  # callback = function(...) list(status = "ok"), ...) {

	results <- list(
		title = "Random Forest Clustering",
		.meta = list(
			list(name = "title",                     type = "title"),
			list(name = "tableSummary",   type = "table"),				
			list(name = "tableVariableImportance",   type = "table"),		
			list(name = "plotVariableImportance",    type = "image"),
			list(name = "plotTreesVsModelError",     type = "image")
		)
	)

	if (perform == "init") {

		return(list(results=results, status="inited"))#, state=state))

	} else {

		return(list(results=results, status="complete"))#, state=state))

	}
}