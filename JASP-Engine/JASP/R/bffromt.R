#
# Copyright (C) 2016 University of Amsterdam
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

BFFromT <- function(dataset=NULL, options, perform = 'run', callback) {

	results <- list()
	
	results[[".meta"]] <- list(list(name="table", type="table"))
	results[["title"]] <- "Summary Statistics"

	bayesFactor01 <- BayesFactor::ttest.tstat(options$tStatistic, options$n1Size, options$n2Size)

	table <- list()
	table[["title"]] <- "BF from <i>t</i>"

	fields=list()

	fields[[length(fields)+1]] <- list(name="tStatistic", type="number", format="sf:4;dp:3", title="t value")
	fields[[length(fields)+1]] <- list(name="n1Size", type="number", title="group 1 size")
	fields[[length(fields)+1]] <- list(name="n2Size", type="number", title="group 2 size")
	fields[[length(fields)+1]] <- list(name="BF", type="number", format="sf:4;dp:3", title="BF\u2080\u2081")

	table[["schema"]] <- list(fields=fields)

	row <- list(BF = .clean(exp(bayesFactor01$bf)), tStatistic = options$tStatistic, n1Size = options$n1Size, n2Size = options$n2Size)

	table[["data"]] <- list(row)
	
	results[["table"]] <- table

	#state <- .retrieveState()

	
	list(results=results, status="complete")

#	if (perform == "init") {
#		return(list(results=results, status="inited", state=state, keep=keep))
#	} else {
#		return(list(results=results, status="complete"))
#	}

}