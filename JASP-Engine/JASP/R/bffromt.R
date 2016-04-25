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

	if (options$hypothesis == "notEqualToTestValue") {
		nullInterval <- NULL
		oneSided <- FALSE
	}
	if (options$hypothesis == "greaterThanTestValue") {
		nullInterval <- c(0, Inf)
		oneSided <- "right"
	}
	if (options$hypothesis == "lessThanTestValue") {
		nullInterval <- c(-Inf, 0)
		oneSided <- "left"
	}

	if (oneSided == FALSE) {
		nullInterval <- NULL
	}
	if (oneSided == "right") {
		nullInterval <- c(0, Inf)
	}
	if (oneSided == "left") {
		nullInterval <- c(-Inf, 0)
	}

	bayesFactor01 <- BayesFactor::ttest.tstat(t = options$tStatistic, n1 = options$n1Size, n2 = options$n2Size, rscale = options$priorWidth, nullInterval = nullInterval)

	table <- list()
	table[["title"]] <- "BF from <i>t</i>"

	fields=list()

	fields[[length(fields)+1]] <- list(name="tStatistic", type="number", format="sf:4;dp:3", title="t value")
	fields[[length(fields)+1]] <- list(name="n1Size", type="number", title="n<sub>1</sub>")

	if(options$n2Size > 0)
	{
		fields[[length(fields)+1]] <- list(name="n2Size", type="number", title="n<sub>2</sub>")
	}

	fields[[length(fields)+1]] <- list(name="BF", type="number", format="sf:4;dp:3", title="BF\u2080\u2081")

	if(options$errorEstimate)
	{
		fields[[length(fields)+1]] <- list(name="errorEstimate", type="number", format="sf:4;dp:3", title="Error estimate")		
	}

	table[["schema"]] <- list(fields=fields)

	
	if(options$bayesFactorType == "BF10")
	{
		row <- list(BF = .clean(exp(bayesFactor01$bf)), tStatistic = options$tStatistic, n1Size = options$n1Size, n2Size = options$n2Size, errorEstimate = .clean(bayesFactor01$properror))
	}
	else if(options$bayesFactorType == "BF01")
	{
		row <- list(BF = .clean(1/exp(bayesFactor01$bf)), tStatistic = options$tStatistic, n1Size = options$n1Size, n2Size = options$n2Size, errorEstimate = .clean(bayesFactor01$properror))
	}
	else
	{
		row <- list(BF = .clean(bayesFactor01$bf), tStatistic = options$tStatistic, n1Size = options$n1Size, n2Size = options$n2Size, errorEstimate = .clean(bayesFactor01$properror))
	}





	table[["data"]] <- list(row)
	
	footnotes <- .newFootnotes()
	message <- paste("The prior width used is ", options$priorWidth, sep="")
	.addFootnote(footnotes, symbol="<em>Note.</em>", text=message)

	table[["footnotes"]] <- as.list(footnotes)




	#state <- .retrieveState()

	results[["table"]] <- table
	
	list(results=results, status="complete")

#	if (perform == "init") {
#		return(list(results=results, status="inited", state=state, keep=keep))
#	} else {
#		return(list(results=results, status="complete"))
#	}

}