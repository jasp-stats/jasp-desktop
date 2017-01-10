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

.getBayesfactorTitle.summarystats.ttest <- function(bayesFactorType, hypothesis) {
	# returns the Bayes factor title to be shown on the table
	#
	# Args:
	#   bayesFactorType: the BF type selected by user
	#   hypothesis: hypothesis type selected by user
	#
	# Output:
	#   A list containing:
	#     bftitle: title of Bayes factor to be used in the output table
	#     BFH1H0: true if BF10 or Log(BF10) is selected

	hypothesisMap <- NULL
	BFH1H0 <- TRUE

	# map hypothesis type to a number
	if (hypothesis == "groupsNotEqual" ||
			hypothesis == "notEqualToTestValue" ||
			hypothesis == "correlated") {

		hypothesisMap <- 1
	} else if (hypothesis == "groupOneGreater" ||
			hypothesis == "greaterThanTestValue" ||
			hypothesis == "correlatedPositively") {

		hypothesisMap <- 2
	} else if (hypothesis == "groupTwoGreater" ||
			hypothesis == "lessThanTestValue" ||
			hypothesis == "correlatedNegatively") {

		hypothesisMap <- 3
	}

	if (bayesFactorType == "BF01") {

		BFH1H0 <- FALSE
		bf.title <- switch(
									hypothesisMap,
									"BF\u2080\u2081",
									"BF\u2080\u208A",
									"BF\u2080\u208B"
								)
	} else if (bayesFactorType == "BF10") {

		bf.title <- switch(
									hypothesisMap,
									"BF\u2081\u2080",
									"BF\u208A\u2080",
									"BF\u208B\u2080"
								)

	} else if (bayesFactorType == "LogBF10") {

		bf.title <- switch(
									hypothesisMap,
									"Log(\u2009\u0042\u0046\u2081\u2080\u2009)",
									"Log(\u2009\u0042\u0046\u208A\u2080\u2009)",
									"Log(\u2009\u0042\u0046\u208B\u2080\u2009)"
								)
	}

	return(list(bftitle = bf.title, BFH1H0 = BFH1H0))
}
