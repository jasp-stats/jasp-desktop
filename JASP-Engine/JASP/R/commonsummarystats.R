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

	if (bayesFactorType == "BF01") {
		BFH1H0 <- FALSE

		if (hypothesis == "groupsNotEqual" || hypothesis == "notEqualToTestValue" ||
				hypothesis == "correlated") {
			bf.title <- "BF\u2080\u2081"
		} else if (hypothesis == "groupOneGreater" ||
				hypothesis == "greaterThanTestValue" ||
				hypothesis == "correlatedPositively") {
			bf.title <- "BF\u2080\u208A"
		} else if (hypothesis == "groupTwoGreater" || hypothesis == "lessThanTestValue") {
			bf.title <-  "BF\u2080\u208B"
		}
	} else if (bayesFactorType == "BF10") {
		BFH1H0 <- TRUE

		if (hypothesis == "groupsNotEqual" || hypothesis == "notEqualToTestValue") {
			bf.title <- "BF\u2081\u2080"
		} else if (hypothesis == "groupOneGreater" || hypothesis == "greaterThanTestValue") {
			bf.title <- "BF\u208A\u2080"
		} else if (hypothesis == "groupTwoGreater" || hypothesis == "lessThanTestValue") {
			bf.title <- "BF\u208B\u2080"
		}
	} else if (bayesFactorType == "LogBF10") {
		BFH1H0 <- TRUE

		if (hypothesis == "groupsNotEqual" || hypothesis == "notEqualToTestValue") {
			bf.title <- "Log(\u2009\u0042\u0046\u2081\u2080\u2009)"
		} else if (hypothesis == "groupOneGreater" || hypothesis == "greaterThanTestValue") {
			bf.title <-"Log(\u2009\u0042\u0046\u208A\u2080\u2009)"
		} else if (hypothesis == "groupTwoGreater" || hypothesis == "lessThanTestValue") {
			bf.title <- "Log(\u2009\u0042\u0046\u208B\u2080\u2009)"
		}
	}

	return(list(bftitle = bf.title, BFH1H0 = BFH1H0))
}
