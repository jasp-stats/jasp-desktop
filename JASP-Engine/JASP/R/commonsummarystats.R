#
# Copyright (C) 2018 University of Amsterdam
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

# set BF title for main table
.getBayesfactorTitleSummaryStats <- function(bayesFactorType, hypothesis) {
	# returns the Bayes factor title to be shown on the table
	#
	# Args:
	#   bayesFactorType: the BF type selected by user
	#   hypothesis: hypothesis type selected by user
	#
	# Output:
	#   A list containing:
	#     bftitle: title of Bayes factor to be used in the output table

	hypothesisMap <- NULL

	# map hypothesis type to a number
	if (hypothesis == "twoSided" || hypothesis == "correlated") {

		hypothesisMap <- 1
	} else if (hypothesis == "plusSided" || hypothesis == "correlatedPositively") {

		hypothesisMap <- 2
	} else if (hypothesis == "minSided" || hypothesis == "correlatedNegatively") {

		hypothesisMap <- 3
	}

  switch(bayesFactorType,
    BF01 = {
      bf.title <- switch(
                    hypothesisMap,
                    gettext("BF\u2080\u2081"),
                    gettext("BF\u2080\u208A"),
                    gettext("BF\u2080\u208B")
                  )
    },
    BF10 = {
      bf.title <- switch(
                    hypothesisMap,
                    gettext("BF\u2081\u2080"),
                    gettext("BF\u208A\u2080"),
                    gettext("BF\u208B\u2080")
                  )
    },
    LogBF10 = {
      bf.title <- switch(
                    hypothesisMap,
                    gettext("Log(BF\u2081\u2080)"),
                    gettext("Log(BF\u208A\u2080)"),
                    gettext("Log(BF\u208B\u2080)")
                  )
    }
  )

	return(bf.title)
}

# citations for summary stats module
.summaryStatsCitations <- c(
  "GronauEtAl2017"     = "Gronau, Q. F., Ly, A., & Wagenmakers, E.-J. (2017). Informed Bayesian T-Tests. The American Statistician.",
  "Haldane1932"        = "Haldane, J. B. S. (1932). A note on inverse probability. Mathematical Proceedings of the Cambridge Philosophical Society, 28, 55-61.",
  "Jeffreys1961"       = "Jeffreys, H. (1961). Theory of Probability. Oxford, Oxford University Press.",
  "LiangEtAl2008"      = "Liang, F. and Paulo, R. and Molina, G. and Clyde, M. A. and Berger, J. O. (2008). Mixtures of g-priors for Bayesian Variable Selection. Journal of the American Statistical Association, 103, pp. 410-423",
  "LyEtAl2016"         = "Ly, A., Verhagen, A. J. & Wagenmakers, E.-J. (2016). Harold Jeffreys's Default Bayes Factor Hypothesis Tests: Explanation, Extension, and Application in Psychology. Journal of Mathematical Psychology, 72, 19-32.",
  "MoreyRounder2015"   = "Morey, R. D., & Rouder, J. N. (2015). BayesFactor (Version 0.9.11-3)[Computer software].",
  "OHaganForster2004"  = "O'Hagan, A., & Forster, J. (2004). Kendall's advanced theory of statistics vol. 2B: Bayesian inference (2nd ed.). London: Arnold.",
  "RounderEtAl2009"    = "Rouder, J. N., Speckman, P. L., Sun, D., Morey, R. D., & Iverson, G. (2009). Bayesian t tests for accepting and rejecting the null hypothesis. Psychonomic Bulletin & Review, 16, 225–237.",
  "RouderMoreyInPress" = "Rouder, J. N. and Morey, R. D. (in press). Bayesian testing in regression. Multivariate Behavioral Research."
)
