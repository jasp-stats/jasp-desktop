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
.getBayesfactorTitle.summarystats <- function(bayesFactorType, hypothesis) {
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
	if (hypothesis == "twoSided") {

		hypothesisMap <- 1
	} else if (hypothesis == "plusSided") {

		hypothesisMap <- 2
	} else if (hypothesis == "minSided") {

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
									"Log(\u0042\u0046\u2081\u2080)",
									"Log(\u0042\u0046\u208A\u2080)",
									"Log(\u0042\u0046\u208B\u2080)"
								)
	}

	return(bf.title)
}

# set BF subscripts for prior and posterior plot
.setBFsubscripts.summarystats <- function(hypothesis){
  
  if (hypothesis == "twoSided") {
    
    bf.subscripts <- "BF[1][0]"
    
  } else if (hypothesis == "plusSided") {
    
    bf.subscripts <- "BF['+'][0]"
    
  } else if (hypothesis == "minSided") {
    
    bf.subscripts <- "BF['-'][0]"
  }
  
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


# t-test BF from summary statistic function 

# TODO(raoul): - Change to combined one-sample/two-sample/dependent t-test function
#              - Add uniform informed prior

.generalSummaryTtestBF <- function(tValue=options$tStatistic, size=options$n1Size, options, paired=TRUE) {
    # Converts a t-statistic and sample size into the corresponding Bayes Factor.
    #
    # Args:
    #   tValue:  the value of the t-statistic to be converted
    #   size:    the sample size underlying the t-statistic
    #   options: options object passed from JASP
    #
    # Value:
    #   list with components:
    #     bf:         the Bayes Factor
    #     properror:  percentage of error in BF estimate
    #     tValue:     the input t-statistic
    #     n1:         the sample size
    #     pValue:     p-value associated with tValue and n1
    
    # help vars
    n1 <- size
    n2 <- if (!is.null(options$n2Size)) options$n2Size else 0 # single sample case
    oneSided = !(options$hypothesis %in% c("notEqualToTestValue","groupsNotEqual"))
    
    ### Default case: a non-informative zero-centered Cauchy prior
    if(options$effectSizeStandardized == "default") {
      nullInterval <-
        switch(options$hypothesis, greaterThanTestValue = c(0, Inf), groupOneGreater = c(0, Inf),
               lessThanTestValue = c(-Inf,0), groupTwoGreater = c(-Inf, 0), c(-Inf,Inf))    # default is notEqualToTestValue
      
      bfObject <- BayesFactor::ttest.tstat(t=tValue, n1=n1, n2=n2, rscale=options$priorWidth,
                                           nullInterval = nullInterval)
      bf <- exp(bfObject$bf)
      error <- 100*bfObject$properror
    }
    
    ### Informed prior case: non-central scaled Cauchy, Student t, or Normal (uniform is lacking?)
    if (options$effectSizeStandardized == "informative") {
      # Note that strictly speaking, in case of the independent samples t-test,
      # for the informed prior n1 corresponds to n1 and n2 to n2 and not vice-versa.
      # However, since in the expression for the Bayes factor they only appear
      # as an "effective" sample size and in the degrees of freedom for which it does
      # not matter whether we swap the two, we retain this order for easier extension
      # of the one-sample case.
      
      side = switch(options$hypothesis, greaterThanTestValue = "right", groupOneGreater = "right",
                    lessThanTestValue= "left", groupTwoGreater = "left", FALSE)
      
      # Note: .bf10_ functions gives weired value if paired = FALSE in single sample case
      if (options[["informativeStandardizedEffectSize"]] == "cauchy") {
        bfObject <- .bf10_t(t = tValue, n1 = n1, n2 = n2, oneSided = side,
                            independentSamples = !paired,
                            prior.location = options[["informativeCauchyLocation"]],
                            prior.scale = options[["informativeCauchyScale"]],
                            prior.df = 1)
        bf <- bfObject$bf
        error <- 100*bfObject$error
      } else if (options[["informativeStandardizedEffectSize"]] == "t") {
        bfObject <- .bf10_t(t = tValue, n1 = n1, n2 = n2, oneSided = side,
                            independentSamples = !paired,
                            prior.location = options[["informativeTLocation"]],
                            prior.scale = options[["informativeTScale"]],
                            prior.df = options[["informativeTDf"]])
        bf <- bfObject$bf
        error <- 100*bfObject$error
      } else if (options[["informativeStandardizedEffectSize"]] == "normal") {
        bf <- .bf10_normal(t = tValue, n1 = n1, n2 = n2, oneSided = side,
                           independentSamples = !paired,
                           prior.mean = options[["informativeNormalMean"]],
                           prior.variance = options[["informativeNormalStd"]]^2)
        error <- NULL
      }
    }
    result <- list(bf = bf, properror = error, tValue = tValue, n1 = n1,
                   pValue = .pValueFromT(t=tValue, n1=n1, n2=n2))
    return(result)
  }
