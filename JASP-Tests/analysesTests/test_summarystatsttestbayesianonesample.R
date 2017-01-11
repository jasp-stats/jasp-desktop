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


# initialize function parameters
options <- list(
            bayesFactorType = "BF10",
            hypothesis = "notEqualToTestValue",
            n1Size = 0,
            tStatistic = 0,
            plotBayesFactorRobustness = FALSE,
            plotPriorAndPosterior = FALSE,
            plotPriorAndPosteriorAdditionalInfo = FALSE,
            priorWidth = 0.707
        )

perform <- "init"


test_that("T-Test One Sample Init", {
    output <- SummaryStatsTTestBayesianOneSample(
                options = options, 
                perform = perform, 
                callback = NULL
            )

    # Sanity Check - output should not be NULL
    expect_equal(!is.null(output["results"]), TRUE)

    expectedOutput <- list()

    if (perform == "init") {
        expectedOutput = list(list(
                                BF = ".",
                                tStatistic = 0,
                                n1Size = ".",
                                errorEstimate = "."
                            )
                        )
    }
    
    perform <- output$status

    # status is 'inited' after 'init' in performed
    expect_equal(perform, "inited")
    expect_equal(output$results$table$data, expectedOutput)
})

perform <- "run"

test_that("T-Test One Sample Run", {

    # generate a random sample size
    n1Size = floor(runif(1, min = 2, max = 999999))

    # generate a random t value in the range (-10,10)
    tStatistic = runif(1, min = -10, max = 10)

    expectedBF <- BayesFactor::ttest.tstat(
                    t = tStatistic,
                    n1 = n1Size,
                    n2 = 0,
                    rscale = options$priorWidth,
                    nullInterval = NULL
                )

    expectedOutput <- list(
                        BF = .clean(exp(expectedBF$bf)),
                        tStatistic = .clean(tStatistic),
                        n1Size = n1Size,
                        errorEstimate = .clean(expectedBF$properror)
                    )

    options$tStatistic <- tStatistic
    options$n1Size <- n1Size
    output <- SummaryStatsTTestBayesianOneSample(
                options = options,
                perform = perform,
                callback = NULL
            )

    expect_equal(output$state$rowsTTestBayesianOneSample, expectedOutput)
})

