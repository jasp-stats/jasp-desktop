context("Descriptives")

# does not test
# - error handling

test_that("Main table results match", {
  options <- jasptools::analysisOptions("Descriptives")
  options$variables <- "contNormal"
  options$splitby <- "contBinom"
  options$median <- TRUE
  options$mode <- TRUE
  options$sum <- TRUE
  options$variance <- TRUE
  options$range <- TRUE
  options$standardErrorMean <- TRUE
  options$kurtosis <- TRUE
  options$skewness <- TRUE
  options$shapiro <- TRUE
  options$mode <- TRUE
  options$percentileValuesEqualGroups <- TRUE
  options$percentileValuesEqualGroupsNo <- 5
  options$percentileValuesPercentiles <- TRUE
  options$percentileValuesPercentilesPercentiles <- c(2, 5, 8)
  options$percentileValuesQuartiles <- TRUE
  results <- jasptools::run("Descriptives", "test.csv", options)
  table <- results[["results"]][["stats"]][["data"]]
  expect_equal_tables(table,
    list(1.89652072756094, 0, 3.356094448, -0.120135614827586, -0.2223981035,
				 -2.336742886, 0, -2.336742886, 0.00342000811150064, 5.692837334,
				 0.933547444665698, 0.885861572513177, 1.10575982846952, 0.618135836828014,
				 0.145193378675912, 0.313719932561217, -6.96786566, 58, "contNormal",
				 1.22270479825695, -0.8465404722, -0.6015855064, 0.189093977,
				 0.5121792992, -2.12776693668, -1.6743740472, -1.38430134284,
				 -0.77748184225, -0.2223981035, 0.38502497975, 0.972132667292966,
				 1, 2.179421126, -0.283499835571428, -0.405769511, -3.023963827,
				 0, -3.023963827, 0.401705854633909, 5.203384953, 0.972586424088514,
				 0.166587887409046, 0.994612407217046, 0.716632727345669, 0.15347202634745,
				 0.365360605557062, -11.906993094, 42, "contNormal", 0.989253840590086,
				 -0.9755913562, -0.5800195022, -0.2167812726, 0.521794901, -1.89726255948,
				 -1.61858187715, -1.43841230624, -0.80595539125, -0.405769511,
				 0.4460704255)
  )
})

test_that("Frequencies table matches", {
  options <- jasptools::analysisOptions("Descriptives")
  options$variables <- "facGender"
  options$splitby <- "contBinom"
  options$frequencyTables <- TRUE
  results <- jasptools::run("Descriptives", "test.csv", options)
  table <- results[["results"]][["tables"]][["collection"]][["tables_facGender"]][["data"]]
  expect_equal_tables(table,
  list("TRUE", 44.8275862068966, 26, "f", 44.8275862068966, 44.8275862068966,
			 0, "FALSE", 100, 32, "m", 55.1724137931034, 55.1724137931034,
			 0, "FALSE", "", 0, "Missing", 0, "", "", "FALSE", "", 58, "Total",
			 100, "", "", "TRUE", 57.1428571428571, 24, "f", 57.1428571428571,
			 57.1428571428571, 1, "FALSE", 100, 18, "m", 42.8571428571428,
			 42.8571428571428, 1, "FALSE", "", 0, "Missing", 0, "", "", "FALSE",
			 "", 42, "Total", 100, "", "")
  )
})

test_that("Frequencies table matches with missing values", {
  options <- jasptools::analysisOptions("Descriptives")
  x <- c(rep(NA, 10), rep(1:2, times=10))
  split <- rep(1:2, each=15)
  data <- data.frame(x=as.factor(x), split=as.factor(split))
  options$variables <- "x"
  options$splitby <- "split"
  options$frequencyTables <- TRUE
  results <- jasptools::run("Descriptives", data, options)
  table <- results[["results"]][["tables"]][["collection"]][["tables_x"]][["data"]]
  expect_equal_tables(table,
  list("TRUE", 60, 3, 1, 20, 60, 1, "FALSE", 100, 2, 2, 13.3333333333333,
			 40, 1, "FALSE", "", 10, "Missing", 66.6666666666667, "", "",
			 "FALSE", "", 15, "Total", 100, "", "", "TRUE", 46.6666666666667,
			 7, 1, 46.6666666666667, 46.6666666666667, 2, "FALSE", 100, 8,
			 2, 53.3333333333333, 53.3333333333333, 2, "FALSE", "", 0, "Missing",
			 0, "", "", "FALSE", "", 15, "Total", 100, "", "")
  )
})

test_that("Distribution plot matches", {
  skip("This test need to be verified")
  options <- jasptools::analysisOptions("Descriptives")
  options$variables <- "contNormal"
  options$plotVariables <- TRUE
  results <- jasptools::run("Descriptives", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  expect_equal_plots(testPlot, "distribution", dir="Descriptives")
})

test_that("Correlation plot matches", {
  options <- jasptools::analysisOptions("Descriptives")
  options$variables <- c("contNormal", "contGamma")
  options$plotCorrelationMatrix <- TRUE
  results <- jasptools::run("Descriptives", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  expect_equal_plots(testPlot, "correlation", dir="Descriptives")
})

test_that("Boxplot matches", {
  set.seed(0)
  options <- jasptools::analysisOptions("Descriptives")
  options$variables <- "contGamma"
  options$splitby <- "facFive"
  options$splitPlotBoxplot <- TRUE
  options$splitPlotColour <- TRUE
  options$splitPlotJitter <- TRUE
  options$splitPlotOutlierLabel <- TRUE
  options$splitPlotViolin <- TRUE
  options$splitPlots <- TRUE
  options$colorPalette <- "ggplot2"
  results <- jasptools::run("Descriptives", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  expect_equal_plots(testPlot, "boxplot", dir="Descriptives")
})

test_that("Q-QPlot plot matches", {
  options <- jasptools::analysisOptions("Descriptives")
  options$variables <- "contNormal"
  options$descriptivesQQPlot <- TRUE
  results <- jasptools::run("Descriptives", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  expect_equal_plots(testPlot, "qqplot", dir="Descriptives")
})

test_that("Scatter plot matches", {
  options <- jasptools::analysisOptions("Descriptives")
  options$variables <- c("contcor1", "contcor2")
  options$scatterPlot <- TRUE
  options$colorPalette <- "ggplot2"
  results <- jasptools::run("Descriptives", "test.csv", options)

  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  expect_equal_plots(testPlot, "scatterplot", dir="Descriptives")
})

test_that("Pie chart matches", {
  options <- jasptools::analysisOptions("Descriptives")
  options$variables <- "facFive"
  options$descriptivesPiechart <- TRUE
  options$colorPalette <- "ggplot2"
  results <- jasptools::run("Descriptives", "test.csv", options)
  
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  expect_equal_plots(testPlot, "pieChart", dir="Descriptives")
})

test_that("Analysis handles identical variables", {
  # catches this: https://github.com/jasp-stats/jasp-issues/issues/553
  options <- jasptools::analysisOptions("descriptives")
  options$variables <- list("contNormal", "debSame")
  options$splitby <- "facFive"
  options$shapiro <- TRUE
  options$skewness <- TRUE
  options$kurtosis <- TRUE
  
  results <- jasptools::run("descriptives", "test.csv", options)
  
  expect_equal_tables(results[['results']][['stats']][['data']],
                      list(-1.19915675837133, 1, 1.007309698, -0.33853731055, -1.625143884,
                           0, 0.0819021844894419, 0.915696014062066, 0.338805595442952,
                           0.850644958728125, 0.992383612541845, 0.512103336707757, 20,
                           "contNormal", 0.992813181737034, 2, 1.889051803, -0.38388772215,
                           -1.953344972, 0, 0.467911256938122, 0.956031076407404, 0.535107137711909,
                           0.893846327627993, 0.992383612541845, 0.512103336707757, 20,
                           "contNormal", 1.36037376866094, 3, 2.958797116, 0.1427499711,
                           -1.627592736, 0, 0.532926410776693, 0.959450290686737, 0.725619918998665,
                           1.0709839671614, 0.992383612541845, 0.512103336707757, 20, "contNormal",
                           2.42193307435088, 4, 2.179421126, -0.357863015, -3.023963827,
                           0, 0.358996514131301, 0.949461949369392, -0.0489162694087717,
                           1.04541944723916, 0.992383612541845, 0.512103336707757, 20,
                           "contNormal", 1.62009376503733, 5, 3.356094448, -0.00620486110000001,
                           -2.336742886, 0, 0.0681263561514, 0.911517098559219, 0.828552911812968,
                           1.35277978138929, 0.992383612541845, 0.512103336707757, 20,
                           "contNormal", 0, 0, 0, 0, "NaN", 1, 12.3, 12.3, 12.3, 0, "NaN",
                           "NaN", "NaN", 0, 0.992383612541845, 0.512103336707757, 20, "debSame",
                           0, 0, 0, 0, "NaN", 2, 12.3, 12.3, 12.3, 0, "NaN", "NaN", "NaN",
                           0, 0.992383612541845, 0.512103336707757, 20, "debSame", 0, 0,
                           0, 0, "NaN", 3, 12.3, 12.3, 12.3, 0, "NaN", "NaN", "NaN", 0,
                           0.992383612541845, 0.512103336707757, 20, "debSame", 0, 0, 0,
                           0, "NaN", 4, 12.3, 12.3, 12.3, 0, "NaN", "NaN", "NaN", 0, 0.992383612541845,
                           0.512103336707757, 20, "debSame", 0, 0, 0, 0, "NaN", 5, 12.3,
                           12.3, 12.3, 0, "NaN", "NaN", "NaN", 0, 0.992383612541845, 0.512103336707757,
                           20, "debSame"))
  
  # also check footnotes
  expect_equal_tables(results[['results']][['stats']][['footnotes']],
                      list("Kurtosis", "P-value of Shapiro-Wilk", "Shapiro-Wilk", "Skewness",
                           174, "debSame1", 0, "All values are identical"))
})

test_that("Analysis explains supremum and infimum of empty sets", {
  options <- analysisOptions("descriptives")
  options$variables <- "debMiss99"
  options$splitby <- "contBinom"
  
  results <- jasptools::run("descriptives", "test.csv", options)
  
  expect_equal_tables(results[['results']][['stats']][['footnotes']],
                      list("Maximum", "Minimum", 22, "debMiss991", 0, 
                           "Infimum (minimum) of an empty set is <unicode>, supremum (maximum) of an empty set is -<unicode>.")
                      )
})