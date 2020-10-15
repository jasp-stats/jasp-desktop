context("Bayesian Log-Linear Regression")

initOpts <- function() {
  options <- jaspTools::analysisOptions("RegressionLogLinearBayesian")
  options$sampleMode <- "manual"
  options$fixedSamplesNumber <- 100
  return(options)
}

test_that("Main table results match", {
  set.seed(0)
  options <- initOpts()
  options$counts <- "facFifty"
  options$factors <- c("contBinom", "facGender")
  options$modelTerms <- list(
    list(components="contBinom"),
    list(components="facGender"),
    list(components=c("contBinom", "facGender"))
  )
  options$priorScale <- 1
  options$priorShape <- 0
  options$maxModels <- 2
  options$posteriorProbabilityCutOff <- 0.001
  results <- jaspTools::run("RegressionLogLinearBayesian", "test.csv", options)
  table <- results[["results"]][["Container"]][["collection"]][["Container_MainTable"]][["data"]]
  expect_equal_tables(table,
    list(1, "contBinom + facGender", 0.963333333333333, 1, 2,
         "contBinom + facGender + contBinom<unicode><unicode><unicode>facGender",
         0.0366666666666667, 0.0380622837370242)
  )
})

test_that("General summary statistics table matches", {
  set.seed(0)
  options <- initOpts()
  options$factors <- c("contBinom", "facFive")
  options$modelTerms <- list(
    list(components="contBinom"),
    list(components="facFive"),
    list(components=c("contBinom", "facFive"))
  )
  options$regressionCoefficientsEstimates <- TRUE
  options$regressionCoefficientsCredibleIntervals <- TRUE
  options$regressionCoefficientsCredibleIntervalsInterval <- 0.90
  results <- jaspTools::run("RegressionLogLinearBayesian", "test.csv", options)
  table <- results[["results"]][["Container"]][["collection"]][["Container_SummaryTable"]][["data"]]
  expect_equal_tables(table,
    list("(Intercept)", 1, 2.28941355597128, 0.0114477565469203, 2.12177466183418,
         2.45495750281602, "contBinom = 0", 1, 0.0303566571915708, 0.00356265251723887,
         -0.0378564465631073, 0.144271031109649, "facFive = 1", 1, -0.00552214172853799,
         0.00720162344071968, -0.130345670875483, 0.138580850953902,
         "facFive = 2", 1, -0.00240692298068409, 0.00784162472732633,
         -0.121705317175139, 0.165537141029262, "facFive = 3", 1, 0.00293548076176623,
         0.00745020526106448, -0.0900898563442242, 0.187558384342406,
         "facFive = 4", 1, 0.00138448207875663, 0.00779485819569601,
         -0.122842441315205, 0.155851155312602, "contBinom = 0*facFive = 1",
         0.571428571428571, 0.00197519410432235, 0.00569094808602158,
         -0.125500914290167, 0.103584632765005, "contBinom = 0*facFive = 2",
         0.571428571428571, -0.0270367686155361, 0.00624820819787772,
         -0.180746332353481, 0.0610871410555307, "contBinom = 0*facFive = 3",
         0.571428571428571, 0.00343309539396836, 0.00598953855547496,
         -0.145370964229942, 0.0973020746027855, "contBinom = 0*facFive = 4",
         0.571428571428571, -0.0218844778500034, 0.00501003480774965,
         -0.131413707533984, 0.101000867077934)
  )
})

test_that("Submodel summary statistics table matches", {
  set.seed(0)
  options <- initOpts()
  options$factors <- c("contBinom", "facFive")
  options$modelTerms <- list(
    list(components="contBinom"),
    list(components="facFive"),
    list(components=c("contBinom", "facFive"))
  )
  options$regressionCoefficientsSubmodel <- TRUE
  options$regressionCoefficientsSubmodelCredibleIntervals <- TRUE
  options$regressionCoefficientsSubmodelEstimates <- TRUE
  options$regressionCoefficientsSubmodelNo <- 2
  results <- jaspTools::run("RegressionLogLinearBayesian", "test.csv", options)
  table <- results[["results"]][["Container"]][["collection"]][["Container_SubSummaryTable"]][["data"]]
  expect_equal_tables(table,
    list("(Intercept)", 2.29560729883006, 0.00945972825329099, 2.12809954463567,
         2.52220705393406, "contBinom = 0", 0.045757962353209, 0.00487119274553831,
         -0.0378564465631073, 0.235518792759572, "facFive = 1", -0.00411654631578624,
         0.00919225838347475, -0.189678327954162, 0.225524507296169,
         "facFive = 2", -0.00755884938209783, 0.0107688538955323, -0.230464286621307,
         0.203303292163532, "facFive = 3", 0.00693535371456428, 0.00958840576151784,
         -0.151633708350598, 0.242411127225971, "facFive = 4", 0.00306711588492548,
         0.0107120223942474, -0.234264000793185, 0.230287649489829)
  )
})

test_that("Analysis handles errors - Infinity", {
  set.seed(0)
  options <- initOpts()
  options$factors <- c("contBinom", "facFive")
  options$counts <- "debInf"
  results <- jaspTools::run("RegressionLogLinearBayesian", "test.csv", options)
  status <- results[["status"]]
  expect_identical(status, "validationError")
})

test_that("Analysis handles errors - Missing values (factor)", {
  set.seed(0)
  options <- initOpts()
  options$factors <- c("debBinMiss20", "contBinom")
  options$modelTerms <- list(
    list(components="debBinMiss20")
  )
  results <- jaspTools::run("RegressionLogLinearBayesian", "test.csv", options)
  status <- results[["status"]]
  expect_identical(status, "validationError")
})
