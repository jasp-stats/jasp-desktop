context("Logistic Regression")

# Below are the unit tests for Andy Field's book

# Chapter 10
test_that("Fields Book - Chapter 10 results match", {
  options <- jasptools::analysisOptions("RegressionLogistic")
  options$dependent <- "delivered"
  options$factors <- c("treat")
  options$modelTerms <- list(
    list(components="treat", isNuisance=FALSE)
  )
  options$factorDescriptivesOpt <- TRUE
  options$oddsRatios <- TRUE
  options$coeffCI <- TRUE
  options$coeffCIOR <- TRUE
  results <- jasptools::run("RegressionLogistic", dataset = "santas_log.csv", options)
  output1 <- results[["results"]][["factorDescriptives"]][["data"]]
  expect_equal_tables(output1,
                      list(0, 178, "TRUE",
                           1, 222, "FALSE")
  )
  output2 <- results[["results"]][["modelSummary"]][["data"]]
  expect_equal_tables(output2,
                      list("H<unicode>", 529.250590526386, 531.250590526386, 535.242055073494,
                           399, "", "", "", "", "", "", 
                           "H<unicode>", 460.494488544053,
                           464.494488544053, 472.477417638269, 398, 68.7561019823323, 1.11022302462516e-16,
                           0.129912187559297, 0.215249724800182, 0.162128420464791, 0.157928418719378)
  )
  output3 <- results[["results"]][["estimatesTable"]][["data"]]
  expect_equal_tables(output3,
                      list("(Intercept)", 1.678431, 0.2058631, 
                           5.357143, 8.15314, 3.54595e-16, 66.47369, 1, 
                           3.57851, 8.019813,
                           "treat (1)", -1.877282, 0.2461223, 
                           0.1530055, -7.627436, 2.394692e-14, 58.17778, 1, 
                           0.09445116, 0.2478601)
  )
  options <- jasptools::analysisOptions("RegressionLogistic")
  options$dependent <- "delivered"
  options$factors <- c("treat")
  options$covariates <- c("quantity")
  options$modelTerms <- list(
    list(components="treat", isNuisance=FALSE),
    list(components="quantity", isNuisance=FALSE),
    list(components=list("treat", "quantity"), isNuisance=FALSE)
  )
  options$oddsRatios <- TRUE
  options$coeffCI <- TRUE
  options$coeffCIOR <- TRUE
  results <- jasptools::run("RegressionLogistic", dataset = "santas_log.csv", options)
  output4 <- results[["results"]][["modelSummary"]][["data"]]
  expect_equal_tables(output4,
                      list("H<unicode>", 529.250590526386, 531.250590526386, 535.242055073494,
                           399, "", "", "", "", "", "", 
                           "H<unicode>", 390.185034282251,
                           398.185034282251, 414.150892470683, 396, 139.065556244135, 0,
                           0.262759378512591, 0.400251224473979, 0.331341240867329, 0.293663757434995)
  )
  output5 <- results[["results"]][["estimatesTable"]][["data"]]
  expect_equal_tables(output5,
                      list("(Intercept)", 1.829715, 0.3810035, 6.232108, 4.802356, 1.568094e-06, 23.06263, 1, 2.953413, 13.1506,
                           "treat (1)", 0.199482, 0.520007, 1.22077, 0.3836141, 0.7012645, 0.1471597, 1, 0.4405581, 3.382709,
                           "quantity", -0.08100546, 0.1678705, 0.9221887, -0.4825472, 0.6294173, 0.2328518, 1, 0.6636332, 1.281479,
                           "treat (1) * quantity", -1.027643, 0.2309776, 0.3578493, -4.449103, 8.622966e-06, 19.79452, 1, 0.2275578, 0.5627412)
  )
  
  options <- jasptools::analysisOptions("RegressionLogistic")
  options$dependent <- "delivered"
  options$covariates <- c("quantity")
  options$modelTerms <- list(
    list(components="quantity", isNuisance=FALSE)
  )
  options$oddsRatios <- TRUE
  options$coeffCI <- TRUE
  options$coeffCIOR <- TRUE
  options$estimatesPlotsOpt <- TRUE
  options$showPoints <- FALSE
  results <- jasptools::run("RegressionLogistic", dataset = "santas_log_subset_treat0.csv", options)
  output6 <- results[["results"]][["estimatesTable"]][["data"]]
  expect_equal_tables(output6,
                      list("(Intercept)", 1.829715, 0.3810035, 6.232108, 4.802356, 1.568094e-06, 23.06263, 1, 2.953413, 13.1506,
                           "quantity", -0.08100546, 0.1678705, 0.9221887, -0.4825472, 0.6294173, 0.2328518, 1, 0.6636332, 1.281479)
  )
  unnumberedFigureA <- results[["state"]][["estimatesPlots"]][["collection"]][[1]]
  #expect_equal_plots(unnumberedFigureA, "?", dir="Ancova") # This command needs to be updated
  
  options <- jasptools::analysisOptions("RegressionLogistic")
  options$dependent <- "delivered"
  options$covariates <- c("quantity")
  options$modelTerms <- list(
    list(components="quantity", isNuisance=FALSE)
  )
  options$oddsRatios <- TRUE
  options$coeffCI <- TRUE
  options$coeffCIOR <- TRUE
  options$estimatesPlotsOpt <- TRUE
  options$showPoints <- FALSE
  results <- jasptools::run("RegressionLogistic", dataset = "santas_log_subset_treat1.csv", options)
  output7 <- results[["results"]][["estimatesTable"]][["data"]]
  expect_equal_tables(output7,
                      list("(Intercept)", 2.029197, 0.3538977, 7.607972, 5.73385, 9.817603e-09, 32.87704, 1, 3.802162, 15.22324,
                           "quantity", -1.108649, 0.158651, 0.3300046, -6.987972, 2.788889e-12, 48.83175, 1, 0.241811, 0.4503643)
  )
  unnumberedFigureB <- results[["state"]][["estimatesPlots"]][["collection"]][[1]]
  #expect_equal_plots(unnumberedFigureB, "?", dir="Ancova") # This command needs to be updated
  
  options <- jasptools::analysisOptions("RegressionLogistic")
  options$dependent <- "delivered"
  options$factors <- c("treat")
  options$covariates <- c("quantity")
  options$modelTerms <- list(
    list(components="treat", isNuisance=FALSE),
    list(components="quantity", isNuisance=FALSE),
    list(components=list("treat", "quantity"), isNuisance=FALSE)
  )
  options$casewiseDiagnostics <- TRUE
  options$casewiseDiagnosticsResidualZ <- 2
  options$coeffEstimatesBootstrapping <- TRUE
  options$coeffEstimatesBootstrappingReplicates <- 1000
  set.seed(1) # For Bootstrapping Unit Tests
  results <- jasptools::run("RegressionLogistic", dataset = "santas_log.csv", options)
  output9 <- results[["results"]][["casewiseDiagnosticsTable"]][["data"]]
  
  expect_equal_tables(output9,
                      list(18,	0.0121239460461442,		0,	1,	0.851789911140433,	-0.851789911140433,	-2.39732747153848,
                           40,	0.00763627653324742,	0,	1,	0.841269420104102,	-0.841269420104102,	-2.30216925615302,
                           51,	0.0279057279786341,		0,	1,	0.861727722493428,	-0.861727722493428,	-2.49641897112151,
                           64,	0.0121239460461442,		0,	1,	0.851789911140433,	-0.851789911140433,	-2.39732747153848,
                           78,	0.0133723433832867,		0,	1,	0.830151056615241,	-0.830151056615241,	-2.21078819931717,
                           90,	0.00763627653324742,	0,	1,	0.841269420104102,	-0.841269420104102,	-2.30216925615302,
                           93,	0.0272626386242438,		1,	0,	0.0827619829298745,	0.917238017070126,	3.32909033263681,
                           96,	0.0133723433832867,		0,	1,	0.830151056615241,	-0.830151056615241,	-2.21078819931717,
                           98,	0.0291505277819276,		0,	1,	0.818421994612829,	-0.818421994612829,	-2.12303437254972,
                           105,	0.0279057279786341,		0,	1,	0.861727722493428,	-0.861727722493428,	-2.49641897112151,
                           111,	0.0279057279786341,		0,	1,	0.861727722493428,	-0.861727722493428,	-2.49641897112151,
                           112,	0.00763627653324742,	0,	1,	0.841269420104102,	-0.841269420104102,	-2.30216925615302,
                           113,	0.0121239460461442,		0,	1,	0.851789911140433,	-0.851789911140433,	-2.39732747153848,
                           138,	0.0121239460461442,		0,	1,	0.851789911140433,	-0.851789911140433,	-2.39732747153848,
                           162,	0.0291505277819276,		0,	1,	0.818421994612829,	-0.818421994612829,	-2.12303437254972,
                           170,	0.00763627653324742,	0,	1,	0.841269420104102,	-0.841269420104102,	-2.30216925615302,
                           188,	0.0272626386242438,		1,	0,	0.0827619829298745,	0.917238017070126,	3.32909033263681,
                           195,	0.0121239460461442,		0,	1,	0.851789911140433,	-0.851789911140433,	-2.39732747153848,
                           214,	0.0251000549472288,		0,	1,	0.883828611727055,	-0.883828611727055,	-2.75825515596946,
                           215,	0.0133723433832867,		0,	1,	0.830151056615241,	-0.830151056615241,	-2.21078819931717,
                           219,	0.0121239460461442,		0,	1,	0.851789911140433,	-0.851789911140433,	-2.39732747153848,
                           222,	0.0291505277819276,		0,	1,	0.818421994612829,	-0.818421994612829,	-2.12303437254972,
                           249,	0.00763627653324742,	0,	1,	0.841269420104102,	-0.841269420104102,	-2.30216925615302,
                           258,	0.0133723433832867,		0,	1,	0.830151056615241,	-0.830151056615241,	-2.21078819931717,
                           265,	0.0272626386242438,		1,	0,	0.0827619829298745,	0.917238017070126,	3.32909033263681,
                           270,	0.0291505277819276,		0,	1,	0.818421994612829,	-0.818421994612829,	-2.12303437254972,
                           285,	0.0133723433832867,		0,	1,	0.830151056615241,	-0.830151056615241,	-2.21078819931717,
                           288,	0.0121239460461442,		0,	1,	0.851789911140433,	-0.851789911140433,	-2.39732747153848,
                           301,	0.0121239460461442,		0,	1,	0.851789911140433,	-0.851789911140433,	-2.39732747153848,
                           307,	0.0251000549472288,		0,	1,	0.883828611727055,	-0.883828611727055,	-2.75825515596946,
                           335,	0.0121239460461442,		0,	1,	0.851789911140433,	-0.851789911140433,	-2.39732747153848,
                           370,	0.0121239460461442,		0,	1,	0.851789911140433,	-0.851789911140433,	-2.39732747153848,
                           374,	0.0133723433832867,		0,	1,	0.830151056615241,	-0.830151056615241,	-2.21078819931717,
                           382,	0.0251000549472288,		0,	1,	0.883828611727055,	-0.883828611727055,	-2.75825515596946)
  )
  output10 <- results[["results"]][["estimatesTableBootstrapping"]][["data"]]
  expect_equal_tables(output10,
                      list("TRUE", 0.0499135607262049, 1.8487167467187, "(Intercept)", 0.408753000376651,
                           "FALSE", -0.00676602090084683, 0.221423402843459, "treat (1)",
                           0.545714072912926, "FALSE", -0.0107304860810226, -0.0951900669778652,
                           "quantity", 0.176386961737937, "FALSE", -0.0115090562401219,
                           -1.0338758817337, "treat (1) * quantity", 0.245734251200521
                      )
  )
})

# test the methods for entering predictors
options <- jasptools::analysisOptions("RegressionLogistic")
options$covariates <- list("contNormal")
options$dependent <- "contBinom"
options$modelTerms <- list(list(components = list("contNormal"), isNuisance = FALSE))
  
#backward
test_that("Method=backward model summary table results match", {
  options$method <- "backward"
  results <- jasptools::run("RegressionLogistic", "debug.csv", options)
	table <- results[["results"]][["modelSummary"]][["data"]]
	expect_equal_tables(table,
    list(139.466477068092, 144.676817440068, 0, 135.466477068092, 98, 0,
         1, 0, 0.00577009513822135, 138.058400038431, 140.663570224419,
         -0.00590174557777201, 0.59192297033843, 136.058400038431, 99,
         -0.00435050662194492, 2, -0.00793790496945946, 0.441676479938567,
         0))
})

#forward
test_that("Method=forward model summary table results match", {
  options$method <- "forward"
  results <- jasptools::run("RegressionLogistic", "debug.csv", options)
	table <- results[["results"]][["modelSummary"]][["data"]]
	expect_equal_tables(table,
		list(1, 136.058400038431, 138.058400038431, 140.663570224419, 99, 0, 0))
})

#stepwise
test_that("Method=stepwise model summary table results match", {
  options$method <- "stepwise"
  results <- jasptools::run("RegressionLogistic", "debug.csv", options)
	table <- results[["results"]][["modelSummary"]][["data"]]
	expect_equal_tables(table,
    list(138.058400038431, 140.663570224419, 136.058400038431, 99, 0, 1,0))
})

test_that("Confusion Matrix Table Matches", {
  options <- jasptools::analysisOptions("RegressionLogistic")
  options$covariates <- list("contNormal")
  options$dependent  <- "contBinom"
  options$modelTerms <- list(
    list(components="contNormal", isNuisance=FALSE)
  )
  options$confusionMatrixOpt <- TRUE
  results <- jasptools::run("RegressionLogistic", "debug.csv", options)
  table <- results[["results"]][["perfDiag"]][["collection"]][["perfDiag_confusionMatrix"]][["data"]]
  expect_equal_tables(table, list(0, 58, 0, 1, 41, 1))
  
  options$confusionMatrixProportions <- TRUE
  results <- jasptools::run("RegressionLogistic", "debug.csv", options)
  table <- results[["results"]][["perfDiag"]][["collection"]][["perfDiag_confusionMatrix"]][["data"]]
  expect_equal_tables(table, list(0, 0.58, 0, 1, 0.41, 0.01))
})

test_that("Performance Metrics Table Matches", {
  options <- jasptools::analysisOptions("RegressionLogistic")
  options$covariates <- list("contNormal")
  options$dependent  <- "contBinom"
  options$modelTerms <- list(
    list(components="contNormal", isNuisance=FALSE)
  )
  options$AUC  <- TRUE
  options$Sens <- TRUE
  options$Spec <- TRUE
  options$Prec <- TRUE
  options$Fmsr <- TRUE
  options$BrierScr  <- TRUE
  options$Hmsr <- TRUE
  results <- jasptools::run("RegressionLogistic", "debug.csv", options)
  table <- results[["results"]][["perfDiag"]][["collection"]][["perfDiag_performanceMetrics"]][["data"]]
  expect_equal_tables(table, 
                      list("AUC", 0.529556650246305, "Sensitivity", 0.0238095238095238, "Specificity",
                           1, "Precision", 1, "F-measure", 0.0465116279069767, "Brier score",
                           0.242217791647847, "H-measure", 0.0686265172331011)
  )
  
  options <- jasptools::analysisOptions("RegressionLogistic")
  options$covariates <- list("contNormal")
  options$dependent  <- "contBinom"
  options$modelTerms <- list(
    list(components="contNormal", isNuisance=FALSE)
  )
  options$AUC  <- TRUE
  options$Sens <- TRUE
  options$Prec <- TRUE
  options$Fmsr <- TRUE
  options$Hmsr <- TRUE
  results <- jasptools::run("RegressionLogistic", "debug.csv", options)
  table <- results[["results"]][["perfDiag"]][["collection"]][["perfDiag_performanceMetrics"]][["data"]]
  expect_equal_tables(table, 
                      list("AUC", 0.529556650246305, "Sensitivity", 0.0238095238095238, "Precision",
                           1, "F-measure", 0.0465116279069767, "H-measure", 0.0686265172331011
                      )
  )
})

test_that("Error Handling", {
  # factor levels not equal to 2
  options <- jasptools::analysisOptions("RegressionLogistic")
  options$covariates <- list("contNormal")
  options$dependent <- "facFive"
  results <- jasptools::run("RegressionLogistic", "debug.csv", options)
  status <- results[["status"]]
  expect_identical(status, "validationError")
  
  # infinity check
  options <- jasptools::analysisOptions("RegressionLogistic")
  options$covariates <- list("debInf")
  options$dependent <- "contBinom"
  results <- jasptools::run("RegressionLogistic", "debug.csv", options)
  status <- results[["status"]]
  expect_identical(status, "validationError")
  
  # <2 observations 
  options <- jasptools::analysisOptions("RegressionLogistic")
  options$covariates <- list("debMiss99")
  options$dependent <- "contBinom"
  results <- jasptools::run("RegressionLogistic", "debug.csv", options)
  status <- results[["status"]]
  expect_identical(status, "validationError")
  
  # variance check
  options <- jasptools::analysisOptions("RegressionLogistic")
  options$covariates <- list("debSame")
  options$dependent <- "contBinom"
  results <- jasptools::run("RegressionLogistic", "debug.csv", options)
  status <- results[["status"]]
  expect_identical(status, "validationError")
})

test_that("Pseudo R-squared are correct", {
  # Specifically, tests McFadden, Nagelkerke, Tjur, and Cox & Snell coefficients
  # Example from Tjur (2008) http://dx.doi.org/10.1198/tast.2009.08210, page 370
  # Tjur's results for Tjur R2: 0.096231
  #.
  # Check against code from performance package
  # library(aplore3)
  # library(performance)
  # data("lowbwt", package = "aplore3")
  # fit <- glm(low ~ age + lwt + race + smoke, data = lowbwt, family = binomial)
  #
  # performance::r2_mcfadden(fit)$R2
  # McFadden's R2: 0.08562914
  #
  # performance::r2_nagelkerke(fit)
  # Nagelkerke's R2: 0.1418442 
  #
  # performance::r2_tjur(fit)
  # Tjur's R2: 0.09623107 
  #
  # performance::r2_coxsnell(fit)
  # Cox & Snell's R2: 0.1008645 
  
  options            <- jasptools::analysisOptions("regressionlogistic")
  options$dependent  <- "low"
  options$covariates <- c("age", "lwt")
  options$factors    <- c("race", "smoke")
  options$modelTerms <- list(list(components = "age",   isNuisance = FALSE),
                             list(components = "lwt",   isNuisance = FALSE),
                             list(components = "race",  isNuisance = FALSE),
                             list(components = "smoke", isNuisance = FALSE)
  )
  
  results <- jasptools::run("regressionlogistic", "lowbwt.csv", options)
  r_squared <- results$results$modelSummary$data[[2]][c("fad", "nag", "tju", "cas")]
  expect_equal_tables(r_squared,
                      list(0.0856291418878957, 0.141844242772774, 0.0962310669224921, 0.100864461712579)
                      )
})