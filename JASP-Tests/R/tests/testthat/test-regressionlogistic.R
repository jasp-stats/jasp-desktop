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
  results <- jasptools::run("RegressionLogistic", dataset = rio::import("~/Dropbox/ej_andy_shared/spss_tutorials/spss_glm_10/www/santas_log.sav"), options, view=FALSE, quiet=TRUE)
  output1 <- results[["results"]][["factorDescriptives"]][["data"]]
  expect_equal_tables(output1,
                      list(0, 178, "TRUE",
                           1, 222, "FALSE")
  )
  output2 <- results[["results"]][["modelSummary"]][["data"]]
  expect_equal_tables(output2,
                      list("H<unicode>", 529.2506, 531.2506, 535.2421, 399, "", "", "", "", "", "",
                           "H<unicode>", 460.4945, 464.4945, 472.4774, 398, 68.7561, 1.110223e-16, 0.1299122, 0.2152497, 0, 0.1579284)
  )
  output3 <- results[["results"]][["estimatesTable"]][["data"]]
  expect_equal_tables(output3,
                      list("(Intercept)", 1.678431, 0.2058631, 1.678431, 5.357143, 8.15314, 3.54595e-16, 66.47369, 1, 2.916226e+13, 3.57851, 8.019813,
                           "treat (1)", -1.877282, 0.2461223, -1.877282, 0.1530055, -7.627436, 2.394692e-14, 58.17778, 1, 4.89823e+11, 0.09445116, 0.2478601)
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
  results <- jasptools::run("RegressionLogistic", dataset = rio::import("~/Dropbox/ej_andy_shared/spss_tutorials/spss_glm_10/www/santas_log.sav"), options, view=FALSE, quiet=TRUE)
  output4 <- results[["results"]][["modelSummary"]][["data"]]
  expect_equal_tables(output4,
                      list("H<unicode>", 529.2506, 531.2506, 535.2421, 399, "", "", "", "", "", "",
                           "H<unicode>", 390.185, 398.185, 414.1509, 396, 139.0656, 0, 0.2627594, 0.4002512, 0, 0.2936638)
  )
  output5 <- results[["results"]][["estimatesTable"]][["data"]]
  expect_equal_tables(output5,
                      list("(Intercept)", 1.829715, 0.3810035, 1.671349, 6.232108, 4.802356, 1.568094e-06, 23.06263, 1, 17552.68, 2.953413, 13.1506,
                           "treat (1)", 0.199482, 0.520007, -0.09976397, 1.22077, 0.3836141, 0.7012645, 0.1471597, 1, 1, 0.4405581, 3.382709,
                           "quantity", -0.08100546, 0.1678705, -1.809561, 0.9221887, -0.4825472, 0.6294173, 0.2328518, 1, 1, 0.6636332, 1.281479,
                           "treat (1) * quantity", -1.027643, 0.2309776, -1.265616, 0.3578493, -4.449103, 8.622966e-06, 19.79452, 1, 3658.559, 0.2275578, 0.5627412)
  )
  
  dataTreat0 <- rio::import("~/Dropbox/ej_andy_shared/spss_tutorials/spss_glm_10/www/santas_log.sav")
  dataTreat0 <- dataTreat0[dataTreat0$treat == 0, ]
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
  results <- jasptools::run("RegressionLogistic", dataset = dataTreat0, options, view=FALSE, quiet=TRUE)
  output6 <- results[["results"]][["estimatesTable"]][["data"]]
  expect_equal_tables(output6,
                      list("(Intercept)", 1.829715, 0.3810035, 1.681811, 6.232108, 4.802356, 1.568094e-06, 23.06263, 1, 17552.68, 2.953413, 13.1506,
                           "quantity", -0.08100546, 0.1678705, -0.09960282, 0.9221887, -0.4825472, 0.6294173, 0.2328518, 1, 1, 0.6636332, 1.281479)
  )
  unnumberedFigureA <- results[["state"]][["estimatesPlots"]][["collection"]][[1]]
  #expect_equal_plots(unnumberedFigureA, "?", dir="Ancova") # This command needs to be updated
  
  dataTreat1 <- rio::import("~/Dropbox/ej_andy_shared/spss_tutorials/spss_glm_10/www/santas_log.sav")
  dataTreat1 <- dataTreat1[dataTreat1$treat == 1, ]
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
  results <- jasptools::run("RegressionLogistic", dataset = dataTreat1, options, view=FALSE, quiet=TRUE)
  output7 <- results[["results"]][["estimatesTable"]][["data"]]
  expect_equal_tables(output7,
                      list("(Intercept)", 2.029197, 0.3538977, -0.2530217, 7.607972, 5.73385, 9.817603e-09, 32.87704, 1, 2032173, 3.802162, 15.22324,
                           "quantity", -1.108649, 0.158651, -1.359324, 0.3300046, -6.987972, 2.788889e-12, 48.83175, 1, 4957980522, 0.241811, 0.4503643)
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
  results <- jasptools::run("RegressionLogistic", dataset = rio::import("~/Dropbox/ej_andy_shared/spss_tutorials/spss_glm_10/www/santas_log.sav"), options, view=FALSE, quiet=TRUE)
  output9 <- results[["results"]][["casewiseDiagnosticsTable"]][["data"]]
  expect_equal_tables(output9,
                      list(18, 0, 0.8517899, 1, -0.8517899, -2.397327, 0.01212395,
                           40, 0, 0.8412694, 1, -0.8412694, -2.302169, 0.007636277,
                           51, 0, 0.8617277, 1, -0.8617277, -2.496419, 0.02790573,
                           64, 0, 0.8517899, 1, -0.8517899, -2.397327, 0.01212395,
                           78, 0, 0.8301511, 1, -0.8301511, -2.210788, 0.01337234,
                           90, 0, 0.8412694, 1, -0.8412694, -2.302169, 0.007636277,
                           93, 1, 0.08276198, 0, 0.917238, 3.32909, 0.02726264,
                           96, 0, 0.8301511, 1, -0.8301511, -2.210788, 0.01337234,
                           98, 0, 0.818422, 1, -0.818422, -2.123034, 0.02915053,
                           105, 0, 0.8617277, 1, -0.8617277, -2.496419, 0.02790573,
                           111, 0, 0.8617277, 1, -0.8617277, -2.496419, 0.02790573,
                           112, 0, 0.8412694, 1, -0.8412694, -2.302169, 0.007636277,
                           113, 0, 0.8517899, 1, -0.8517899, -2.397327, 0.01212395,
                           138, 0, 0.8517899, 1, -0.8517899, -2.397327, 0.01212395,
                           162, 0, 0.818422, 1, -0.818422, -2.123034, 0.02915053,
                           170, 0, 0.8412694, 1, -0.8412694, -2.302169, 0.007636277,
                           188, 1, 0.08276198, 0, 0.917238, 3.32909, 0.02726264,
                           195, 0, 0.8517899, 1, -0.8517899, -2.397327, 0.01212395,
                           214, 0, 0.8838286, 1, -0.8838286, -2.758255, 0.02510005,
                           215, 0, 0.8301511, 1, -0.8301511, -2.210788, 0.01337234,
                           219, 0, 0.8517899, 1, -0.8517899, -2.397327, 0.01212395,
                           222, 0, 0.818422, 1, -0.818422, -2.123034, 0.02915053,
                           249, 0, 0.8412694, 1, -0.8412694, -2.302169, 0.007636277,
                           258, 0, 0.8301511, 1, -0.8301511, -2.210788, 0.01337234,
                           265, 1, 0.08276198, 0, 0.917238, 3.32909, 0.02726264,
                           270, 0, 0.818422, 1, -0.818422, -2.123034, 0.02915053,
                           285, 0, 0.8301511, 1, -0.8301511, -2.210788, 0.01337234,
                           288, 0, 0.8517899, 1, -0.8517899, -2.397327, 0.01212395,
                           301, 0, 0.8517899, 1, -0.8517899, -2.397327, 0.01212395,
                           307, 0, 0.8838286, 1, -0.8838286, -2.758255, 0.02510005,
                           335, 0, 0.8517899, 1, -0.8517899, -2.397327, 0.01212395,
                           370, 0, 0.8517899, 1, -0.8517899, -2.397327, 0.01212395,
                           374, 0, 0.8301511, 1, -0.8301511, -2.210788, 0.01337234,
                           382, 0, 0.8838286, 1, -0.8838286, -2.758255, 0.02510005)
  )
  output10 <- results[["results"]][["estimatesTableBootstrapping"]][["data"]]
  expect_equal_tables(output10,
                      list(2, "(Intercept)", 1.829715, 0.04991356, 0.408753, 1.109881, 2.705956, "TRUE",
                           2, "treat (1)", 0.199482, -0.006766021, 0.5457141, -1.015603, 1.165043, "FALSE",
                           2, "quantity", -0.08100546, -0.01073049, 0.176387, -0.4214278, 0.2660843, "FALSE",
                           2, "treat (1) * quantity", -1.027643, -0.01150906, 0.2457343, -1.504704, -0.524605, "FALSE")
  )
})
