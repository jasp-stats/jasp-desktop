context("Bayesian Linear Regression")

# does not test
# - plots (will/ should be updated in the future)

test_that("Main tables results match", {
    set.seed(1)
    options <- jasptools::analysisOptions("RegressionLinearBayesian")
    options$dependent <- "contNormal"
    options$covariates <- "contGamma"
    options$wlsWeights <- "facFifty"
    options$modelTerms <- list(
        list(components="contGamma", isNuisance=FALSE)
    )
    options$postSummaryTable <- TRUE
    options$descriptives <- TRUE
    
    results <- jasptools::run("RegressionLinearBayesian", "test.csv", options)
    table <- results[["results"]][["regressionTable"]][["data"]]
    expect_equal_tables(
        table,
        list("Null model", 1, 4.74865017735093, 0.826046120541498, 0, 0.5,
             "contGamma", 0.210586158729818, 0.210586158729818, 0.173953879458502,
             1.55940279678024e-06, 0.5), 
        label = "regressionTable"
    )

    table <- results[["results"]][["posteriorSummary"]][["posteriorSummaryTable"]][["data"]]
    expect_equal_tables(
        table,
        list("Intercept", -0.255843391953333, 0.0989748100578513, 1, 1, 1,
             -0.47763974261721, -0.0887658028069459, "contGamma", -0.000116767975422579,
             0.0241168865879483, 0.173953879458502, 0.5, 0.210586158729818,
             -0.0551020698857689, 0.0781883358646909), 
        label = "posteriorSummaryTable"
    )
    
    table <- results[["results"]][["descriptivesTable"]][["data"]]
    expect_equal_tables(
        table,
        list("contNormal", 100, -0.18874858754, 1.05841360919316, "contGamma",
             100, 2.03296079621, 1.53241112621044), 
        label = "descriptivesTable"
    )
})

test_that("Coefficient plots match", {
    set.seed(1)
    options <- jasptools::analysisOptions("RegressionLinearBayesian")
    options$dependent <- "contNormal"
    options$covariates <- list("contGamma", "debCollin1", "contcor2")
    options$modelTerms <- list(
        list(components="contGamma", isNuisance=FALSE),
        list(components="debCollin1", isNuisance=FALSE),
        list(components="contcor2", isNuisance=FALSE)
    )
    options$plotInclusionProbabilities <- TRUE
    options$plotCoefficientsPosterior <- TRUE
    
    results <- jasptools::run("RegressionLinearBayesian", "test.csv", options)
    
    inclusionProbabilities <- results[['state']][['figures']][[5]][["obj"]]
    expect_equal_plots(inclusionProbabilities, "inclusionProbabilities", "RegressionLinearBayesian")
    
    posteriorCoefficients <- results[['state']][['figures']][[1]][["obj"]]
    expect_equal_plots(posteriorCoefficients, "posteriorCoefficients", "RegressionLinearBayesian")
})

test_that("Residuals plots match", {
    set.seed(1)
    options <- jasptools::analysisOptions("RegressionLinearBayesian")
    options$dependent <- "contNormal"
    options$covariates <- list("contGamma")
    options$modelTerms <- list(
        list(components="contGamma", isNuisance=FALSE)
    )
    options$plotResidualsVsFitted <- TRUE
    options$plotQQplot <- TRUE
    
    results <- jasptools::run("RegressionLinearBayesian", "test.csv", options)
    
    residualsVsFitted <- results[['state']][['figures']][[1]][["obj"]]
    expect_equal_plots(residualsVsFitted, "residualsVsFitted", "RegressionLinearBayesian")
    
    qqPlot <- results[['state']][['figures']][[2]][["obj"]]
    expect_equal_plots(qqPlot, "qqPlot", "RegressionLinearBayesian")
})

test_that("Models plots match", {
    set.seed(1)
    options <- jasptools::analysisOptions("RegressionLinearBayesian")
    options$dependent <- "contNormal"
    options$covariates <- list("contGamma", "contExpon", "contcor1")
    options$modelTerms <- list(
        list(components="contGamma", isNuisance=FALSE),
        list(components="contExpon", isNuisance=FALSE),
        list(components="contcor1", isNuisance=FALSE)
    )
    options$plotLogPosteriorOdds <- TRUE
    options$plotModelComplexity <- TRUE
    options$plotModelProbabilities <- TRUE
    
    results <- jasptools::run("RegressionLinearBayesian", "test.csv", options)
    
    logPosteriorOdds <- results[['state']][['figures']][[1]][["obj"]]
    expect_equal_plots(logPosteriorOdds, "logPosteriorOdds", "RegressionLinearBayesian")
    
    modelProbabilities <- results[['state']][['figures']][[2]][["obj"]]
    expect_equal_plots(modelProbabilities, "modelProbabilities", "RegressionLinearBayesian")
        
    modelComplexity <- results[['state']][['figures']][[3]][["obj"]]
    expect_equal_plots(modelComplexity, "modelComplexity", "RegressionLinearBayesian")
})