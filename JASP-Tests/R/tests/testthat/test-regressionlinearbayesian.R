context("Bayesian Linear Regression")

# does not test
# - plots (will/ should be updated in the future)

test_that("Main tables results match", {
    options <- jasptools::analysisOptions("RegressionLinearBayesian")
    options$dependent <- "contNormal"
    options$covariates <- "contGamma"
    options$wlsWeights <- "facFifty"
    options$modelTerms <- list(
        list(components="contGamma", isNuisance=FALSE)
    )
    options$postSummary <- TRUE
    options$descriptives <- TRUE
    
    results <- jasptools::run("RegressionLinearBayesian", "test.csv", options, view=FALSE)#, quiet=TRUE)
    table <- results[["results"]][["regressionTable"]][["data"]]
    expect_equal_tables(
        table,
        list("contGamma", 1, 3.39687431385796, 0.772565707223374, 0.061000885146905,
             0.5, "Null model", 0.29438828393514, 0.29438828393514, 0.227434292776626,
             0, 0.5), 
        label = "regressionTable"
    )
    
    table <- results[["results"]][["posteriorSummary"]][["posteriorSummaryTable"]][["data"]]
    expect_equal_tables(
        table,
        list("Intercept", -0.255843391953333, 0.0993902662352492, 1, -0.453055243040002,
             -0.0586315408666647, "contGamma", -0.000690480780951939, 0.0586422991479854,
             0.772565707223374, -0.117049524830386, 0.115668563268482), 
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
