context("Bayesian Linear Regression")

# does not test
# - plots (will/ should be updated in the future)

test_that("Main tables results match", {
    set.seed(1)
    options <- jasptools::analysisOptions("RegressionLinearBayesian")
    options$modelPrior <- "beta.binomial"
    options$dependent <- "contNormal"
    options$covariates <- "contGamma"
    options$wlsWeights <- "facFifty"
    options$modelTerms <- list(
        list(components="contGamma", isNuisance=FALSE)
    )
    options$postSummaryTable <- TRUE
    options$descriptives <- TRUE
    
    results <- jasptools::run("RegressionLinearBayesian", "test.csv", options)
    table <- results[["results"]][["basreg"]][["collection"]][["basreg_modelComparisonTable"]][["data"]]
    expect_equal_tables(
        table,
        list("Null model", 1, 4.74865017735093, 0.826046120541498, 0, 0.5,
             "contGamma", 0.210586158729818, 0.210586158729818, 0.173953879458502,
             1.55940279678024e-06, 0.5), 
        label = "regressionTable"
    )

    table <- results[["results"]][["basreg"]][["collection"]][["basreg_postSumContainer"]][["collection"]][["basreg_postSumContainer_postSumTable"]][["data"]]
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
    options$modelPrior <- "beta.binomial"
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
    
    inclusionProbabilities <- results[['state']][['figures']][[1]][["obj"]]
    expect_equal_plots(inclusionProbabilities, "inclusionProbabilities", "RegressionLinearBayesian")
    
    posteriorCoefficients <- results[['state']][['figures']][[2]][["obj"]]
    expect_equal_plots(posteriorCoefficients, "posteriorCoefficients", "RegressionLinearBayesian")
})

test_that("Residuals plots match", {
    set.seed(1)
    options <- jasptools::analysisOptions("RegressionLinearBayesian")
    options$modelPrior <- "beta.binomial"
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
    options$modelPrior <- "beta.binomial"
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

test_that("Model priors match", {
    options <- jasptools::analysisOptions("RegressionLinearBayesian")
    options$dependent <- "contNormal"
    options$covariates <- list("contGamma", "contExpon", "contcor1")
    options$modelTerms <- list(
        list(components="contGamma", isNuisance=FALSE),
        list(components="contExpon", isNuisance=FALSE),
        list(components="contcor1", isNuisance=FALSE)
    )

    modelPriors <- list(
        uniform      = list(modelPrior = "uniform"),
        betabinomial = list(modelPrior = "beta.binomial", betaBinomialParamA = 2, betaBinomialParamB = 3),
        Wilson       = list(modelPrior = "Wilson", wilsonParamLambda = 2),
        bernoulli    = list(modelPrior = "Bernoulli", bernoulliParam = 0.5)
    )

    tables <- list(
      uniform = list(1, 3.90143837337827, "Null model", 0, 0.357882899462674, 0.125,
                     0.667063040584368, 2.19516621516207, "contcor1", 0.0259312818065142,
                     0.238730455088721, 0.125, 0.250266509028401, 0.688641679428745,
                     "contExpon", 0.00394490646287682, 0.0895661038894857, 0.125,
                     0.245477808280897, 0.674195829319842, "contGamma", 0.00350468569669571,
                     0.0878523097813097, 0.125, 0.24073279559456, 0.659935214199163,
                     "contExpon + contcor1", 0.0305485694813512, 0.0861541508831363,
                     0.125, 0.207447147529827, 0.561369587657518, "contGamma + contcor1",
                     0.0271156286160025, 0.0742417866432354, 0.125, 0.0916338337573983,
                     0.237342735695682, "contGamma + contExpon", 0.00794701370052642,
                     0.0327941821139783, 0.125, 0.0915889308672561, 0.237222490352522,
                     "contGamma + contExpon + contcor1", 0.0320272344954695, 0.03277811213746,
                     0.125),
      betabinomial = list(1, 3.76674705968188, "Null model", 0, 0.601068947543112, 0.285714285714286,
                          0.667063040584368, 1.48036998661697, "contcor1", 0.0259312818065142,
                          0.160380351899582, 0.114285714285714, 0.250266509028401, 0.496180698646568,
                          "contExpon", 0.00394490646287682, 0.060170970874796, 0.114285714285714,
                          0.245477808280897, 0.486091091246406, "contGamma", 0.00350468569669571,
                          0.0590196351474355, 0.114285714285714, 0.24073279559456, 0.48404226619047,
                          "contExpon + contcor1", 0.0305485694813512, 0.04340910242614,
                          0.0857142857142857, 0.207447147529827, 0.414513847951994, "contGamma + contcor1",
                          0.0271156286160025, 0.0374070115909721, 0.0857142857142857,
                          0.0915889308672561, 0.17450152476059, "contGamma + contExpon + contcor1",
                          0.0320272344954695, 0.0220205049131922, 0.114285714285714, 0.0916338337573983,
                          0.179211605034763, "contGamma + contExpon", 0.00794701370052642,
                          0.01652347560477, 0.0857142857142857),
      Wilson    = list(1, 3.00723842615036, "Null model", 0, 0.857437693351001, 0.666666666666667,
                       0.667063040584368, 0.847009328691918, "contcor1", 0.0259312818065142,
                       0.0714956243547957, 0.0833333333333333, 0.250266509028401, 0.303191058063168,
                       "contExpon", 0.00394490646287682, 0.02682349227804, 0.0833333333333333,
                       0.245477808280897, 0.297232917444893, "contGamma", 0.00350468569669571,
                       0.0263102407126539, 0.0833333333333333, 0.24073279559456, 0.304492845967535,
                       "contExpon + contcor1", 0.0305485694813512, 0.00737190617744777,
                       0.0238095238095238, 0.207447147529827, 0.262122056715543, "contGamma + contcor1",
                       0.0271156286160025, 0.00635260727393641, 0.0238095238095238,
                       0.0916338337573983, 0.115373118783951, "contGamma + contExpon",
                       0.00794701370052642, 0.00280608225178045, 0.0238095238095238,
                       0.0915889308672561, 0.116558805489141, "contGamma + contExpon + contcor1",
                       0.0320272344954695, 0.00140235360034472, 0.0119047619047619),
      bernoulli = list(1, 3.90143837337827, "Null model", 0, 0.357882899462674, 0.125,
                       0.667063040584368, 2.19516621516207, "contcor1", 0.0259312818065142,
                       0.238730455088721, 0.125, 0.250266509028401, 0.688641679428745,
                       "contExpon", 0.00394490646287682, 0.0895661038894857, 0.125,
                       0.245477808280897, 0.674195829319842, "contGamma", 0.00350468569669571,
                       0.0878523097813097, 0.125, 0.24073279559456, 0.659935214199163,
                       "contExpon + contcor1", 0.0305485694813512, 0.0861541508831363,
                       0.125, 0.207447147529827, 0.561369587657518, "contGamma + contcor1",
                       0.0271156286160025, 0.0742417866432354, 0.125, 0.0916338337573983,
                       0.237342735695682, "contGamma + contExpon", 0.00794701370052642,
                       0.0327941821139783, 0.125, 0.0915889308672561, 0.237222490352522,
                       "contGamma + contExpon + contcor1", 0.0320272344954695, 0.03277811213746,
                       0.125)
    )

    for (nm in names(modelPriors)) {
        set.seed(1)
        options[names(modelPriors[[nm]])] <- modelPriors[[nm]]
        results <- jasptools::run("RegressionLinearBayesian", "test.csv", options, view = FALSE)
        table <- results[["results"]][["basreg"]][["collection"]][["basreg_modelComparisonTable"]][["data"]]
        expect_equal_tables(
            table,
            tables[[nm]],
            label = paste("regressionTable modelprior:", paste(modelPriors[[nm]], collapse = ""))
        )
    }
})