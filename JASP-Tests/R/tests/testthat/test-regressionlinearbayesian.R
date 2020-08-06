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

test_that("summary tables match", {
  options <- jasptools::analysisOptions("RegressionLinearBayesian")
  options$covariates <- c("adverts", "airplay", "attract")
  options$dependent <- "sales"
  options$modelPrior <- "beta.binomial"
  options$modelTerms <- list(list(components = "adverts", isNuisance = FALSE), list(components = "airplay",
                                                                                    isNuisance = FALSE), list(components = "attract", isNuisance = FALSE),
                             list(components = c("adverts", "airplay"), isNuisance = FALSE),
                             list(components = c("adverts", "attract"), isNuisance = FALSE),
                             list(components = c("airplay", "attract"), isNuisance = FALSE),
                             list(components = c("adverts", "airplay", "attract"), isNuisance = FALSE))
  options$postSummaryTable <- TRUE
  set.seed(1)
  results <- jasptools::run("RegressionLinearBayesian", "Album Sales.csv", options)

  test_that("Model Comparison - sales table results match", {
    table <- results[["results"]][["basreg"]][["collection"]][["basreg_modelComparisonTable"]][["data"]]
    expect_equal_tables(table,
                        list(1, 174.417226359448, "adverts + airplay + attract", 0.664667686974516,
                             0.621991839174225, 0.00934579439252337, 0.186047794498428, 13.8715623535198,
                             "adverts + airplay + attract + airplay<unicode><unicode><unicode>attract",
                             0.66707467853794, 0.115720209874385, 0.00934579439252337, 0.142997743978807,
                             10.3484282582346, "adverts + airplay + attract + adverts<unicode><unicode><unicode>airplay",
                             0.666160366889052, 0.0889434297751434, 0.00934579439252337,
                             0.107406332340749, 7.58836892068143, "adverts + airplay + attract + adverts<unicode><unicode><unicode>attract",
                             0.665163133892889, 0.0668058621915803, 0.00934579439252337,
                             0.0272649838741297, 1.83826286627873, "adverts + airplay + attract + adverts<unicode><unicode><unicode>airplay + airplay<unicode><unicode><unicode>attract",
                             0.668232187780119, 0.0282643291082092, 0.0155763239875389, 0.0244460302418582,
                             1.64326131197498, "adverts + airplay + attract + adverts<unicode><unicode><unicode>attract + airplay<unicode><unicode><unicode>attract",
                             0.66785188769348, 0.0253420521844035, 0.0155763239875389, 0.000965925271001277,
                             0.0441865815594237, "adverts + airplay + attract + adverts<unicode><unicode><unicode>airplay + adverts<unicode><unicode><unicode>attract + airplay<unicode><unicode><unicode>attract + adverts<unicode><unicode><unicode>airplay<unicode><unicode><unicode>attract",
                             0.671330894983083, 0.0210279172535231, 0.327102803738318, 0.0178304900522086,
                             1.19019034974452, "adverts + airplay + attract + adverts<unicode><unicode><unicode>airplay + adverts<unicode><unicode><unicode>attract",
                             0.666749775458152, 0.0184840321682516, 0.0155763239875389, 0.00404508705638788,
                             0.259902726835239, "adverts + airplay + attract + adverts<unicode><unicode><unicode>airplay + adverts<unicode><unicode><unicode>attract + airplay<unicode><unicode><unicode>attract",
                             0.669090708597362, 0.0125800556891127, 0.0467289719626168, 0.000729479900775137,
                             0.047829119974167, "adverts + airplay", 0.629285746598297, 0.000756217575206264,
                             0.0155763239875389),
                        label = "regressionTable")
  })

  test_that("Posterior Summaries of Coefficients table results match", {
    table <- results[["results"]][["basreg"]][["collection"]][["basreg_postSumContainer"]][["collection"]][["basreg_postSumContainer_postSumTable"]][["data"]]
    expect_equal_tables(table,
                        list(1, "Intercept", 185.9359109822, 193.2, 1, 1, 3.33001519392629,
                             199.230069179688, 5.32503141479899e+22, "adverts", 0.0550921437820945,
                             0.0870677283108485, 1, 0.554517133956386, 0.0330086923987083,
                             0.150459099025842, 2.01764580197328e+22, "airplay", 0.457724418488145,
                             2.97961262311916, 1, 0.554517133956386, 1.1461062869467, 4.524690591752,
                             955.280172249543, "attract", -0.102388179038019, 9.67945340083919,
                             0.999159727418835, 0.554517133956386, 4.7776099631142, 17.4065595563688,
                             0.277398656420613, "adverts<unicode><unicode><unicode>airplay",
                             -0.00179349545935815, 2.16219311069655e-06, 0.169383819000199,
                             0.423676012461059, 0.000995345061743704, 1.64427677195647e-05,
                             0.22928004995305, "adverts<unicode><unicode><unicode>attract",
                             -0.00681136356232946, 1.93623197708716e-05, 0.144239919486871,
                             0.423676012461059, 0.00453577686425953, 0.00427686124707723,
                             0.346333790672081, "airplay<unicode><unicode><unicode>attract",
                             -0.000699936303713344, 0.0579310693810951, 0.202934564109634,
                             0.423676012461059, 0.160646671460175, 0.555676707155026, 0.0441865815594237,
                             "adverts<unicode><unicode><unicode>airplay<unicode><unicode><unicode>attract",
                             0, -1.5448872845036e-05, 0.0210279172535231, 0.327102803738318,
                             0.000141268474333337, 0),
                        label = "posteriorSummaryTable - all models")
  })

  options$effectsType <- "matchedModels"
  results <- jasptools::run("RegressionLinearBayesian", "Album Sales.csv", options)

  test_that("Posterior Summaries of Coefficients table results match", {
    table <- results[["results"]][["basreg"]][["collection"]][["basreg_postSumContainer"]][["collection"]][["basreg_postSumContainer_postSumTable"]][["data"]]
    expect_equal_tables(table,
                        list(1, "Intercept", 186.161290509004, 193.2, 0, 0, 1, 1, 3.33001519392629,
                             199.488123662299, 2.25793959971455e+23, "adverts", 0.0507687008869569,
                             0.0870677283108485, 1.50866863298529e-23, 0.445482866043614,
                             0.738468266623817, 0.0965732087227414, 0.0330086923987083, 0.144952448077947,
                             7.98861456199448e+22, "airplay", -0.0993815024762557, 2.97961262311916,
                             3.98172357968454e-23, 0.445482866043614, 0.689553918941012,
                             0.0965732087227414, 1.1461062869467, 4.32488778911079, 3902.87112656581,
                             "attract", -1.94856184146502, 9.67945340083919, 0.000840272581165592,
                             0.445482866043614, 0.710935268949368, 0.0965732087227414, 4.7776099631142,
                             17.5457886962235, 0.0284895917404954, "adverts<unicode><unicode><unicode>airplay",
                             -0.00139780263317473, 2.16219311069654e-06, 0.830616180999801,
                             0.059190031152648, 0.169383819000199, 0.423676012461059, 0.000995345061743704,
                             0.000155332500540226, 0.023570825047528, "adverts<unicode><unicode><unicode>attract",
                             -0.00917102096072798, 1.93623197708715e-05, 0.854919807931963,
                             0.059190031152648, 0.144239919486871, 0.423676012461059, 0.00453577686425953,
                             0.00332597634990196, 0.0356069534665488, "airplay<unicode><unicode><unicode>attract",
                             -0.0124131272141111, 0.0579310693810951, 0.7962251633092, 0.059190031152648,
                             0.202934564109634, 0.423676012461059, 0.160646671460175, 0.52580208102547,
                             0.00859921619355953, "adverts<unicode><unicode><unicode>airplay<unicode><unicode><unicode>attract",
                             0, -1.5448872845036e-05, 0.978131810165311, 0.130841121495327,
                             0.0210279172535231, 0.327102803738318, 0.000141268474333337,
                             0),
                        label = "posteriorSummaryTable - matched models")
  })
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
        bernoulli    = list(modelPrior = "Bernoulli", bernoulliParam = 0.75),
        castillo     = list(modelPrior = "Castillo", castilloParamU = 2)
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
      bernoulli = list(1, 0.362563657801133, "contGamma + contExpon + contcor1", 0.0320272344954695,
                       0.209219538601646, 0.421875, 2.62840491001543, 1.37161839640395,
                       "contExpon + contcor1", 0.0305485694813512, 0.183304554177243,
                       0.140625, 7.28322772487837, 4.14432135426702, "contcor1", 0.0259312818065142,
                       0.169310393792196, 0.046875, 2.26498055567969, 1.14639058082694,
                       "contGamma + contcor1", 0.0271156286160025, 0.157959395600335,
                       0.140625, 10.9183499635927, 5.82274069025381, "Null model",
                       0, 0.08460489413608, 0.015625, 1.00049026546895, 0.458379911234064,
                       "contGamma + contExpon", 0.00794701370052642, 0.0697740372389506,
                       0.140625, 2.73249732973872, 1.37920924591115, "contExpon", 0.00394490646287682,
                       0.0635213145064626, 0.046875, 2.68021261910655, 1.35106536878368,
                       "contGamma", 0.00350468569669571, 0.0623058719470867, 0.046875),
      castillo = list(1, 2.87159171765746, "Null model", 0, 0.895993407636698, 0.75,
                      0.667063040584368, 0.785243329949621, "contcor1", 0.0259312818065142,
                      0.0543349169856078, 0.0681818181818182, 0.250266509028401, 0.284395111302656,
                      "contExpon", 0.00394490646287682, 0.0203851947492453, 0.0681818181818182,
                      0.245477808280897, 0.278842351875457, "contGamma", 0.00350468569669571,
                      0.0199951361764354, 0.0681818181818182, 0.24073279559456, 0.284788465921838,
                      "contExpon + contcor1", 0.0305485694813512, 0.00392172723372142,
                      0.0136363636363636, 0.207447147529827, 0.24527780194552, "contGamma + contcor1",
                      0.0271156286160025, 0.00337947775672295, 0.0136363636363636,
                      0.0916338337573983, 0.108139723436462, "contGamma + contExpon",
                      0.00794701370052642, 0.00149278747205647, 0.0136363636363636,
                      0.0915889308672561, 0.10897428428032, "contGamma + contExpon + contcor1",
                      0.0320272344954695, 0.000497351989512453, 0.00454545454545455)
    )

    for (nm in names(modelPriors)) {
        set.seed(1)
        options[names(modelPriors[[nm]])] <- modelPriors[[nm]]
        results <- jasptools::run("RegressionLinearBayesian", "test.csv", options)
        table <- results[["results"]][["basreg"]][["collection"]][["basreg_modelComparisonTable"]][["data"]]
        expect_equal_tables(
            table,
            tables[[nm]],
            label = paste("regressionTable modelprior:", paste(modelPriors[[nm]], collapse = ""))
        )
    }
})