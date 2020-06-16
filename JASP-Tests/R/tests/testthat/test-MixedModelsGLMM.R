context("Generalized Linear Mixed Models")
# the testing doesn't cover all links / families

#### ANOVA tables different model tests / families / links ###
{
  test_that("ANOVA Summary table results match (binomial / logit)", {
    options <- jasptools::analysisOptions("MixedModelsGLMM")
    options$.meta <- list(Contrasts = list(list(levels = list(containsColumn = TRUE))), 
                          dependentVariable = list(containsColumn = TRUE), dependentVariableAggregation = list(
                            containsColumn = TRUE), fixedEffects = list(list()), 
                          fixedVariables = list(containsColumn = TRUE), plotsAgregatedOver = list(
                            containsColumn = TRUE), randomEffects = list(list(randomComponents = list(
                              list(value = list(containsColumn = TRUE))), value = list(
                                containsColumn = TRUE))), randomVariables = list(containsColumn = TRUE), 
                          trendsContrasts = list(list(levels = list(containsColumn = TRUE))))
    options$Contrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                   values = list()))
    options$dependentVariable <- "contBinom"
    options$fixedEffects <- list(list(components = "contNormal"))
    options$fixedVariables <- "contNormal"
    options$plotsAgregatedOver <- "facFive"
    options$randomEffects <- list(list(correlations = TRUE, randomComponents = list(list(randomSlopes = TRUE, 
                                                                                         value = "contNormal")), value = "facFive"))
    options$randomVariables <- "facFive"
    options$trendsContrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                         values = list()))
    options$trendsTrend <- list()
    set.seed(1)
    results <- jasptools::run("MixedModelsGLMM", "debug.csv", options)
    table <- results[["results"]][["ANOVAsummary"]][["data"]]
    expect_equal_tables(table,
                        list(1, "contNormal", 0.496613369631141, 0.462166121618139))
  })
  test_that("ANOVA Summary table results match (binomial / probit)", {
    options <- jasptools::analysisOptions("MixedModelsGLMM")
    options$.meta <- list(Contrasts = list(list(levels = list(containsColumn = TRUE))), 
                          dependentVariable = list(containsColumn = TRUE), dependentVariableAggregation = list(
                            containsColumn = TRUE), fixedEffects = list(list()), 
                          fixedVariables = list(containsColumn = TRUE), plotsAgregatedOver = list(
                            containsColumn = TRUE), randomEffects = list(list(randomComponents = list(
                              list(value = list(containsColumn = TRUE))), value = list(
                                containsColumn = TRUE))), randomVariables = list(containsColumn = TRUE), 
                          trendsContrasts = list(list(levels = list(containsColumn = TRUE))))
    options$Contrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                   values = list()))
    options$dependentVariable <- "contBinom"
    options$fixedEffects <- list(list(components = "contNormal"))
    options$fixedVariables <- "contNormal"
    options$link <- "probit"
    options$plotsAgregatedOver <- "facFive"
    options$randomEffects <- list(list(correlations = TRUE, randomComponents = list(list(randomSlopes = TRUE, 
                                                                                         value = "contNormal")), value = "facFive"))
    options$randomVariables <- "facFive"
    options$trendsContrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                         values = list()))
    options$trendsTrend <- list()
    set.seed(1)
    results <- jasptools::run("MixedModelsGLMM", "debug.csv", options)
    table <- results[["results"]][["ANOVAsummary"]][["data"]]
    expect_equal_tables(table,
                        list(1, "contNormal", 0.493344377834231, 0.469224008127867))
  })
  test_that("ANOVA Summary table results match (binomial / cauchit)", {
    options <- jasptools::analysisOptions("MixedModelsGLMM")
    options$.meta <- list(Contrasts = list(list(levels = list(containsColumn = TRUE))), 
                          dependentVariable = list(containsColumn = TRUE), dependentVariableAggregation = list(
                            containsColumn = TRUE), fixedEffects = list(list()), 
                          fixedVariables = list(containsColumn = TRUE), plotsAgregatedOver = list(
                            containsColumn = TRUE), randomEffects = list(list(randomComponents = list(
                              list(value = list(containsColumn = TRUE))), value = list(
                                containsColumn = TRUE))), randomVariables = list(containsColumn = TRUE), 
                          trendsContrasts = list(list(levels = list(containsColumn = TRUE))))
    options$Contrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                   values = list()))
    options$dependentVariable <- "contBinom"
    options$fixedEffects <- list(list(components = "contNormal"))
    options$fixedVariables <- "contNormal"
    options$link <- "cauchit"
    options$plotsAgregatedOver <- "facFive"
    options$randomEffects <- list(list(correlations = TRUE, randomComponents = list(list(randomSlopes = TRUE, 
                                                                                         value = "contNormal")), value = "facFive"))
    options$randomVariables <- "facFive"
    options$trendsContrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                         values = list()))
    options$trendsTrend <- list()
    set.seed(1)
    results <- jasptools::run("MixedModelsGLMM", "debug.csv", options)
    table <- results[["results"]][["ANOVAsummary"]][["data"]]
    expect_equal_tables(table,
                        list(1, "contNormal", 0.511278057099532, 0.431450520272136))
  })
  test_that("ANOVA Summary table results match (binomial / cloglog)", {
    options <- jasptools::analysisOptions("MixedModelsGLMM")
    options$.meta <- list(Contrasts = list(list(levels = list(containsColumn = TRUE))), 
                          dependentVariable = list(containsColumn = TRUE), dependentVariableAggregation = list(
                            containsColumn = TRUE), fixedEffects = list(list()), 
                          fixedVariables = list(containsColumn = TRUE), plotsAgregatedOver = list(
                            containsColumn = TRUE), randomEffects = list(list(randomComponents = list(
                              list(value = list(containsColumn = TRUE))), value = list(
                                containsColumn = TRUE))), randomVariables = list(containsColumn = TRUE), 
                          trendsContrasts = list(list(levels = list(containsColumn = TRUE))))
    options$Contrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                   values = list()))
    options$dependentVariable <- "contBinom"
    options$fixedEffects <- list(list(components = "contNormal"))
    options$fixedVariables <- "contNormal"
    options$link <- "cloglog"
    options$plotsAgregatedOver <- "facFive"
    options$randomEffects <- list(list(correlations = TRUE, randomComponents = list(list(randomSlopes = TRUE, 
                                                                                         value = "contNormal")), value = "facFive"))
    options$randomVariables <- "facFive"
    options$trendsContrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                         values = list()))
    options$trendsTrend <- list()
    set.seed(1)
    results <- jasptools::run("MixedModelsGLMM", "debug.csv", options)
    table <- results[["results"]][["ANOVAsummary"]][["data"]]
    expect_equal_tables(table,
                        list(1, "contNormal", 0.502062833294605, 0.450573290973296))
  })
  test_that("ANOVA Summary table results match (binomial / log)", {
    options <- jasptools::analysisOptions("MixedModelsGLMM")
    options$.meta <- list(Contrasts = list(list(levels = list(containsColumn = TRUE))), 
                          dependentVariable = list(containsColumn = TRUE), dependentVariableAggregation = list(
                            containsColumn = TRUE), fixedEffects = list(list()), 
                          fixedVariables = list(containsColumn = TRUE), plotsAgregatedOver = list(
                            containsColumn = TRUE), randomEffects = list(list(randomComponents = list(
                              list(value = list(containsColumn = TRUE))), value = list(
                                containsColumn = TRUE))), randomVariables = list(containsColumn = TRUE), 
                          trendsContrasts = list(list(levels = list(containsColumn = TRUE))))
    options$Contrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                   values = list()))
    options$dependentVariable <- "contBinom"
    options$fixedEffects <- list(list(components = "facGender"))
    options$fixedVariables <- "facGender"
    options$link <- "log"
    options$plotsAgregatedOver <- "facFive"
    options$randomEffects <- list(list(correlations = TRUE, randomComponents = list(list(randomSlopes = TRUE, 
                                                                                         value = "facGender")), value = "facFive"))
    options$randomVariables <- "facFive"
    options$trendsContrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                         values = list()))
    options$trendsTrend <- list()
    set.seed(1)
    results <- jasptools::run("MixedModelsGLMM", "debug.csv", options)
    table <- results[["results"]][["ANOVAsummary"]][["data"]]
    expect_equal_tables(table,
                        list(1, "facGender", 0.246403123723302, 1.34358278549087))
  })
  test_that("ANOVA Summary table results match (poisson  / identity)", {
    options <- jasptools::analysisOptions("MixedModelsGLMM")
    options$.meta <- list(Contrasts = list(list(levels = list(containsColumn = TRUE))), 
                          dependentVariable = list(containsColumn = TRUE), dependentVariableAggregation = list(
                            containsColumn = TRUE), fixedEffects = list(list()), 
                          fixedVariables = list(containsColumn = TRUE), plotsAgregatedOver = list(
                            containsColumn = TRUE), randomEffects = list(list(randomComponents = list(
                              list(value = list(containsColumn = TRUE))), value = list(
                                containsColumn = TRUE))), randomVariables = list(containsColumn = TRUE), 
                          trendsContrasts = list(list(levels = list(containsColumn = TRUE))))
    options$Contrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                   values = list()))
    options$dependentVariable <- "V1"
    options$family <- "poisson"
    options$fixedEffects <- list(list(components = "facGender"))
    options$fixedVariables <- "facGender"
    options$link <- "identity"
    options$plotsAgregatedOver <- "contBinom"
    options$randomEffects <- list(list(correlations = TRUE, randomComponents = list(list(randomSlopes = TRUE, 
                                                                                         value = "facGender")), value = "contBinom"))
    options$randomVariables <- "contBinom"
    options$trendsContrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                         values = list()))
    options$trendsTrend <- list()
    set.seed(1)
    results <- jasptools::run("MixedModelsGLMM", "debug.csv", options)
    table <- results[["results"]][["ANOVAsummary"]][["data"]]
    expect_equal_tables(table,
                        list(1, "facGender", 0.361954228971617, 0.83110144154216))
  })
  test_that("ANOVA Summary table results match (poisson  / log)", {
    options <- jasptools::analysisOptions("MixedModelsGLMM")
    options$.meta <- list(Contrasts = list(list(levels = list(containsColumn = TRUE))), 
                          dependentVariable = list(containsColumn = TRUE), dependentVariableAggregation = list(
                            containsColumn = TRUE), fixedEffects = list(list()), 
                          fixedVariables = list(containsColumn = TRUE), plotsAgregatedOver = list(
                            containsColumn = TRUE), randomEffects = list(list(randomComponents = list(
                              list(value = list(containsColumn = TRUE))), value = list(
                                containsColumn = TRUE))), randomVariables = list(containsColumn = TRUE), 
                          trendsContrasts = list(list(levels = list(containsColumn = TRUE))))
    options$Contrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                   values = list()))
    options$dependentVariable <- "V1"
    options$family <- "poisson"
    options$fixedEffects <- list(list(components = "facGender"))
    options$fixedVariables <- "facGender"
    options$link <- "log"
    options$plotsAgregatedOver <- "contBinom"
    options$randomEffects <- list(list(correlations = TRUE, randomComponents = list(list(randomSlopes = TRUE, 
                                                                                         value = "facGender")), value = "contBinom"))
    options$randomVariables <- "contBinom"
    options$trendsContrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                         values = list()))
    options$trendsTrend <- list()
    set.seed(1)
    results <- jasptools::run("MixedModelsGLMM", "debug.csv", options)
    table <- results[["results"]][["ANOVAsummary"]][["data"]]
    expect_equal_tables(table,
                        list(1, "facGender", 0.378606857258377, 0.775220568488294))
  })
  test_that("ANOVA Summary table results match (poisson  / sqrt)", {
    options <- jasptools::analysisOptions("MixedModelsGLMM")
    options$.meta <- list(Contrasts = list(list(levels = list(containsColumn = TRUE))), 
                          dependentVariable = list(containsColumn = TRUE), dependentVariableAggregation = list(
                            containsColumn = TRUE), fixedEffects = list(list()), 
                          fixedVariables = list(containsColumn = TRUE), plotsAgregatedOver = list(
                            containsColumn = TRUE), randomEffects = list(list(randomComponents = list(
                              list(value = list(containsColumn = TRUE))), value = list(
                                containsColumn = TRUE))), randomVariables = list(containsColumn = TRUE), 
                          trendsContrasts = list(list(levels = list(containsColumn = TRUE))))
    options$Contrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                   values = list()))
    options$dependentVariable <- "V1"
    options$family <- "poisson"
    options$fixedEffects <- list(list(components = "facGender"))
    options$fixedVariables <- "facGender"
    options$link <- "sqrt"
    options$plotsAgregatedOver <- "contBinom"
    options$randomEffects <- list(list(correlations = TRUE, randomComponents = list(list(randomSlopes = TRUE, 
                                                                                         value = "facGender")), value = "contBinom"))
    options$randomVariables <- "contBinom"
    options$trendsContrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                         values = list()))
    options$trendsTrend <- list()
    set.seed(1)
    results <- jasptools::run("MixedModelsGLMM", "debug.csv", options)
    table <- results[["results"]][["ANOVAsummary"]][["data"]]
    expect_equal_tables(table,
                        list(1, "facGender", 0.37005249358997, 0.803488211907279))
  })
  test_that("ANOVA Summary table results match (gamma    / identity), type II", {
    options <- jasptools::analysisOptions("MixedModelsGLMM")
    options$.meta <- list(Contrasts = list(list(levels = list(containsColumn = TRUE))), 
                          dependentVariable = list(containsColumn = TRUE), dependentVariableAggregation = list(
                            containsColumn = TRUE), fixedEffects = list(list()), 
                          fixedVariables = list(containsColumn = TRUE), plotsAgregatedOver = list(
                            containsColumn = TRUE), randomEffects = list(list(randomComponents = list(
                              list(value = list(containsColumn = TRUE))), value = list(
                                containsColumn = TRUE))), randomVariables = list(containsColumn = TRUE), 
                          trendsContrasts = list(list(levels = list(containsColumn = TRUE))))
    options$Contrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                   values = list()))
    options$dependentVariable <- "V1"
    options$family <- "Gamma"
    options$fixedEffects <- list(list(components = "facGender"))
    options$fixedVariables <- "facGender"
    options$link <- "identity"
    options$plotsAgregatedOver <- "contBinom"
    options$randomEffects <- list(list(correlations = TRUE, randomComponents = list(list(randomSlopes = TRUE, 
                                                                                         value = "facGender")), value = "contBinom"))
    options$randomVariables <- "contBinom"
    options$trendsContrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                         values = list()))
    options$trendsTrend <- list()
    options$type <- "2"
    set.seed(1)
    results <- jasptools::run("MixedModelsGLMM", "debug.csv", options)
    table <- results[["results"]][["ANOVAsummary"]][["data"]]
    expect_equal_tables(table,
                        list(1, "facGender", 0.529079144695415, 0.396159887255862))
  })
  test_that("ANOVA Summary table results match (gamma    / inverse), type II, PB", {
    options <- jasptools::analysisOptions("MixedModelsGLMM")
    options$.meta <- list(Contrasts = list(list(levels = list(containsColumn = TRUE))), 
                          dependentVariable = list(containsColumn = TRUE), dependentVariableAggregation = list(
                            containsColumn = TRUE), fixedEffects = list(list()), 
                          fixedVariables = list(containsColumn = TRUE), plotsAgregatedOver = list(
                            containsColumn = TRUE), randomEffects = list(list(randomComponents = list(
                              list(value = list(containsColumn = TRUE))), value = list(
                                containsColumn = TRUE))), randomVariables = list(containsColumn = TRUE), 
                          trendsContrasts = list(list(levels = list(containsColumn = TRUE))))
    options$Contrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                   values = list()))
    options$bootstrap_samples <- 1
    options$dependentVariable <- "V1"
    options$family <- "Gamma"
    options$fixedEffects <- list(list(components = "contBinom"))
    options$fixedVariables <- "contBinom"
    options$link <- "log"
    options$method <- "PB"
    options$plotsAgregatedOver <- "facFive"
    options$randomEffects <- list(list(correlations = TRUE, randomComponents = list(list(randomSlopes = TRUE, 
                                                                                         value = "contBinom")), value = "facFive"))
    options$randomVariables <- "facFive"
    options$trendsContrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                         values = list()))
    options$trendsTrend <- list()
    options$type <- "2"
    set.seed(1)
    results <- jasptools::run("MixedModelsGLMM", "debug.csv", options)
    table <- results[["results"]][["ANOVAsummary"]][["data"]]
    expect_equal_tables(table,
                        list(1, "contBinom", 0.324635800502782, 1, 0.970177958797194))
  })
}
#### overall summary ####
{
  options <- jasptools::analysisOptions("MixedModelsGLMM")
  options$.meta <- list(Contrasts = list(list(levels = list(containsColumn = TRUE))), 
                        dependentVariable = list(containsColumn = TRUE), dependentVariableAggregation = list(
                          containsColumn = TRUE), fixedEffects = list(list(), list(), 
                                                                      list()), fixedVariables = list(containsColumn = TRUE), 
                        plotsAgregatedOver = list(containsColumn = TRUE), randomEffects = list(
                          list(randomComponents = list(list(value = list(containsColumn = TRUE)), 
                                                       list(value = list(containsColumn = TRUE)), list(value = list(
                                                         containsColumn = TRUE))), value = list(containsColumn = TRUE))), 
                        randomVariables = list(containsColumn = TRUE), trendsContrasts = list(
                          list(levels = list(containsColumn = TRUE))))
  options$Contrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                 values = list()))
  options$dependentVariable <- "contBinom"
  options$fixedEffects <- list(list(components = "facGender"), list(components = "facExperim"), 
                               list(components = c("facGender", "facExperim")))
  options$fixedVariables <- c("\"facGender\"", "\"facExperim\"")
  options$plotsAgregatedOver <- "facFive"
  options$pvalVS <- TRUE
  options$randomEffects <- list(list(correlations = TRUE, randomComponents = list(list(randomSlopes = TRUE, 
                                                                                       value = "facGender"), list(randomSlopes = FALSE, value = "facExperim"), 
                                                                                  list(randomSlopes = FALSE, value = "facGender * facExperim")), 
                                     value = "facFive"))
  options$randomVariables <- "facFive"
  options$showFE <- TRUE
  options$showRE <- TRUE
  options$test_intercept <- TRUE
  options$trendsContrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                       values = list()))
  options$trendsTrend <- list()
  set.seed(1)
  results <- jasptools::run("MixedModelsGLMM", "debug.csv", options)
  
  
  test_that("ANOVA Summary table results match", {
    table <- results[["results"]][["ANOVAsummary"]][["data"]]
    expect_equal_tables(table,
                        list(1, "Intercept", 0.150482451789201, 1.29080487630352, 2.06735352072263,
                             1, "facGender", 0.299408628643831, 1.01885727098676, 1.07682626485743,
                             1, "facExperim", 0.535584641777668, 1, 0.383784727871415, 1,
                             "facGender * facExperim", 0.809031449966947, 1, 0.058407150274121
                        ))
  })
  
  test_that("Fixed Effects Estimates table results match", {
    table <- results[["results"]][["FEsummary"]][["data"]]
    expect_equal_tables(table,
                        list(-0.336546361893194, 0.12413163209057, 1.42043978931245, 0.218869086561083,
                             -1.53766055856074, "Intercept", 0.225035381809514, 0.292295049336879,
                             1.02325042263629, 0.213688471057369, 1.05310024773914, "facGender (1)",
                             0.128916181562163, 0.535690269960241, 1, 0.208149818886858,
                             0.619343231964266, "facExperim (1)", 0.050302936566387, 0.809091014020084,
                             1, 0.208208510803305, 0.241598849020673, "facGender (1) * facExperim (1)"
                        ))
  })
  
  test_that("Random Effects Correlation Estimates table results match", {
    table <- results[["results"]][["REsummary"]][["collection"]][["REsummary_CE1"]][["data"]]
    expect_equal_tables(table,
                        list(1, "Intercept", -1, 1, "facGender (1)"))
  })
  
  test_that("Residual Variance Estimates table results match", {
    table <- results[["results"]][["REsummary"]][["collection"]][["REsummary_RES1"]][["data"]]
    expect_equal_tables(table,
                        list(1, 1))
  })
  
  test_that("Random Effects Variance Estimates table results match", {
    table <- results[["results"]][["REsummary"]][["collection"]][["REsummary_VE1"]][["data"]]
    expect_equal_tables(table,
                        list(0.150435850013205, 0.387860606420921, "Intercept", 0.105078371901533,
                             0.324157942832708, "facGender (1)"))
  })
}
#### estimated marginal means + contrasts #####
{
  options <- jasptools::analysisOptions("MixedModelsGLMM")
  options$.meta <- list(Contrasts = list(list(levels = list(containsColumn = TRUE)), 
                                         list(levels = list(containsColumn = TRUE)), list(levels = list(
                                           containsColumn = TRUE))), dependentVariable = list(containsColumn = TRUE), 
                        dependentVariableAggregation = list(containsColumn = TRUE), 
                        fixedEffects = list(list(), list(), list(), list(), list(), 
                                            list(), list()), fixedVariables = list(containsColumn = TRUE), 
                        marginalMeans = list(list(), list()), plotsAgregatedOver = list(
                          containsColumn = TRUE), randomEffects = list(list(randomComponents = list(
                            list(value = list(containsColumn = TRUE)), list(value = list(
                              containsColumn = TRUE)), list(value = list(containsColumn = TRUE)), 
                            list(value = list(containsColumn = TRUE)), list(value = list(
                              containsColumn = TRUE)), list(value = list(containsColumn = TRUE)), 
                            list(value = list(containsColumn = TRUE))), value = list(
                              containsColumn = TRUE))), randomVariables = list(containsColumn = TRUE), 
                        trendsContrasts = list(list(levels = list(containsColumn = TRUE))))
  options$Contrasts <- list(list(isContrast = FALSE, levels = c("Row 1", "Row 2", "Row 3", 
                                                                "Row 4", "Row 5", "Row 6"), name = "facGender", values = c("f", 
                                                                                                                           "m", "f", "m", "f", "m")), list(isContrast = FALSE, levels = c("Row 1", 
                                                                                                                                                                                          "Row 2", "Row 3", "Row 4", "Row 5", "Row 6"), name = "contNormal", 
                                                                                                                                                           values = c("-1", "-1", "0", "0", "1", "1")), list(isContrast = TRUE, 
                                                                                                                                                                                                             levels = c("Row 1", "Row 2", "Row 3", "Row 4", "Row 5", "Row 6"
                                                                                                                                                                                                             ), name = "Contrast 1", values = c("-1", "1", "0", "0", "0", 
                                                                                                                                                                                                                                                "0")))
  options$dependentVariable <- "contBinom"
  options$fixedEffects <- list(list(components = "facGender"), list(components = "facExperim"), 
                               list(components = "contNormal"), list(components = c("facGender", 
                                                                                    "facExperim")), list(components = c("facGender", "contNormal"
                                                                                    )), list(components = c("facExperim", "contNormal")), list(
                                                                                      components = c("facGender", "facExperim", "contNormal"
                                                                                      )))
  options$fixedVariables <- c("\"facGender\"", "\"facExperim\"", "\"contNormal\"")
  options$marginalMeans <- list(list(variable = "facGender"), list(variable = "contNormal"))
  options$marginalMeansCompare <- TRUE
  options$marginalMeansCompareTo <- 1
  options$marginalMeansContrast <- TRUE
  options$plotsAgregatedOver <- "facFive"
  options$randomEffects <- list(list(correlations = TRUE, randomComponents = list(list(randomSlopes = FALSE, 
                                                                                       value = "facGender"), list(randomSlopes = FALSE, value = "facExperim"), 
                                                                                  list(randomSlopes = FALSE, value = "contNormal"), list(randomSlopes = FALSE, 
                                                                                                                                         value = "facGender * facExperim"), list(randomSlopes = FALSE, 
                                                                                                                                                                                 value = "facGender * contNormal"), list(randomSlopes = FALSE, 
                                                                                                                                                                                                                         value = "facExperim * contNormal"), list(randomSlopes = FALSE, 
                                                                                                                                                                                                                                                                  value = "facGender * facExperim * contNormal")), value = "facFive"))
  options$randomVariables <- "facFive"
  options$trendsContrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                       values = list()))
  options$trendsTrend <- list()
  set.seed(1)
  results <- jasptools::run("MixedModelsGLMM", "debug.csv", options)
  
  
  test_that("ANOVA Summary table results match", {
    table <- results[["results"]][["ANOVAsummary"]][["data"]]
    expect_equal_tables(table,
                        list(1, "facGender", 0.480796631332514, 0.497055825482136, 1, "facExperim",
                             0.552974967089275, 0.352014522528833, 1, "contNormal", 0.539437816661619,
                             0.376582698078323, 1, "facGender * facExperim", 0.808114989881373,
                             0.0589802670167785, 1, "facGender * contNormal", 0.340867270412155,
                             0.907166795685725, 1, "facExperim * contNormal", 0.994581145216534,
                             4.61255523589443e-05, 1, "facGender * facExperim * contNormal",
                             0.762077944500614, 0.091659505636386))
  })
  
  test_that("Estimated Marginal Means table results match", {
    table <- results[["results"]][["EMMsummary"]][["data"]]
    expect_equal_tables(table,
                        list(-1.24716219673316, "<unicode><unicode><unicode>", 0.546666219557524,
                             "f", 0.346415475763165, 1, 0.0526349798504055, 0.103940938415945,
                             -1.93790363963291, 0.732873834796012, -1.24716219673316, "<unicode><unicode><unicode>",
                             0.341127002896136, "m", 0.170875126855412, 2, 0.000418193307941987,
                             0.105634463963906, -3.52832819872673, 0.565345564302481, -0.18874858754,
                             "<unicode><unicode><unicode>", 0.453803595880402, "f", 0.314904570066181,
                             3, 8.69286119636144e-05, 0.0748637511317291, -3.92444861251449,
                             0.600286798047493, -0.18874858754, "<unicode><unicode><unicode>",
                             0.359330268326127, "m", 0.235462685872198, 4, 2.46479272113402e-07,
                             0.0704093196025165, -5.16035707556267, 0.505293320847128, 0.869665021653162,
                             "<unicode><unicode><unicode>", 0.364047853019255, "f", 0.165813592989471,
                             5, 0.00389438629915676, 0.124945255960341, -2.88659219650169,
                             0.622439748198315, 0.869665021653162, "<unicode><unicode><unicode>",
                             0.377947697091284, "m", 0.225284622277967, 6, 6.74207843018523e-05,
                             0.0883887763879911, -3.98520890327973, 0.559367231934548))
  })
  
  test_that("Contrasts table results match", {
    table <- results[["results"]][["contrasts_Means"]][["data"]]
    expect_equal_tables(table,
                        list("Contrast 1", "<unicode><unicode><unicode>", -0.205539216661388,
                             0.165462464391811, 0.148197027573587, -1.38693211346177))
  })
}
#### estimated marginal means + contrasts (non-response scale) #####
{
  options <- jasptools::analysisOptions("MixedModelsGLMM")
  options$.meta <- list(Contrasts = list(list(levels = list(containsColumn = TRUE)), 
                                         list(levels = list(containsColumn = TRUE)), list(levels = list(
                                           containsColumn = TRUE))), dependentVariable = list(containsColumn = TRUE), 
                        dependentVariableAggregation = list(containsColumn = TRUE), 
                        fixedEffects = list(list(), list(), list(), list(), list(), 
                                            list(), list()), fixedVariables = list(containsColumn = TRUE), 
                        marginalMeans = list(list(), list()), plotsAgregatedOver = list(
                          containsColumn = TRUE), randomEffects = list(list(randomComponents = list(
                            list(value = list(containsColumn = TRUE)), list(value = list(
                              containsColumn = TRUE)), list(value = list(containsColumn = TRUE)), 
                            list(value = list(containsColumn = TRUE)), list(value = list(
                              containsColumn = TRUE)), list(value = list(containsColumn = TRUE)), 
                            list(value = list(containsColumn = TRUE))), value = list(
                              containsColumn = TRUE))), randomVariables = list(containsColumn = TRUE), 
                        trendsContrasts = list(list(levels = list(containsColumn = TRUE))))
  options$Contrasts <- list(list(isContrast = FALSE, levels = c("Row 1", "Row 2", "Row 3", 
                                                                "Row 4", "Row 5", "Row 6"), name = "facGender", values = c("f", 
                                                                                                                           "m", "f", "m", "f", "m")), list(isContrast = FALSE, levels = c("Row 1", 
                                                                                                                                                                                          "Row 2", "Row 3", "Row 4", "Row 5", "Row 6"), name = "contNormal", 
                                                                                                                                                           values = c("-1", "-1", "0", "0", "1", "1")), list(isContrast = TRUE, 
                                                                                                                                                                                                             levels = c("Row 1", "Row 2", "Row 3", "Row 4", "Row 5", "Row 6"
                                                                                                                                                                                                             ), name = "Contrast 1", values = c("-1", "1", "0", "0", "0", 
                                                                                                                                                                                                                                                "0")))
  options$dependentVariable <- "contBinom"
  options$fixedEffects <- list(list(components = "facGender"), list(components = "facExperim"), 
                               list(components = "contNormal"), list(components = c("facGender", 
                                                                                    "facExperim")), list(components = c("facGender", "contNormal"
                                                                                    )), list(components = c("facExperim", "contNormal")), list(
                                                                                      components = c("facGender", "facExperim", "contNormal"
                                                                                      )))
  options$fixedVariables <- c("\"facGender\"", "\"facExperim\"", "\"contNormal\"")
  options$marginalMeans <- list(list(variable = "facGender"), list(variable = "contNormal"))
  options$marginalMeansAdjustment <- "tukey"
  options$marginalMeansCIwidth <- 0.69
  options$marginalMeansCompare <- TRUE
  options$marginalMeansCompareTo <- 1
  options$marginalMeansContrast <- TRUE
  options$marginalMeansDf <- "satterthwaite"
  options$marginalMeansResponse <- FALSE
  options$plotsAgregatedOver <- "facFive"
  options$randomEffects <- list(list(correlations = TRUE, randomComponents = list(list(randomSlopes = FALSE, 
                                                                                       value = "facGender"), list(randomSlopes = FALSE, value = "facExperim"), 
                                                                                  list(randomSlopes = FALSE, value = "contNormal"), list(randomSlopes = FALSE, 
                                                                                                                                         value = "facGender * facExperim"), list(randomSlopes = FALSE, 
                                                                                                                                                                                 value = "facGender * contNormal"), list(randomSlopes = FALSE, 
                                                                                                                                                                                                                         value = "facExperim * contNormal"), list(randomSlopes = FALSE, 
                                                                                                                                                                                                                                                                  value = "facGender * facExperim * contNormal")), value = "facFive"))
  options$randomVariables <- "facFive"
  options$trendsContrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                       values = list()))
  options$trendsTrend <- list()
  set.seed(1)
  results <- jasptools::run("MixedModelsGLMM", "debug.csv", options)
  
  
  test_that("ANOVA Summary table results match", {
    table <- results[["results"]][["ANOVAsummary"]][["data"]]
    expect_equal_tables(table,
                        list(1, "facGender", 0.480796631332514, 0.497055825482136, 1, "facExperim",
                             0.552974967089275, 0.352014522528833, 1, "contNormal", 0.539437816661619,
                             0.376582698078323, 1, "facGender * facExperim", 0.808114989881373,
                             0.0589802670167785, 1, "facGender * contNormal", 0.340867270412155,
                             0.907166795685725, 1, "facExperim * contNormal", 0.994581145216534,
                             4.61255523589443e-05, 1, "facGender * facExperim * contNormal",
                             0.762077944500614, 0.091659505636386))
  })
  
  test_that("Estimated Marginal Means table results match", {
    table <- results[["results"]][["EMMsummary"]][["data"]]
    expect_equal_tables(table,
                        list(-1.24716219673316, "<unicode><unicode><unicode>", 0.187209737935553,
                             "f", -0.238591919874624, 1, 0.0526349798504055, 0.419417274131551,
                             -1.93790363963291, 0.61301139574573, -1.24716219673316, "<unicode><unicode><unicode>",
                             -0.658275944843351, "m", -1.13541927775844, 2, 0.000418193307941987,
                             0.469989142575165, -3.52832819872673, -0.181132611928264, -0.18874858754,
                             "<unicode><unicode><unicode>", -0.185314129321433, "f", -0.491944980986364,
                             3, 8.69286119636144e-05, 0.30203329087853, -3.92444861251449,
                             0.121316722343499, -0.18874858754, "<unicode><unicode><unicode>",
                             -0.578272151881219, "m", -0.888773274098223, 4, 2.46479272113402e-07,
                             0.305845531379072, -5.16035707556267, -0.267771029664216, 0.869665021653162,
                             "<unicode><unicode><unicode>", -0.557837996578418, "f", -1.1057336987236,
                             5, 0.00389438629915676, 0.53968066513392, -2.88659219650169,
                             -0.00994229443324157, 0.869665021653162, "<unicode><unicode><unicode>",
                             -0.498268358919088, "m", -0.879948488258046, 6, 6.74207843018523e-05,
                             0.37595729490769, -3.98520890327973, -0.116588229580129))
  })
  
  test_that("Contrasts table results match", {
    table <- results[["results"]][["contrasts_Means"]][["data"]]
    expect_equal_tables(table,
                        list("Contrast 1", "<unicode><unicode><unicode>", -0.845485682778904,
                             0.179528315454149, 0.629921145723605, -1.34220876457112))
  })
}
#### estimated trends + contrasts #####
{
  options <- jasptools::analysisOptions("MixedModelsGLMM")
  options$.meta <- list(Contrasts = list(list(levels = list(containsColumn = TRUE))), 
                        dependentVariable = list(containsColumn = TRUE), dependentVariableAggregation = list(
                          containsColumn = TRUE), fixedEffects = list(list(), list(), 
                                                                      list(), list(), list(), list(), list()), fixedVariables = list(
                                                                        containsColumn = TRUE), plotsAgregatedOver = list(containsColumn = TRUE), 
                        randomEffects = list(list(randomComponents = list(list(value = list(
                          containsColumn = TRUE)), list(value = list(containsColumn = TRUE)), 
                          list(value = list(containsColumn = TRUE)), list(value = list(
                            containsColumn = TRUE)), list(value = list(containsColumn = TRUE)), 
                          list(value = list(containsColumn = TRUE)), list(value = list(
                            containsColumn = TRUE))), value = list(containsColumn = TRUE))), 
                        randomVariables = list(containsColumn = TRUE), trendsContrasts = list(
                          list(levels = list(containsColumn = TRUE)), list(levels = list(
                            containsColumn = TRUE)), list(levels = list(containsColumn = TRUE))), 
                        trendsTrend = list(list()), trendsVariables = list(list(), 
                                                                           list()))
  options$Contrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                 values = list()))
  options$dependentVariable <- "contBinom"
  options$fixedEffects <- list(list(components = "facGender"), list(components = "facExperim"), 
                               list(components = "contNormal"), list(components = c("facGender", 
                                                                                    "facExperim")), list(components = c("facGender", "contNormal"
                                                                                    )), list(components = c("facExperim", "contNormal")), list(
                                                                                      components = c("facGender", "facExperim", "contNormal"
                                                                                      )))
  options$fixedVariables <- c("\"facGender\"", "\"facExperim\"", "\"contNormal\"")
  options$marginalMeansCompareTo <- 1
  options$marginalMeansResponse <- FALSE
  options$plotsAgregatedOver <- "facFive"
  options$randomEffects <- list(list(correlations = TRUE, randomComponents = list(list(randomSlopes = FALSE, 
                                                                                       value = "facGender"), list(randomSlopes = FALSE, value = "facExperim"), 
                                                                                  list(randomSlopes = FALSE, value = "contNormal"), list(randomSlopes = FALSE, 
                                                                                                                                         value = "facGender * facExperim"), list(randomSlopes = FALSE, 
                                                                                                                                                                                 value = "facGender * contNormal"), list(randomSlopes = FALSE, 
                                                                                                                                                                                                                         value = "facExperim * contNormal"), list(randomSlopes = FALSE, 
                                                                                                                                                                                                                                                                  value = "facGender * facExperim * contNormal")), value = "facFive"))
  options$randomVariables <- "facFive"
  options$trendsContrast <- TRUE
  options$trendsContrasts <- list(list(isContrast = FALSE, levels = c("Row 1", "Row 2", "Row 3", 
                                                                      "Row 4"), name = "facExperim", values = c("control", "experimental", 
                                                                                                                "control", "experimental")), list(isContrast = FALSE, levels = c("Row 1", 
                                                                                                                                                                                 "Row 2", "Row 3", "Row 4"), name = "facGender", values = c("f", 
                                                                                                                                                                                                                                            "f", "m", "m")), list(isContrast = TRUE, levels = c("Row 1", 
                                                                                                                                                                                                                                                                                                "Row 2", "Row 3", "Row 4"), name = "Contrast 1", values = c("0", 
                                                                                                                                                                                                                                                                                                                                                            "1", "-1", "0")))
  options$trendsDf <- "kenward-roger"
  options$trendsTrend <- list(list(variable = "contNormal"))
  options$trendsVariables <- list(list(variable = "facExperim"), list(variable = "facGender"))
  set.seed(1)
  results <- jasptools::run("MixedModelsGLMM", "debug.csv", options)
  
  
  test_that("ANOVA Summary table results match", {
    table <- results[["results"]][["ANOVAsummary"]][["data"]]
    expect_equal_tables(table,
                        list(1, "facGender", 0.480796631332514, 0.497055825482136, 1, "facExperim",
                             0.552974967089275, 0.352014522528833, 1, "contNormal", 0.539437816661619,
                             0.376582698078323, 1, "facGender * facExperim", 0.808114989881373,
                             0.0589802670167785, 1, "facGender * contNormal", 0.340867270412155,
                             0.907166795685725, 1, "facExperim * contNormal", 0.994581145216534,
                             4.61255523589443e-05, 1, "facGender * facExperim * contNormal",
                             0.762077944500614, 0.091659505636386))
  })
  
  test_that("Contrasts table results match", {
    table <- results[["results"]][["contrasts_Trends"]][["data"]]
    expect_equal_tables(table,
                        list("Contrast 1", "<unicode><unicode><unicode>", -0.430629631988971,
                             0.52447400521876, 0.676596944809906, -0.636464050410335))
  })
  
  test_that("Estimated Trends table results match", {
    table <- results[["results"]][["trendsSummary"]][["data"]]
    expect_equal_tables(table,
                        list("<unicode><unicode><unicode>", "control", "f", -1.04404007513637,
                             1, 0.388838699008104, -0.281930229285077, 0.480179616566217,
                             "<unicode><unicode><unicode>", "experimental", "f", -1.59326446261707,
                             2, 0.597595661773082, -0.421998488224448, 0.749267486168173,
                             "<unicode><unicode><unicode>", "control", "m", -0.61321459368389,
                             3, 0.317274063377415, 0.00863114376452298, 0.630476881212936,
                             "<unicode><unicode><unicode>", "experimental", "m", -0.759973494768535,
                             4, 0.460477417618264, 0.142545659457271, 1.04506481368308))
  })
}