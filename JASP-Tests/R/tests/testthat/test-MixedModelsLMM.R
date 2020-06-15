context("Linear Mixed Models")

#### ANOVA tables for each model test (+ different predictor types) ####
test_that("ANOVA Summary table results match (default)", {
  options <- jasptools::analysisOptions("MixedModelsLMM")
  options$dependentVariable <- "contNormal"
  options$fixedVariables    <- "contBinom"
  options$randomVariables   <- "facFive"
  options$fixedEffects      <- list(list(components = "contBinom"))
  options$randomEffects     <- list(
    list(
      correlations = TRUE,
      randomComponents = list(
        list(
          randomSlopes = TRUE,
          value = "contBinom")
      ),
      value = "facFive")
  )
  set.seed(1)
  results <- jasptools::run("MixedModelsLMM", "debug.csv", options)
  table   <- results[["results"]][["ANOVAsummary"]][["data"]]
  expect_equal_tables(table,
                      list("1, 11.68", "contBinom", 0.525071410422317, 0.429211584644305
                      ))
})
test_that("ANOVA Summary table results match (Kenward-Roger)", {
  options <- jasptools::analysisOptions("MixedModelsLMM")
  options$.meta <- list(Contrasts = list(list(levels = list(containsColumn = TRUE))), 
                        dependentVariable = list(containsColumn = TRUE), fixedEffects = list(
                          list(), list(), list()), fixedVariables = list(containsColumn = TRUE), 
                        plotsAgregatedOver = list(containsColumn = TRUE), randomEffects = list(
                          list(randomComponents = list(list(value = list(containsColumn = TRUE)), 
                                                       list(value = list(containsColumn = TRUE)), list(value = list(
                                                         containsColumn = TRUE))), value = list(containsColumn = TRUE))), 
                        randomVariables = list(containsColumn = TRUE), trendsContrasts = list(
                          list(levels = list(containsColumn = TRUE))))
  options$Contrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                 values = list()))
  options$dependentVariable <- "contNormal"
  options$fixedEffects <- list(list(components = "contBinom"), list(components = "contcor1"), 
                               list(components = c("contBinom", "contcor1")))
  options$fixedVariables <- c("\"contBinom\"", "\"contcor1\"")
  options$method <- "KR"
  options$plotsAgregatedOver <- "facFive"
  options$randomEffects <- list(list(correlations = TRUE, randomComponents = list(list(randomSlopes = TRUE, 
                                                                                       value = "contBinom"), list(randomSlopes = TRUE, value = "contcor1"), 
                                                                                  list(randomSlopes = TRUE, value = "contBinom * contcor1")), 
                                     value = "facFive"))
  options$randomVariables <- "facFive"
  options$trendsContrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                       values = list()))
  options$trendsTrend <- list()
  set.seed(1)
  results <- jasptools::run("MixedModelsLMM", "debug.csv", options)
  table <- results[["results"]][["ANOVAsummary"]][["data"]]
  expect_equal_tables(table,
                      list("1, 3.75", "contBinom", 0.864420973195014, 0.0334131021745932,
                           "1, 3.83", "contcor1", 0.218135125818658, 2.16585018438563,
                           "1, 3.28", "contBinom * contcor1", 0.203410316082062, 2.51301375360694
                      ))
})
test_that("ANOVA Summary table results match (LRT)", {
  options <- jasptools::analysisOptions("MixedModelsLMM")
  options$.meta <- list(Contrasts = list(list(levels = list(containsColumn = TRUE))), 
                        dependentVariable = list(containsColumn = TRUE), fixedEffects = list(
                          list(), list()), fixedVariables = list(containsColumn = TRUE), 
                        plotsAgregatedOver = list(containsColumn = TRUE), randomEffects = list(
                          list(randomComponents = list(list(value = list(containsColumn = TRUE)), 
                                                       list(value = list(containsColumn = TRUE))), value = list(
                                                         containsColumn = TRUE))), randomVariables = list(
                                                           containsColumn = TRUE), trendsContrasts = list(list(levels = list(
                                                             containsColumn = TRUE))))
  options$Contrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                 values = list()))
  options$dependentVariable <- "contNormal"
  options$fixedEffects <- list(list(components = "contBinom"), list(components = "contcor1"))
  options$fixedVariables <- c("\"contBinom\"", "\"contcor1\"")
  options$method <- "LRT"
  options$plotsAgregatedOver <- "facFive"
  options$randomEffects <- list(list(correlations = FALSE, randomComponents = list(list(
    randomSlopes = TRUE, value = "contBinom"), list(randomSlopes = FALSE, 
                                                    value = "contcor1")), value = "facFive"))
  options$randomVariables <- "facFive"
  options$test_intercept <- TRUE
  options$trendsContrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                       values = list()))
  options$trendsTrend <- list()
  set.seed(1)
  results <- jasptools::run("MixedModelsLMM", "debug.csv", options)
  table <- results[["results"]][["ANOVAsummary"]][["data"]]
  expect_equal_tables(table,
                      list(1, "Intercept", 0.271975662272865, 1.20675538818875, 1, "contBinom",
                           0.614498031861321, 0.253676677349858, 1, "contcor1", 0.129950545359242,
                           2.2930952101114))
})
test_that("ANOVA Summary table results match (PB)", {
  options <- jasptools::analysisOptions("MixedModelsLMM")
  options$.meta <- list(Contrasts = list(list(levels = list(containsColumn = TRUE))), 
                        dependentVariable = list(containsColumn = TRUE), fixedEffects = list(
                          list(), list(), list()), fixedVariables = list(containsColumn = TRUE), 
                        plotsAgregatedOver = list(containsColumn = TRUE), randomEffects = list(
                          list(randomComponents = list(list(value = list(containsColumn = TRUE)), 
                                                       list(value = list(containsColumn = TRUE)), list(value = list(
                                                         containsColumn = TRUE))), value = list(containsColumn = TRUE))), 
                        randomVariables = list(containsColumn = TRUE), trendsContrasts = list(
                          list(levels = list(containsColumn = TRUE))))
  options$Contrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                 values = list()))
  options$dependentVariable <- "contNormal"
  options$fixedEffects <- list(list(components = "contBinom"), list(components = "facGender"), 
                               list(components = c("contBinom", "facGender")))
  options$fixedVariables <- c("\"contBinom\"", "\"facGender\"")
  options$method <- "PB"
  options$plotsAgregatedOver <- "facFive"
  options$randomEffects <- list(list(correlations = TRUE, randomComponents = list(list(randomSlopes = FALSE, 
                                                                                       value = "contBinom"), list(randomSlopes = TRUE, value = "facGender"), 
                                                                                  list(randomSlopes = FALSE, value = "contBinom * facGender")), 
                                     value = "facFive"))
  options$randomVariables <- "facFive"
  options$test_intercept <- TRUE
  options$trendsContrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                       values = list()))
  options$trendsTrend <- list()
  set.seed(1)
  results <- jasptools::run("MixedModelsLMM", "debug.csv", options)
  table <- results[["results"]][["ANOVAsummary"]][["data"]]
  expect_equal_tables(table,
                      list(1, "Intercept", 0.295414395974365, 0.181818181818182, 1.09478063900156,
                           1, "contBinom", 0.73049974663167, 0.454545454545454, 0.118653265351327,
                           1, "facGender", 0.358308045977731, 0.363636363636364, 0.843814675227179,
                           1, "contBinom * facGender", 0.334875504547539, 0.454545454545454,
                           0.929951590597057))
})

#### overall summary ####
{
  options <- jasptools::analysisOptions("MixedModelsLMM")
  options$.meta <- list(Contrasts = list(list(levels = list(containsColumn = TRUE))), 
                        dependentVariable = list(containsColumn = TRUE), fixedEffects = list(
                          list(), list(), list()), fixedVariables = list(containsColumn = TRUE), 
                        plotsAgregatedOver = list(containsColumn = TRUE), randomEffects = list(
                          list(randomComponents = list(list(value = list(containsColumn = TRUE)), 
                                                       list(value = list(containsColumn = TRUE)), list(value = list(
                                                         containsColumn = TRUE))), value = list(containsColumn = TRUE))), 
                        randomVariables = list(containsColumn = TRUE), trendsContrasts = list(
                          list(levels = list(containsColumn = TRUE))))
  options$Contrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                 values = list()))
  options$dependentVariable <- "contNormal"
  options$fixedEffects <- list(list(components = "contBinom"), list(components = "facGender"), 
                               list(components = c("contBinom", "facGender")))
  options$fixedVariables <- c("\"contBinom\"", "\"facGender\"")
  options$plotsAgregatedOver <- "facFive"
  options$pvalVS <- TRUE
  options$randomEffects <- list(list(correlations = TRUE, randomComponents = list(list(randomSlopes = TRUE, 
                                                                                       value = "contBinom"), list(randomSlopes = TRUE, value = "facGender"), 
                                                                                  list(randomSlopes = TRUE, value = "contBinom * facGender")), 
                                     value = "facFive"))
  options$randomVariables <- "facFive"
  options$showFE <- TRUE
  options$showRE <- TRUE
  options$trendsContrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                       values = list()))
  options$trendsTrend <- list()
  set.seed(1)
  results <- jasptools::run("MixedModelsLMM", "debug.csv", options)
  
  
  test_that("ANOVA Summary table results match", {
    table <- results[["results"]][["ANOVAsummary"]][["data"]]
    expect_equal_tables(table,
                        list("1, 17.60", "contBinom", 0.733352922625956, 1, 0.119801203343151,
                             "1, 3.97", "facGender", 0.566721706516214, 1, 0.389235962132383,
                             "1, 6.25", "contBinom * facGender", 0.440543077870661, 1, 0.678097394430448
                        ))
  })
  
  test_that("Fixed Effects Estimates table results match", {
    table <- results[["results"]][["FEsummary"]][["data"]]
    expect_equal_tables(table,
                        list(3.68902156923157, -0.158282099742882, 0.367888096904366, 1, 0.154449751209726,
                             -1.0248129149004, "Intercept", 17.6020679929917, -0.0755914578281564,
                             0.733352922625956, 1, 0.218394718194177, -0.346123104318609,
                             "contBinom", 3.9744880886567, -0.125998707273309, 0.566721706516214,
                             1, 0.201957325173571, -0.623887780079385, "facGender (1)", 6.24639665656232,
                             -0.204985185013878, 0.440543077870661, 1, 0.248929539912091,
                             -0.823466692969696, "contBinom * facGender (1)"))
  })
  
  test_that("Random Effects: Correlation Estimates table results match", {
    table <- results[["results"]][["REsummary"]][["collection"]][["REsummary_CE1"]][["data"]]
    expect_equal_tables(table,
                        list(1, "Intercept", -0.996202813154937, 1, "contBinom", -0.843632345814824,
                             0.86021832881242, 1, "facGender (1)", 0.796104468470753, -0.815860352734006,
                             -0.99652622207547, 1, "contBinom * facGender (1)"))
  })
  
  test_that("Residual Variance Estimates table results match", {
    table <- results[["results"]][["REsummary"]][["collection"]][["REsummary_RES1"]][["data"]]
    expect_equal_tables(table,
                        list(1.00819762776764, 1.0040904480014))
  })
  
  test_that("Random Effects: Variance Estimates table results match", {
    table <- results[["results"]][["REsummary"]][["collection"]][["REsummary_VE1"]][["data"]]
    expect_equal_tables(table,
                        list(0.162883750586052, 0.403588590753073, "Intercept", 0.147435051184209,
                             0.383972721927234, "contBinom", 0.335201872076143, 0.57896620978788,
                             "facGender (1)", 0.306491969742199, 0.553617168937343, "contBinom * facGender (1)"
                        ))
  })
}

#### main summary for type II, RE without correlations ####
{
options <- jasptools::analysisOptions("MixedModelsLMM")
options$.meta <- list(Contrasts = list(list(levels = list(containsColumn = TRUE))), 
                      dependentVariable = list(containsColumn = TRUE), fixedEffects = list(
                        list(), list(), list()), fixedVariables = list(containsColumn = TRUE), 
                      plotsAgregatedOver = list(containsColumn = TRUE), randomEffects = list(
                        list(randomComponents = list(list(value = list(containsColumn = TRUE)), 
                                                     list(value = list(containsColumn = TRUE)), list(value = list(
                                                       containsColumn = TRUE))), value = list(containsColumn = TRUE))), 
                      randomVariables = list(containsColumn = TRUE), trendsContrasts = list(
                        list(levels = list(containsColumn = TRUE))))
options$Contrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                               values = list()))
options$dependentVariable <- "contNormal"
options$fixedEffects <- list(list(components = "contBinom"), list(components = "facGender"), 
                             list(components = c("contBinom", "facGender")))
options$fixedVariables <- c("\"contBinom\"", "\"facGender\"")
options$plotsAgregatedOver <- "facFive"
options$pvalVS <- TRUE
options$randomEffects <- list(list(correlations = FALSE, randomComponents = list(list(
  randomSlopes = TRUE, value = "contBinom"), list(randomSlopes = TRUE, 
                                                  value = "facGender"), list(randomSlopes = TRUE, value = "contBinom * facGender")), 
  value = "facFive"))
options$randomVariables <- "facFive"
options$showFE <- TRUE
options$showRE <- TRUE
options$trendsContrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                     values = list()))
options$trendsTrend <- list()
options$type <- "2"
set.seed(1)
results <- jasptools::run("MixedModelsLMM", "debug.csv", options)

test_that("ANOVA Summary table results match (type II, RE without correlations)", {
  table <- results[["results"]][["ANOVAsummary"]][["data"]]
  expect_equal_tables(table,
                      list("1, 95.47", "contBinom", 0.744933457420901, 1, 0.106452765533696,
                           "1, 14.05", "facGender", 0.384709793229879, 1, 0.805031165383322,
                           "1, 95.87", "contBinom * facGender", 0.319265108537529, 1.00922809226254,
                           1.00234106128648))
})
test_that("Fixed Effects Estimates table results match (type II, RE without correlations)", {
  table <- results[["results"]][["FEsummary"]][["data"]]
  expect_equal_tables(table,
                      list(13.3532700084219, -0.153031958155717, 0.334458673374783, 1.00427576774849,
                           0.152825635260345, -1.00135005423024, "Intercept", 95.5727935385195,
                           -0.0606505470010832, 0.775793413620051, 1, 0.212352946700071,
                           -0.285611986758755, "contBinom", 14.0504187901984, -0.137120553151975,
                           0.384709793229879, 1, 0.152825635260345, -0.897235289867336,
                           "facGender (1)", 95.8655974542156, -0.212601367024558, 0.319265108537529,
                           1.00922809226254, 0.212352946700071, -1.00116984637297, "contBinom * facGender (1)"
                      ))
})
test_that("Random Effects: Correlation Estimates table results match (type II, RE without correlations)", {
  table <- results[["results"]][["REsummary"]][["collection"]][["REsummary_CE3"]][["data"]]
  expect_equal_tables(table,
                      list(1, "facGender (f)", "NaN", 1, "facGender (m)"))
})
test_that("Random Effects: Correlation Estimates table results match (type II, RE without correlations)", {
  table <- results[["results"]][["REsummary"]][["collection"]][["REsummary_CE4"]][["data"]]
  expect_equal_tables(table,
                      list(1, "contBinom * facGender (f)", "NaN", 1, "contBinom * facGender (m)"
                      ))
})
test_that("Residual Variance Estimates table results match (type II, RE without correlations)", {
  table <- results[["results"]][["REsummary"]][["collection"]][["REsummary_RES4"]][["data"]]
  expect_equal_tables(table,
                      list(1.02483160325087, 1.01233966792321))
})
test_that("Random Effects: Variance Estimates table results match (type II, RE without correlations)", {
  table <- results[["results"]][["REsummary"]][["collection"]][["REsummary_VE1"]][["data"]]
  expect_equal_tables(table,
                      list(0, 0, "Intercept"))
})
test_that("Random Effects: Variance Estimates table results match (type II, RE without correlations)", {
  table <- results[["results"]][["REsummary"]][["collection"]][["REsummary_VE2"]][["data"]]
  expect_equal_tables(table,
                      list(0, 0, "contBinom"))
})
test_that("Random Effects: Variance Estimates table results match (type II, RE without correlations)", {
  table <- results[["results"]][["REsummary"]][["collection"]][["REsummary_VE3"]][["data"]]
  expect_equal_tables(table,
                      list(0, 0, "facGender (f)", 0.305896298128008, 0.553078925767388, "facGender (m)"
                      ))
})
test_that("Random Effects: Variance Estimates table results match (type II, RE without correlations)", {
  table <- results[["results"]][["REsummary"]][["collection"]][["REsummary_VE4"]][["data"]]
  expect_equal_tables(table,
                      list(0, 0, "contBinom * facGender (f)", 0.000210590326931208, 0.0145117306662992,
                           "contBinom * facGender (m)"))
})
}
#### two random effects grouping factors (2x RE grouping, type II, LRT) ####
{
  options <- jasptools::analysisOptions("MixedModelsLMM")
  options$.meta <- list(Contrasts = list(list(levels = list(containsColumn = TRUE))), 
                        dependentVariable = list(containsColumn = TRUE), fixedEffects = list(
                          list(), list(), list()), fixedVariables = list(containsColumn = TRUE), 
                        plotsAgregatedOver = list(containsColumn = TRUE), randomEffects = list(
                          list(randomComponents = list(list(value = list(containsColumn = TRUE)), 
                                                       list(value = list(containsColumn = TRUE)), list(value = list(
                                                         containsColumn = TRUE))), value = list(containsColumn = TRUE)), 
                          list(randomComponents = list(list(value = list(containsColumn = TRUE)), 
                                                       list(value = list(containsColumn = TRUE)), list(value = list(
                                                         containsColumn = TRUE))), value = list(containsColumn = TRUE))), 
                        randomVariables = list(containsColumn = TRUE), trendsContrasts = list(
                          list(levels = list(containsColumn = TRUE))))
  options$Contrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                 values = list()))
  options$dependentVariable <- "contNormal"
  options$fixedEffects <- list(list(components = "contBinom"), list(components = "facExperim"), 
                               list(components = c("contBinom", "facExperim")))
  options$fixedVariables <- c("\"contBinom\"", "\"facExperim\"")
  options$method <- "LRT"
  options$plotsAgregatedOver <- c("\"facFive\"", "\"facGender\"")
  options$randomEffects <- list(list(correlations = TRUE, randomComponents = list(list(randomSlopes = TRUE, 
                                                                                       value = "contBinom"), list(randomSlopes = TRUE, value = "facExperim"), 
                                                                                  list(randomSlopes = TRUE, value = "contBinom * facExperim")), 
                                     value = "facFive"), list(correlations = FALSE, randomComponents = list(
                                       list(randomSlopes = TRUE, value = "contBinom"), list(randomSlopes = TRUE, 
                                                                                            value = "facExperim"), list(randomSlopes = TRUE, value = "contBinom * facExperim")), 
                                       value = "facGender"))
  options$randomVariables <- c("\"facFive\"", "\"facGender\"")
  options$showRE <- TRUE
  options$trendsContrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                       values = list()))
  options$trendsTrend <- list()
  options$type <- "2"
  set.seed(1)
  results <- jasptools::run("MixedModelsLMM", "debug.csv", options)
  
  
  test_that("ANOVA Summary table results match (2x RE grouping, type II, LRT)", {
    table <- results[["results"]][["ANOVAsummary"]][["data"]]
    expect_equal_tables(table,
                        list(1, "contBinom", 0.686729209619707, 0.162649061054537, 1, "facExperim",
                             1, 0, 1, "contBinom * facExperim", 0.750514493307942, 0.101099220184494
                        ))
  })
  test_that("Random Effects (1): Correlation Estimates table results match (2x RE grouping, type II, LRT)", {
    table <- results[["results"]][["REsummary"]][["collection"]][["REsummary_CE1"]][["data"]]
    expect_equal_tables(table,
                        list(1, "Intercept", -0.990850213706184, 1, "contBinom", 0.864398640455826,
                             -0.924351503276018, 1, "facExperim (1)", -0.300316746845771,
                             0.168898812831609, 0.219758321991567, 1, "contBinom * facExperim (1)"
                        ))
  })
  test_that("Random Effects (2): Correlation Estimates table results match (2x RE grouping, type II, LRT)", {
    table <- results[["results"]][["REsummary"]][["collection"]][["REsummary_CE4"]][["data"]]
    expect_equal_tables(table,
                        list(1, "facExperim (control)", 0.999994377996593, 1, "facExperim (experimental)"
                        ))
  })
  test_that("facGender.3: Correlation Estimates table results match (2x RE grouping, type II, LRT)", {
    table <- results[["results"]][["REsummary"]][["collection"]][["REsummary_CE5"]][["data"]]
    expect_equal_tables(table,
                        list(1, "contBinom * facExperim (control)", 1, 1, "contBinom * facExperim (experimental)"
                        ))
  })
  test_that("Residual Variance Estimates table results match (2x RE grouping, type II, LRT)", {
    table <- results[["results"]][["REsummary"]][["collection"]][["REsummary_RES5"]][["data"]]
    expect_equal_tables(table,
                        list(0.997490363614008, 0.998744393533204))
  })
  test_that("Random Effects (1): Variance Estimates table results match (2x RE grouping, type II, LRT)", {
    table <- results[["results"]][["REsummary"]][["collection"]][["REsummary_VE1"]][["data"]]
    expect_equal_tables(table,
                        list(0.186111343551722, 0.431406239583669, "Intercept", 0.116622701422985,
                             0.341500660940773, "contBinom", 0.143680101697961, 0.37905158184337,
                             "facExperim (1)", 0.221424700926424, 0.47055786140115, "contBinom * facExperim (1)"
                        ))
  })
  test_that("Random Effects (2): Variance Estimates table results match (2x RE grouping, type II, LRT)", {
    table <- results[["results"]][["REsummary"]][["collection"]][["REsummary_VE2"]][["data"]]
    expect_equal_tables(table,
                        list(0.000627024839020522, 0.02504046403365, "Intercept"))
  })
  test_that("Random Effects (2): Variance Estimates table results match (2x RE grouping, type II, LRT)", {
    table <- results[["results"]][["REsummary"]][["collection"]][["REsummary_VE3"]][["data"]]
    expect_equal_tables(table,
                        list(0.0175898156961553, 0.132626602520593, "contBinom"))
  })
  test_that("Random Effects (2): Variance Estimates table results match (2x RE grouping, type II, LRT)", {
    table <- results[["results"]][["REsummary"]][["collection"]][["REsummary_VE4"]][["data"]]
    expect_equal_tables(table,
                        list(0.265113576476425, 0.514891810457717, "facExperim (control)",
                             0.124480200632079, 0.352817517467712, "facExperim (experimental)"
                        ))
  })
  test_that("Random Effects (2): Variance Estimates table results match (2x RE grouping, type II, LRT)", {
    table <- results[["results"]][["REsummary"]][["collection"]][["REsummary_VE5"]][["data"]]
    expect_equal_tables(table,
                        list(0.112209836715714, 0.334977367467884, "contBinom * facExperim (control)",
                             0.0834626046074974, 0.288898952243682, "contBinom * facExperim (experimental)"
                        ))
  })
}
#### estimated marginal means + contrasts ####
{
  options <- jasptools::analysisOptions("MixedModelsLMM")
  options$.meta <- list(Contrasts = list(list(levels = list(containsColumn = TRUE)), 
                                         list(levels = list(containsColumn = TRUE)), list(levels = list(
                                           containsColumn = TRUE))), dependentVariable = list(containsColumn = TRUE), 
                        fixedEffects = list(list(), list(), list()), fixedVariables = list(
                          containsColumn = TRUE), marginalMeans = list(list()), 
                        plotsAgregatedOver = list(containsColumn = TRUE), randomEffects = list(
                          list(randomComponents = list(list(value = list(containsColumn = TRUE)), 
                                                       list(value = list(containsColumn = TRUE)), list(value = list(
                                                         containsColumn = TRUE))), value = list(containsColumn = TRUE))), 
                        randomVariables = list(containsColumn = TRUE), trendsContrasts = list(
                          list(levels = list(containsColumn = TRUE))))
  options$Contrasts <- list(list(isContrast = FALSE, levels = c("Row 1", "Row 2"), name = "contBinom", 
                                 values = c("0", "1")), list(isContrast = TRUE, levels = c("Row 1", 
                                                                                           "Row 2"), name = "Contrast 1", values = c("1", "-1")), list(isContrast = TRUE, 
                                                                                                                                                       levels = c("Row 1", "Row 2"), name = "Contrast 2", values = c("-1", 
                                                                                                                                                                                                                     "1")))
  options$dependentVariable <- "contNormal"
  options$fixedEffects <- list(list(components = "contBinom"), list(components = "contGamma"), 
                               list(components = c("contBinom", "contGamma")))
  options$fixedVariables <- c("\"contBinom\"", "\"contGamma\"")
  options$marginalMeans <- list(list(variable = "contBinom"))
  options$marginalMeansContrast <- TRUE
  options$plotsAgregatedOver <- "facExperim"
  options$randomEffects <- list(list(correlations = TRUE, randomComponents = list(list(randomSlopes = FALSE, 
                                                                                       value = "contBinom"), list(randomSlopes = FALSE, value = "contGamma"), 
                                                                                  list(randomSlopes = FALSE, value = "contBinom * contGamma")), 
                                     value = "facExperim"))
  options$randomVariables <- "facExperim"
  options$trendsContrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                       values = list()))
  options$trendsTrend <- list()
  set.seed(1)
  results <- jasptools::run("MixedModelsLMM", "debug.csv", options)
  
  
  test_that("ANOVA Summary table results match", {
    table <- results[["results"]][["ANOVAsummary"]][["data"]]
    expect_equal_tables(table,
                        list("1, 96", "contBinom", 0.0933901140243668, 2.87172168648119, "1, 96",
                             "contGamma", 0.183125458399541, 1.79794345791228, "1, 96", "contBinom * contGamma",
                             0.138528059496446, 2.23120340454709))
  })
  
  test_that("Estimated Marginal Means table results match", {
    table <- results[["results"]][["EMMsummary"]][["data"]]
    expect_equal_tables(table,
                        list(0, "<unicode><unicode><unicode>", -0.106804244278956, -0.379540470414914,
                             1, 0.139153692765411, 0.165931981857002, 1, "<unicode><unicode><unicode>",
                             -0.262222077646193, -0.584999952371153, 2, 0.164685615282215,
                             0.0605557970787673))
  })
  
  test_that("Contrasts table results match", {
    table <- results[["results"]][["contrasts_Means"]][["data"]]
    expect_equal_tables(table,
                        list("Contrast 1", "<unicode><unicode><unicode>", 0.155417833367237,
                             0.942005551662742, 0.215604040062175, 0.720848428083341, "Contrast 2",
                             "<unicode><unicode><unicode>", -0.155417833367237, 0.942005551662742,
                             0.215604040062175, -0.720848428083341))
  })
}
#### estimated marginal means + contrasts (with continuous variable) ####
{
  options <- jasptools::analysisOptions("MixedModelsLMM")
  options$.meta <- list(Contrasts = list(list(levels = list(containsColumn = TRUE)), 
                                         list(levels = list(containsColumn = TRUE)), list(levels = list(
                                           containsColumn = TRUE)), list(levels = list(containsColumn = TRUE))), 
                        dependentVariable = list(containsColumn = TRUE), fixedEffects = list(
                          list(), list(), list()), fixedVariables = list(containsColumn = TRUE), 
                        marginalMeans = list(list(), list()), plotsAgregatedOver = list(
                          containsColumn = TRUE), randomEffects = list(list(randomComponents = list(
                            list(value = list(containsColumn = TRUE)), list(value = list(
                              containsColumn = TRUE)), list(value = list(containsColumn = TRUE))), 
                            value = list(containsColumn = TRUE))), randomVariables = list(
                              containsColumn = TRUE), trendsContrasts = list(list(levels = list(
                                containsColumn = TRUE))))
  options$Contrasts <- list(list(isContrast = FALSE, levels = c("Row 1", "Row 2", "Row 3", 
                                                                "Row 4", "Row 5", "Row 6"), name = "contBinom", values = c("0", 
                                                                                                                           "1", "0", "1", "0", "1")), list(isContrast = FALSE, levels = c("Row 1", 
                                                                                                                                                                                          "Row 2", "Row 3", "Row 4", "Row 5", "Row 6"), name = "contGamma", 
                                                                                                                                                           values = c("-0.5", "-0.5", "0", "0", "0.5", "0.5")), list(
                                                                                                                                                             isContrast = TRUE, levels = c("Row 1", "Row 2", "Row 3", 
                                                                                                                                                                                           "Row 4", "Row 5", "Row 6"), name = "Contrast 1", values = c("1", 
                                                                                                                                                                                                                                                       "-1", "1", "-1", "1", "-1")), list(isContrast = TRUE, levels = c("Row 1", 
                                                                                                                                                                                                                                                                                                                        "Row 2", "Row 3", "Row 4", "Row 5", "Row 6"), name = "Contrast 2", 
                                                                                                                                                                                                                                                                                          values = c("-1", "1", "-1", "1", "-1", "1")))
  options$dependentVariable <- "contNormal"
  options$fixedEffects <- list(list(components = "contBinom"), list(components = "contGamma"), 
                               list(components = c("contBinom", "contGamma")))
  options$fixedVariables <- c("\"contBinom\"", "\"contGamma\"")
  options$marginalMeans <- list(list(variable = "contBinom"), list(variable = "contGamma"))
  options$marginalMeansAdjustment <- "mvt"
  options$marginalMeansContrast <- TRUE
  options$marginalMeansDf <- "satterthwaite"
  options$marginalMeansSD <- 0.5
  options$plotsAgregatedOver <- "facExperim"
  options$randomEffects <- list(list(correlations = TRUE, randomComponents = list(list(randomSlopes = FALSE, 
                                                                                       value = "contBinom"), list(randomSlopes = FALSE, value = "contGamma"), 
                                                                                  list(randomSlopes = FALSE, value = "contBinom * contGamma")), 
                                     value = "facExperim"))
  options$randomVariables <- "facExperim"
  options$trendsContrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                       values = list()))
  options$trendsTrend <- list()
  set.seed(1)
  results <- jasptools::run("MixedModelsLMM", "debug.csv", options)
  
  
  test_that("ANOVA Summary table results match", {
    table <- results[["results"]][["ANOVAsummary"]][["data"]]
    expect_equal_tables(table,
                        list("1, 96", "contBinom", 0.0933901140243668, 2.87172168648119, "1, 96",
                             "contGamma", 0.183125458399541, 1.79794345791228, "1, 96", "contBinom * contGamma",
                             0.138528059496446, 2.23120340454709))
  })
  
  test_that("Estimated Marginal Means table results match", {
    table <- results[["results"]][["EMMsummary"]][["data"]]
    expect_equal_tables(table,
                        list(0, 1.26675523310478, 96.0000000027921, -0.0232381685713805, -0.333855185906882,
                             1, 0.156483361370941, 0.287378848764121, 1, 1.26675523310478,
                             96.0000000004324, -0.358805450719484, -0.71986115129474, 2,
                             0.181893478189937, 0.00225024985577182, 0, 2.03296079621, 96.0000000035608,
                             -0.106804244278956, -0.383022141308582, 3, 0.139153692765411,
                             0.169413652750671, 1, 2.03296079621, 95.9999999985044, -0.262222077646193,
                             -0.589120440314877, 4, 0.164685615282215, 0.0646762850224916,
                             0, 2.79916635931522, 96.0000000036619, -0.190370319986531, -0.484848233628597,
                             5, 0.148352766282746, 0.104107593655534, 1, 2.79916635931522,
                             95.9999999981647, -0.165638704572901, -0.574758614862199, 6,
                             0.206107377229298, 0.243481205716397))
  })
  
  test_that("Contrasts table results match", {
    table <- results[["results"]][["contrasts_Means"]][["data"]]
    expect_equal_tables(table,
                        list("Contrast 1", 95.9999999982605, 0.46625350010171, 0.472754525182752,
                             0.646812120186526, 0.720848428083341, "Contrast 2", 95.9999999982605,
                             -0.46625350010171, 0.472754525182752, 0.646812120186526, -0.720848428083341
                        ))
  })
}
#### estimated trends + contrasts ####
{
  options <- jasptools::analysisOptions("MixedModelsLMM")
  options$.meta <- list(Contrasts = list(list(levels = list(containsColumn = TRUE))), 
                        dependentVariable = list(containsColumn = TRUE), fixedEffects = list(
                          list(), list(), list()), fixedVariables = list(containsColumn = TRUE), 
                        plotsAgregatedOver = list(containsColumn = TRUE), randomEffects = list(
                          list(randomComponents = list(list(value = list(containsColumn = TRUE)), 
                                                       list(value = list(containsColumn = TRUE)), list(value = list(
                                                         containsColumn = TRUE))), value = list(containsColumn = TRUE))), 
                        randomVariables = list(containsColumn = TRUE), trendsContrasts = list(
                          list(levels = list(containsColumn = TRUE)), list(levels = list(
                            containsColumn = TRUE))), trendsTrend = list(list()), 
                        trendsVariables = list(list()))
  options$Contrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                 values = list()))
  options$dependentVariable <- "contNormal"
  options$fixedEffects <- list(list(components = "contGamma"), list(components = "contBinom"), 
                               list(components = c("contGamma", "contBinom")))
  options$fixedVariables <- c("\"contGamma\"", "\"contBinom\"")
  options$plotsAgregatedOver <- "facFive"
  options$randomEffects <- list(list(correlations = TRUE, randomComponents = list(list(randomSlopes = FALSE, 
                                                                                       value = "contGamma"), list(randomSlopes = FALSE, value = "contBinom"), 
                                                                                  list(randomSlopes = FALSE, value = "contGamma * contBinom")), 
                                     value = "facFive"))
  options$randomVariables <- "facFive"
  options$trendsContrast <- TRUE
  options$trendsContrasts <- list(list(isContrast = FALSE, levels = c("Row 1", "Row 2"), name = "contBinom", 
                                       values = c("0", "1")), list(isContrast = TRUE, levels = c("Row 1", 
                                                                                                 "Row 2"), name = "Contrast 1", values = c("-1", "1")))
  options$trendsTrend <- list(list(variable = "contGamma"))
  options$trendsVariables <- list(list(variable = "contBinom"))
  set.seed(1)
  results <- jasptools::run("MixedModelsLMM", "debug.csv", options)
  
  
  test_that("ANOVA Summary table results match", {
    table <- results[["results"]][["ANOVAsummary"]][["data"]]
    expect_equal_tables(table,
                        list("1, 96", "contGamma", 0.183125458399537, 1.79794345791228, "1, 96",
                             "contBinom", 0.0933901140243669, 2.87172168648118, "1, 96",
                             "contGamma * contBinom", 0.138528059496446, 2.23120340454709
                        ))
  })
  
  test_that("Contrasts table results match", {
    table <- results[["results"]][["contrasts_Trends"]][["data"]]
    expect_equal_tables(table,
                        list("Contrast 1", "<unicode><unicode><unicode>", 0.235118951695902,
                             0.135248471691765, 0.15740483003789, 1.49372132760669))
  })
  
  test_that("Estimated Trends table results match", {
    table <- results[["results"]][["trendsSummary"]][["data"]]
    expect_equal_tables(table,
                        list(0, "<unicode><unicode><unicode>", -0.268485551313865, 1, 0.0813385998424251,
                             -0.109064825069797, 0.0503559011742717, 1, "<unicode><unicode><unicode>",
                             -0.138071020583519, 2, 0.134760204418555, 0.126054126626106,
                             0.39017927383573))
  })
}
#### estimated trends + contrasts (with test against 0) ####
{
  options <- jasptools::analysisOptions("MixedModelsLMM")
  options$.meta <- list(Contrasts = list(list(levels = list(containsColumn = TRUE))), 
                        dependentVariable = list(containsColumn = TRUE), fixedEffects = list(
                          list(), list(), list()), fixedVariables = list(containsColumn = TRUE), 
                        plotsAgregatedOver = list(containsColumn = TRUE), randomEffects = list(
                          list(randomComponents = list(list(value = list(containsColumn = TRUE)), 
                                                       list(value = list(containsColumn = TRUE)), list(value = list(
                                                         containsColumn = TRUE))), value = list(containsColumn = TRUE))), 
                        randomVariables = list(containsColumn = TRUE), trendsContrasts = list(
                          list(levels = list(containsColumn = TRUE)), list(levels = list(
                            containsColumn = TRUE)), list(levels = list(containsColumn = TRUE))), 
                        trendsTrend = list(list()), trendsVariables = list(list()))
  options$Contrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                 values = list()))
  options$dependentVariable <- "contNormal"
  options$fixedEffects <- list(list(components = "contGamma"), list(components = "contBinom"), 
                               list(components = c("contGamma", "contBinom")))
  options$fixedVariables <- c("\"contGamma\"", "\"contBinom\"")
  options$plotsAgregatedOver <- "facFive"
  options$randomEffects <- list(list(correlations = TRUE, randomComponents = list(list(randomSlopes = FALSE, 
                                                                                       value = "contGamma"), list(randomSlopes = FALSE, value = "contBinom"), 
                                                                                  list(randomSlopes = FALSE, value = "contGamma * contBinom")), 
                                     value = "facFive"))
  options$randomVariables <- "facFive"
  options$trendsCIwidth <- 0.8
  options$trendsCompare <- TRUE
  options$trendsContrast <- TRUE
  options$trendsContrasts <- list(list(isContrast = FALSE, levels = c("Row 1", "Row 2"), name = "contBinom", 
                                       values = c("0", "1")), list(isContrast = TRUE, levels = c("Row 1", 
                                                                                                 "Row 2"), name = "Contrast 1", values = c("-1", "1")), list(isContrast = TRUE, 
                                                                                                                                                             levels = c("Row 1", "Row 2"), name = "Contrast 2", values = c("-.5", 
                                                                                                                                                                                                                           ".5")))
  options$trendsDf <- "kenward-roger"
  options$trendsTrend <- list(list(variable = "contGamma"))
  options$trendsVariables <- list(list(variable = "contBinom"))
  set.seed(1)
  results <- jasptools::run("MixedModelsLMM", "debug.csv", options)
  
  
  test_that("ANOVA Summary table results match", {
    table <- results[["results"]][["ANOVAsummary"]][["data"]]
    expect_equal_tables(table,
                        list("1, 96", "contGamma", 0.183125458399537, 1.79794345791228, "1, 96",
                             "contBinom", 0.0933901140243669, 2.87172168648118, "1, 96",
                             "contGamma * contBinom", 0.138528059496446, 2.23120340454709
                        ))
  })
  
  test_that("Contrasts table results match", {
    table <- results[["results"]][["contrasts_Trends"]][["data"]]
    expect_equal_tables(table,
                        list("Contrast 1", 95.956367668831, 0.235118951695902, 0.295483972845833,
                             0.161115005404534, 1.459323736517, "Contrast 2", 95.956367668831,
                             0.117559475847951, 0.295483972845833, 0.0805575027022672, 1.459323736517
                        ))
  })
  
  test_that("Estimated Trends table results match", {
    table <- results[["results"]][["trendsSummary"]][["data"]]
    expect_equal_tables(table,
                        list(0, 94.5332968093253, -0.217500332333939, 1, 0.197424466754143,
                             0.0840213309508411, -0.109064825069797, -1.29806114513477, -0.000629317805654103,
                             1, 94.7809500135927, -0.0534334089623999, 2, 0.367048151762761,
                             0.139078604153612, 0.126054126626106, 0.906351680714879, 0.305541662214611
                        ))
  })
}