context("Bayesian Linear Mixed Models")

#### overall summary ####
{
  options <- jasptools::analysisOptions("MixedModelsBLMM")
  options$.meta <- list(Contrasts = list(list(levels = list(containsColumn = TRUE))), 
                        dependentVariable = list(containsColumn = TRUE), fixedEffects = list(
                          list()), fixedVariables = list(containsColumn = TRUE), 
                        plotsAgregatedOver = list(containsColumn = TRUE), randomEffects = list(
                          list(randomComponents = list(list(value = list(containsColumn = TRUE))), 
                               value = list(containsColumn = TRUE))), randomVariables = list(
                                 containsColumn = TRUE), trendsContrasts = list(list(levels = list(
                                   containsColumn = TRUE))))
  options$Contrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                 values = list()))
  options$chains <- 1
  options$dependentVariable <- "contNormal"
  options$fixedEffects <- list(list(components = "contBinom"))
  options$fixedVariables <- "contBinom"
  options$iteration <- 1000
  options$plotsAgregatedOver <- "facFive"
  options$randomEffects <- list(list(correlations = TRUE, randomComponents = list(list(randomSlopes = TRUE, 
                                                                                       value = "contBinom")), value = "facFive"))
  options$randomVariables <- "facFive"
  options$samplingVariable1 <- list()
  options$samplingVariable2 <- list()
  options$trendsContrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                       values = list()))
  options$trendsTrend <- list()
  options$warmup <- 500
  set.seed(1)
  results <- jasptools::run("MixedModelsBLMM", "debug.csv", options)
  
  
  test_that("Intercept table results match", {
    table <- results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_1"]][["data"]]
    expect_equal_tables(table,
                        list(662.60163246803, 292.159174795872, -0.12165108951337, -0.432882377056572,
                             0.998966268787717, 0.150674719708029, 0.203791035234034))
  })
  
  test_that("contBinom (difference from intercept) table results match", {
    table <- results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_2"]][["data"]]
    expect_equal_tables(table,
                        list(917.432770730613, 372.264532224382, -0.143664092724055, -0.543406865752885,
                             1.00372509955491, 0.230252726049724, 0.284400170765089))
  })
  
  
  
}
#### overall summary (continuous predictior) ####
{
  options <- jasptools::analysisOptions("MixedModelsBLMM")
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
  options$chains <- 1
  options$dependentVariable <- "contNormal"
  options$fixedEffects <- list(list(components = "contGamma"), list(components = "contBinom"), 
                               list(components = c("contGamma", "contBinom")))
  options$fixedVariables <- c("\"contGamma\"", "\"contBinom\"")
  options$iteration <- 1000
  options$plotsAgregatedOver <- "facFive"
  options$randomEffects <- list(list(correlations = TRUE, randomComponents = list(list(randomSlopes = FALSE, 
                                                                                       value = "contGamma"), list(randomSlopes = FALSE, value = "contBinom"), 
                                                                                  list(randomSlopes = FALSE, value = "contGamma * contBinom")), 
                                     value = "facFive"))
  options$randomVariables <- "facFive"
  options$samplingVariable1 <- list()
  options$samplingVariable2 <- list()
  options$showFE <- TRUE
  options$showRE <- TRUE
  options$trendsContrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                       values = list()))
  options$trendsTrend <- list()
  options$warmup <- 500
  set.seed(1)
  results <- jasptools::run("MixedModelsBLMM", "debug.csv", options)
  
  
  test_that("Fixed Effects Estimates table results match", {
    table <- results[["results"]][["FEsummary"]][["data"]]
    expect_equal_tables(table,
                        list(0.0795319521704863, -0.426542776013684, 274.57359183437, 1.00418527303096,
                             0.259612496259921, "Intercept", 0.560143711903638, -0.104833994645794,
                             -0.256916105739368, 283.624240569115, 1.00409856733932, 0.0869123104687506,
                             "contGamma", 0.0709879142608917, -0.622423578387246, -1.30131842427563,
                             255.160143947052, 1.00290213276225, 0.367252611972785, "contBinom",
                             0.0883075171017041, 0.238212498722272, -0.0467286128927047,
                             168.621571243963, 0.99997761609969, 0.151810072746981, "contGamma * contBinom",
                             0.52954632392738))
  })
  
  test_that("Residual Variance Estimates table results match", {
    table <- results[["results"]][["REsummary"]][["collection"]][["REsummary_RES"]][["data"]]
    expect_equal_tables(table,
                        list(1.04960365943981, 1.02450166395171))
  })
  
  test_that("facFive: Variance Estimates table results match", {
    table <- results[["results"]][["REsummary"]][["collection"]][["REsummary_VE1"]][["data"]]
    expect_equal_tables(table,
                        list(0.249912739284678, 0.499912731668916, "Intercept"))
  })
  
  test_that("Intercept table results match", {
    table <- results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_1"]][["data"]]
    expect_equal_tables(table,
                        list(269.785270222651, 265.78287501894, 0.0795319521704863, -0.426542776013684,
                             1.02841464060011, 0.259869170824954, 0.560143711903638))
  })
  
  test_that("contGamma (difference from intercept) table results match", {
    table <- results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_2"]][["data"]]
    expect_equal_tables(table,
                        list(276.19599119831, 239.47692636878, -0.104833994645794, -0.256916105739368,
                             1.01306013951732, 0.0950075287765779, 0.0709879142608917))
  })
  
  test_that("contBinom (difference from intercept) table results match", {
    table <- results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_3"]][["data"]]
    expect_equal_tables(table,
                        list(250.948495927238, 141.383549606953, -0.622423578387246, -1.30131842427563,
                             1.00250771686331, 0.377588978444019, 0.0883075171017041))
  })
  
  test_that("contGamma*contBinom (difference from intercept) table results match", {
    table <- results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_4"]][["data"]]
    expect_equal_tables(table,
                        list(276.19599119831, 239.47692636878, -0.104833994645795, "contBinom0",
                             -0.256916105739369, 1.01306013951732, 0.0950075287765783, 0.070987914260892,
                             289.784841347041, 200.944449083032, 0.133378504076478, "contBinom1",
                             -0.113642742767884, 1.00369152353465, 0.120256119509182, 0.384735256495964
                        ))
  })
}
#### overall summary (deviation from the mean + RE/FE) ####
{
  options <- jasptools::analysisOptions("MixedModelsBLMM")
  options$.meta <- list(Contrasts = list(list(levels = list(containsColumn = TRUE))), 
                        dependentVariable = list(containsColumn = TRUE), fixedEffects = list(
                          list()), fixedVariables = list(containsColumn = TRUE), 
                        plotsAgregatedOver = list(containsColumn = TRUE), randomEffects = list(
                          list(randomComponents = list(list(value = list(containsColumn = TRUE))), 
                               value = list(containsColumn = TRUE))), randomVariables = list(
                                 containsColumn = TRUE), trendsContrasts = list(list(levels = list(
                                   containsColumn = TRUE))))
  options$Contrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                 values = list()))
  options$chains <- 1
  options$dependentVariable <- "contNormal"
  options$fixedEffects <- list(list(components = "contBinom"))
  options$fixedVariables <- "contBinom"
  options$iteration <- 1000
  options$plotsAgregatedOver <- "facFive"
  options$randomEffects <- list(list(correlations = TRUE, randomComponents = list(list(randomSlopes = TRUE, 
                                                                                       value = "contBinom")), value = "facFive"))
  options$randomVariables <- "facFive"
  options$samplingVariable1 <- list()
  options$samplingVariable2 <- list()
  options$show <- "mmeans"
  options$showFE <- TRUE
  options$showRE <- TRUE
  options$summaryCI <- 0.7
  options$trendsContrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                       values = list()))
  options$trendsTrend <- list()
  options$warmup <- 500
  set.seed(1)
  results <- jasptools::run("MixedModelsBLMM", "debug.csv", options)
  
  
  test_that("Fixed Effects Estimates table results match", {
    table <- results[["results"]][["FEsummary"]][["data"]]
    expect_equal_tables(table,
                        list(-0.12165108951337, -0.293768575679256, 648.955037781113, 0.998763045559602,
                             0.162832897455234, "Intercept", 0.0293025236696265, -0.143664092724055,
                             -0.374649722222156, 872.690766470973, 0.998418672969414, 0.224210368852301,
                             "contBinom", 0.0966780909154852))
  })
  
  test_that("facFive: Correlation Estimates table results match", {
    table <- results[["results"]][["REsummary"]][["collection"]][["REsummary_CE1"]][["data"]]
    expect_equal_tables(table,
                        list(1, "Intercept", -0.321346801844539, 1, "contBinom"))
  })
  
  test_that("Residual Variance Estimates table results match", {
    table <- results[["results"]][["REsummary"]][["collection"]][["REsummary_RES"]][["data"]]
    expect_equal_tables(table,
                        list(1.06393444856987, 1.03147198147593))
  })
  
  test_that("facFive: Variance Estimates table results match", {
    table <- results[["results"]][["REsummary"]][["collection"]][["REsummary_VE1"]][["data"]]
    expect_equal_tables(table,
                        list(0.187542035802691, 0.433061237936035, "Intercept", 0.189476407607874,
                             0.435288878341583, "contBinom"))
  })
  
  test_that("Intercept table results match", {
    table <- results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_1"]][["data"]]
    expect_equal_tables(table,
                        list(662.60163246803, 292.159174795872, -0.12165108951337, -0.293768575679256,
                             0.998966268787717, 0.150674719708029, 0.0293025236696265))
  })
  
  test_that("contBinom (trend) table results match", {
    table <- results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_2"]][["data"]]
    expect_equal_tables(table,
                        list(917.432770730613, 372.264532224382, -0.143664092724055, -0.374649722222156,
                             1.00372509955491, 0.230252726049724, 0.0966780909154852))
  })
}
#### estimated marginal means + contrasts ####

#### estimated trends + contrasts ####
